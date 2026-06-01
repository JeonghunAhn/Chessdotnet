module PstTuner

(*
PST tuner for CC0 Lichess Stockfish position evaluations.

Accepted samples are cached in data/samples.jsonl, so interrupted collection can
continue later. The default filters require Stockfish depth 18 or higher,
centipawn scores between -1500 and 1500, and valid positions with both kings.
A phase-and-score bucket cap keeps common position classes from dominating.

Quick generation:
  dotnet run --project PstTuner.fsproj -- --samples 5000 --epochs 18

Larger cache collection should stay conservative because the Hugging Face
Dataset Viewer API rate-limits bursts:
  dotnet run --project PstTuner.fsproj -- --samples 200000 --epochs 18 --workers 2 --delay-ms 1000

Generated output:
  generated/GeneratedPst.fs
  generated/report.md

Cached files under data/ are intentionally ignored by Git.
Source: https://huggingface.co/datasets/Lichess/chess-position-evaluations
License: CC0-1.0
*)

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading.Tasks

type PieceKind =
    | Pawn = 0
    | Knight = 1
    | Bishop = 2
    | Rook = 3
    | Queen = 4
    | King = 5

type Sample =
    { Fen: string
      Cp: float
      Phase: int
      Features: (int * float) array }

type Settings =
    { SampleCount: int
      Epochs: int
      Seed: int
      Workers: int
      DelayMilliseconds: int
      MinimumDepth: int
      MaximumAbsoluteCp: int
      CachePath: string
      OutputPath: string
      ReportPath: string }

let featureCount = 6 * 64 * 2 + 1
let tempoFeature = featureCount - 1
let maximumPhase = 24
let datasetRows = 844812067
let rowsPerRequest = 100
let bucketCount = 15

let ensureDirectory (path: string) =
    let directory = Path.GetDirectoryName path

    if not (String.IsNullOrWhiteSpace directory) then
        Directory.CreateDirectory directory |> ignore

let pieceInfo (character: char) =
    let isWhite = Char.IsUpper character

    let kind =
        match Char.ToLowerInvariant character with
        | 'p' -> PieceKind.Pawn
        | 'n' -> PieceKind.Knight
        | 'b' -> PieceKind.Bishop
        | 'r' -> PieceKind.Rook
        | 'q' -> PieceKind.Queen
        | 'k' -> PieceKind.King
        | _ -> failwithf "Unknown FEN piece: %c" character

    kind, isWhite

let phaseValue (kind: PieceKind) =
    match kind with
    | PieceKind.Knight
    | PieceKind.Bishop -> 1
    | PieceKind.Rook -> 2
    | PieceKind.Queen -> 4
    | _ -> 0

let parseFen (fen: string) (cp: float) : Sample option =
    let sections = fen.Split(' ', StringSplitOptions.RemoveEmptyEntries)

    if sections.Length < 2 then
        None
    else
        let ranks = sections.[0].Split('/')

        if ranks.Length <> 8 then
            None
        else
            let pieces = ResizeArray<int * PieceKind * bool>()
            let mutable valid = true
            let mutable whiteKings = 0
            let mutable blackKings = 0

            for rank in 0 .. 7 do
                let mutable file = 0

                for character in ranks.[rank] do
                    if Char.IsDigit character then
                        file <- file + int character - int '0'
                    else
                        let kind, isWhite = pieceInfo character

                        if kind = PieceKind.King then
                            if isWhite then
                                whiteKings <- whiteKings + 1
                            else
                                blackKings <- blackKings + 1

                        if file >= 8 then
                            valid <- false
                        else
                            pieces.Add(rank * 8 + file, kind, isWhite)

                        file <- file + 1

                if file <> 8 then
                    valid <- false

            if not valid || whiteKings <> 1 || blackKings <> 1 then
                None
            else
                let phase =
                    pieces
                    |> Seq.sumBy (fun (_, kind, _) -> phaseValue kind)
                    |> min maximumPhase

                let middleGameWeight = float phase / float maximumPhase
                let endGameWeight = 1.0 - middleGameWeight
                let features = ResizeArray<int * float>()

                for square, kind, isWhite in pieces do
                    let normalizedSquare = if isWhite then square else square ^^^ 56
                    let sign = if isWhite then 1.0 else -1.0
                    let pieceOffset = int kind * 64 + normalizedSquare
                    features.Add(pieceOffset, sign * middleGameWeight)
                    features.Add(6 * 64 + pieceOffset, sign * endGameWeight)

                let sideToMove = sections.[1]
                features.Add(tempoFeature, if sideToMove = "w" then 1.0 else -1.0)

                Some
                    { Fen = fen
                      Cp = cp
                      Phase = phase
                      Features = features.ToArray() }

let phaseBucket (phase: int) =
    if phase >= 20 then 0
    elif phase >= 10 then 1
    else 2

let scoreBucket (cp: float) =
    if cp <= -250.0 then 0
    elif cp <= -80.0 then 1
    elif cp < 80.0 then 2
    elif cp < 250.0 then 3
    else 4

let bucketIndex (sample: Sample) =
    phaseBucket sample.Phase * 5 + scoreBucket sample.Cp

let sampleToJson (sample: Sample) =
    JsonSerializer.Serialize({| fen = sample.Fen; cp = int sample.Cp |})

let trySampleFromJson (line: string) =
    try
        use document = JsonDocument.Parse line
        let root = document.RootElement
        let fen = root.GetProperty("fen").GetString()
        let cp = root.GetProperty("cp").GetInt32()
        parseFen fen (float cp)
    with _ ->
        None

let readCachedSamples (settings: Settings) =
    if File.Exists settings.CachePath then
        printfn "Reading cached samples from %s" settings.CachePath

        File.ReadLines settings.CachePath
        |> Seq.choose trySampleFromJson
        |> Seq.toArray
    else
        [||]

let tryReadApiRows (maximumAbsoluteCp: int) (minimumDepth: int) (json: string) =
    let samples = ResizeArray<Sample>()

    use document = JsonDocument.Parse json

    for item in document.RootElement.GetProperty("rows").EnumerateArray() do
        let row = item.GetProperty("row")
        let cpProperty = row.GetProperty("cp")

        if cpProperty.ValueKind = JsonValueKind.Number then
            let cp = cpProperty.GetInt32()
            let depth = row.GetProperty("depth").GetInt32()

            if abs cp <= maximumAbsoluteCp && depth >= minimumDepth then
                let fen = row.GetProperty("fen").GetString()

                match parseFen fen (float cp) with
                | Some sample -> samples.Add sample
                | None -> ()

    samples.ToArray()

let collectSamples (settings: Settings) (cachedSamples: Sample array) : Task<Sample array> =
    task {
        let maximumPerBucket = int (Math.Ceiling(float settings.SampleCount * 0.2))
        let buckets = Array.init bucketCount (fun _ -> ResizeArray<Sample>())
        let seen = HashSet<string>()
        let gate = obj ()
        let mutable accepted = 0
        let mutable requested = 0
        let mutable failed = 0
        let mutable completed = false

        let tryAdd (sample: Sample) =
            lock gate (fun () ->
                let bucket = bucketIndex sample

                if buckets.[bucket].Count < maximumPerBucket && seen.Add sample.Fen then
                    buckets.[bucket].Add sample
                    accepted <- accepted + 1

                    if accepted >= settings.SampleCount then
                        completed <- true)

        for sample in cachedSamples do
            tryAdd sample

        if completed then
            return
                buckets
                |> Array.collect (fun bucket -> bucket.ToArray())
                |> Array.take settings.SampleCount
        else
            printfn "Collecting diverse samples: %d total, up to %d per bucket" settings.SampleCount maximumPerBucket
            ensureDirectory settings.CachePath

            use writer = new StreamWriter(settings.CachePath, true, Encoding.UTF8)
            use client = new HttpClient()
            client.Timeout <- TimeSpan.FromSeconds 30.0

            let random = Random settings.Seed
            let randomGate = obj ()
            let rateGate = obj ()
            let mutable nextRequestAt = DateTimeOffset.MinValue
            let mutable blockedUntil = DateTimeOffset.MinValue

            let nextOffset () =
                lock randomGate (fun () -> random.Next(0, datasetRows - rowsPerRequest))

            let waitForApiSlot () =
                task {
                    let delay =
                        lock rateGate (fun () ->
                            let now = DateTimeOffset.UtcNow
                            let requestAt = max now (max nextRequestAt blockedUntil)
                            nextRequestAt <- requestAt.AddMilliseconds(float settings.DelayMilliseconds)
                            max 0 (int (Math.Ceiling((requestAt - now).TotalMilliseconds))))

                    if delay > 0 then
                        do! Task.Delay delay
                }

            let pauseAfterFailure () =
                lock rateGate (fun () ->
                    let retryAt = DateTimeOffset.UtcNow.AddSeconds 15.0

                    if retryAt > blockedUntil then
                        blockedUntil <- retryAt)

            let fetchWorker (workerId: int) =
                task {
                    while not completed do
                        do! waitForApiSlot ()
                        let offset = nextOffset ()
                        let url =
                            sprintf
                                "https://datasets-server.huggingface.co/rows?dataset=Lichess%%2Fchess-position-evaluations&config=default&split=train&offset=%d&length=%d"
                                offset
                                rowsPerRequest

                        try
                            let! json = client.GetStringAsync url
                            let samples = tryReadApiRows settings.MaximumAbsoluteCp settings.MinimumDepth json

                            lock gate (fun () ->
                                requested <- requested + rowsPerRequest

                                for sample in samples do
                                    let bucket = bucketIndex sample

                                    if buckets.[bucket].Count < maximumPerBucket && seen.Add sample.Fen then
                                        buckets.[bucket].Add sample
                                        writer.WriteLine(sampleToJson sample)
                                        accepted <- accepted + 1

                                writer.Flush()

                                if accepted >= settings.SampleCount then
                                    completed <- true

                                if requested % 10000 = 0 then
                                    let counts =
                                        buckets
                                        |> Array.map (fun bucket -> bucket.Count.ToString())
                                        |> String.concat ","

                                    printfn "rows=%d accepted=%d failed=%d buckets=[%s]" requested accepted failed counts)
                        with error ->
                            pauseAfterFailure ()

                            lock gate (fun () ->
                                failed <- failed + 1

                                if failed % 10 = 0 then
                                    printfn "API retries=%d latest=%s worker=%d" failed error.Message workerId)

                            do! Task.Delay settings.DelayMilliseconds
                }

            let workers = [| for workerId in 1 .. settings.Workers -> fetchWorker workerId |]
            let! _ = Task.WhenAll workers

            return
                buckets
                |> Array.collect (fun bucket -> bucket.ToArray())
                |> Array.take settings.SampleCount
    }

let predict (weights: float array) (sample: Sample) =
    sample.Features
    |> Array.sumBy (fun (index, value) -> weights.[index] * value)

let calculateMetrics (weights: float array) (samples: Sample array) =
    let mutable absoluteError = 0.0
    let mutable squaredError = 0.0

    for sample in samples do
        let error = predict weights sample - sample.Cp
        absoluteError <- absoluteError + abs error
        squaredError <- squaredError + error * error

    absoluteError / float samples.Length,
    Math.Sqrt(squaredError / float samples.Length)

let initializeWeights () =
    let values = [| 100.0; 320.0; 330.0; 500.0; 900.0; 0.0 |]
    let weights = Array.zeroCreate<float> featureCount

    for phaseOffset in [ 0; 6 * 64 ] do
        for kind in 0 .. 5 do
            for square in 0 .. 63 do
                weights.[phaseOffset + kind * 64 + square] <- values.[kind]

    weights.[tempoFeature] <- 10.0
    weights

let shuffle (random: Random) (items: 'T array) =
    for index in items.Length - 1 .. -1 .. 1 do
        let other = random.Next(index + 1)
        let value = items.[index]
        items.[index] <- items.[other]
        items.[other] <- value

let train (settings: Settings) (trainSamples: Sample array) (validationSamples: Sample array) =
    let weights = initializeWeights ()
    let firstMoment = Array.zeroCreate<float> featureCount
    let secondMoment = Array.zeroCreate<float> featureCount
    let random = Random(settings.Seed + 1)
    let learningRate = 0.5
    let beta1 = 0.9
    let beta2 = 0.999
    let epsilon = 0.00000001
    let lambda = 0.00001
    let mutable step = 0

    let baselineMae, baselineRmse = calculateMetrics weights validationSamples
    let mutable bestWeights = Array.copy weights
    let mutable bestValidationMae = baselineMae
    let mutable bestEpoch = 0
    printfn "Baseline validation MAE=%.2f RMSE=%.2f" baselineMae baselineRmse

    for epoch in 1 .. settings.Epochs do
        shuffle random trainSamples

        for sample in trainSamples do
            step <- step + 1
            let error = predict weights sample - sample.Cp
            let gradientScale = max -600.0 (min 600.0 error)
            let beta1Power = Math.Pow(beta1, float step)
            let beta2Power = Math.Pow(beta2, float step)

            for index, featureValue in sample.Features do
                let gradient = gradientScale * featureValue + lambda * weights.[index]
                firstMoment.[index] <- beta1 * firstMoment.[index] + (1.0 - beta1) * gradient
                secondMoment.[index] <- beta2 * secondMoment.[index] + (1.0 - beta2) * gradient * gradient
                let correctedFirst = firstMoment.[index] / (1.0 - beta1Power)
                let correctedSecond = secondMoment.[index] / (1.0 - beta2Power)
                weights.[index] <- weights.[index] - learningRate * correctedFirst / (Math.Sqrt correctedSecond + epsilon)

        let trainMae, trainRmse = calculateMetrics weights trainSamples
        let validationMae, validationRmse = calculateMetrics weights validationSamples

        if validationMae < bestValidationMae then
            bestWeights <- Array.copy weights
            bestValidationMae <- validationMae
            bestEpoch <- epoch

        printfn
            "epoch=%02d train MAE=%.2f RMSE=%.2f validation MAE=%.2f RMSE=%.2f"
            epoch
            trainMae
            trainRmse
            validationMae
            validationRmse

    bestWeights, baselineMae, baselineRmse, bestEpoch

let pieceNames =
    [| "Pawn"; "Knight"; "Bishop"; "Rook"; "Queen"; "King" |]

let centeredTable (weights: float array) (offset: int) (kind: int) =
    let raw = [| for square in 0 .. 63 -> weights.[offset + kind * 64 + square] |]
    let mean = raw |> Array.average
    int (Math.Round mean),
    raw |> Array.map (fun value -> int (Math.Round(value - mean)))

let writeArray (builder: StringBuilder) (name: string) (values: int array) =
    builder.AppendLine(sprintf "let %s =" name).AppendLine("    [|") |> ignore

    values
    |> Array.chunkBySize 8
    |> Array.iter (fun row ->
        row
        |> Array.map string
        |> String.concat "; "
        |> sprintf "        %s"
        |> builder.AppendLine
        |> ignore)

    builder.AppendLine("    |]").AppendLine() |> ignore

let writeOutput
    (settings: Settings)
    (weights: float array)
    (baselineMae: float)
    (baselineRmse: float)
    (validationMae: float)
    (validationRmse: float)
    (selectedEpoch: int)
    (samples: int)
    =
    ensureDirectory settings.OutputPath
    ensureDirectory settings.ReportPath

    let builder = StringBuilder()
    builder.AppendLine("module Chess.GeneratedPst").AppendLine() |> ignore
    builder.AppendLine("// Generated from CC0 Lichess Stockfish evaluation data by tools/PstTuner.").AppendLine() |> ignore

    let middleGameValues = ResizeArray<int>()
    let endGameValues = ResizeArray<int>()

    for kind in 0 .. 5 do
        let middleGameValue, middleGameTable = centeredTable weights 0 kind
        let endGameValue, endGameTable = centeredTable weights (6 * 64) kind
        middleGameValues.Add middleGameValue
        endGameValues.Add endGameValue
        writeArray builder (sprintf "middleGame%s" pieceNames.[kind]) middleGameTable
        writeArray builder (sprintf "endGame%s" pieceNames.[kind]) endGameTable

    writeArray builder "middleGamePieceValues" (middleGameValues.ToArray())
    writeArray builder "endGamePieceValues" (endGameValues.ToArray())
    builder.AppendLine(sprintf "let tempoBonus = %d" (int (Math.Round weights.[tempoFeature]))) |> ignore

    File.WriteAllText(settings.OutputPath, builder.ToString())

    let report =
        [ "# Generated PST Report"
          ""
          sprintf "- Samples: %d" samples
          sprintf "- Epochs: %d" settings.Epochs
          sprintf "- Selected epoch: %d" selectedEpoch
          sprintf "- Minimum Stockfish depth: %d" settings.MinimumDepth
          sprintf "- Maximum absolute centipawn score: %d" settings.MaximumAbsoluteCp
          sprintf "- Baseline validation MAE: %.2f" baselineMae
          sprintf "- Baseline validation RMSE: %.2f" baselineRmse
          sprintf "- Tuned validation MAE: %.2f" validationMae
          sprintf "- Tuned validation RMSE: %.2f" validationRmse
          ""
          "Source: https://huggingface.co/datasets/Lichess/chess-position-evaluations"
          "License: CC0-1.0" ]
        |> String.concat Environment.NewLine

    File.WriteAllText(settings.ReportPath, report)

let parseArguments (arguments: string array) =
    let valueAfter (name: string) (fallback: string) =
        arguments
        |> Array.tryFindIndex ((=) name)
        |> Option.bind (fun index ->
            if index + 1 < arguments.Length then Some arguments.[index + 1] else None)
        |> Option.defaultValue fallback

    { SampleCount = valueAfter "--samples" "200000" |> int
      Epochs = valueAfter "--epochs" "18" |> int
      Seed = valueAfter "--seed" "20260601" |> int
      Workers = valueAfter "--workers" "2" |> int
      DelayMilliseconds = valueAfter "--delay-ms" "1000" |> int
      MinimumDepth = valueAfter "--min-depth" "18" |> int
      MaximumAbsoluteCp = valueAfter "--max-cp" "1500" |> int
      CachePath = valueAfter "--cache" "data/samples.jsonl"
      OutputPath = valueAfter "--output" "generated/GeneratedPst.fs"
      ReportPath = valueAfter "--report" "generated/report.md" }

[<EntryPoint>]
let main arguments =
    try
        let settings = parseArguments arguments
        let cachedSamples = readCachedSamples settings
        let samples = collectSamples settings cachedSamples |> fun task -> task.GetAwaiter().GetResult()
        let random = Random(settings.Seed + 2)
        shuffle random samples
        let validationCount = max 1 (samples.Length / 10)
        let validationSamples = samples.[0 .. validationCount - 1]
        let trainSamples = samples.[validationCount ..]
        let weights, baselineMae, baselineRmse, selectedEpoch = train settings trainSamples validationSamples
        let validationMae, validationRmse = calculateMetrics weights validationSamples
        writeOutput settings weights baselineMae baselineRmse validationMae validationRmse selectedEpoch samples.Length
        printfn "Generated %s" settings.OutputPath
        printfn "Generated %s" settings.ReportPath
        0
    with error ->
        eprintfn "%s" (error.ToString())
        1
