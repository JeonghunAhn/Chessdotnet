module Chess.OpeningBook

open System
open Chess.Types

type private WeightedMove =
    { Move: Move
      Weight: int }

let private square (file: char) (rank: char) =
    { File = int file - int 'a'
      Rank = 8 - int (string rank) }

let private move (notation: string) =
    { From = square notation.[0] notation.[1]
      To = square notation.[2] notation.[3]
      Promotion = None }

let private weighted notation weight =
    { Move = move notation
      Weight = weight }

let private moveKey move =
    let squareKey square =
        sprintf "%c%d" (char (int 'a' + square.File)) (8 - square.Rank)

    sprintf "%s%s" (squareKey move.From) (squareKey move.To)

let private historyKey history =
    history
    |> List.map moveKey
    |> String.concat " "

let private entries =
    [ "", [ weighted "e2e4" 45; weighted "d2d4" 30; weighted "g1f3" 15; weighted "c2c4" 10 ]

      "e2e4", [ weighted "c7c5" 30; weighted "e7e5" 30; weighted "e7e6" 12; weighted "c7c6" 10; weighted "d7d6" 10; weighted "d7d5" 8 ]
      "e2e4 e7e5", [ weighted "g1f3" 65; weighted "f1c4" 20; weighted "b1c3" 15 ]
      "e2e4 e7e5 g1f3", [ weighted "b8c6" 55; weighted "g8f6" 25; weighted "d7d6" 20 ]
      "e2e4 e7e5 g1f3 b8c6", [ weighted "f1b5" 48; weighted "f1c4" 42; weighted "d2d4" 10 ]
      "e2e4 e7e5 g1f3 b8c6 f1b5", [ weighted "a7a6" 55; weighted "g8f6" 30; weighted "f7f5" 15 ]
      "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6", [ weighted "b5a4" 82; weighted "b5c6" 18 ]
      "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4", [ weighted "g8f6" 70; weighted "f8e7" 30 ]
      "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6", [ weighted "e1g1" 72; weighted "d2d3" 28 ]
      "e2e4 e7e5 g1f3 b8c6 f1b5 a7a6 b5a4 g8f6 e1g1", [ weighted "f8e7" 60; weighted "b7b5" 40 ]
      "e2e4 e7e5 g1f3 b8c6 f1c4", [ weighted "g8f6" 55; weighted "f8c5" 30; weighted "d7d6" 15 ]
      "e2e4 e7e5 g1f3 b8c6 f1c4 g8f6", [ weighted "d2d3" 68; weighted "f3g5" 20; weighted "d2d4" 12 ]
      "e2e4 e7e5 g1f3 b8c6 f1c4 g8f6 d2d3", [ weighted "f8c5" 62; weighted "f8e7" 22; weighted "d7d6" 16 ]
      "e2e4 e7e5 g1f3 b8c6 f1c4 g8f6 d2d3 f8c5", [ weighted "e1g1" 62; weighted "c2c3" 38 ]

      "e2e4 c7c5", [ weighted "g1f3" 58; weighted "b1c3" 25; weighted "c2c3" 17 ]
      "e2e4 c7c5 g1f3", [ weighted "d7d6" 35; weighted "b8c6" 35; weighted "e7e6" 30 ]
      "e2e4 c7c5 g1f3 d7d6", [ weighted "d2d4" 80; weighted "f1b5" 20 ]
      "e2e4 c7c5 g1f3 d7d6 d2d4", [ weighted "c5d4" 100 ]
      "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4", [ weighted "f3d4" 100 ]
      "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4", [ weighted "g8f6" 72; weighted "b8c6" 28 ]
      "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6", [ weighted "b1c3" 82; weighted "f2f3" 18 ]
      "e2e4 c7c5 g1f3 d7d6 d2d4 c5d4 f3d4 g8f6 b1c3", [ weighted "a7a6" 62; weighted "g7g6" 22; weighted "e7e6" 16 ]
      "e2e4 c7c5 g1f3 b8c6", [ weighted "d2d4" 60; weighted "f1b5" 25; weighted "b1c3" 15 ]
      "e2e4 c7c5 g1f3 b8c6 d2d4", [ weighted "c5d4" 100 ]
      "e2e4 c7c5 g1f3 b8c6 d2d4 c5d4", [ weighted "f3d4" 100 ]
      "e2e4 c7c5 g1f3 b8c6 d2d4 c5d4 f3d4", [ weighted "g8f6" 72; weighted "e7e5" 28 ]
      "e2e4 c7c5 g1f3 b8c6 d2d4 c5d4 f3d4 g8f6", [ weighted "b1c3" 100 ]
      "e2e4 c7c5 g1f3 e7e6", [ weighted "d2d4" 72; weighted "c2c3" 28 ]
      "e2e4 c7c5 g1f3 e7e6 d2d4", [ weighted "c5d4" 100 ]
      "e2e4 c7c5 g1f3 e7e6 d2d4 c5d4", [ weighted "f3d4" 100 ]
      "e2e4 c7c5 g1f3 e7e6 d2d4 c5d4 f3d4", [ weighted "b8c6" 62; weighted "a7a6" 38 ]

      "e2e4 e7e6", [ weighted "d2d4" 82; weighted "b1c3" 18 ]
      "e2e4 e7e6 d2d4", [ weighted "d7d5" 100 ]
      "e2e4 e7e6 d2d4 d7d5", [ weighted "b1c3" 52; weighted "e4e5" 36; weighted "b1d2" 12 ]
      "e2e4 e7e6 d2d4 d7d5 b1c3", [ weighted "g8f6" 56; weighted "f8b4" 44 ]
      "e2e4 e7e6 d2d4 d7d5 e4e5", [ weighted "c7c5" 62; weighted "b7b6" 20; weighted "g8e7" 18 ]
      "e2e4 c7c6", [ weighted "d2d4" 88; weighted "b1c3" 12 ]
      "e2e4 c7c6 d2d4", [ weighted "d7d5" 100 ]
      "e2e4 c7c6 d2d4 d7d5", [ weighted "e4e5" 58; weighted "b1c3" 34; weighted "e4d5" 8 ]
      "e2e4 c7c6 d2d4 d7d5 e4e5", [ weighted "c8f5" 72; weighted "c8g4" 28 ]
      "e2e4 d7d6", [ weighted "d2d4" 58; weighted "b1c3" 42 ]
      "e2e4 d7d6 d2d4", [ weighted "g8f6" 72; weighted "g7g6" 28 ]
      "e2e4 d7d6 d2d4 g8f6", [ weighted "b1c3" 72; weighted "f2f3" 28 ]
      "e2e4 d7d5", [ weighted "e4d5" 88; weighted "b1c3" 12 ]
      "e2e4 d7d5 e4d5", [ weighted "d8d5" 72; weighted "g8f6" 28 ]
      "e2e4 d7d5 e4d5 d8d5", [ weighted "b1c3" 82; weighted "g1f3" 18 ]

      "d2d4", [ weighted "g8f6" 42; weighted "d7d5" 38; weighted "e7e6" 12; weighted "f7f5" 8 ]
      "d2d4 g8f6", [ weighted "c2c4" 60; weighted "g1f3" 25; weighted "c1f4" 15 ]
      "d2d4 g8f6 c2c4", [ weighted "e7e6" 38; weighted "g7g6" 32; weighted "c7c5" 18; weighted "e7e5" 12 ]
      "d2d4 g8f6 c2c4 e7e6", [ weighted "b1c3" 58; weighted "g1f3" 42 ]
      "d2d4 g8f6 c2c4 e7e6 b1c3", [ weighted "f8b4" 66; weighted "d7d5" 34 ]
      "d2d4 g8f6 c2c4 g7g6", [ weighted "b1c3" 58; weighted "g1f3" 24; weighted "g2g3" 18 ]
      "d2d4 g8f6 c2c4 g7g6 b1c3", [ weighted "f8g7" 100 ]
      "d2d4 g8f6 c2c4 g7g6 b1c3 f8g7", [ weighted "e2e4" 72; weighted "g1f3" 28 ]
      "d2d4 g8f6 c2c4 g7g6 b1c3 f8g7 e2e4", [ weighted "d7d6" 100 ]
      "d2d4 g8f6 c1f4", [ weighted "d7d5" 46; weighted "e7e6" 34; weighted "g7g6" 20 ]
      "d2d4 g8f6 c1f4 d7d5", [ weighted "e2e3" 68; weighted "g1f3" 32 ]
      "d2d4 d7d5", [ weighted "c2c4" 60; weighted "g1f3" 25; weighted "c1f4" 15 ]
      "d2d4 d7d5 c2c4", [ weighted "e7e6" 48; weighted "c7c6" 32; weighted "e7e5" 20 ]
      "d2d4 d7d5 c2c4 e7e6", [ weighted "b1c3" 52; weighted "g1f3" 48 ]
      "d2d4 d7d5 c2c4 e7e6 b1c3", [ weighted "g8f6" 68; weighted "c7c5" 32 ]
      "d2d4 d7d5 c2c4 c7c6", [ weighted "g1f3" 56; weighted "b1c3" 44 ]
      "d2d4 d7d5 c2c4 c7c6 g1f3", [ weighted "g8f6" 100 ]
      "d2d4 d7d5 c1f4", [ weighted "g8f6" 58; weighted "c7c5" 24; weighted "e7e6" 18 ]

      "g1f3", [ weighted "d7d5" 38; weighted "g8f6" 34; weighted "c7c5" 18; weighted "g7g6" 10 ]
      "g1f3 d7d5", [ weighted "d2d4" 45; weighted "c2c4" 35; weighted "g2g3" 20 ]
      "g1f3 g8f6", [ weighted "d2d4" 44; weighted "c2c4" 36; weighted "g2g3" 20 ]
      "g1f3 g8f6 g2g3", [ weighted "g7g6" 55; weighted "d7d5" 45 ]
      "g1f3 g8f6 g2g3 g7g6", [ weighted "f1g2" 100 ]

      "c2c4", [ weighted "e7e5" 34; weighted "g8f6" 30; weighted "e7e6" 20; weighted "c7c5" 16 ]
      "c2c4 e7e5", [ weighted "b1c3" 62; weighted "g1f3" 25; weighted "g2g3" 13 ]
      "c2c4 e7e5 b1c3", [ weighted "g8f6" 58; weighted "b8c6" 42 ]
      "c2c4 e7e5 b1c3 g8f6", [ weighted "g2g3" 48; weighted "g1f3" 34; weighted "e2e4" 18 ]
      "c2c4 g8f6", [ weighted "b1c3" 42; weighted "g1f3" 33; weighted "d2d4" 25 ]
      "c2c4 g8f6 b1c3", [ weighted "g7g6" 44; weighted "e7e5" 34; weighted "e7e6" 22 ]
      "c2c4 g8f6 b1c3 g7g6", [ weighted "g2g3" 62; weighted "e2e4" 38 ]
      "c2c4 g8f6 b1c3 g7g6 g2g3", [ weighted "f8g7" 100 ] ]
    |> Map.ofList

let entryCount = entries.Count

let maximumPly =
    entries
    |> Map.toSeq
    |> Seq.map (fun (history, _) ->
        if String.IsNullOrWhiteSpace history then 1
        else history.Split(' ').Length + 1)
    |> Seq.max

let continuations =
    entries
    |> Map.toList
    |> List.collect (fun (history, candidates) ->
        candidates
        |> List.map (fun candidate -> history, candidate.Move))

let private random = Random()

let tryChooseMove legalMoves moveHistory =
    let key = historyKey moveHistory

    match entries |> Map.tryFind key with
    | None -> None
    | Some candidates ->
        let legalCandidates =
            candidates
            |> List.filter (fun candidate -> legalMoves |> List.contains candidate.Move)

        let totalWeight = legalCandidates |> List.sumBy (fun candidate -> candidate.Weight)

        if totalWeight = 0 then
            None
        else
            let selectedWeight = random.Next totalWeight

            let rec choose remaining candidates =
                match candidates with
                | [] -> None
                | candidate :: tail ->
                    if remaining < candidate.Weight then
                        Some candidate.Move
                    else
                        choose (remaining - candidate.Weight) tail

            choose selectedWeight legalCandidates
