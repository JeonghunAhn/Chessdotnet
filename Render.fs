module Chess.Render

open Fable.Core
open Browser.Types
open Chess.Board
open Chess.Rules
open Chess.Types
open Chess.Ui

let private lightSquare = "#f0d9b5"
let private darkSquare = "#b58863"
let private selectedSquare = "rgba(246, 246, 105, 0.65)"
let private legalMoveDot = "rgba(36, 38, 38, 0.35)"
let private legalCaptureRing = "rgba(36, 38, 38, 0.5)"
let private whitePiece = "#f8f8f8"
let private blackPiece = "#1f1f1f"
let private whitePieceOutline = "#2b2520"
let private blackPieceOutline = "#f5ead8"
let private panelBackground = "#202528"
let private panelText = "#f4f1ea"
let private mutedText = "#aeb7b9"
let private activeClock = "#3f7652"
let private inactiveClock = "#343c3f"
let private buttonBackground = "#d4a74f"
let private buttonText = "#202528"
let private disabledButton = "#4a5355"
let private disabledButtonText = "#929b9d"

let private pieceSymbol piece =
    match piece.Color, piece.Kind with
    | White, King -> "\u2654"
    | White, Queen -> "\u2655"
    | White, Rook -> "\u2656"
    | White, Bishop -> "\u2657"
    | White, Knight -> "\u2658"
    | White, Pawn -> "\u2659"
    | Black, King -> "\u265A"
    | Black, Queen -> "\u265B"
    | Black, Rook -> "\u265C"
    | Black, Bishop -> "\u265D"
    | Black, Knight -> "\u265E"
    | Black, Pawn -> "\u265F"

let private colorName color =
    match color with
    | White -> "White"
    | Black -> "Black"

let private fillRect color x y width height (ctx: CanvasRenderingContext2D) =
    ctx.fillStyle <- U3.Case1 color
    ctx.fillRect (x, y, width, height)

let private fillText color font text x y (ctx: CanvasRenderingContext2D) =
    ctx.fillStyle <- U3.Case1 color
    ctx.font <- font
    ctx.textAlign <- "left"
    ctx.textBaseline <- "middle"
    ctx.fillText (text, x, y)

let private drawSymbol piece x y fontSize (ctx: CanvasRenderingContext2D) =
    ctx.font <- sprintf "%dpx Georgia, 'Times New Roman', serif" fontSize
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.lineWidth <- 2.5
    ctx.strokeStyle <- U3.Case1 (if piece.Color = White then whitePieceOutline else blackPieceOutline)
    ctx.strokeText (pieceSymbol piece, x, y)
    ctx.fillStyle <- U3.Case1 (if piece.Color = White then whitePiece else blackPiece)
    ctx.fillText (pieceSymbol piece, x, y)

let private drawSquare rank file square model ctx =
    let x = float file * squareSize
    let y = float rank * squareSize
    let squareColor = if (rank + file) % 2 = 0 then lightSquare else darkSquare

    fillRect squareColor x y squareSize squareSize ctx

    match model.SelectedSquare with
    | Some selected when selected = square ->
        fillRect selectedSquare x y squareSize squareSize ctx
    | _ -> ()

let private drawLegalTarget rank file isCapture (ctx: CanvasRenderingContext2D) =
    let x = float file * squareSize + squareSize / 2.0
    let y = float rank * squareSize + squareSize / 2.0

    ctx.beginPath ()

    if isCapture then
        ctx.lineWidth <- 4.0
        ctx.strokeStyle <- U3.Case1 legalCaptureRing
        ctx.arc (x, y, squareSize * 0.36, 0.0, System.Math.PI * 2.0)
        ctx.stroke ()
    else
        ctx.fillStyle <- U3.Case1 legalMoveDot
        ctx.arc (x, y, squareSize * 0.12, 0.0, System.Math.PI * 2.0)
        ctx.fill ()

let private drawPiece rank file piece ctx =
    let x = float file * squareSize + squareSize / 2.0
    let y = float rank * squareSize + squareSize / 2.0 + 2.0
    drawSymbol piece x y 56 ctx

let private isLegalTarget square model =
    model.LegalMoves
    |> List.exists (fun move -> move.To = square)

let private drawBoard model ctx =
    for rank in 0 .. boardSize - 1 do
        for file in 0 .. boardSize - 1 do
            let square = boardSquareAtDisplay model rank file
            drawSquare rank file square model ctx

            if isLegalTarget square model then
                drawLegalTarget rank file (pieceAt model.Board square |> Option.isSome) ctx

            match pieceAt model.Board square with
            | Some piece -> drawPiece rank file piece ctx
            | None -> ()

let private formatTime milliseconds =
    let totalSeconds = int (System.Math.Ceiling (milliseconds / 1000.0))
    sprintf "%02d:%02d" (totalSeconds / 60) (totalSeconds % 60)

let private drawClock label milliseconds isActive y ctx =
    let x = canvasSize + 24.0
    let width = panelWidth - 48.0
    let background = if isActive then activeClock else inactiveClock

    fillRect background x y width 92.0 ctx
    fillText mutedText "15px Arial, sans-serif" label (x + 16.0) (y + 22.0) ctx
    fillText panelText "bold 38px Consolas, monospace" (formatTime milliseconds) (x + 16.0) (y + 60.0) ctx

let private statusText model =
    match model.GameResult with
    | Some (Checkmate winner) -> sprintf "%s wins" (colorName winner)
    | Some Stalemate -> "Stalemate"
    | Some (Timeout winner) -> sprintf "%s wins on time" (colorName winner)
    | Some (Draw ThreefoldRepetition) -> "Draw by repetition"
    | Some (Draw FiftyMoveRule) -> "Draw by 50-move rule"
    | Some (Draw InsufficientMaterial) -> "Draw by material"
    | None when model.ComputerThinking -> "Computer is thinking"
    | None when model.PendingPromotion |> Option.isSome -> "Choose promotion"
    | None when isKingInCheck model.Board model.SideToMove -> sprintf "%s is in check" (colorName model.SideToMove)
    | None -> sprintf "%s to move" (colorName model.SideToMove)

let private drawPanel model ctx =
    fillRect panelBackground canvasSize 0.0 panelWidth sceneHeight ctx
    fillText panelText "bold 23px Arial, sans-serif" "Chess dotnet" (canvasSize + 24.0) 42.0 ctx
    fillText mutedText "15px Arial, sans-serif" (statusText model) (canvasSize + 24.0) 80.0 ctx

    match model.LastSearch with
    | Some search when search.UsedOpeningBook ->
        fillText mutedText "12px Arial, sans-serif" "Opening book" (canvasSize + 24.0) 106.0 ctx
    | Some search ->
        let summary =
            sprintf "d%d | %d nodes | %.0f ms" search.Depth search.Nodes search.ElapsedMs

        fillText mutedText "12px Arial, sans-serif" summary (canvasSize + 24.0) 106.0 ctx
    | None -> ()

    drawClock "BLACK" model.Clock.BlackRemainingMs (model.SideToMove = Black && model.GameResult.IsNone) 132.0 ctx
    drawClock "WHITE" model.Clock.WhiteRemainingMs (model.SideToMove = White && model.GameResult.IsNone) 250.0 ctx

    let canUndo = not model.History.IsEmpty && model.PendingPromotion.IsNone
    fillRect (if canUndo then buttonBackground else disabledButton) undoButton.X undoButton.Y undoButton.Width undoButton.Height ctx
    fillText
        (if canUndo then buttonText else disabledButtonText)
        "bold 16px Arial, sans-serif"
        "Undo"
        (undoButton.X + 74.0)
        (undoButton.Y + 23.0)
        ctx

    match model.GameMode with
    | LocalGame ->
        let canPlayComputerMove =
            model.GameResult.IsNone
            && model.PendingPromotion.IsNone
            && not model.ComputerThinking

        fillRect
            (if canPlayComputerMove then buttonBackground else disabledButton)
            computerMoveButton.X
            computerMoveButton.Y
            computerMoveButton.Width
            computerMoveButton.Height
            ctx
        fillText
            (if canPlayComputerMove then buttonText else disabledButtonText)
            "bold 16px Arial, sans-serif"
            "Computer move"
            (computerMoveButton.X + 38.0)
            (computerMoveButton.Y + 23.0)
            ctx
    | VsComputer humanColor ->
        fillText mutedText "13px Arial, sans-serif" "PLAYING COMPUTER" (canvasSize + 24.0) (computerMoveButton.Y + 10.0) ctx
        fillText
            panelText
            "bold 16px Arial, sans-serif"
            (sprintf "You play %s" (colorName humanColor))
            (canvasSize + 24.0)
            (computerMoveButton.Y + 34.0)
            ctx

    fillRect buttonBackground newGameButton.X newGameButton.Y newGameButton.Width newGameButton.Height ctx
    fillText buttonText "bold 16px Arial, sans-serif" "New game" (newGameButton.X + 54.0) (newGameButton.Y + 23.0) ctx

let private drawPromotion model ctx =
    match model.PendingPromotion with
    | None -> ()
    | Some _ ->
        fillRect "rgba(20, 24, 25, 0.7)" 0.0 0.0 canvasSize canvasSize ctx
        let boxX = 168.0
        let boxY = 286.0
        fillRect "#f4f1ea" boxX boxY 384.0 148.0 ctx
        fillText "#202528" "bold 18px Arial, sans-serif" "Choose promotion" (boxX + 24.0) (boxY + 26.0) ctx

        promotionOptions
        |> List.iter (fun (kind, rect) ->
            fillRect "#d8ddd9" rect.X rect.Y rect.Width rect.Height ctx

            drawSymbol
                { Color = model.SideToMove
                  Kind = kind }
                (rect.X + rect.Width / 2.0)
                (rect.Y + rect.Height / 2.0 + 2.0)
                48
                ctx)

let private drawMenu model ctx =
    fillRect panelBackground 0.0 0.0 sceneWidth sceneHeight ctx
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.fillStyle <- U3.Case1 panelText
    ctx.font <- "bold 48px Arial, sans-serif"
    ctx.fillText ("Chess dotnet", sceneWidth / 2.0, sceneHeight / 2.0 - 104.0)

    ctx.fillStyle <- U3.Case1 mutedText
    ctx.font <- "15px Arial, sans-serif"
    ctx.fillText ("Time control", sceneWidth / 2.0, playAsWhiteButton.Y - 78.0)

    timeControlButtons
    |> List.iter (fun (minutes, rect) ->
        let selected = minutes = model.TimeControlMinutes
        fillRect (if selected then buttonBackground else inactiveClock) rect.X rect.Y rect.Width rect.Height ctx
        ctx.fillStyle <- U3.Case1 (if selected then buttonText else panelText)
        ctx.font <- "bold 15px Arial, sans-serif"
        ctx.fillText (sprintf "%dm" minutes, rect.X + rect.Width / 2.0, rect.Y + rect.Height / 2.0))

    fillRect buttonBackground playAsWhiteButton.X playAsWhiteButton.Y playAsWhiteButton.Width playAsWhiteButton.Height ctx
    fillRect buttonBackground playAsBlackButton.X playAsBlackButton.Y playAsBlackButton.Width playAsBlackButton.Height ctx
    ctx.fillStyle <- U3.Case1 buttonText
    ctx.font <- "bold 18px Arial, sans-serif"
    ctx.fillText ("Play as White", playAsWhiteButton.X + playAsWhiteButton.Width / 2.0, playAsWhiteButton.Y + playAsWhiteButton.Height / 2.0)
    ctx.fillText ("Play as Black", playAsBlackButton.X + playAsBlackButton.Width / 2.0, playAsBlackButton.Y + playAsBlackButton.Height / 2.0)

    fillRect inactiveClock localGameButton.X localGameButton.Y localGameButton.Width localGameButton.Height ctx
    ctx.fillStyle <- U3.Case1 panelText
    ctx.font <- "bold 16px Arial, sans-serif"
    ctx.fillText ("Two-player game", sceneWidth / 2.0, localGameButton.Y + localGameButton.Height / 2.0)

let render model (ctx: CanvasRenderingContext2D) =
    ctx.clearRect (0.0, 0.0, sceneWidth, sceneHeight)

    match model.Scene with
    | MainMenu -> drawMenu model ctx
    | Playing ->
        drawBoard model ctx
        drawPanel model ctx
        drawPromotion model ctx
