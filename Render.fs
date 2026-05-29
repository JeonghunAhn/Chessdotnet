module Chess.Render

open Fable.Core
open Browser.Types
open Chess.Board
open Chess.Types

let private lightSquare = "#f0d9b5"
let private darkSquare = "#b58863"
let private selectedSquare = "rgba(246, 246, 105, 0.65)"
let private legalMoveDot = "rgba(36, 38, 38, 0.35)"
let private legalCaptureRing = "rgba(36, 38, 38, 0.5)"
let private whitePiece = "#f8f8f8"
let private blackPiece = "#1f1f1f"
let private whitePieceOutline = "#2b2520"
let private blackPieceOutline = "#f5ead8"

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

let private fillRect color x y width height (ctx: CanvasRenderingContext2D) =
    ctx.fillStyle <- U3.Case1 color
    ctx.fillRect (x, y, width, height)

let private drawSquare rank file model ctx =
    let x = float file * squareSize
    let y = float rank * squareSize
    let squareColor = if (rank + file) % 2 = 0 then lightSquare else darkSquare

    fillRect squareColor x y squareSize squareSize ctx

    match model.SelectedSquare with
    | Some selected when selected.File = file && selected.Rank = rank ->
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

let private drawPiece rank file piece (ctx: CanvasRenderingContext2D) =
    let x = float file * squareSize + squareSize / 2.0
    let y = float rank * squareSize + squareSize / 2.0 + 2.0

    ctx.font <- "56px Georgia, 'Times New Roman', serif"
    ctx.textAlign <- "center"
    ctx.textBaseline <- "middle"
    ctx.lineWidth <- 2.5
    ctx.strokeStyle <- U3.Case1 (if piece.Color = White then whitePieceOutline else blackPieceOutline)
    ctx.strokeText (pieceSymbol piece, x, y)
    ctx.fillStyle <- U3.Case1 (if piece.Color = White then whitePiece else blackPiece)
    ctx.fillText (pieceSymbol piece, x, y)

let render model (ctx: CanvasRenderingContext2D) =
    ctx.clearRect (0.0, 0.0, canvasSize, canvasSize)

    for rank in 0 .. boardSize - 1 do
        for file in 0 .. boardSize - 1 do
            drawSquare rank file model ctx

            let square = { File = file; Rank = rank }

            if model.LegalTargets |> List.exists (fun target -> target = square) then
                drawLegalTarget rank file (pieceAt model.Board square |> Option.isSome) ctx

            match pieceAt model.Board square with
            | Some piece -> drawPiece rank file piece ctx
            | None -> ()
