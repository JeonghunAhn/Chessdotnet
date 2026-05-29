module Program

open Browser.Dom
open Browser.Types
open Chess.Board
open Chess.Game
open Chess.Input
open Chess.Render

let canvas =
    document.getElementById("board") :?> HTMLCanvasElement

let ctx =
    canvas.getContext_2d()

let mutable model = initialModel ()

let redraw () =
    render model ctx

canvas.addEventListener (
    "click",
    fun event ->
        match mouseEventToSquare canvas (event :?> MouseEvent) with
        | Some square ->
            model <- handleSquareClick square model
            redraw ()
        | None -> ()
)

redraw ()
