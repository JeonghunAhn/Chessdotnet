module Chess.Input

open Browser.Types
open Chess.Board
open Chess.Types
open Chess.Ui

let mouseEventToCanvasPoint (canvas: HTMLCanvasElement) (event: MouseEvent) =
    let rect = canvas.getBoundingClientRect ()
    let scaleX = float canvas.width / rect.width
    let scaleY = float canvas.height / rect.height
    let x = (event.clientX - rect.left) * scaleX
    let y = (event.clientY - rect.top) * scaleY

    x, y

let canvasPointToSquare model x y =
    let file = int (floor (x / squareSize))
    let rank = int (floor (y / squareSize))
    let square = boardSquareAtDisplay model rank file

    if isInside square then Some square else None

let promotionChoiceAt x y =
    promotionOptions
    |> List.tryPick (fun (kind, rect) ->
        if contains x y rect then Some kind else None)

let isNewGameButton x y =
    contains x y newGameButton

let isUndoButton x y =
    contains x y undoButton

let isComputerMoveButton x y =
    contains x y computerMoveButton

let isPlayAsWhiteButton x y =
    contains x y playAsWhiteButton

let isPlayAsBlackButton x y =
    contains x y playAsBlackButton

let isLocalGameButton x y =
    contains x y localGameButton

let timeControlAt x y =
    timeControlButtons
    |> List.tryPick (fun (minutes, rect) ->
        if contains x y rect then Some minutes else None)
