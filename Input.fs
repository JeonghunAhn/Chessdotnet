module Chess.Input

open Browser.Types
open Chess.Board
open Chess.Types

let mouseEventToSquare (canvas: HTMLCanvasElement) (event: MouseEvent) =
    let rect = canvas.getBoundingClientRect ()
    let scaleX = float canvas.width / rect.width
    let scaleY = float canvas.height / rect.height
    let x = (event.clientX - rect.left) * scaleX
    let y = (event.clientY - rect.top) * scaleY

    let square =
        { File = int (floor (x / squareSize))
          Rank = int (floor (y / squareSize)) }

    if isInside square then Some square else None
