module Chess.Ui

open Chess.Board
open Chess.Types

type Rect =
    { X: float
      Y: float
      Width: float
      Height: float }

let panelWidth = 240.0
let sceneWidth = canvasSize + panelWidth
let sceneHeight = canvasSize

let newGameButton =
    { X = canvasSize + 24.0
      Y = canvasSize - 72.0
      Width = panelWidth - 48.0
      Height = 44.0 }

let undoButton =
    { X = canvasSize + 24.0
      Y = canvasSize - 128.0
      Width = panelWidth - 48.0
      Height = 44.0 }

let computerMoveButton =
    { X = canvasSize + 24.0
      Y = canvasSize - 184.0
      Width = panelWidth - 48.0
      Height = 44.0 }

let playAsWhiteButton =
    { X = sceneWidth / 2.0 - 108.0
      Y = sceneHeight / 2.0 + 28.0
      Width = 216.0
      Height = 52.0 }

let playAsBlackButton =
    { X = playAsWhiteButton.X
      Y = playAsWhiteButton.Y + 64.0
      Width = 216.0
      Height = 52.0 }

let localGameButton =
    { X = playAsWhiteButton.X
      Y = playAsBlackButton.Y + 64.0
      Width = 216.0
      Height = 52.0 }

let timeControlButtons =
    let minutes = [ 1; 3; 5; 10 ]
    let width = 52.0
    let gap = 12.0
    let totalWidth = float minutes.Length * width + float (minutes.Length - 1) * gap
    let startX = (sceneWidth - totalWidth) / 2.0

    minutes
    |> List.mapi (fun index value ->
        value,
        { X = startX + float index * (width + gap)
          Y = playAsWhiteButton.Y - 64.0
          Width = width
          Height = 36.0 })

let boardPerspective model =
    match model.GameMode with
    | VsComputer Black -> Black
    | _ -> White

let boardSquareAtDisplay model rank file =
    match boardPerspective model with
    | White -> { File = file; Rank = rank }
    | Black ->
        { File = boardSize - 1 - file
          Rank = boardSize - 1 - rank }

let promotionOptions =
    let size = 72.0
    let gap = 12.0
    let totalWidth = size * 4.0 + gap * 3.0
    let startX = (canvasSize - totalWidth) / 2.0
    let y = (canvasSize - size) / 2.0

    [ Queen; Rook; Bishop; Knight ]
    |> List.mapi (fun index kind ->
        kind,
        { X = startX + float index * (size + gap)
          Y = y
          Width = size
          Height = size })

let contains x y rect =
    x >= rect.X
    && x <= rect.X + rect.Width
    && y >= rect.Y
    && y <= rect.Y + rect.Height
