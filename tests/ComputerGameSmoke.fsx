#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../Game.fs"
#load "../Ui.fs"

open Chess.Board
open Chess.Game
open Chess.Types
open Chess.Ui

let summary =
    { UsedOpeningBook = true
      Depth = 0
      Nodes = 0
      QuiescenceNodes = 0
      Cutoffs = 0
      TranspositionHits = 0
      ElapsedMs = 0.0 }

let square file rank =
    { File = file
      Rank = rank }

let menu = initialModel ()
let threeMinuteMenu = selectTimeControl 3 menu

if threeMinuteMenu.TimeControlMinutes <> 3 then
    failwith "Menu should keep the selected time control"

let whiteGame = startGame 0.0 (VsComputer White) 3

if whiteGame.Clock.WhiteRemainingMs <> 180000.0 || whiteGame.Clock.BlackRemainingMs <> 180000.0 then
    failwith "Selected time control should initialize both clocks"

if isComputerTurn whiteGame then
    failwith "White player should move first"

let afterWhiteMove =
    whiteGame
    |> handleSquareClick 10.0 (square 4 6)
    |> handleSquareClick 20.0 (square 4 4)

if not (isComputerTurn afterWhiteMove) then
    failwith "Computer should respond after the white player's move"

let blackReply =
    { From = square 4 1
      To = square 4 3
      Promotion = None }

let afterBlackReply =
    afterWhiteMove
    |> beginComputerMove
    |> tick 720.0
    |> completeComputerMove 720.0 blackReply summary

if afterBlackReply.Clock.BlackRemainingMs <> 179300.0 then
    failwithf "Computer thinking time should reduce its clock, received %.0f" afterBlackReply.Clock.BlackRemainingMs

if isComputerTurn afterBlackReply || afterBlackReply.SideToMove <> White then
    failwith "White player should move after the computer reply"

let afterUndo = undoTurn 40.0 afterBlackReply

if afterUndo.MoveHistory <> [] || afterUndo.SideToMove <> White then
    failwith "Undo should rewind the complete player turn"

let blackGame = startGame 0.0 (VsComputer Black) 10

if not (isComputerTurn blackGame) then
    failwith "Computer should open when the player chooses black"

let topLeft = boardSquareAtDisplay blackGame 0 0

if topLeft <> square 7 7 then
    failwithf "Expected a flipped board for black, received %A" topLeft

printfn "computer-game-flow=ok"
