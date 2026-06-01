#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../Game.fs"

open Chess.Game
open Chess.Types

let sampleMove =
    { From = { File = 4; Rank = 6 }
      To = { File = 4; Rank = 4 }
      Promotion = None }

let game minutes =
    startGame 0.0 (VsComputer White) minutes

let withPly ply (model: Model) =
    { model with MoveHistory = List.replicate ply sampleMove }

let oneMinuteOpening = game 1 |> engineThinkTimeLimitMs
let tenMinuteOpening = game 10 |> engineThinkTimeLimitMs
let tenMinuteMiddleGame = game 10 |> withPly 24 |> engineThinkTimeLimitMs

let lowTime =
    { game 10 with
        Clock =
            { WhiteRemainingMs = 10000.0
              BlackRemainingMs = 10000.0
              LastTickMs = Some 0.0
              Started = true } }
    |> withPly 24
    |> engineThinkTimeLimitMs

if oneMinuteOpening > 2500.0 then
    failwithf "One-minute opening budget is too large: %.0f" oneMinuteOpening

if tenMinuteMiddleGame <= 10000.0 then
    failwithf "Expected a ten-second middle-game budget: %.0f" tenMinuteMiddleGame

if tenMinuteMiddleGame <= tenMinuteOpening then
    failwith "Middle-game budget should grow after the opening"

if lowTime > 800.0 then
    failwithf "Low-time budget should shrink automatically: %.0f" lowTime

printfn
    "oneMinuteOpening=%.0fms tenMinuteOpening=%.0fms tenMinuteMiddleGame=%.0fms lowTime=%.0fms"
    oneMinuteOpening
    tenMinuteOpening
    tenMinuteMiddleGame
    lowTime
