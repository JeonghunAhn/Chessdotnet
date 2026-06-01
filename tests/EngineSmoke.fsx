#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../tools/PstTuner/generated/GeneratedPst.fs"
#load "../Evaluation.fs"
#load "../Engine.fs"

open Chess.Board
open Chess.Engine
open Chess.Types

let model = initialModel ()

let result =
    chooseMove
        4
        model.Board
        model.CastlingRights
        model.EnPassantTarget
        model.SideToMove
    |> Option.defaultWith (fun () -> failwith "Expected an opening move")

if result.Nodes <= 0 then
    failwith "Expected searched nodes"

if result.Cutoffs <= 0 then
    failwith "Expected alpha-beta cutoffs"

let expectedOpeningMoves =
    [ { File = 6; Rank = 7 }, { File = 5; Rank = 5 }
      { File = 3; Rank = 6 }, { File = 3; Rank = 4 }
      { File = 4; Rank = 6 }, { File = 4; Rank = 4 }
      { File = 2; Rank = 6 }, { File = 2; Rank = 4 } ]

if expectedOpeningMoves |> List.contains (result.Move.From, result.Move.To) |> not then
    failwithf "Expected a principled opening move, received %A" result.Move

printfn
    "move=%A score=%d nodes=%d cutoffs=%d"
    result.Move
    result.Score
    result.Nodes
    result.Cutoffs
