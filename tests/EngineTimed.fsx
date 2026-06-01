#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../tools/PstTuner/generated/GeneratedPst.fs"
#load "../Evaluation.fs"
#load "../Engine.fs"

open Chess.Board
open Chess.Engine

let model = initialModel ()

let result =
    chooseMoveWithin
        8
        2000.0
        model.Board
        model.CastlingRights
        model.EnPassantTarget
        model.SideToMove
    |> Option.defaultWith (fun () -> failwith "Expected an opening move")

if result.Depth < 1 || result.Depth > 8 then
    failwithf "Unexpected completed depth: %d" result.Depth

if result.QuiescenceNodes <= 0 then
    failwith "Expected quiescence nodes"

if result.TranspositionHits <= 0 then
    failwith "Expected transposition hits"

printfn
    "depth=%d nodes=%d qnodes=%d cutoffs=%d ttHits=%d elapsed=%.1fms"
    result.Depth
    result.Nodes
    result.QuiescenceNodes
    result.Cutoffs
    result.TranspositionHits
    result.ElapsedMs
