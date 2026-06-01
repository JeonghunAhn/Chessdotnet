#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../tools/PstTuner/generated/GeneratedPst.fs"
#load "../Evaluation.fs"
#load "../Engine.fs"

open Chess.Board
open Chess.Engine
open Chess.Evaluation
open Chess.Rules
open Chess.Types
open System.Diagnostics

let model = initialModel ()

let squareName (square: Square) =
    sprintf "%c%d" (char (int 'a' + square.File)) (8 - square.Rank)

let moveName (move: Move) =
    sprintf "%s-%s" (squareName move.From) (squareName move.To)

printfn "Static scores after each opening move:"

allLegalMoves model.Board model.CastlingRights model.EnPassantTarget model.SideToMove
|> List.map (fun move ->
    move,
    evaluate (applyMove model.Board model.EnPassantTarget move) Black)
|> List.sortByDescending snd
|> List.iter (fun (move, score) -> printfn "  %s %d" (moveName move) score)

printfn ""

for depth in 1 .. 5 do
    let stopwatch = Stopwatch.StartNew()

    let result =
        chooseMove
            depth
            model.Board
            model.CastlingRights
            model.EnPassantTarget
            model.SideToMove
        |> Option.defaultWith (fun () -> failwith "Expected an opening move")

    stopwatch.Stop()

    printfn
        "depth=%d move=%s score=%d nodes=%d cutoffs=%d elapsed=%.1fms"
        depth
        (moveName result.Move)
        result.Score
        result.Nodes
        result.Cutoffs
        stopwatch.Elapsed.TotalMilliseconds
