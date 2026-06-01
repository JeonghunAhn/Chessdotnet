#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../OpeningBook.fs"

open Chess.Board
open Chess.OpeningBook
open Chess.Rules
open Chess.Types

let model = initialModel ()

let legalMoves =
    allLegalMoves model.Board model.CastlingRights model.EnPassantTarget model.SideToMove

let squareName (square: Square) =
    sprintf "%c%d" (char (int 'a' + square.File)) (8 - square.Rank)

let moveName (move: Move) =
    sprintf "%s%s" (squareName move.From) (squareName move.To)

let allowedMoves =
    set [ "e2e4"; "d2d4"; "g1f3"; "c2c4" ]

let selectedMoves =
    [ 1 .. 200 ]
    |> List.map (fun _ ->
        tryChooseMove legalMoves []
        |> Option.defaultWith (fun () -> failwith "Expected an opening-book move")
        |> moveName)

if selectedMoves |> List.exists (fun move -> allowedMoves |> Set.contains move |> not) then
    failwithf "Unexpected opening-book move: %A" selectedMoves

selectedMoves
|> List.countBy id
|> List.sortBy fst
|> List.iter (fun (move, count) -> printfn "%s=%d" move count)

let e4 =
    legalMoves
    |> List.find (fun move -> moveName move = "e2e4")

let afterE4 = applyMove model.Board model.EnPassantTarget e4

let blackReplies =
    allLegalMoves
        afterE4
        (updateCastlingRights model.Board e4 model.CastlingRights)
        (nextEnPassantTarget model.Board e4)
        Black

let selectedReply =
    tryChooseMove blackReplies [ e4 ]
    |> Option.defaultWith (fun () -> failwith "Expected a black opening-book reply")

printfn "reply-to-e4=%s" (moveName selectedReply)
