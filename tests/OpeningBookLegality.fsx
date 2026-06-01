#load "../Types.fs"
#load "../Board.fs"
#load "../Rules.fs"
#load "../OpeningBook.fs"

open Chess.Board
open Chess.OpeningBook
open Chess.Rules
open Chess.Types

type Position =
    { Board: Board
      SideToMove: Color
      CastlingRights: CastlingRights
      EnPassantTarget: Square option }

let square file rank =
    { File = int file - int 'a'
      Rank = 8 - int (string rank) }

let move (notation: string) =
    { From = square notation.[0] notation.[1]
      To = square notation.[2] notation.[3]
      Promotion = None }

let moveName move =
    let squareName square =
        sprintf "%c%d" (char (int 'a' + square.File)) (8 - square.Rank)

    sprintf "%s%s" (squareName move.From) (squareName move.To)

let initial = initialModel ()

let initialPosition =
    { Board = initial.Board
      SideToMove = initial.SideToMove
      CastlingRights = initial.CastlingRights
      EnPassantTarget = initial.EnPassantTarget }

let legalMoves position =
    allLegalMoves position.Board position.CastlingRights position.EnPassantTarget position.SideToMove

let advance position move =
    { Board = applyMove position.Board position.EnPassantTarget move
      SideToMove = opposite position.SideToMove
      CastlingRights = updateCastlingRights position.Board move position.CastlingRights
      EnPassantTarget = nextEnPassantTarget position.Board move }

let replay (history: string) =
    history.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.fold (fun position notation ->
        let nextMove = move notation

        if legalMoves position |> List.contains nextMove |> not then
            failwithf "Illegal history move %s in %s" notation history

        advance position nextMove) initialPosition

for history, candidate in continuations do
    let position = replay history

    if legalMoves position |> List.contains candidate |> not then
        failwithf "Illegal continuation %s after %s" (moveName candidate) history

printfn "entries=%d continuations=%d maximumPly=%d" entryCount continuations.Length maximumPly
