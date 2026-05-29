module Chess.Game

open Chess.Board
open Chess.Rules
open Chess.Types

let private opposite color =
    match color with
    | White -> Black
    | Black -> White

let private isLegalTarget square model =
    model.LegalTargets
    |> List.exists (fun target -> target = square)

let private clearSelection model =
    { model with
        SelectedSquare = None
        LegalTargets = [] }

let private selectSquare square model =
    { model with
        SelectedSquare = Some square
        LegalTargets = legalTargetsForSquare model.Board square model.SideToMove }

let private canSelect square model =
    match pieceAt model.Board square with
    | Some piece -> piece.Color = model.SideToMove
    | None -> false

let handleSquareClick square model =
    match model.SelectedSquare with
    | None ->
        if canSelect square model then
            selectSquare square model
        else
            model
    | Some selected when selected = square ->
        clearSelection model
    | Some selected when isLegalTarget square model ->
        { Board = movePiece selected square model.Board
          SelectedSquare = None
          LegalTargets = []
          SideToMove = opposite model.SideToMove }
    | Some _ ->
        if canSelect square model then
            selectSquare square model
        else
            clearSelection model
