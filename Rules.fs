module Chess.Rules

open Chess.Board
open Chess.Types

let private offset square fileDelta rankDelta =
    { File = square.File + fileDelta
      Rank = square.Rank + rankDelta }

let private isEmpty board square =
    pieceAt board square |> Option.isNone

let private hasEnemy board color square =
    match pieceAt board square with
    | Some piece -> piece.Color <> color
    | None -> false

let private isNotOwnPiece board color square =
    match pieceAt board square with
    | Some piece -> piece.Color <> color
    | None -> true

let private jumpMoves board color fromSquare offsets =
    offsets
    |> List.map (fun (fileDelta, rankDelta) -> offset fromSquare fileDelta rankDelta)
    |> List.filter (fun square -> isInside square && isNotOwnPiece board color square)

let private rayMoves board color fromSquare directions =
    let walk direction =
        let fileDelta, rankDelta = direction

        let rec loop current acc =
            let next = offset current fileDelta rankDelta

            if not (isInside next) then
                List.rev acc
            else
                match pieceAt board next with
                | None -> loop next (next :: acc)
                | Some piece when piece.Color <> color -> List.rev (next :: acc)
                | Some _ -> List.rev acc

        loop fromSquare []

    directions |> List.collect walk

let private pawnMoves board color fromSquare =
    let direction =
        match color with
        | White -> -1
        | Black -> 1

    let startRank =
        match color with
        | White -> 6
        | Black -> 1

    let oneForward = offset fromSquare 0 direction
    let twoForward = offset fromSquare 0 (direction * 2)

    let forwardMoves =
        if isInside oneForward && isEmpty board oneForward then
            if fromSquare.Rank = startRank && isInside twoForward && isEmpty board twoForward then
                [ oneForward; twoForward ]
            else
                [ oneForward ]
        else
            []

    let captureMoves =
        [ offset fromSquare -1 direction
          offset fromSquare 1 direction ]
        |> List.filter (fun square -> isInside square && hasEnemy board color square)

    forwardMoves @ captureMoves

let private knightMoves board color fromSquare =
    [ -2, -1
      -2, 1
      -1, -2
      -1, 2
      1, -2
      1, 2
      2, -1
      2, 1 ]
    |> jumpMoves board color fromSquare

let private bishopMoves board color fromSquare =
    [ -1, -1
      -1, 1
      1, -1
      1, 1 ]
    |> rayMoves board color fromSquare

let private rookMoves board color fromSquare =
    [ 0, -1
      0, 1
      -1, 0
      1, 0 ]
    |> rayMoves board color fromSquare

let private queenMoves board color fromSquare =
    bishopMoves board color fromSquare @ rookMoves board color fromSquare

let private kingMoves board color fromSquare =
    [ -1, -1
      -1, 0
      -1, 1
      0, -1
      0, 1
      1, -1
      1, 0
      1, 1 ]
    |> jumpMoves board color fromSquare

let legalTargetsForSquare board square sideToMove =
    match pieceAt board square with
    | Some piece when piece.Color = sideToMove ->
        match piece.Kind with
        | Pawn -> pawnMoves board piece.Color square
        | Knight -> knightMoves board piece.Color square
        | Bishop -> bishopMoves board piece.Color square
        | Rook -> rookMoves board piece.Color square
        | Queen -> queenMoves board piece.Color square
        | King -> kingMoves board piece.Color square
    | _ -> []
