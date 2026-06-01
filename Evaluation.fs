module Chess.Evaluation

open Chess.Board
open Chess.GeneratedPst
open Chess.Types

let private maximumPhase = 24

let private kindIndex kind =
    match kind with
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5

let private phaseValue kind =
    match kind with
    | Knight
    | Bishop -> 1
    | Rook -> 2
    | Queen -> 4
    | _ -> 0

let private tables kind =
    match kind with
    | Pawn -> middleGamePawn, endGamePawn
    | Knight -> middleGameKnight, endGameKnight
    | Bishop -> middleGameBishop, endGameBishop
    | Rook -> middleGameRook, endGameRook
    | Queen -> middleGameQueen, endGameQueen
    | King -> middleGameKing, endGameKing

let private normalizedSquare index color =
    match color with
    | White -> index
    | Black -> index ^^^ 56

let private pieceScore phase index piece =
    let kind = kindIndex piece.Kind
    let middleGameTable, endGameTable = tables piece.Kind
    let square = normalizedSquare index piece.Color
    let middleGame = middleGamePieceValues.[kind] + middleGameTable.[square]
    let endGame = endGamePieceValues.[kind] + endGameTable.[square]
    (middleGame * phase + endGame * (maximumPhase - phase)) / maximumPhase

let evaluate board sideToMove =
    let phase =
        board
        |> Array.choose id
        |> Array.sumBy (fun piece -> phaseValue piece.Kind)
        |> min maximumPhase

    let boardScore =
        board
        |> Array.mapi (fun index piece ->
            match piece with
            | Some value ->
                let score = pieceScore phase index value
                if value.Color = White then score else -score
            | None -> 0)
        |> Array.sum

    boardScore
    + if sideToMove = White then tempoBonus else -tempoBonus
