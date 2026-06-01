module Chess.Engine

open System
open System.Collections.Generic
open Chess.Board
open Chess.Evaluation
open Chess.Rules
open Chess.Types

type SearchResult =
    { Move: Move
      Score: int
      Depth: int
      Nodes: int
      QuiescenceNodes: int
      Cutoffs: int
      TranspositionHits: int
      ElapsedMs: float }

type private Position =
    { Board: Board
      SideToMove: Color
      CastlingRights: CastlingRights
      EnPassantTarget: Square option }

type private Bound =
    | Exact
    | Lower
    | Upper

type private TranspositionEntry =
    { Depth: int
      Score: int
      Bound: Bound
      BestMove: Move option }

exception private SearchDeadlineReached

let private mateScore = 1_000_000
let private maximumQuiescenceDepth = 6

let private nowMilliseconds () =
    DateTimeOffset.UtcNow.ToUnixTimeMilliseconds() |> float

let private pieceValue kind =
    match kind with
    | Pawn -> 100
    | Knight -> 320
    | Bishop -> 330
    | Rook -> 500
    | Queen -> 900
    | King -> 20_000

let private pieceKey piece =
    match piece.Color, piece.Kind with
    | White, King -> "K"
    | White, Queen -> "Q"
    | White, Rook -> "R"
    | White, Bishop -> "B"
    | White, Knight -> "N"
    | White, Pawn -> "P"
    | Black, King -> "k"
    | Black, Queen -> "q"
    | Black, Rook -> "r"
    | Black, Bishop -> "b"
    | Black, Knight -> "n"
    | Black, Pawn -> "p"

let private positionKey position =
    let boardKey =
        position.Board
        |> Array.map (fun piece -> piece |> Option.map pieceKey |> Option.defaultValue ".")
        |> String.concat ""

    let sideKey = if position.SideToMove = White then "w" else "b"

    let castlingKey =
        [ if position.CastlingRights.WhiteKingSide then "K"
          if position.CastlingRights.WhiteQueenSide then "Q"
          if position.CastlingRights.BlackKingSide then "k"
          if position.CastlingRights.BlackQueenSide then "q" ]
        |> String.concat ""

    let enPassantKey =
        position.EnPassantTarget
        |> Option.map (fun square -> sprintf "%d,%d" square.File square.Rank)
        |> Option.defaultValue "-"

    sprintf "%s %s %s %s" boardKey sideKey castlingKey enPassantKey

let private isCapture position move =
    pieceAt position.Board move.To |> Option.isSome
    || position.EnPassantTarget = Some move.To

let private isTactical position move =
    isCapture position move || move.Promotion.IsSome

let private movePriority preferredMove position move =
    let preferredScore = if preferredMove = Some move then 1_000_000 else 0

    let captureScore =
        match pieceAt position.Board move.To with
        | Some captured ->
            let attacker =
                pieceAt position.Board move.From
                |> Option.map (fun piece -> pieceValue piece.Kind)
                |> Option.defaultValue 0

            pieceValue captured.Kind * 10 - attacker
        | None when position.EnPassantTarget = Some move.To -> pieceValue Pawn * 10
        | None -> 0

    let promotionScore =
        move.Promotion
        |> Option.map pieceValue
        |> Option.defaultValue 0

    preferredScore + captureScore + promotionScore

let private orderedMoves preferredMove position =
    allLegalMoves
        position.Board
        position.CastlingRights
        position.EnPassantTarget
        position.SideToMove
    |> List.sortByDescending (movePriority preferredMove position)

let private advance position move =
    { Board = applyMove position.Board position.EnPassantTarget move
      SideToMove = opposite position.SideToMove
      CastlingRights = updateCastlingRights position.Board move position.CastlingRights
      EnPassantTarget = nextEnPassantTarget position.Board move }

let private evaluateForSide position =
    let whiteScore = evaluate position.Board position.SideToMove
    if position.SideToMove = White then whiteScore else -whiteScore

let private search maximumDepth deadline board castlingRights enPassantTarget sideToMove =
    let root =
        { Board = board
          SideToMove = sideToMove
          CastlingRights = castlingRights
          EnPassantTarget = enPassantTarget }

    let startedAt = nowMilliseconds ()
    let table = Dictionary<string, TranspositionEntry>()
    let mutable nodes = 0
    let mutable quiescenceNodes = 0
    let mutable cutoffs = 0
    let mutable transpositionHits = 0

    let checkDeadline () =
        if nodes &&& 255 = 0 then
            match deadline with
            | Some value when nowMilliseconds () >= value -> raise SearchDeadlineReached
            | _ -> ()

    let visitNode () =
        nodes <- nodes + 1
        checkDeadline ()

    let rec quiescence remainingDepth ply alpha beta position =
        visitNode ()
        quiescenceNodes <- quiescenceNodes + 1

        if hasInsufficientMaterial position.Board then
            0
        else
            let inCheck = isKingInCheck position.Board position.SideToMove
            let moves = orderedMoves None position

            match moves with
            | [] ->
                if inCheck then -mateScore + ply else 0
            | _ when remainingDepth = 0 -> evaluateForSide position
            | _ ->
                let standPat = evaluateForSide position

                if not inCheck && standPat >= beta then
                    standPat
                else
                    let currentAlpha =
                        if inCheck then alpha else max alpha standPat

                    let candidates =
                        if inCheck then moves else moves |> List.filter (isTactical position)

                    let rec searchMoves bestAlpha remaining =
                        match remaining with
                        | [] -> bestAlpha
                        | move :: tail ->
                            let score =
                                -quiescence
                                    (remainingDepth - 1)
                                    (ply + 1)
                                    -beta
                                    -bestAlpha
                                    (advance position move)

                            if score >= beta then
                                cutoffs <- cutoffs + 1
                                score
                            else
                                searchMoves (max bestAlpha score) tail

                    searchMoves currentAlpha candidates

    let rec negamax remainingDepth ply alpha beta position =
        if remainingDepth = 0 then
            quiescence maximumQuiescenceDepth ply alpha beta position
        else
            visitNode ()

            if hasInsufficientMaterial position.Board then
                0
            else
                let key = positionKey position
                let mutable cachedEntry = Unchecked.defaultof<TranspositionEntry>
                let hasCachedEntry = table.TryGetValue(key, &cachedEntry)

                let cachedScore =
                    if hasCachedEntry && cachedEntry.Depth >= remainingDepth then
                        match cachedEntry.Bound with
                        | Exact -> Some cachedEntry.Score
                        | Lower when cachedEntry.Score >= beta -> Some cachedEntry.Score
                        | Upper when cachedEntry.Score <= alpha -> Some cachedEntry.Score
                        | _ -> None
                    else
                        None

                match cachedScore with
                | Some score ->
                    transpositionHits <- transpositionHits + 1
                    score
                | None ->
                    let preferredMove =
                        if hasCachedEntry then cachedEntry.BestMove else None

                    match orderedMoves preferredMove position with
                    | [] ->
                        if isKingInCheck position.Board position.SideToMove then
                            -mateScore + ply
                        else
                            0
                    | moves ->
                        let originalAlpha = alpha

                        let rec searchMoves bestScore bestMove currentAlpha remaining =
                            match remaining with
                            | [] -> bestScore, bestMove
                            | move :: tail ->
                                let score =
                                    -negamax
                                        (remainingDepth - 1)
                                        (ply + 1)
                                        -beta
                                        -currentAlpha
                                        (advance position move)

                                let nextBestScore, nextBestMove =
                                    if score > bestScore then score, Some move else bestScore, bestMove

                                let nextAlpha = max currentAlpha score

                                if nextAlpha >= beta then
                                    cutoffs <- cutoffs + 1
                                    nextBestScore, nextBestMove
                                else
                                    searchMoves nextBestScore nextBestMove nextAlpha tail

                        let score, bestMove = searchMoves -mateScore None alpha moves

                        let bound =
                            if score <= originalAlpha then Upper
                            elif score >= beta then Lower
                            else Exact

                        table.[key] <-
                            { Depth = remainingDepth
                              Score = score
                              Bound = bound
                              BestMove = bestMove }

                        score

    let searchDepth depth preferredMove =
        let moves = orderedMoves preferredMove root

        let rec searchMoves bestMove bestScore alpha remaining =
            match remaining with
            | [] -> bestMove, bestScore
            | move :: tail ->
                let score = -negamax (depth - 1) 1 -mateScore -alpha (advance root move)

                if score > bestScore then
                    searchMoves (Some move) score (max alpha score) tail
                else
                    searchMoves bestMove bestScore alpha tail

        searchMoves None -mateScore -mateScore moves

    let mutable completedResult: (Move * int * int) option = None
    let mutable preferredMove = None
    let mutable timedOut = false
    let mutable depth = 1

    while depth <= maximumDepth && not timedOut do
        try
            let move, score = searchDepth depth preferredMove

            match move with
            | Some value ->
                completedResult <- Some(value, score, depth)
                preferredMove <- Some value
            | None -> ()

            depth <- depth + 1
        with SearchDeadlineReached ->
            timedOut <- true

    completedResult
    |> Option.map (fun (move, score, completedDepth) ->
        { Move = move
          Score = score
          Depth = completedDepth
          Nodes = nodes
          QuiescenceNodes = quiescenceNodes
          Cutoffs = cutoffs
          TranspositionHits = transpositionHits
          ElapsedMs = nowMilliseconds () - startedAt })

let chooseMove depth board castlingRights enPassantTarget sideToMove =
    search depth None board castlingRights enPassantTarget sideToMove

let chooseMoveWithin maxDepth timeLimitMs board castlingRights enPassantTarget sideToMove =
    let deadline = nowMilliseconds () + timeLimitMs
    search maxDepth (Some deadline) board castlingRights enPassantTarget sideToMove
