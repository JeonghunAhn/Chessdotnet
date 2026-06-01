module Chess.Rules

open Chess.Board
open Chess.Types

let opposite color =
    match color with
    | White -> Black
    | Black -> White

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

let hasInsufficientMaterial board =
    let remainingPieces =
        board
        |> Array.mapi (fun index piece ->
            piece
            |> Option.map (fun value ->
                value,
                { File = index % boardSize
                  Rank = index / boardSize }))
        |> Array.choose id
        |> Array.filter (fun (piece, _) -> piece.Kind <> King)
        |> Array.toList

    match remainingPieces with
    | [] -> true
    | [ piece, _ ] ->
        piece.Kind = Bishop || piece.Kind = Knight
    | [ firstPiece, firstSquare; secondPiece, secondSquare ] ->
        firstPiece.Kind = Bishop
        && secondPiece.Kind = Bishop
        && (firstSquare.File + firstSquare.Rank) % 2 = (secondSquare.File + secondSquare.Rank) % 2
    | _ -> false

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

let private jumpTargets board color fromSquare offsets =
    offsets
    |> List.map (fun (fileDelta, rankDelta) -> offset fromSquare fileDelta rankDelta)
    |> List.filter (fun square -> isInside square && isNotOwnPiece board color square)

let private rayTargets board color fromSquare directions =
    let walk (fileDelta, rankDelta) =
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

let private knightOffsets =
    [ -2, -1
      -2, 1
      -1, -2
      -1, 2
      1, -2
      1, 2
      2, -1
      2, 1 ]

let private bishopDirections =
    [ -1, -1
      -1, 1
      1, -1
      1, 1 ]

let private rookDirections =
    [ 0, -1
      0, 1
      -1, 0
      1, 0 ]

let private kingOffsets =
    [ -1, -1
      -1, 0
      -1, 1
      0, -1
      0, 1
      1, -1
      1, 0
      1, 1 ]

let private containsSquare square squares =
    squares |> List.exists (fun candidate -> candidate = square)

let isSquareAttacked board attackerColor square =
    let pawnDirection =
        match attackerColor with
        | White -> -1
        | Black -> 1

    let pawnAttackers =
        [ offset square -1 -pawnDirection
          offset square 1 -pawnDirection ]

    let hasPiece kind candidate =
        match pieceAt board candidate with
        | Some piece -> piece.Color = attackerColor && piece.Kind = kind
        | None -> false

    let rayHasAttacker directions kinds =
        directions
        |> List.exists (fun (fileDelta, rankDelta) ->
            let rec walk current =
                let next = offset current fileDelta rankDelta

                if not (isInside next) then
                    false
                else
                    match pieceAt board next with
                    | None -> walk next
                    | Some piece ->
                        piece.Color = attackerColor
                        && (kinds |> List.exists (fun kind -> kind = piece.Kind))

            walk square)

    (pawnAttackers |> List.exists (hasPiece Pawn))
    || (knightOffsets
        |> List.map (fun (fileDelta, rankDelta) -> offset square fileDelta rankDelta)
        |> List.exists (hasPiece Knight))
    || (kingOffsets
        |> List.map (fun (fileDelta, rankDelta) -> offset square fileDelta rankDelta)
        |> List.exists (hasPiece King))
    || rayHasAttacker bishopDirections [ Bishop; Queen ]
    || rayHasAttacker rookDirections [ Rook; Queen ]

let private tryFindKing (board: Board) color =
    [ 0 .. boardSize * boardSize - 1 ]
    |> List.tryPick (fun index ->
        match board.[index] with
        | Some piece when piece.Color = color && piece.Kind = King ->
            Some
                { File = index % boardSize
                  Rank = index / boardSize }
        | _ -> None)

let isKingInCheck board color =
    match tryFindKing board color with
    | Some kingSquare -> isSquareAttacked board (opposite color) kingSquare
    | None -> true

let private normalMove fromSquare toSquare =
    { From = fromSquare
      To = toSquare
      Promotion = None }

let private promotionMoves fromSquare toSquare =
    [ Queen; Rook; Bishop; Knight ]
    |> List.map (fun kind ->
        { From = fromSquare
          To = toSquare
          Promotion = Some kind })

let private pawnMoves board enPassantTarget color fromSquare =
    let direction, startRank, promotionRank =
        match color with
        | White -> -1, 6, 0
        | Black -> 1, 1, 7

    let makePawnMoves target =
        if target.Rank = promotionRank then
            promotionMoves fromSquare target
        else
            [ normalMove fromSquare target ]

    let oneForward = offset fromSquare 0 direction
    let twoForward = offset fromSquare 0 (direction * 2)

    let forwardMoves =
        if isInside oneForward && isEmpty board oneForward then
            let oneForwardMoves = makePawnMoves oneForward

            if fromSquare.Rank = startRank && isInside twoForward && isEmpty board twoForward then
                oneForwardMoves @ [ normalMove fromSquare twoForward ]
            else
                oneForwardMoves
        else
            []

    let captureMoves =
        [ offset fromSquare -1 direction
          offset fromSquare 1 direction ]
        |> List.filter (fun target ->
            isInside target
            && (hasEnemy board color target || enPassantTarget = Some target))
        |> List.collect makePawnMoves

    forwardMoves @ captureMoves

let private hasRook board color square =
    match pieceAt board square with
    | Some piece -> piece.Color = color && piece.Kind = Rook
    | None -> false

let private castlingMoves board castlingRights color fromSquare =
    let homeRank, canKingSide, canQueenSide =
        match color with
        | White -> 7, castlingRights.WhiteKingSide, castlingRights.WhiteQueenSide
        | Black -> 0, castlingRights.BlackKingSide, castlingRights.BlackQueenSide

    let enemy = opposite color
    let home = { File = 4; Rank = homeRank }

    if fromSquare <> home || isSquareAttacked board enemy home then
        []
    else
        let kingSide =
            let through = { File = 5; Rank = homeRank }
            let destination = { File = 6; Rank = homeRank }
            let rook = { File = 7; Rank = homeRank }

            if canKingSide
               && hasRook board color rook
               && isEmpty board through
               && isEmpty board destination
               && not (isSquareAttacked board enemy through)
               && not (isSquareAttacked board enemy destination) then
                [ normalMove fromSquare destination ]
            else
                []

        let queenSide =
            let rook = { File = 0; Rank = homeRank }
            let emptyB = { File = 1; Rank = homeRank }
            let emptyC = { File = 2; Rank = homeRank }
            let through = { File = 3; Rank = homeRank }

            if canQueenSide
               && hasRook board color rook
               && isEmpty board emptyB
               && isEmpty board emptyC
               && isEmpty board through
               && not (isSquareAttacked board enemy through)
               && not (isSquareAttacked board enemy emptyC) then
                [ normalMove fromSquare emptyC ]
            else
                []

        kingSide @ queenSide

let private pseudoLegalMovesForSquare board castlingRights enPassantTarget square =
    match pieceAt board square with
    | None -> []
    | Some piece ->
        let targets =
            match piece.Kind with
            | Pawn -> []
            | Knight -> jumpTargets board piece.Color square knightOffsets
            | Bishop -> rayTargets board piece.Color square bishopDirections
            | Rook -> rayTargets board piece.Color square rookDirections
            | Queen ->
                rayTargets board piece.Color square bishopDirections
                @ rayTargets board piece.Color square rookDirections
            | King -> jumpTargets board piece.Color square kingOffsets

        let regularMoves =
            match piece.Kind with
            | Pawn -> pawnMoves board enPassantTarget piece.Color square
            | _ -> targets |> List.map (normalMove square)

        match piece.Kind with
        | King -> regularMoves @ castlingMoves board castlingRights piece.Color square
        | _ -> regularMoves

let applyMove board enPassantTarget move =
    match pieceAt board move.From with
    | None -> board
    | Some piece ->
        let mutable next = board |> setPiece move.From None

        if piece.Kind = Pawn
           && enPassantTarget = Some move.To
           && isEmpty board move.To
           && move.From.File <> move.To.File then
            let capturedPawn =
                { File = move.To.File
                  Rank = move.From.Rank }

            next <- next |> setPiece capturedPawn None

        if piece.Kind = King && abs (move.To.File - move.From.File) = 2 then
            let rookFrom, rookTo =
                if move.To.File = 6 then
                    { File = 7; Rank = move.From.Rank }, { File = 5; Rank = move.From.Rank }
                else
                    { File = 0; Rank = move.From.Rank }, { File = 3; Rank = move.From.Rank }

            next <- next |> setPiece rookTo (pieceAt next rookFrom)
            next <- next |> setPiece rookFrom None

        let movedPiece =
            match move.Promotion with
            | Some kind -> { piece with Kind = kind }
            | None -> piece

        next |> setPiece move.To (Some movedPiece)

let private revokeRookRight square rights =
    match square.File, square.Rank with
    | 0, 7 -> { rights with WhiteQueenSide = false }
    | 7, 7 -> { rights with WhiteKingSide = false }
    | 0, 0 -> { rights with BlackQueenSide = false }
    | 7, 0 -> { rights with BlackKingSide = false }
    | _ -> rights

let updateCastlingRights board move rights =
    let afterMovedPiece =
        match pieceAt board move.From with
        | Some { Color = White; Kind = King } ->
            { rights with
                WhiteKingSide = false
                WhiteQueenSide = false }
        | Some { Color = Black; Kind = King } ->
            { rights with
                BlackKingSide = false
                BlackQueenSide = false }
        | Some { Kind = Rook } -> revokeRookRight move.From rights
        | _ -> rights

    revokeRookRight move.To afterMovedPiece

let nextEnPassantTarget board move =
    match pieceAt board move.From with
    | Some piece when piece.Kind = Pawn && abs (move.To.Rank - move.From.Rank) = 2 ->
        Some
            { File = move.From.File
              Rank = (move.From.Rank + move.To.Rank) / 2 }
    | _ -> None

let legalMovesForSquare board castlingRights enPassantTarget sideToMove square =
    match pieceAt board square with
    | Some piece when piece.Color = sideToMove ->
        pseudoLegalMovesForSquare board castlingRights enPassantTarget square
        |> List.filter (fun move ->
            let next = applyMove board enPassantTarget move
            not (isKingInCheck next sideToMove))
    | _ -> []

let allLegalMoves board castlingRights enPassantTarget sideToMove =
    [ 0 .. boardSize * boardSize - 1 ]
    |> List.collect (fun index ->
        let square =
            { File = index % boardSize
              Rank = index / boardSize }

        legalMovesForSquare board castlingRights enPassantTarget sideToMove square)

let positionKey board castlingRights enPassantTarget sideToMove =
    let boardKey =
        board
        |> Array.map (fun piece ->
            match piece with
            | Some value -> pieceKey value
            | None -> ".")
        |> String.concat ""

    let sideKey =
        match sideToMove with
        | White -> "w"
        | Black -> "b"

    let castlingKey =
        [ if castlingRights.WhiteKingSide then "K"
          if castlingRights.WhiteQueenSide then "Q"
          if castlingRights.BlackKingSide then "k"
          if castlingRights.BlackQueenSide then "q" ]
        |> String.concat ""

    let hasLegalEnPassant target =
        let pawnDirection =
            match sideToMove with
            | White -> -1
            | Black -> 1

        [ { File = target.File - 1
            Rank = target.Rank - pawnDirection }
          { File = target.File + 1
            Rank = target.Rank - pawnDirection } ]
        |> List.exists (fun fromSquare ->
            legalMovesForSquare board castlingRights enPassantTarget sideToMove fromSquare
            |> List.exists (fun move -> move.To = target))

    let enPassantKey =
        match enPassantTarget with
        | Some square when hasLegalEnPassant square -> sprintf "%d,%d" square.File square.Rank
        | _ -> "-"

    sprintf "%s %s %s %s" boardKey sideKey castlingKey enPassantKey
