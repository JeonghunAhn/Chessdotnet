module Chess.Game

open Chess.Board
open Chess.Rules
open Chess.Types

let selectTimeControl minutes model =
    if model.Scene = MainMenu then
        { model with TimeControlMinutes = minutes }
    else
        model

let startGame now gameMode timeControlMinutes =
    let model = initialModel ()
    let initialPosition = positionKey model.Board model.CastlingRights model.EnPassantTarget model.SideToMove
    let remainingMs = float timeControlMinutes * 60.0 * 1000.0

    { model with
        Scene = Playing
        GameMode = gameMode
        TimeControlMinutes = timeControlMinutes
        PositionHistory = [ initialPosition ]
        Clock =
            { WhiteRemainingMs = remainingMs
              BlackRemainingMs = remainingMs
              LastTickMs = Some now
              Started = true } }

let private clearSelection model =
    { model with
        SelectedSquare = None
        LegalMoves = [] }

let private selectSquare square model =
    { model with
        SelectedSquare = Some square
        LegalMoves =
            legalMovesForSquare
                model.Board
                model.CastlingRights
                model.EnPassantTarget
                model.SideToMove
                square }

let private canSelect square model =
    match pieceAt model.Board square with
    | Some piece -> piece.Color = model.SideToMove
    | None -> false

let private movesTo square model =
    model.LegalMoves
    |> List.filter (fun move -> move.To = square)

let private updateGameResult board castlingRights enPassantTarget sideToMove halfmoveClock positionHistory =
    if allLegalMoves board castlingRights enPassantTarget sideToMove |> List.isEmpty then
        if isKingInCheck board sideToMove then
            Some (Checkmate (opposite sideToMove))
        else
            Some Stalemate
    elif hasInsufficientMaterial board then
        Some (Draw InsufficientMaterial)
    elif halfmoveClock >= 100 then
        Some (Draw FiftyMoveRule)
    else
        let currentPosition = positionKey board castlingRights enPassantTarget sideToMove
        let repetitions =
            positionHistory
            |> List.filter (fun key -> key = currentPosition)
            |> List.length

        if repetitions >= 3 then
            Some (Draw ThreefoldRepetition)
        else
            None

let private snapshot model =
    { Board = model.Board
      SideToMove = model.SideToMove
      CastlingRights = model.CastlingRights
      EnPassantTarget = model.EnPassantTarget
      GameResult = model.GameResult
      Clock = model.Clock
      HalfmoveClock = model.HalfmoveClock
      PositionHistory = model.PositionHistory
      MoveHistory = model.MoveHistory }

let private finalizeMove now move model =
    let movedPiece = pieceAt model.Board move.From
    let isCapture = pieceAt model.Board move.To |> Option.isSome
    let nextBoard = applyMove model.Board model.EnPassantTarget move
    let nextRights = updateCastlingRights model.Board move model.CastlingRights
    let nextEnPassant = nextEnPassantTarget model.Board move
    let nextSide = opposite model.SideToMove
    let nextHalfmoveClock =
        match movedPiece with
        | Some piece when piece.Kind = Pawn -> 0
        | _ when isCapture -> 0
        | _ -> model.HalfmoveClock + 1

    let nextPositionHistory =
        positionKey nextBoard nextRights nextEnPassant nextSide
        :: model.PositionHistory

    { Board = nextBoard
      SelectedSquare = None
      LegalMoves = []
      SideToMove = nextSide
      CastlingRights = nextRights
      EnPassantTarget = nextEnPassant
      PendingPromotion = None
      GameResult =
        updateGameResult
            nextBoard
            nextRights
            nextEnPassant
            nextSide
            nextHalfmoveClock
            nextPositionHistory
      Clock =
        { model.Clock with
            Started = true
            LastTickMs = Some now }
      Scene = Playing
      History = snapshot model :: model.History
      HalfmoveClock = nextHalfmoveClock
      PositionHistory = nextPositionHistory
      MoveHistory = model.MoveHistory @ [ move ]
      ComputerThinking = false
      LastSearch = model.LastSearch
      GameMode = model.GameMode
      TimeControlMinutes = model.TimeControlMinutes }

let undoMove now model =
    if model.Scene <> Playing || model.PendingPromotion |> Option.isSome then
        model
    else
        match model.History with
        | [] -> model
        | previous :: remaining ->
            { Board = previous.Board
              SelectedSquare = None
              LegalMoves = []
              SideToMove = previous.SideToMove
              CastlingRights = previous.CastlingRights
              EnPassantTarget = previous.EnPassantTarget
              PendingPromotion = None
              GameResult = previous.GameResult
              Clock =
                { previous.Clock with
                    LastTickMs = Some now }
              Scene = Playing
              History = remaining
              HalfmoveClock = previous.HalfmoveClock
              PositionHistory = previous.PositionHistory
              MoveHistory = previous.MoveHistory
              ComputerThinking = false
              LastSearch = model.LastSearch
              GameMode = model.GameMode
              TimeControlMinutes = model.TimeControlMinutes }

let isComputerTurn model =
    match model.GameMode with
    | LocalGame -> false
    | VsComputer humanColor -> model.SideToMove <> humanColor

let engineThinkTimeLimitMs model =
    let remainingMs =
        match model.SideToMove with
        | White -> model.Clock.WhiteRemainingMs
        | Black -> model.Clock.BlackRemainingMs

    let completedMoves = model.MoveHistory.Length / 2
    let expectedTurnsRemaining = max 10 (36 - completedMoves)
    let reserveMs = max 5000.0 (remainingMs * 0.12)
    let spendableMs = max 0.0 (remainingMs - reserveMs)

    let stageFactor =
        match model.MoveHistory.Length with
        | ply when ply < 12 -> 0.7
        | ply when ply < 40 -> 1.35
        | ply when ply < 70 -> 1.15
        | _ -> 0.9

    let timeControlCapMs =
        match model.TimeControlMinutes with
        | minutes when minutes <= 1 -> 2500.0
        | minutes when minutes <= 3 -> 7000.0
        | minutes when minutes <= 5 -> 12000.0
        | _ -> 18000.0

    let lowTimeCapMs = max 250.0 (remainingMs * 0.08)
    let fairShareMs = spendableMs / float expectedTurnsRemaining

    fairShareMs * stageFactor
    |> max 250.0
    |> min timeControlCapMs
    |> min lowTimeCapMs

let undoTurn now model =
    let previous = undoMove now model

    match previous.GameMode with
    | VsComputer humanColor when previous.SideToMove <> humanColor && not previous.History.IsEmpty ->
        undoMove now previous
    | _ -> previous

let tick now model =
    match model.Scene, model.GameResult, model.Clock.Started, model.Clock.LastTickMs with
    | MainMenu, _, _, _
    | _, Some _, _, _
    | _, _, false, _
    | _, _, _, None -> model
    | Playing, None, true, Some lastTick ->
        let elapsed = max 0.0 (now - lastTick)

        match model.SideToMove with
        | White ->
            let remaining = max 0.0 (model.Clock.WhiteRemainingMs - elapsed)

            { model with
                Clock =
                    { model.Clock with
                        WhiteRemainingMs = remaining
                        LastTickMs = Some now }
                GameResult =
                    if remaining <= 0.0 then
                        Some (Timeout Black)
                    else
                        None }
        | Black ->
            let remaining = max 0.0 (model.Clock.BlackRemainingMs - elapsed)

            { model with
                Clock =
                    { model.Clock with
                        BlackRemainingMs = remaining
                        LastTickMs = Some now }
                GameResult =
                    if remaining <= 0.0 then
                        Some (Timeout White)
                    else
                        None }

let handleSquareClick now square model =
    if model.Scene <> Playing
       || model.GameResult |> Option.isSome
       || model.PendingPromotion |> Option.isSome
       || model.ComputerThinking
       || isComputerTurn model then
        model
    else
        match model.SelectedSquare with
        | None ->
            if canSelect square model then
                selectSquare square model
            else
                model
        | Some selected when selected = square ->
            clearSelection model
        | Some _ ->
            match movesTo square model with
            | [] ->
                if canSelect square model then
                    selectSquare square model
                else
                    clearSelection model
            | [ move ] -> finalizeMove now move model
            | promotionMoves ->
                { model with
                    PendingPromotion = Some { Moves = promotionMoves } }

let choosePromotion now kind model =
    match model.PendingPromotion with
    | None -> model
    | Some pending ->
        match pending.Moves |> List.tryFind (fun move -> move.Promotion = Some kind) with
        | Some move -> finalizeMove now move model
        | None -> model

let canRequestComputerMove model =
    model.Scene = Playing
    && model.GameResult.IsNone
    && model.PendingPromotion.IsNone
    && not model.ComputerThinking
    && (model.GameMode = LocalGame || isComputerTurn model)

let beginComputerMove model =
    if canRequestComputerMove model then
        { model with
            SelectedSquare = None
            LegalMoves = []
            ComputerThinking = true }
    else
        model

let cancelComputerMove model =
    { model with ComputerThinking = false }

let completeComputerMove now move summary model =
    if model.Scene <> Playing
       || model.GameResult |> Option.isSome
       || model.PendingPromotion |> Option.isSome
       || not model.ComputerThinking then
        model
    else
        { finalizeMove now move model with
            LastSearch = Some summary }
