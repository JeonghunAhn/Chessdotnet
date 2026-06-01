module Program

open Browser.Dom
open Browser.Types
open Fable.Core
open Chess.Board
open Chess.Game
open Chess.Input
open Chess.Render
open Chess.Types

[<AllowNullLiteral>]
type EngineWorker =
    abstract postMessage: message: obj -> unit
    abstract onmessage: (MessageEvent -> unit) with get, set

[<Emit("new Worker(new URL('./EngineWorker.fs.js', import.meta.url), { type: 'module' })")>]
let createEngineWorker () : EngineWorker =
    jsNative

let canvas =
    document.getElementById("board") :?> HTMLCanvasElement

let ctx =
    canvas.getContext_2d()

let mutable model = initialModel ()
let engineWorker = createEngineWorker ()
let mutable nextRequestId = 0
let mutable activeRequestId = None
let mutable activeRequestStartedAt = None

let minimumOpeningBookThinkMs = 700.0

let now () =
    JS.Constructors.Date.now ()

let redraw () =
    render model ctx

let invalidateComputerRequest () =
    nextRequestId <- nextRequestId + 1
    activeRequestId <- None
    activeRequestStartedAt <- None
    model <- cancelComputerMove model

let requestComputerMove () =
    if canRequestComputerMove model then
        nextRequestId <- nextRequestId + 1
        let requestId = nextRequestId
        activeRequestId <- Some requestId
        activeRequestStartedAt <- Some (now ())
        model <- beginComputerMove model
        let thinkTimeLimitMs = engineThinkTimeLimitMs model

        engineWorker.postMessage (
            box
                { RequestId = requestId
                  Board = model.Board
                  SideToMove = model.SideToMove
                  CastlingRights = model.CastlingRights
                  EnPassantTarget = model.EnPassantTarget
                  MoveHistory = model.MoveHistory
                  MaxDepth = 8
                  TimeLimitMs = thinkTimeLimitMs }
        )

let requestComputerMoveIfNeeded () =
    if isComputerTurn model then
        requestComputerMove ()

let completeEngineResponse response =
    if activeRequestId = Some response.RequestId then
        activeRequestId <- None
        activeRequestStartedAt <- None
        let currentTime = now ()
        model <- tick currentTime model

        match response.Move, model.GameResult with
        | Some move, None -> model <- completeComputerMove currentTime move response.Summary model
        | None, _ -> model <- cancelComputerMove model
        | _, Some _ -> model <- cancelComputerMove model

        redraw ()

engineWorker.onmessage <-
    fun event ->
        let response = event.data :?> EngineResponse

        if activeRequestId = Some response.RequestId then
            let elapsed =
                activeRequestStartedAt
                |> Option.map (fun startedAt -> now () - startedAt)
                |> Option.defaultValue minimumOpeningBookThinkMs

            let remainingDelay =
                if response.Summary.UsedOpeningBook then
                    max 0.0 (minimumOpeningBookThinkMs - elapsed)
                else
                    0.0

            if remainingDelay > 0.0 then
                window.setTimeout (
                    (fun _ -> completeEngineResponse response),
                    int (ceil remainingDelay)
                )
                |> ignore
            else
                completeEngineResponse response

canvas.addEventListener (
    "click",
    fun event ->
        let x, y = mouseEventToCanvasPoint canvas (event :?> MouseEvent)
        let currentTime = now ()
        model <- tick currentTime model

        if model.Scene = MainMenu then
            match timeControlAt x y with
            | Some minutes ->
                model <- selectTimeControl minutes model
            | None when isPlayAsWhiteButton x y ->
                invalidateComputerRequest ()
                model <- startGame currentTime (VsComputer White) model.TimeControlMinutes
            | None when isPlayAsBlackButton x y ->
                invalidateComputerRequest ()
                model <- startGame currentTime (VsComputer Black) model.TimeControlMinutes
            | None when isLocalGameButton x y ->
                invalidateComputerRequest ()
                model <- startGame currentTime LocalGame model.TimeControlMinutes
            | None -> ()
        elif isNewGameButton x y then
            invalidateComputerRequest ()
            model <- initialModel ()
        elif isUndoButton x y then
            invalidateComputerRequest ()
            model <- undoTurn currentTime model
        elif isComputerMoveButton x y then
            requestComputerMove ()
        elif model.PendingPromotion |> Option.isSome then
            match promotionChoiceAt x y with
            | Some kind -> model <- choosePromotion currentTime kind model
            | None -> ()
        else
            match canvasPointToSquare model x y with
            | Some square -> model <- handleSquareClick currentTime square model
            | None -> ()

        requestComputerMoveIfNeeded ()
        redraw ()
)

window.setInterval (
    (fun _ ->
        model <- tick (now ()) model
        redraw ()),
    250
)
|> ignore

redraw ()
