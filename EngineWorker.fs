module Chess.EngineWorker

open Browser.Types
open Fable.Core
open Chess.Engine
open Chess.OpeningBook
open Chess.Rules
open Chess.Types

[<Emit("self.postMessage($0)")>]
let private postMessage (_message: EngineResponse) : unit =
    jsNative

[<Emit("self.onmessage = $0")>]
let private setOnMessage (_handler: MessageEvent -> unit) : unit =
    jsNative

let private handleMessage (event: MessageEvent) =
    let request = event.data :?> EngineRequest

    let responseFromSearch () =
        match chooseMoveWithin request.MaxDepth request.TimeLimitMs request.Board request.CastlingRights request.EnPassantTarget request.SideToMove with
        | Some result ->
            { RequestId = request.RequestId
              Move = Some result.Move
              Score = result.Score
              Summary =
                { UsedOpeningBook = false
                  Depth = result.Depth
                  Nodes = result.Nodes
                  QuiescenceNodes = result.QuiescenceNodes
                  Cutoffs = result.Cutoffs
                  TranspositionHits = result.TranspositionHits
                  ElapsedMs = result.ElapsedMs } }
        | None ->
            { RequestId = request.RequestId
              Move = None
              Score = 0
              Summary =
                { UsedOpeningBook = false
                  Depth = 0
                  Nodes = 0
                  QuiescenceNodes = 0
                  Cutoffs = 0
                  TranspositionHits = 0
                  ElapsedMs = 0.0 } }

    let legalMoves =
        allLegalMoves request.Board request.CastlingRights request.EnPassantTarget request.SideToMove

    let response =
        match tryChooseMove legalMoves request.MoveHistory with
        | Some move ->
            { RequestId = request.RequestId
              Move = Some move
              Score = 0
              Summary =
                { UsedOpeningBook = true
                  Depth = 0
                  Nodes = 0
                  QuiescenceNodes = 0
                  Cutoffs = 0
                  TranspositionHits = 0
                  ElapsedMs = 0.0 } }
        | None -> responseFromSearch ()

    postMessage response

setOnMessage handleMessage
