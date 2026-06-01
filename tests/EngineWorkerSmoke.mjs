import { Worker } from "node:worker_threads";
import { initialModel } from "../Board.fs.js";

const model = initialModel();
const worker = new Worker(new URL("./EngineWorkerHarness.mjs", import.meta.url));
const timeout = setTimeout(() => {
    worker.terminate();
    throw new Error("Worker response timed out");
}, 10000);

worker.on("message", response => {
    clearTimeout(timeout);

    if (!response.Move) {
        throw new Error("Expected an opening move");
    }

    if (!response.Summary.UsedOpeningBook && response.Summary.Depth < 1) {
        throw new Error("Expected a completed iterative-deepening depth");
    }

    console.log(
        `book=${response.Summary.UsedOpeningBook} depth=${response.Summary.Depth} nodes=${response.Summary.Nodes} qnodes=${response.Summary.QuiescenceNodes} ttHits=${response.Summary.TranspositionHits} elapsed=${response.Summary.ElapsedMs.toFixed(1)}ms`
    );

    worker.terminate();
});

worker.postMessage({
    RequestId: 1,
    Board: model.Board,
    SideToMove: model.SideToMove,
    CastlingRights: model.CastlingRights,
    EnPassantTarget: model.EnPassantTarget,
    MoveHistory: model.MoveHistory,
    MaxDepth: 8,
    TimeLimitMs: 1200
});
