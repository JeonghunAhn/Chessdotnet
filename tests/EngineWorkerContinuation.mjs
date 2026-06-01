import { Worker } from "node:worker_threads";
import { singleton } from "../fable_modules/fable-library-js.5.0.0/List.js";
import { initialModel } from "../Board.fs.js";
import { applyMove, nextEnPassantTarget, updateCastlingRights } from "../Rules.fs.js";
import { Color, Move, Square } from "../Types.fs.js";

const model = initialModel();
const e4 = new Move(new Square(4, 6), new Square(4, 4), undefined);
const worker = new Worker(new URL("./EngineWorkerHarness.mjs", import.meta.url));
const timeout = setTimeout(() => {
    worker.terminate();
    throw new Error("Worker response timed out");
}, 10000);

worker.on("message", response => {
    clearTimeout(timeout);

    if (!response.Summary.UsedOpeningBook || !response.Move) {
        throw new Error("Expected a continuation from the opening book");
    }

    console.log(
        `reply=${response.Move.From.File},${response.Move.From.Rank}-${response.Move.To.File},${response.Move.To.Rank}`
    );

    worker.terminate();
});

worker.postMessage({
    RequestId: 2,
    Board: applyMove(model.Board, model.EnPassantTarget, e4),
    SideToMove: new Color(1, []),
    CastlingRights: updateCastlingRights(model.Board, e4, model.CastlingRights),
    EnPassantTarget: nextEnPassantTarget(model.Board, e4),
    MoveHistory: singleton(e4),
    MaxDepth: 8,
    TimeLimitMs: 1200
});
