import { parentPort } from "node:worker_threads";

globalThis.self = {
    onmessage: null,
    postMessage: message => parentPort.postMessage(message)
};

parentPort.on("message", data => globalThis.self.onmessage({ data }));

await import("../EngineWorker.fs.js");
