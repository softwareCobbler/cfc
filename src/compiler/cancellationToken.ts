import { randomBytes } from "crypto";
import * as net from "net";
import * as fs from "fs";

export function CancellationToken() {
    function randomName() {
        return randomBytes(16).toString("hex");
    }
    
    const id = `\\\\.\\pipe\\cfls-token-${randomName()}`;
    let socket = net.createServer();

    function requestCancellation() {
        if (socket.listening) {
            // fixme: asynchronicity issues; ideally this wouldn't ever happen?
            return;
        }
        socket.listen(id);
    }

    function reset() {
        socket.close();
    }

    return {
        requestCancellation,
        reset,
        getId: () => id
    }
}
export type CancellationToken = ReturnType<typeof CancellationToken>;

export class CancellationException {
    name : string;
    constructor() {
        this.name = "CancellationException"; // for debugability, generally want to NOT break on these
    };
}

export function CancellationTokenConsumer(cancellationTokenId: string) {
    function cancellationRequested() {
        return fs.existsSync(cancellationTokenId);
    }
    function throwIfCancellationRequested() {
        if (cancellationRequested()) {
            throw new CancellationException();
        }
    }
    return {
        cancellationRequested,
        throwIfCancellationRequested
    }
}

export type CancellationTokenConsumer = ReturnType<typeof CancellationTokenConsumer>;