import { randomBytes } from "crypto";
import * as net from "net";
import * as fs from "fs";

export function CancellationToken() {
    function randomName() {
        return randomBytes(16).toString("hex");
    }
    
    const id = `\\\\.\\pipe\\cfls-token-${randomName()}`;

    //
    // ENGAGED = true if the socket is open
    //   - "this socket is listening"
    //   - "or we've asked it to and it could start at any time"
    //   - "or we've asked it to close but it hasn't yet"
    //
    // Plenty of EADDRINUSE if we try to open a socket on the same name
    //
    let ENGAGED = false;
    let socket = net.createServer();

    socket.on("close", () => {
        // it can take a few microseconds between `socket.close()` and the actual close event
        // during which time, the OS (or at least Windows) will say that
        //   - socket.listening is false,
        //   - but socket.listen(id) is an error because it really actually is still listening
        //
        ENGAGED = false;
    })

    socket.on("error", (_err) => {
        // almost certainly EADDRINUSE
        // which is thrown asynchronously,
        // so registering this listener acts as a catch block
        // 
        // can put a breakpoint here if necessary,
        // otherwise this is a no-op
    })

    function requestCancellation() {
        if (ENGAGED) {
            return;
        }
        else {
            socket.listen(id);
            ENGAGED = true;
        }
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