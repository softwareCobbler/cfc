/**
 * the languageService is the interop between something that can speak LSP and our own language tooling
 * we don't understand LSP here, but we provide an interface for something that does to reach out to us for diagnostics or completions or etc.
 * we fork the actual language tool such that long-running requests don't block the entire server, and so that such long-requests can be cancelled
 * 
 * cancellation is acheived with a cancellation token, which is at its core just a particular file on disk (a named pipe) whose existence
 * can be tested for and, when it exists, means "do cancel"
 */

import * as child_process from "child_process";
import * as path from "path";
import { CancellationToken } from "../compiler/cancellationToken";
import { CflsResponse, CflsResponseType, CflsRequest, CflsRequestType, CflsConfig, InitArgs } from "./cflsTypes";
import { ClientAdapter } from "./clientAdapter";
import type { AbsPath } from "../compiler/utils";
import type { IREPLACED_AT_BUILD } from "./buildShim";

declare const REPLACED_AT_BUILD : IREPLACED_AT_BUILD;

interface EventHandlers {
    diagnostics: (fsPath: string, diagnostics: unknown[]) => void
}

export function LanguageService<T extends ClientAdapter>() {
    const forkInfo = {
        languageToolFilePath: path.join(__dirname, REPLACED_AT_BUILD.runtimeLanguageToolPath),
        forkArgs: REPLACED_AT_BUILD.debug
            ? {execArgv: REPLACED_AT_BUILD.debugExecArgv_forkedLangToolProcess}
            : {}
    } as const;

    const openFiles = new Set<string>();
    const trackFile = (path: string) => {
        if (openFiles.has(path)) {
            return;
        }
        console.log("track file " + path);
        pushTask({
            msg: {
                type: CflsRequestType.track,
                id: messageId.bump(),
                fsPath: path
            },
            task: function() { send(this.msg) },
            onSuccess: () => openFiles.add(path)
        })
    }
    const untrackFile = (path: string) => {
        if (!openFiles.has(path)) {
            return;
        }

        pushTask({
            msg: {
                type: CflsRequestType.untrack,
                id: messageId.bump(),
                fsPath: path
            },
            task: function() { send(this.msg) },
            onSuccess: () => openFiles.delete(path)
        })
    }

    let server! : child_process.ChildProcess;
    let config! : InitArgs["config"];
    let _killWhenDrained = false;
    let forked = false;

    /**
     * if undefined, the message queue was drained on an earlier run (or has never had any messages placed into it)
     */
    let messageQueueDrainedEvent : ExplodedPromise<void> | undefined = undefined;

    const cancellationToken = CancellationToken();
    const handlerMappings : Partial<EventHandlers> = {};

    let taskQueue : TaskDef[] = [];
    let currentTask: TaskDef | null = null;
    let requestTimeoutId : NodeJS.Timeout | null = null;
    const pendingResponses = new PendingResponses();

    const messageId = (() => {
        let id = 0;
        return {
            bump: () => {
                id = id === Number.MAX_SAFE_INTEGER
                    ? 0
                    : id + 1;
                return id;
            },
            current: () => id
        }
    })();

    function fork(freshConfig: InitArgs["config"], workspaceRoots: AbsPath[]) : {childProcess: child_process.ChildProcess, didInitSignal: Promise<void>} {
        config = freshConfig;
        server = child_process.fork(forkInfo.languageToolFilePath, forkInfo.forkArgs);


        server.on("message", (msg: CflsResponse) => {
            let autoRunNext = true;

            if (!currentTask || currentTask.msg.id !== msg.id) {
                // find msg in pending
                // exists ? onNoResponse/onFailure / cleanup
                console.log(`msg.id ${msg.id} will be onNoRespons'd/cleaned-up`);
            }
            else {
                clearRequestTimeout(); // no auto cleanup, though unnecessary on this event loop turn right?
                switch (msg.type) {
                    case CflsResponseType.initialized: {
                        currentTask.onSuccess?.();
                        autoRunNext = false; // require a manual "runEnqueuedTasks" call or etc.
                        break;
                    }
                    case CflsResponseType.diagnostics: {
                        handlerMappings.diagnostics?.(msg.fsPath, msg.diagnostics);

                        const batchedCheck = pendingResponses.getBatchedCheckForBatchId(msg.batch);
                        batchedCheck?.markResponseAsReceived(msg.id);

                        // fixme: affectedDependencies includes itself? which is not right
                        for (const fsPath of msg.affectedDependents) {
                            if (fsPath === msg.fsPath) {
                                // this shouldn't happen, fix where it does happen and remove this
                                // aug/5/2022 -- should be fixed; todo: add Debug.assert(...) logging or something
                                // debugger;
                                continue;
                            }
                            if (openFiles.has(fsPath) && !batchedCheck?.hasSeenFsPath(fsPath)) {
                                emitDiagnostics(fsPath, null, msg.batch);
                            }
                        }

                        // if nothing was pushed into the queue for this batch, this batch is done
                        batchedCheck?.maybeCleanup();

                        break;
                    }
                    case CflsResponseType.completions: {
                        if (currentTask?.onSuccess) currentTask.onSuccess(msg.completionItems);
                        break;
                    }
                    case CflsResponseType.definitionLocations: {
                        if (currentTask?.onSuccess) currentTask.onSuccess(msg.locations);
                        break;
                    }
                    case CflsResponseType.track: {
                        currentTask.onSuccess?.();
                        break;
                    }
                    case CflsResponseType.untrack: {
                        currentTask.onSuccess?.();
                        break;
                    }
                }
            }

            pendingResponses.getPendingResponseForMsgId(msg.id)?.cleanup();

            if (autoRunNext) {
                maybeRunNextTask();
            }
        })

        const {resolve, promise} = explodedPromise<void>();
        const initTask : TaskDef = {
            msg: {
                type: CflsRequestType.init,
                id: messageId.bump(),
                initArgs: {
                    config,
                    workspaceRoots,
                    cancellationTokenId: cancellationToken.getId(),
                },
            },
            task: function() { send(this.msg) },
            timeout_ms: 1000 * 60 * 2, // long timeout for init request, to allow for setting up path mappings and etc.
            onSuccess: () => {
                forked = true;
                resolve(void 0);
            }
        };

        taskQueue.unshift(initTask);
        runNextTask();

        return {
            childProcess: server,
            didInitSignal: promise
        }
    }

    function maybeRunNextTask() {
        if (!forked) {
            return;
        }
        runNextTask();
    }

    function runNextTask() {
        clearRequestTimeout();
        const task = taskQueue.shift();
        if (task) {
            currentTask = task;
            requestTimeoutId = setTimeout(noResponseAndRunNext, task.timeout_ms ?? 5000);
            task.task();
        }
        else {
            currentTask = null;
            if (_killWhenDrained) {
                server.kill();
            }
            if (pendingResponses.getSize() === 0) {
                messageQueueDrainedEvent?.resolve();
            }
        }
    }

    function noResponseAndRunNext() {
        currentTask?.onNoResponse?.();
        maybeRunNextTask();
    }

    function clearRequestTimeout() {
        if (requestTimeoutId) {
            clearTimeout(requestTimeoutId);
            requestTimeoutId = null;
        }
    }

    function on<K extends keyof EventHandlers>(eventName: K, handler: EventHandlers[K]) {
        handlerMappings[eventName] = handler;
    }

    function send(msg: CflsRequest) {
        server.send(msg);
        pendingResponses.add(msg.id, /*timeout*/10000)
    }

    function emitDiagnostics(fsPath: AbsPath, freshText: string | null, incomingBatchId?: number) {
        let workingBatchId : number;
        let workingMessageId : number;
        if (incomingBatchId !== undefined) {
            const batchedCheck = pendingResponses.getBatchedCheckForBatchId(incomingBatchId);
            if (!batchedCheck) {
                // ??? we should have found one, maybe it timedout
                // probably nothing good would come of trying to reanimate it
                return;
            }
            if (batchedCheck.hasSeenFsPath(fsPath)) {
                return;
            }
            workingMessageId = messageId.bump();
            workingBatchId = incomingBatchId;

            batchedCheck.markFsPathAsSeen(fsPath);
            batchedCheck.markResponseAsPending(workingMessageId);
        }
        else {
            workingMessageId = messageId.bump();
            const batchedCheck = pendingResponses.freshBatchedCheck();
            batchedCheck.markResponseAsPending(workingMessageId);
            workingBatchId = batchedCheck.id;
        }

        pushTask({
            msg: {
                type: CflsRequestType.diagnostics,
                id: workingMessageId, fsPath,
                freshText,
                batch: workingBatchId
            },
            task: function() { send(this.msg) },
        })
    }

    function pushTask(freshTask: TaskDef) {
        if (messageQueueDrainedEvent === undefined) {
            messageQueueDrainedEvent = explodedPromise<void>();
        }

        if (currentTask) {
            // if this is for diagnostics and we're already in the middle of diagnostics for this file, enqueue this request at the front of the line and cancel the currently running request
            if (currentTask.msg.type === CflsRequestType.diagnostics
                && freshTask.msg.type === CflsRequestType.diagnostics
                && currentTask.msg.fsPath === freshTask.msg.fsPath
            ) {
                console.log("diagnostics requiring cancellation " + currentTask.msg.fsPath);
                taskQueue = [{
                    msg: freshTask.msg,
                    task: function() {
                        cancellationToken.reset();
                        send(this.msg);
                    }
                }, ...taskQueue.filter(queuedTask => {
                    // filter out other same requests, if any
                    return !(queuedTask.msg.type === CflsRequestType.diagnostics
                        && freshTask.msg.type === CflsRequestType.diagnostics
                        && queuedTask.msg.fsPath === freshTask.msg.fsPath);
                })];
                cancellationToken.requestCancellation();
            }
            else {
                taskQueue.push(freshTask);
            }
        }
        else {
            // no current tasks; queue it and then immediately run it
            taskQueue.push(freshTask);
            maybeRunNextTask();
        }
    }

    function resetAssumingChangedConfig(freshConfig: InitArgs["config"]) {
        let didChange = false;
        for (const key of Object.keys(freshConfig) as (keyof CflsConfig)[]) {
            if (freshConfig[key] !== config[key]) {
                didChange = true;
                break;
            }
        }

        if (!didChange) {
            return;
        }

        config = freshConfig;
        send({type: CflsRequestType.reset, id: messageId.bump(), config});
    }

    function forceReset() {
        send({type: CflsRequestType.reset, id: messageId.bump(), config});
    }

    function explodedPromise<T>() {
        let resolve!: (value: T) => void;
        let reject!: () => void;
        const promise = new Promise<T>((ok, fail) => [resolve, reject] = [ok, fail]);
        return {resolve, reject, promise};
    }

    type ExplodedPromise<T> = ReturnType<typeof explodedPromise<T>>;

    function getCompletions(fsPath: AbsPath, targetIndex: number, triggerCharacter: string | null) : Promise<ReturnType<T["completionItem"]>[]> {
        const {promise, resolve} = explodedPromise<ReturnType<T["completionItem"]>[]>();

        pushTask({
            msg: {
                type: CflsRequestType.completions,
                id: messageId.bump(),
                fsPath,
                targetIndex,
                triggerCharacter
            },
            task: function() { send(this.msg) },
            onSuccess: (payload: ReturnType<T["completionItem"]>[]) => {
                resolve(payload);
            },
            onNoResponse: () => resolve([])
        })

        return promise;
    }

    function getDefinitionLocations(fsPath: AbsPath, targetIndex: number) {
        const {promise, resolve} = explodedPromise<ReturnType<T["sourceLocation"]>[]>();

        pushTask({
            msg: {
                type: CflsRequestType.definitionLocations,
                id: messageId.bump(),
                fsPath,
                targetIndex,
            },
            task: function() { send(this.msg) },
            onSuccess: (payload: ReturnType<T["sourceLocation"]>[]) => {
                resolve(payload);
            },
            onNoResponse: () => resolve([])
        })

        return promise;
    }

    function killWhenDrained() {
        _killWhenDrained = true;
    }

    return {
        resetAssumingChangedConfig,
        fork,
        on,
        emitDiagnostics,
        getCompletions,
        getDefinitionLocations,
        trackFile,
        untrackFile,
        killWhenDrained,
        allDrained: () => {
            if (messageQueueDrainedEvent) {
                return messageQueueDrainedEvent.promise
            }
            else {
                const r = explodedPromise<void>();
                r.resolve(void 0);
                return r.promise;
            }
        },
        runEnqueuedTasks: () => maybeRunNextTask(),
        forceReset
    }
}

interface TaskDef {
    msg: CflsRequest,
    task: () => void,
    timeout_ms?: number,
    onSuccess?: (payload?: any) => void
    onNoResponse?: () => void
}

class PendingResponses {
    private pendingResponses = new Map<number, NodeJS.Timeout>();
    private batchedChecks = new Map<number, BatchedCheck>();
    constructor() {}

    add(msgId: number, timeout_ms: number) {
        const timeoutId = setTimeout(() => this.cleanup(msgId), timeout_ms);
        this.pendingResponses.set(msgId, timeoutId);
    }

    getSize() {
        return this.pendingResponses.size;
    }

    getPendingResponseForMsgId(msgId: number) {
        if (!this.pendingResponses.get(msgId)) {
            return undefined;
        }
        return {
            cleanup: () => this.cleanup(msgId)
        }
    }

    getBatchedCheckForBatchId(batchId: number) {
        return this.batchedChecks.get(batchId);
    }

    private cleanup(msgId: number) : void {
        const timeoutId = this.pendingResponses.get(msgId);
        if (!timeoutId) {
            return;
        }

        clearTimeout(timeoutId);
        this.pendingResponses.delete(msgId);

        // this assumes that batchChecks doesn't grow to insane proporations
        // like 2 or 3 at a time, and generally just 1
        for (const batchedCheck of this.batchedChecks.values()) {
            batchedCheck.markResponseAsReceived(msgId);
            batchedCheck.maybeCleanup();
        }
    }

    freshBatchedCheck() : BatchedCheck {
        const v = new BatchedCheck(this);
        this.batchedChecks.set(v.id, v);
        return v;
    }

    disposeBatchedCheck(v: BatchedCheck) : void {
        this.batchedChecks.delete(v.id);
    }
}

class BatchedCheck {
    private owner: PendingResponses;
    readonly id : number;
    private fsPathsSeen = new Set<string>();
    private pendingResponses = new Set<number>();
    private static _nextid_ : number = 0;

    constructor(owner: PendingResponses) {
        this.owner = owner;
        this.id = BatchedCheck._nextid_++;
    }

    hasSeenFsPath(fsPath: string) {
        return this.fsPathsSeen.has(fsPath);
    }

    markFsPathAsSeen(fsPath: string) {
        this.fsPathsSeen.add(fsPath);
    }

    markResponseAsReceived(msgID: number) {
        this.pendingResponses.delete(msgID);
    }

    markResponseAsPending(msgID: number) {
        this.pendingResponses.add(msgID);
    }

    maybeCleanup() {
        if (this.pendingResponses.size === 0) {
            this.owner.disposeBatchedCheck(this);
        }
    }
}

export type LanguageService<T extends ClientAdapter> = ReturnType<typeof LanguageService<T>>;