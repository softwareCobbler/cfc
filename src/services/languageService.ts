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
    let config! : InitArgs["config"]; // we keep a copy to diff reset requests against; this is probably unnecessary, a holdover from when we were accidentally registering to receive all client configuration changes
    let _killWhenDrained = false;
    let forked = false;

    /**
     * if undefined, the message queue was drained on an earlier run (or has never had any messages placed into it)
     */
    let messageQueueDrainedEvent : ExplodedPromise<void> | undefined = undefined;
    const pendingResponses = new Set<number>();

    const cancellationToken = CancellationToken();
    const handlerMappings : Partial<EventHandlers> = {};

    interface TaskDef {
        msg: CflsRequest,
        task: () => void,
        timeout_ms?: number,
        onSuccess?: (payload?: any) => void
        onNoResponse?: () => void
    }

    let taskQueue : TaskDef[] = [];
    let currentTask: TaskDef | null = null;
    let requestTimeoutId : NodeJS.Timeout | null = null;

    interface InflightOrCompleteBatchedDiagnostics {
        seen: Set<AbsPath>,
        pendingMsgIds: Set<number>
    }
    const batchChecks = new Map<number, InflightOrCompleteBatchedDiagnostics>();

    const counter = () => {
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
    };

    const messageId = counter();
    const batch = counter();

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
                        // fixme: circularities? can we (do we want to) say "hey don't recursively do this..."?
                        // fixme: affectedDependencies includes itself? which is not right
                        const inflightOrCompleteBatch = batchChecks.get(msg.batch);
                        if (inflightOrCompleteBatch) { // should always be truthy
                            inflightOrCompleteBatch.pendingMsgIds.delete(msg.id);
                        }
                        for (const fsPath of msg.affectedDependents) {
                            if (fsPath === msg.fsPath) { // this shouldn't happen, fix where it does happen and remove this
                                continue;
                            }
                            if (openFiles.has(fsPath)) {
                                emitDiagnostics(fsPath, null, msg.batch);
                            }
                        }

                        // nothing was pushed into the queue for this batch, this batch is done
                        if (inflightOrCompleteBatch && inflightOrCompleteBatch.pendingMsgIds.size === 0) {
                            batchChecks.delete(msg.batch);
                        }

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

            pendingResponses.delete(msg.id);

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
            if (pendingResponses.size === 0) {
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
        pendingResponses.add(msg.id);
    }

    function emitDiagnostics(fsPath: AbsPath, freshText: string | null, incomingBatchId?: number) {
        let workingBatchId : number;
        let workingMessageId : number;
        if (incomingBatchId !== undefined) {
            const inflightOrComplete = batchChecks.get(incomingBatchId)!;
            if (inflightOrComplete.seen.has(fsPath)) {
                return;
            }
            workingMessageId = messageId.bump();
            workingBatchId = incomingBatchId;
            inflightOrComplete.seen.add(fsPath);
            inflightOrComplete.pendingMsgIds.add(workingMessageId);
        }
        else {
            workingBatchId = batch.bump();
            workingMessageId = messageId.bump();
            batchChecks.set(workingBatchId, {pendingMsgIds: new Set([workingMessageId]), seen: new Set()})
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

    function reset(freshConfig: InitArgs["config"]) {
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
        reset,
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
        runEnqueuedTasks: () => maybeRunNextTask()

    }
}

class _LanguageService<T extends ClientAdapter> {
    _LanguageService() { return LanguageService<T>(); }
}

export type LanguageService<T extends ClientAdapter> = ReturnType<_LanguageService<T>["_LanguageService"]>;