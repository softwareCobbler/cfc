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
import { FileSystem } from "../compiler/project";

declare const REPLACED_AT_BUILD : IREPLACED_AT_BUILD;

interface EventHandlers {
    diagnostics: (fsPath: string, diagnostics: unknown[]) => void
}

/**
 * This canonicalization should be the same as the languageTool uses
 */
const canonicalizePath = (() => {
    const filesystem = FileSystem();
    return (s: string) => filesystem.caseSensitive ? s : s.toLowerCase();
})();

export function LanguageService<T extends ClientAdapter>() {
    const forkInfo = {
        languageToolFilePath: path.join(__dirname, REPLACED_AT_BUILD.runtimeLanguageToolPath),
        forkArgs: REPLACED_AT_BUILD.debug
            ? {execArgv: ["--nolazy", "--inspect=6012", /*"--inspect-brk"*/]}
            : {}
    } as const;

    const openFiles = new Set<string>();
    const trackFile = (path: string) => {
        openFiles.add(canonicalizePath(path));
    }
    const untrackFile = (path: string) => {
        openFiles.delete(canonicalizePath(path));
    }

    let server! : child_process.ChildProcess;
    let config! : InitArgs["config"]; // we keep a copy to diff reset requests against; this is probably unnecessary, a holdover from when we were accidentally registering to receive all client configuration changes
    const cancellationToken = CancellationToken();
    const handlerMappings : Partial<EventHandlers> = {};

    interface TaskDef {
        type: CflsRequestType,
        task: () => void,
        timeout_ms?: number,
        onSuccess?: (payload?: any) => void
        onNoResponse?: () => void
    }

    let taskQueue : TaskDef[] = [];
    let currentTask: TaskDef | null = null;
    let requestTimeoutId : NodeJS.Timeout | null = null;

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

    function fork(freshConfig: InitArgs["config"], workspaceRoots: AbsPath[]) : Promise<void> {
        config = freshConfig;
        server = child_process.fork(forkInfo.languageToolFilePath, forkInfo.forkArgs);

        server.on("message", (msg: CflsResponse) => {
            if (messageId.current() === msg.id) {
                if (!currentTask) {
                    // why no current task? who's clearning it
                    // debugger;
                }

                clearRequestTimeout();
                switch (msg.type) {
                    case CflsResponseType.initialized: {
                        if (currentTask?.onSuccess) currentTask.onSuccess();
                        break;
                    }
                    case CflsResponseType.diagnostics: {
                        handlerMappings.diagnostics?.(msg.fsPath, msg.diagnostics);
                        // fixme: circularities? can we (do we want to) say "hey don't recursively do this..."?
                        // fixme: affectedDependencies includes itself? which is not right
                        for (const fsPath of msg.affectedDependents) {
                            if (fsPath === msg.fsPath) { // this shouldn't happen, fix where it does happen and remove this
                                continue;
                            }
                            if (openFiles.has(fsPath)) {
                                emitDiagnostics(fsPath, null);
                            }
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
                }
            }
            runNextTask();
        })

        const {resolve, promise} = explodedPromise<void>();
        const initTask : TaskDef = {
            type: CflsRequestType.init,
            task: () => send({
                type: CflsRequestType.init,
                id: messageId.bump(),
                initArgs: {
                    config,
                    workspaceRoots,
                    cancellationTokenId: cancellationToken.getId(),
                },
            }),
            timeout_ms: 1000 * 60 * 2, // long timeout for init request, to allow for setting up path mappings and etc.
            onSuccess: () => {
                resolve(void 0);
            }
        };

        pushTask(initTask);

        return promise;
    }

    function runNextTask() {
        clearRequestTimeout();
        const task = taskQueue.shift();
        if (task) {
            if (task.type === CflsRequestType.diagnostics) {
                taskQueue = taskQueue.filter(task => task.type !== CflsRequestType.diagnostics)
            }
            currentTask = task;
            requestTimeoutId = setTimeout(noResponseAndRunNext, task.timeout_ms ?? 5000);
            task.task();
        }
        else {
            currentTask = null;
        }
    }

    function noResponseAndRunNext() {
        currentTask?.onNoResponse?.();
        runNextTask();
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
    }

    function emitDiagnostics(fsPath: AbsPath, freshText: string | null) {
        const task = () => {
            const request : CflsRequest = {type: CflsRequestType.diagnostics, id: messageId.bump(), fsPath, freshText};
            send(request);
        }
        pushTask({type: CflsRequestType.diagnostics, task});
    }

    function pushTask(taskDef: TaskDef) {
        if (currentTask) {
            // "diagnostic" task is conceptually overloaded to be responsible for doing all the heavy lifiting of loading a file into the project,
            // and parse/bind/check to get a tree, so that other tasks have a good tree to pull info from
            // so we treat it specially, and if the current task is diagnostics, we cancel the current diagnostic request and queue up another run
            if (taskDef.type === CflsRequestType.diagnostics && currentTask.type === CflsRequestType.diagnostics) {
                taskQueue = [{
                    type: taskDef.type,
                    task: () => {
                        cancellationToken.reset();
                        taskDef.task()
                    }
                }, ...taskQueue.filter((task) => task.type !== CflsRequestType.diagnostics)]; // do we need to tell the language server that we possibly dropped some requests?

                cancellationToken.requestCancellation();
            }
            else {
                taskQueue.push(taskDef);
            }
        }
        else {
            // no current tasks; queue it and then immediately run it
            taskQueue.push(taskDef);
            runNextTask();
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

    function getCompletions(fsPath: AbsPath, targetIndex: number, triggerCharacter: string | null) : Promise<ReturnType<T["completionItem"]>[]> {
        const {promise, resolve} = explodedPromise<ReturnType<T["completionItem"]>[]>();

        pushTask({
            type: CflsRequestType.completions,
            task: () => {
                send({
                    type: CflsRequestType.completions,
                    id: messageId.bump(),
                    fsPath,
                    targetIndex,
                    triggerCharacter
                })
            },
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
            type: CflsRequestType.definitionLocations,
            task: () => {
                send({
                    type: CflsRequestType.definitionLocations,
                    id: messageId.bump(),
                    fsPath,
                    targetIndex,
                })
            },
            onSuccess: (payload: ReturnType<T["sourceLocation"]>[]) => {
                resolve(payload);
            },
            onNoResponse: () => resolve([])
        })

        return promise;
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
    }
}

class _LanguageService<T extends ClientAdapter> {
    _LanguageService() { return LanguageService<T>(); }
}

export type LanguageService<T extends ClientAdapter> = ReturnType<_LanguageService<T>["_LanguageService"]>;