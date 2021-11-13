import * as child_process from "child_process";
import { CflsResponse, CflsResponseType, CflsRequest, CflsRequestType, CflsConfig } from "./cflsTypes";

type AbsPath = string;

interface EventHandlers {
    diagnostics: (fsPath: string, diagnostics: unknown[]) => void
}

export function LanguageService(serverFilePath: AbsPath, clientAdaptersFilePath: AbsPath) {
    let server! : child_process.ChildProcess;
    let config! : CflsConfig;
    const handlerMappings : Partial<EventHandlers> = {};

    function fork(freshConfig: CflsConfig, workspaceRoots: AbsPath[]) {
        config = freshConfig;
        server = child_process.fork(serverFilePath, {execArgv: ["--inspect=6012"]});

        server.on("message", (msg: CflsResponse) => {
            switch (msg.type) {
                case CflsResponseType.diagnostic: {
                    handlerMappings.diagnostics?.(msg.fsPath, msg.diagnostics);
                    return;
                }
            }
        })

        send({type: CflsRequestType.init, initArgs: {
            config,
            workspaceRoots,
            clientAdaptersFilePath,
        }});
    }

    function on<T extends keyof EventHandlers>(eventName: T, handler: EventHandlers[T]) {
        handlerMappings[eventName] = handler;
    }

    function send(msg: CflsRequest) {
        server.send(msg);
    }

    function emitDiagnostics(fsPath: AbsPath, freshText: string) {
        send({type: CflsRequestType.diagnostic, fsPath, freshText});
    }

    function reset(freshConfig: CflsConfig) {
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
        send({type: CflsRequestType.reset, config});
    }

    return {
        reset,
        fork,
        on,
        emitDiagnostics,
    }
}

export type LanguageService = ReturnType<typeof LanguageService>;