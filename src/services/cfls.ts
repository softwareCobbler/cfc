//import * as process from "process";
import * as path from "path";

import { Project, FileSystem } from "../compiler/project"
import { ClientAdapter } from "../services/clientAdapter";
import { CflsRequest, CflsRequestType, CflsResponse, CflsResponseType, CflsConfig, InitRequest } from "./cflsTypes";

function send(msg: CflsResponse) {
    process.send!(msg);
}

const service = LanguageService();

process.on("message", (msg: CflsRequest) => {
    switch (msg.type) {
        case CflsRequestType.diagnostic: {
            const diagnostics = service.naiveGetDiagnostics(msg.fsPath, msg.freshText);
            send({type: CflsResponseType.diagnostic, fsPath: diagnostics.fsPath, diagnostics: diagnostics.diagnostics});
            return;
        }
        case CflsRequestType.init: {
            service.init(msg.initArgs);
            return;
        }
        case CflsRequestType.reset: {
            service.reset(msg.config);
            return;
        }
    }
});

type AbsPath = string;

declare const REPLACED_BY_WEBPACK_AT_BUILD : {ClientAdapterModule: string};

function LanguageService() {
    let config! : CflsConfig;
    let workspaceProjects! : Map<AbsPath, Project>;
    let workspaceRoots! : AbsPath[];
    let clientAdapter!: ClientAdapter;

    function init(initArgs : InitRequest["initArgs"]) {
        workspaceProjects = new Map();
        workspaceRoots = initArgs.workspaceRoots;
        clientAdapter = require(REPLACED_BY_WEBPACK_AT_BUILD.ClientAdapterModule);
        
        reset(initArgs.config);
    }

    function getOwningProjectFromAbsPath(absPath: AbsPath) : Project | undefined {
        for (const workspaceRoot of workspaceProjects.keys()) {
            if (absPath.startsWith(workspaceRoot)) {
                return workspaceProjects.get(workspaceRoot)!;
            }
        }
        return undefined;
    }

    function naiveGetDiagnostics(fsPath: AbsPath, freshText: string | Buffer) : {fsPath: AbsPath, diagnostics: unknown[]} {
        const project = getOwningProjectFromAbsPath(fsPath);
        if (!project) return {fsPath, diagnostics: []};

        /*const timing =*/ project.parseBindCheck(fsPath, freshText);
        //connection.console.info(`${fsPath}\n\tparse ${timing.parse} // bind ${timing.bind} // check ${timing.check}`);

        const diagnostics = project.getDiagnostics(fsPath) ?? [];
        const sourceFile = project.getParsedSourceFile(fsPath) ?? null;
        if (!sourceFile) return { fsPath, diagnostics: [] };

        return {
            fsPath,
            diagnostics: diagnostics.map((diagnostic) => clientAdapter.diagnostic(sourceFile.scanner.getAnnotatedChar, diagnostic))
        }
    }

    function reset(freshConfig: CflsConfig) {
        config = freshConfig;
        const fileSystem = FileSystem();
        let wireboxConfigFileAbsPath : string | null = null;

        if (config.wireboxConfigFile && workspaceRoots[0]) {
            wireboxConfigFileAbsPath = path.join(workspaceRoots[0], config.wireboxConfigFile);
            if (!fileSystem.caseSensitive) wireboxConfigFileAbsPath = wireboxConfigFileAbsPath.toLowerCase();
        }

        workspaceProjects.clear();

        for (const workspace of workspaceRoots) {
            const rootAbsPath = workspace;
            const project = Project(
                workspace,
                fileSystem,
                {
                    parseTypes: config.x_parseTypes,
                    debug: true,
                    engineVersion: config.engineVersion,
                    withWireboxResolution: config.wireboxResolution,
                    wireboxConfigFileCanonicalAbsPath: wireboxConfigFileAbsPath,
                    checkReturnTypes: config.x_checkReturnTypes,
                    genericFunctionInference: config.x_genericFunctionInference,
                }
            );

            if (config.engineLibAbsPath) project.addEngineLib(config.engineLibAbsPath);

            // if we're doing wirebox resolution, add the wirebox config file immediately so we get the mappings
            if (config.wireboxResolution && wireboxConfigFileAbsPath) {
                project.addFile(wireboxConfigFileAbsPath);
            }

            workspaceProjects.set(rootAbsPath, project);
        }
    }

    return {
        naiveGetDiagnostics,
        init,
        reset,
    }
}