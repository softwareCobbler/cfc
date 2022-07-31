import * as fs from "fs";
import * as path from "path";

import * as esbuild from "esbuild";

import { tscBuildOrFail, esBuildOrFail } from "./utils";
import type { IREPLACED_AT_BUILD } from "../services/buildShim"

/**
 * we appropriately transform IREPLACED_AT_BUILD values to match the required "must be valid JSON syntax or single identifier" of esbulid
 */
export function getBuildOptions(v: IREPLACED_AT_BUILD) : esbuild.BuildOptions {
    return {
        bundle: true,
        platform: "node",
        format: "cjs",
        external: ["vscode"],
        define: {
            "REPLACED_AT_BUILD.ClientAdapterModule_StaticRequirePath": JSON.stringify(v.ClientAdapterModule_StaticRequirePath),
            "REPLACED_AT_BUILD.runtimeLanguageToolPath": JSON.stringify(v.runtimeLanguageToolPath),
            "REPLACED_AT_BUILD.debugExecArgv_forkedLangToolProcess": JSON.stringify(v.debugExecArgv_forkedLangToolProcess),
            "REPLACED_AT_BUILD.debugExecArgv_serverProcess": JSON.stringify(v.debugExecArgv_serverProcess),
            "REPLACED_AT_BUILD.debug": JSON.stringify(v.debug)
        },
        sourcemap: true
    }
}

export function getDefaultTscCmd() {
    return process.platform === "win32" ? "tsc.cmd" : "tsc";
}

/**
 * compile
 */
export function doTsc(tscCmd: string, disregardLoneDebuggerOutputLine = false) {
    tscBuildOrFail(tscCmd, "src/compiler", disregardLoneDebuggerOutputLine);
    tscBuildOrFail(tscCmd, "src/services", disregardLoneDebuggerOutputLine);
    tscBuildOrFail(tscCmd, "src/lang-server", disregardLoneDebuggerOutputLine);
}

/**
 * link
 */
export function doEsBuild(options: esbuild.BuildOptions, outfiles: {languageTool: string, server: string, extension: string}) {
    // actual language tool
    // should run in its own process because it will block on long parses or etc.
    esBuildOrFail({
        ...options,
        entryPoints: ["./src/services/languageTool.ts"],
        outfile: outfiles.languageTool
    });

    // "server frontend", handles LSP and asks language tool for diagnostics and etc.
    // currently this is really "the vscode server"
    esBuildOrFail({
        ...options,
        entryPoints: ["./src/lang-server/server/src/server.ts"],
        outfile: outfiles.server,
    });

    // vscode extension entry point, kicks off the "server frontend" in its own process
    // vscode manages the LSP here, crafting requests as a result of user interactions with the editor
    // and interpreting the responses of the server as "put squiggly under these characters" or etc.
    esBuildOrFail({
        ...options,
        entryPoints: ["./src/lang-server/client/src/extension.ts"],
        outfile: outfiles.extension
    });
}

/**
 * we need this separately for tests
 * otherwise it is included in the "server" bundle from the main link
 */
export function link_ForTests(options: esbuild.BuildOptions, outfiles: {languageService: string, languageTool: string}) {
    esBuildOrFail({
        ...options,
        entryPoints: ["./src/services/languageService.ts"],
        outfile: outfiles.languageService
    })
    esBuildOrFail({
        ...options,
        entryPoints: ["./src/services/languageTool.ts"],
        outfile: outfiles.languageTool
    })
}

/**
 * resources
 */
export function doResources() {
    function fromTo(from: string, to: string) {
        console.log(`[copying] '${from}' -> '${to}'`)
        fs.copyFileSync(from, to);
    }
    fromTo(path.resolve("src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm"), path.resolve("cflsp-vscode/out/lib.cf2018.d.cfm"));
    fromTo(path.resolve("src/grammar/grammar.json"), path.resolve("cflsp-vscode/out/grammar.json"));
}
