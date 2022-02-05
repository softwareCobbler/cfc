import * as fs from "fs";
import * as path from "path";

import * as esbuild from "esbuild";

import { tscBuildOrFail, esBuildOrFail } from "./utils";

//
// general config
//

const debug = process.argv.indexOf("--debug") !== -1;
const tscCmd = process.platform === "win32" ? "tsc.cmd" : "tsc";

const commonEsBuildOptions : esbuild.BuildOptions = {
    bundle: true,
    platform: "node",
    format: "cjs",
    external: ["vscode"],
    define: {
        "REPLACED_AT_BUILD.ClientAdapterModule_StaticRequirePath": `"../lang-server/server/src/vscode-adapter"`,
        "REPLACED_AT_BUILD.runtimeLanguageToolPath": `"./languageTool.js"`,
        "REPLACED_AT_BUILD.debug": debug ? "true" : "false",
    },
    sourcemap: true
}

doTsc();
doEsBuild();
doResources();

function doTsc() {
    tscBuildOrFail(tscCmd, "src/compiler");
    tscBuildOrFail(tscCmd, "src/services");
    tscBuildOrFail(tscCmd, "src/lang-server");
}

function doEsBuild() {
    // actual language tool
    // should run in its own process because it will block on long parses or etc.
    esBuildOrFail({
        ...commonEsBuildOptions,
        entryPoints: ["./src/services/languageTool.ts"],
        outfile: "./cflsp-vscode/out/languageTool.js",
    });

    // "server frontend", handles LSP and asks language tool for diagnostics and etc.
    // currently this is really "the vscode server"
    esBuildOrFail({
        ...commonEsBuildOptions,
        entryPoints: ["./src/lang-server/server/src/server.ts"],
        outfile: "./cflsp-vscode/out/server.js",
    });

    // vscode extension entry point, kicks off the "server frontend" in its own process
    // vscode manages the LSP here, crafting requests as a result of user interactions with the editor
    // and interpreting the responses of the server as "put squiggly under these characters" or etc.
    esBuildOrFail({
        ...commonEsBuildOptions,
        entryPoints: ["./src/lang-server/client/src/extension.ts"],
        outfile: "./cflsp-vscode/out/extension.js",
    });
}

function doResources() {
    function fromTo(from: string, to: string) {
        console.log(`[copying] '${from}' -> '${to}'`)
        fs.copyFileSync(from, to);
    }
    fromTo(path.resolve("src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm"), path.resolve("cflsp-vscode/out/lib.cf2018.d.cfm"));
    fromTo(path.resolve("src/grammar/grammar.json"), path.resolve("cflsp-vscode/out/grammar.json"));
}