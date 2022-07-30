import type { IREPLACED_AT_BUILD } from "../services/buildShim"
import * as build from "./build"

const debug = process.argv.indexOf("--debug") !== -1;

const defines : IREPLACED_AT_BUILD = {
    ClientAdapterModule_StaticRequirePath: `../lang-server/server/src/vscode-adapter`,
    runtimeLanguageToolPath: `./languageTool.js`,
    debug: debug
}

build.doTsc(build.getDefaultTscCmd());
build.doEsBuild(build.getBuildOptions(defines), {
    languageTool: "./cflsp-vscode/out/languageTool.js",
    server: "./cflsp-vscode/out/server.js",
    extension: "./cflsp-vscode/out/extension.js",
});
build.doResources();
