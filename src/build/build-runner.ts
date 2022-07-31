import type { IREPLACED_AT_BUILD } from "../services/buildShim"
import * as build from "./build"

const debug = process.argv.indexOf("--debug") !== -1;
const maybeBreakOnServerSpawn = process.argv.indexOf("--brk-server") !== -1;
const maybeBreakOnToolFork = process.argv.indexOf("--brk-tool") !== -1;

const defines : IREPLACED_AT_BUILD = {
    ClientAdapterModule_StaticRequirePath: `../lang-server/server/src/vscode-adapter`,
    runtimeLanguageToolPath: `./languageTool.js`,
    debugExecArgv_serverProcess: ['--nolazy', '--inspect=6011'],
    debugExecArgv_forkedLangToolProcess: ["--nolazy", "--inspect=6012"],
    debug: debug
}

if (maybeBreakOnServerSpawn) {
    defines.debugExecArgv_serverProcess.push("--inspect-brk")
}
if (maybeBreakOnToolFork) {
    defines.debugExecArgv_forkedLangToolProcess.push("--inspect-brk")
}

build.doTsc(build.getDefaultTscCmd());
build.doEsBuild(build.getBuildOptions(defines), {
    languageTool: "./cflsp-vscode/out/languageTool.js",
    server: "./cflsp-vscode/out/server.js",
    extension: "./cflsp-vscode/out/extension.js",
});
build.doResources();
