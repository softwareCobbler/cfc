import * as child_process from "child_process";
import * as esbuild from "esbuild";

interface TighterBuildOptions extends esbuild.BuildOptions {
    entryPoints: [string], // is there a way to say here "entryPoints as a property name must be a property name on BuildOptions", e.g. "override" or etc.
    outfile: string
}

export function esBuildOrFail(options: esbuild.BuildOptions & TighterBuildOptions) : void {
    console.log(`[esbuild] bundling entrypoint ${options.entryPoints[0]}`);
    try {
        esbuild.buildSync(options);
    }
    catch (e) {
        process.exit();
    }
}

export function tscBuildOrFail(tscCmd: string, buildTarget: string) : void {
    console.log(`[tsc] building ${buildTarget}`);
    const tscOutputLines = fixupTscOutput(child_process.spawnSync(tscCmd, ["--build", buildTarget]));
    if (tscOutputLines.length > 0) {
        for (const msg of tscOutputLines) {
            console.log(msg);
        }
        process.exit();
    }
}

// child_process.spawnSync().output effectively returns (null | string)[],
// we want the non-null, non-empty-string elements
function fixupTscOutput(childProcessResult: child_process.SpawnSyncReturns<string>) : string[] {
    return filterMap(
        childProcessResult.output,
        null,
        (line) => line ? (line.toString() || null) : null);
}

// not strictly necessary, messing with typescript
// trying to get a well-typed O(n) instead of O(2n) "filter + map"
type Subtract<TUnionMinuend, TSubtrahend> = TUnionMinuend extends TSubtrahend ? never : TUnionMinuend;
function filterMap<T, U, NilMarker>(ts: T[], nilIdentity: NilMarker, f: (t: T) => U | NilMarker) : Subtract<U, NilMarker>[] {
    const result : U[] = [];
    for (const t of ts) {
        const r = f(t);
        if (r === nilIdentity) continue;
        else result.push(r as U);
    }
    return result as any;
}