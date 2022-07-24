// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree, isExpressionContext, recursiveGetFiles } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { DebugFileSystem, FileSystem, Project } from "../compiler/project";
import { EngineVersions } from "../compiler/engines";
import { getCompletions } from "../services/completions";

import * as fs from "fs";
import * as path from "path";

function projectFiddle() {
    const debugfs = DebugFileSystem(
        {
            "/": {
                "cfconfig.json": `
                    {
                        "cf": {
                            "foo": "foo/bar/baz"
                        },
                        "wirebox": {
                            "someBinding": "foo.someCfc",
                            "someOtherBinding": "haha"
                        }
                    }
                `,
                "lib.d.cfm": `
                    @!interface Array<T> { PLACEHOLDER: any }
                `,
                //"realLib.d.cfm": fs.readFileSync("C:\\Users\\anon\\dev\\cfc\\src\\lang-server\\server\\src\\runtimelib\\lib.cf2018.d.cfm").toString(),
                "coolFolder": {
                    "QuickOrmTypedefs.d.cfm": `
                        @!namespace transforms {
                            @!typedef cfcFunctions<F> = F.cfname extends "scope#infer V#" ? inject<V, (...args: F.cfargs) => F.cfreturn> : 0;
                        }
                    `
                },
                "someFolder": {
                    "generic.cfc": `
                        component {
                            /**
                             * @!typeparam T extends keyof Wirebox["mappings"]
                             * @!arg name : {value: T}
                             * @!returns cfc<Wirebox["mappings"][T]>
                             */
                            function getInstanceLike(name) {}

                            getInstanceLike("someBinding");
                        }
                    `,
                    "someFile.cfc": `
                        /**
                         @!namespace CbOrm {
                            @!namespace method_expression_intellisense_provider {
                                @!typedef start_token = "findBy" | "findAllBy" | "countBy"
                                @!typedef conditional =
                                    | ""
                                    | "LessThanEquals"
                                    | "LessThan"
                                    | "GreaterThanEquals"
                                    | "GreaterThan"
                                    | "Like"
                                    | "NotEqual"
                                    | "isNull"
                                    | "isNotNull"
                                    | "NotBetween"
                                    | "Between"
                                    | "NotInList"
                                    | "inList"
                                @!typedef operator = "And" | "Or"

                                @!typedef prop_and_maybe_condition<Result, S, Fs, Ps> = S extends "#infer p extends Ps["cfname"]##infer c extends conditional##infer rest#"
                                    ? maybe_chain_with_operator<"#Result##p##c#", rest, Fs, Ps>
                                    : "#Result##Ps["cfname"]#" | "#Result##Ps["cfname"]##conditional#"

                                @!typedef maybe_chain_with_operator<Result, S, Fs, Ps> = S extends "#infer op extends operator##infer rest#"
                                    ? pc<"#Result##op#", rest, Fs, Ps>
                                    : "Result#operator##Ps.cfname#"

                                @!typedef main<UserText, Fs, Ps> = UserText extends "#infer head extends start_token##infer rest#"
                                    ? prop_and_maybe_condition<head, rest, Fs, Ps>
                                    : "#start_token##Ps["cfname"]#"
                            }
                        }
                        */
                        component {
                            /**
                             * @!arg foo : CbOrm.method_expression_intellisense_provider.main<"findAllBypr", Fs, Ps>
                             * @!typedef Ps = {cfname: "prop1"} | {cfname: "prop2"}
                             * @!typedef Fs = never
                             **/
                            function scopeWithFoo(required string foo, string baz) {}
                        }
                    `
                },
                "foo": {
                    "bar": {
                        "baz": {
                            "someCfc.cfc": `
                                component {
                                    function fromSomeResolvedWireboxComponent() {
                                        return {helloWorld: 42};
                                    }
                                }
                            `,
                        }
                    }
                },
            }
        }
    );

    //let x = debugfs.readFileSync("/Child.cfc").toString().slice(102,105)
    
    const project = Project("/", debugfs, {
        debug: true,
        parseTypes: true,
        engineVersion: EngineVersions["lucee.5"],
        withWireboxResolution: true,
        cfConfigProjectRelativePath: "cfconfig.json",
        checkReturnTypes: true,
        checkFlowTypes: true,
        genericFunctionInference: true,
        cancellationToken: {
            cancellationRequested: () => false,
            throwIfCancellationRequested: () => void 0
        }
    });
    //const project = Project([path.resolve(".")], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const target = path.join(path.resolve("./test/"), "mxunit/framework/javaloader/JavaProxy.cfc");
    
    //project.addEngineLib("/lib.d.cfm");
    // project.addEngineLib("/realLib.d.cfm");
    //project.addFile("/coolFolder/QuickOrmTypedefs.d.cfm");
    project.addFile("/someFolder/generic.cfc");
    //project.addFile("C:\\Users\\anon\\dev\\cb\\testbox\\tests\\resources\\coldbox\\system\\EventHandler.cfc");
    const diagnostics = project.getDiagnostics("/someFolder/someFile.cfc");

    //const x = project.getInterestingNodeToLeftOfCursor("/someFile.cfc", 378);
    //const completions = getCompletions(project, "/someFile.cfc", 381, null);
    //console.log(completions);
    for (const diagnostic of diagnostics) {
        console.log(diagnostic);
    }
}

function bench() {
    function recursiveGetFiles(root: string, pattern: RegExp) : string [] {
        const result : string[] = [];
        const fds = fs.readdirSync(root, {withFileTypes: true});
        for (const fd of fds) {
            if (fd.isDirectory()) result.push(...recursiveGetFiles(path.resolve(root, fd.name), pattern));
            else if (pattern.test(fd.name)) {
                const fspath = path.resolve(root, fd.name);
                result.push(fspath);
            }
        }
        return result;
    }

    const files = recursiveGetFiles(process.env.XPATH as string, /\.(cfm|cfc)$/i);

    const parser = Parser({
        debug: false,
        parseTypes: true,
        engineVersion: EngineVersions["lucee.5"],
        withWireboxResolution: true,
        cfConfigProjectRelativePath: "cfconfig.json",
        checkReturnTypes: true,
        checkFlowTypes: true,
        genericFunctionInference: true,
        cancellationToken: {
            cancellationRequested: () => false,
            throwIfCancellationRequested: () => void 0
        }
    });

    const times : bigint[] = []
    let lines = 0;

    for (const file of files) {
        const sourceBuffer = fs.readFileSync(file)

        const re = /\r|\n|\r\n/g;
        let linesThis = 0;
        const asString = sourceBuffer.toString();
        while (re.exec(asString)) linesThis++;
        

        const sourceFile = SourceFile(file, cfmOrCfc(file)!, sourceBuffer);
        const start = process.hrtime.bigint();
        parser.parse(sourceFile);
        const end = process.hrtime.bigint();

        if (linesThis > 1000) {
            times.push(end - start);
            lines += linesThis;
            console.log(file, linesThis + " lines in ", ((end-start) / BigInt(1e6)) + "ms")
        }


    }

    let timeSum_ns = BigInt(0);
    for (const time of times) timeSum_ns += time;
    const avgTime_ns = timeSum_ns / BigInt(times.length);
    const avgTime_ms = avgTime_ns / BigInt(1e6)

    console.log("Total time: " + (timeSum_ns / BigInt(1e6)) + "ms")
    console.log("Avg parsetime: " + avgTime_ms + "ms");
    console.log("Total lines: " + lines);
    console.log("Lines/sec: " + (BigInt(lines) / (timeSum_ns / BigInt(1e6))) * BigInt(1000));
}

//bench()
projectFiddle();


/*function xfiddle() {
    const files = recursiveGetFiles("c:/users/anon/dev/coldbox/", /\.cfc$/i);
    const project = Project(["c:\\users\\anon\\dev\\coldbox\\"], FileSystem(), {debug: false, parseTypes: false, language: LanguageVersion.acf2018});
    project.addEngineLib("c:\\Users\\anon\\dev\\cfc\\cflsp-vscode\\out\\lib.cf2018.d.cfm")
    for (const file of files) {
        console.log(file);
        project.addFile(file);
    }
    console.log("done");
}

xfiddle();*/

