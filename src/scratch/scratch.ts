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
                "Wirebox.cfc": `
                    component {
                        public function mega() {
                            x.foobar();
                        }
                    }`,
                "lib.d.cfm": `
                    @interface Array<T> {
                        //[index: numeric]: T,
                        //len: () => numeric,
                        //toList: (sep?: string) => string,
                        //append: (val: T) => T[],
                        //map: <U>(m: (e: T, i?: number, a?: T[]) => U) => T[]
                        //each: (callback: (e?: T, i?: number, a?: T[])) => void
                        // free function:
                        // @declare function arrayEach<T>(
                        // a: T[],
                        // f: (e?: T[], i?: numeric, a?: T[]) => void # no-suggest-param-names
                        //) no-suggest-param-names
                        maplike: <U>(mapper: (e: T) => U) => U[]
                    }

                `,
                "someFile.cfc": `
                    /**
                     */
                    component {
                        function index( event, rc, prc ) {
                            // @type string[]
                            var foo = [];

                            foo.maplike((e) => {
                                return new someFile();
                            }).maplike((v) => v.    // 381
                        }

                        function bar() {}
                    }
                    `,
                "a": {
                    "b": {
                        "x.cfc": `
                        component { function foobar() { this.mlem(); } }
                        `,
                        "y.cfc": "component { function mlem() {} }",
                    },
                    "c": {
                        "x.cfc": "component extends='a.b.x' {}"
                    }
                },
            }
        }
    );

    //let x = debugfs.readFileSync("/Child.cfc").toString().slice(102,105)
    
    const project = Project("/", /*filesystem*/debugfs, {debug: true, parseTypes: true, engineVersion: EngineVersions["acf.2018"], withWireboxResolution: true, wireboxConfigFileCanonicalAbsPath: "/Wirebox.cfc"});
    //const project = Project([path.resolve(".")], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const target = path.join(path.resolve("./test/"), "mxunit/framework/javaloader/JavaProxy.cfc");
    //project.addFile(target);

    project.addEngineLib("/lib.d.cfm");
    project.addFile("/someFile.cfc");
    const diagnostics = project.getDiagnostics("/Base.cfc");

    //const x = project.getInterestingNodeToLeftOfCursor("/someFile.cfc", 378);
    const completions = getCompletions(project, "/someFile.cfc", 381, null);
    console.log(completions);
    for (const diagnostic of diagnostics) {
        console.log(diagnostic);
    }
}
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

