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
                        map: <U>(callback: (e: T, i?: numeric, a?: T[]) => U, parallel?: boolean, maxThreads?: boolean) => U[]
                    }
                `,
                "realLib.d.cfm": fs.readFileSync("C:\\Users\\anon\\dev\\cfc\\src\\lang-server\\server\\src\\runtimelib\\lib.cf2018.d.cfm").toString(),
                "someFile.cfc": `
                    /**
                     */

                     component {

  
                        struct function xxx( event, rc, prc ) {
                            // @type cfc<someFile>[]  
                            var v = [];
                            
                            // uncommeting the following when it resolves to a generic will OOM kill it
                            v.map((v) => v.xxx
                            
                            
                            x.map((vvv) => {
                                switch (v) {
                                    case 42: {
                                        return {ok: true, something: "ok!"};
                                    }
                                    default: {
                                        throw();
                                    }
                                }
                            })[1]
                             
                            
                            switch (v) {
                                default: {
                                    //return {};
                                    throw "";
                                    throw()
                                }
                                case 42: { 
                                    throw()
                                }
                            }
                    
                        }
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
    
    const project = Project("/", /*filesystem*/debugfs, {debug: true, parseTypes: true, engineVersion: EngineVersions["lucee.5"], withWireboxResolution: true, wireboxConfigFileCanonicalAbsPath: "/Wirebox.cfc"});
    //const project = Project([path.resolve(".")], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const target = path.join(path.resolve("./test/"), "mxunit/framework/javaloader/JavaProxy.cfc");
    //project.addFile(target);

    project.addEngineLib("/realLib.d.cfm");
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

