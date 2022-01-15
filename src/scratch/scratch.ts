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
                `,
                //"realLib.d.cfm": fs.readFileSync("C:\\Users\\anon\\dev\\cfc\\src\\lang-server\\server\\src\\runtimelib\\lib.cf2018.d.cfm").toString(),
                "someFile.cfm": `
                    <cfqueryparam cfsqltype="">
                `,
                "a": {
                    "b": {
                        "x.cfc": `
                        /**
                         * @!typedef X_t = {A: 0, B: 1, C: 2}
                         */
                         component accessors="true" {
                             /**
                             * @!type () => X_t
                             */
                             function doit() {
                                 var v = {
                                     A: 42, 
                                     B: 1,
                                     C: 2
                                 }
                                 return v; // should error ...
                             }
                         }
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
    
    const project = Project("/", debugfs, {
        debug: true,
        parseTypes: true,
        engineVersion: EngineVersions["lucee.5"],
        withWireboxResolution: true,
        wireboxConfigFileCanonicalAbsPath: "/Wirebox.cfc",
        checkReturnTypes: true,
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
    project.addFile("/a/b/x.cfc");
    //project.addFile("C:\\Users\\anon\\dev\\cb\\testbox\\tests\\resources\\coldbox\\system\\EventHandler.cfc");
    const diagnostics = project.getDiagnostics("/a/b/x.cfc");

    //const x = project.getInterestingNodeToLeftOfCursor("/someFile.cfc", 378);
    //const completions = getCompletions(project, "/someFile.cfc", 381, null);
    //console.log(completions);
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

