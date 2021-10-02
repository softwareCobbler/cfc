// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree, recursiveGetFiles } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { DebugFileSystem, FileSystem, LanguageVersion, Project } from "../compiler/project";

import * as fs from "fs";
import * as path from "path";

function projectFiddle() {
    const debugfs = DebugFileSystem([
        ["/Base.cfc", `
            component {
                public Base function foo() {
                    return this;
                }
            }
        `],
        //["/b.cfc", `component { function foo() {} }`],
        //["/lib.d.cfm", "@declare function foo(arg0: number[]) : string"]
    ], "/");

    const project = Project(["/"], /*filesystem*/debugfs, {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const project = Project([path.resolve(".")], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const target = path.join(path.resolve("./test/"), "mxunit/framework/javaloader/JavaProxy.cfc");
    //project.addFile(target);

    const a = project.addFile("/Base.cfc");

    const diagnostics = project.getDiagnostics("/Base.cfc");

    for (const diagnostic of diagnostics) {
        console.log(diagnostic);
    }
}
//projectFiddle();


function xfiddle() {
    const files = recursiveGetFiles("c:/users/anon/dev/coldbox/", /\.cfc$/i);
    const project = Project(["c:\\users\\anon\\dev\\coldbox\\"], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    project.addEngineLib("c:\\Users\\anon\\dev\\cfc\\cflsp-vscode\\out\\lib.cf2018.d.cfm")
    for (const file of files) {
        console.log(file);
        project.addFile(file);
    }
    console.log("done");
}

xfiddle();

