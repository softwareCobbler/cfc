// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree, recursiveGetFiles } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { DebugFileSystem, FileSystem, LanguageVersion, Project } from "../compiler/project";
import { getCompletions } from "../services/completions";

import * as fs from "fs";
import * as path from "path";

function projectFiddle() {
    const debugfs = DebugFileSystem(
        {
            "/": {
                "Wirebox.cfc": `
                    component {
                        public someFile function mega() {
                            getInstance();
                        }
        
                        function configure() {
                            //mega().foo().mega();
                            //mapDirectory("a.b");
                            //mapDirectory("c.d");
                        }
                    }`,
                "someFile.cfc": `
                    /** @interface variables { _wirebox: Wirebox, getInstance: Wirebox.getInstance } */
                    /** @interface application { getInstance: Wirebox.getInstance } */
                    component {
                        function foo() {
                            application.getInstance("a.b.x");
                            application.getOtherInstance();
                        }
                    }`,
                "a": {
                    "b": {
                        "x.cfc": "component { function foobar() {} }",
                        "y.cfc": "component {}",
                    }
                },
            }
        }
    , "/");

    //let x = debugfs.readFileSync("/Child.cfc").toString().slice(102,105)
    
    const project = Project("/", /*filesystem*/debugfs, {debug: true, parseTypes: true, language: LanguageVersion.lucee5, withWireboxResolution: true, wireboxConfigFileAbsPath: "/Wirebox.cfc"});
    //const project = Project([path.resolve(".")], FileSystem(), {debug: true, parseTypes: true, language: LanguageVersion.lucee5});
    //const target = path.join(path.resolve("./test/"), "mxunit/framework/javaloader/JavaProxy.cfc");
    //project.addFile(target);

    project.addFile("/Wirebox.cfc");
    //project.addFile("/someFile.cfc");

    const diagnostics = project.getDiagnostics("/Base.cfc");

    //const x = project.getInterestingNodeToLeftOfCursor("/someFile.cfc", 378);
    const completions = getCompletions(project, "/someFile.cfc", 378, null);
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

