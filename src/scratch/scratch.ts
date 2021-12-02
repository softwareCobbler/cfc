// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType, getBOMType, getBOMBytes, BOM } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree, isExpressionContext, recursiveGetFiles } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { DebugFileSystem, FileSystem, Project } from "../compiler/project";
import { EngineVersions } from "../compiler/engines";
import { getCompletions } from "../services/completions";
import { roundTrip } from "../services/printer";

import * as fs from "fs";
import * as path from "path";
import { NodeFlags } from "../compiler/node";

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
                        placeholder: any // map: <U>(callback: (e: T, i?: numeric, a?: T[]) => U, parallel?: boolean, maxThreads?: boolean) => U[]
                    }
                `,
                //"realLib.d.cfm": fs.readFileSync("C:\\Users\\anon\\dev\\cfc\\src\\lang-server\\server\\src\\runtimelib\\lib.cf2018.d.cfm").toString(),
                "someFile.cfc": `

                    `,
                "a": {
                    "b": {
                        "x.cfc": `
                        component extends="someFile" {
                            function foobar() {
                                foo();
                            }
                        }
                        `,
                        "y.cfc": "component { function mlem() {} }",
                        "z.cfm": `
                        <cfscript>
                            this[ arguments.name ] = variables[ arguments.name ] = function(){};
                        </cfscript>
                        `
                    },
                    "c": {
                        "x.cfc": "component extends='a.b.x' {}"
                    }
                },
            }
        }
    );

    //let x = debugfs.readFileSync("/Child.cfc").toString().slice(102,105)
    const sourceFile = SourceFile("", CfFileType.cfc, debugfs.readFileSync("/a/b/z.cfm"))
    roundTrip(sourceFile);

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
    
    // project.addEngineLib("/lib.d.cfm");
    // project.addEngineLib("/realLib.d.cfm");
    project.addFile("/a/b/z.cfm");
    //project.addFile("C:\\Users\\anon\\dev\\cb\\testbox\\tests\\resources\\coldbox\\system\\EventHandler.cfc");
    //const diagnostics = project.getDiagnostics("/someFile.cfc");

    //const x = project.getInterestingNodeToLeftOfCursor("/someFile.cfc", 378);
    //const completions = getCompletions(project, "/someFile.cfc", 381, null);
    //console.log(completions);
    // for (const diagnostic of diagnostics) {
    //     console.log(diagnostic);
    // }
}

// projectFiddle();
// process.exit();


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

function printFiddle() {
    const files = recursiveGetFiles("c:/users/anon/dev/coldbox/", /\.cf(c|m)$/i);
    for (const file of files) {
        //if (file === 'c:\\users\\anon\\dev\\coldbox\\coldbox\\system\\core\\conversion\\XMLConverter.cfc') debugger;
        const raw = fs.readFileSync(file);
        const bom = getBOMType(raw);
        const sourceFile = SourceFile("", cfmOrCfc(file)!, raw);
        const output = roundTrip(sourceFile);
        if (sourceFile.flags & NodeFlags.isCfcInterface) {
            continue; // we don't do anything with cfc interfaces
        }
        if (output.ok) {
            const xbom = bom ? getBOMBytes(bom) : "";
            const xtext = xbom + output.text;
            const xraw = raw.toString();
            if (xraw == xtext) {
                console.log("roundtrip success: " + file);
                continue;
            }
            else {
                console.log("parse ok, roundtrip fail: " + file);
                for (let i = 0; i < Math.min(xraw.length, xtext.length); i++) {
                    if (xraw[i] === xtext[i]) continue;
                    else {
                        console.error("\tmismatch at index: ", i);
                        break;
                    }
                }
            }
        }
        else {
            console.log("parse errors: " + file);
            for (const diagnostic of output.diagnostics) {
                console.log(diagnostic);
            }
        }
    }

}
printFiddle();