// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { DebugFileSystem, Project } from "../compiler/project";

import * as fs from "fs";
import * as path from "path";

function projectFiddle() {
    const debugfs = DebugFileSystem([
        ["/a.cfc", `
            component {
                function foo(required a, required b) {
                    foo(argumentCollection=arguments);
                }
            }
        `],
        //["/b.cfc", `component { function foo() {} }`],
        //["/lib.d.cfm", "@declare function foo(arg0: number[]) : string"]
    ], "/");

    const project = Project(["/"], /*filesystem*/debugfs, {debug: true, parseTypes: true});

    const a = project.addFile("/a.cfc");
    //const b = project.addFile("/b.cfc");
    //const c = project.addFile("/lib.d.cfm");

    for (const diagnostic of project.getDiagnostics("/a.cfc")) {
        console.log(diagnostic);
    }
}

projectFiddle();
process.exit();

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

const libPath = path.resolve("./src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm");
const stdLib = SourceFile(libPath , CfFileType.dCfm, fs.readFileSync(libPath));

//const sourceFile = fromFile("c:\\Users\\anon\\dev\\cf-ts-compiler\\mxunit\\framework\\TestDecorator.cfc");

const sourceFile = NilCfm(`
<cfscript>
    // @type (foo_arg: string) => ({x: number})
`);

const parser = Parser().setDebug(true).setParseTypes(true);
const binder = Binder().setDebug(true);
const checker = Checker();

/*parser.setSourceFile(stdLib);
parser.parse();
binder.bind(stdLib, parser.getScanner(), parser.getDiagnostics());
checker.check(stdLib, parser.getScanner(), parser.getDiagnostics());

sourceFile.libRefs.push(stdLib);*/

parser.setSourceFile(sourceFile);
parser.parse();
binder.bind(sourceFile);
checker.check(sourceFile);

const flatTree = flattenTree(sourceFile);

const diagnostics = sourceFile.diagnostics;
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of diagnostics) {
    //console.log(diag);
}