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
        ["/a.cfm", `
            <cfset var illegal_var_decl_at_top_level = 42>

            <cffunction name="foo">
                <cfargument name="ARGNAME">
                <cfset var ok_var_decl_inside_function = 42>
                <cfset var argname = 42> <!--- can't re-declare a variable that is in arguments scope --->
            </cffunction>

            <cfscript>
                function foo(argName, argName2) {
                    var argName2 = 42;
                }

                f = function(argName) {
                    var argName = 42;

                    function nested(x) {
                        var argName = "ok because the outer arguments scope is not considered";
                    }
                }

                f = (argName) => {
                    var argName = 42;
                    var argName.f.z = 42;

                    argName = 42; // ok, not a redeclaration, just a reassignment
                }

                f = () => {
                    // var x redeclaration should be OK
                    for (var x in y) {}
                    for (var x in y) {}
                }
            </cfscript>
        `],
        //["/b.cfc", `component { function foo() {} }`],
        //["/lib.d.cfm", "@declare function foo(arg0: number[]) : string"]
    ], "/");

    const project = Project(["/"], /*filesystem*/debugfs, {debug: true, parseTypes: true});

    const a = project.addFile("/a.cfm");
    //const b = project.addFile("/b.cfc");
    //const c = project.addFile("/lib.d.cfm");

    for (const diagnostic of project.getDiagnostics("/a.cfm")) {
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