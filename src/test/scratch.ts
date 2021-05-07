// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";

import * as fs from "fs";
import * as path from "path";

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

function fromString(source: string) {
    return NilCfm(source);
}

//const sourceFile = fromFile("./test/mxunit/PluginDemoTests/InvalidMarkupTest.cfc");
const sourceFile = fromString(`<cfset var illegal_var_decl_at_top_level = 42>

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
</cfscript>`);

const parser = Parser().setDebug(true);
parser.setSourceFile(sourceFile);
const binder = Binder().setDebug(true);

parser.parse();
binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);
const diagnostics = parser.getDiagnostics();

console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}