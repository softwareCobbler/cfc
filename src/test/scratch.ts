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

//const sourceFile = fromFile("./test/mxunit/doc/build.cfm");

const sourceFile = NilCfm(`
<cfscript>
    function foo(first, ...rest) {
        final var bar = (...rest) => rest;
        final var struct_check = () => {
            var base = {x: 1, y:2};
            var spread_target = {a:0, ...base};
            spread_target = {...base};
            spread_target = {a:0, ...base, ...base};
        }

        final var array_check = () => {
            var base = [1,2,3];
            var spread_target = [42, ...base];
            spread_target = [...base];
            spread_target = [42, ...base, ...base];
        }
    }
</cfscript>
`);

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