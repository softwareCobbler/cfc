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

//const sourceFile = fromFile("./test/mxunit/PluginDemoTests/InvalidMarkupTest.cfc");

const sourceFile = NilCfm(`<cftry><cfcatch/> <cfset x = 4></cftry>`);

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