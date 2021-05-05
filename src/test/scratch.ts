// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilCfc, NilCfm } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, flattenTree } from "../compiler/utils";


/*import * as fs from "fs";
import * as path from "path";

const fname = path.resolve("./test/mxunit/framework/TestCase.cfc");
console.error("parsing: " + fname);
const scanner = Scanner(fs.readFileSync(fname));*/


//                       ^0             ^15, which after typing the X, the text is like `url.x|>`
//                                                                                            ^16

const parser = Parser().setDebug(true);
const sourceFile = NilCfm(`<cfoutput>#     #</cfoutput>`);
parser.setSourceFile(sourceFile);
const binder = Binder().setDebug(true);

parser.parse();
binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);

let match = binarySearch(
    flatProgram,
    (v) => v.range.fromInclusive < 11
        ? -1
        : v.range.fromInclusive === 11
        ? 0 : 1);

match = match < 0 ? ~match : match;
const node = binder.NodeMap.get(flatProgram[match].nodeId);
console.log(node);

const diagnostics = parser.getDiagnostics();
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}