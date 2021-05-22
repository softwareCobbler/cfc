// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";
import { Checker } from "../compiler/checker";

import * as fs from "fs";
import * as path from "path";

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

//const sourceFile = fromFile("./test/mxunit/doc/build.cfm");

const sourceFile = NilCfm(`
<cfscript>
    function foo() {
        // @type number
        final var v = '42';
        v + v;
    }
</cfscript>
`);

const parser = Parser().setDebug(true).setParseTypes(true);
const binder = Binder().setDebug(true);
const checker = Checker();
parser.setSourceFile(sourceFile);

parser.parse();
binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());
checker.check(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);

const diagnostics = parser.getDiagnostics();

//evaluateTypeCall(sourceFile.content[0] as cfTypeFunctionDefinition, [sourceFile.content[1]] as Type[]);
//evaluateTypeCall(sourceFile.content[0] as cfTypeFunctionDefinition, [sourceFile.content[1]] as Type[]);

console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}