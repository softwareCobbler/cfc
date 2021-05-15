// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";

import * as fs from "fs";
import * as path from "path";
import { cfTypeFunctionDefinition, evaluateType, evaluateTypeCall, Type } from "../compiler/types";

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

//const sourceFile = fromFile("./test/mxunit/doc/build.cfm");

const sourceFile = NilDCfm(`
<!--- in a declaration file, comments are tag comments, and they nest just as they do in <!--- cfm files ---> --->

@type Query = <T> => {
    recordCount: number,
    columnList: string,
    filter: (required predicate: (row: T) => boolean, currentRow: number, query: Query<T>) => Query<T>,
} & T;

<!---
@declare function queryFilter(
    query: Query<Q>,
    required callback /*: (required row: number, currentRow: number, query: query<any>) => void*/,
    parallel /*: {v: number, u: string}[]*/ = 42,
    maxThreadCount /*: number*/) /*: query<any>*/;--->

@type MySchema = {rec_uid: number};
@type OtherSchema = {someDbCol: string};
`);

const parser = Parser().setDebug(true);
parser.setSourceFile(sourceFile);
const binder = Binder().setDebug(true);

parser.parse();
binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);

const diagnostics = parser.getDiagnostics();

evaluateTypeCall(sourceFile.content[0] as cfTypeFunctionDefinition, [sourceFile.content[1]] as Type[]);
evaluateTypeCall(sourceFile.content[0] as cfTypeFunctionDefinition, [sourceFile.content[1]] as Type[]);

console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}