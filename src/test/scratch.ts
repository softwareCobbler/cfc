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
import { cfTypeFunctionDefinition, evaluateType, evaluateTypeCall, Type } from "../compiler/types";

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

//const sourceFile = fromFile("./test/mxunit/doc/build.cfm");

const sourceFile = NilCfm(`
<!---
    @type Query = <T> => {
        recordCount: number,
        columnList: string,
        filter: (required predicate: (row: T) => boolean, currentRow: number, query: Query<T>) => Query<T>,
    } & T;
    @type MySchema = {rec_uid: number};
--->

<cfquery name="q" type:="Query<MySchema>">
    select * from foo where bar = baz;
</cfquery>

<cfquery name="q" type:="Query<MySchema>">
    select * from foo where bar = baz;
</cfquery>

<cfscript>
    x = 3;
    q.filter((row) => { row. });
    q.filter();
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