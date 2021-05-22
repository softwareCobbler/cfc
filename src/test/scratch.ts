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
<!---
    <!--- the query type is included as a library definition during type checking --->
    @type Query = <T> => <U> => {
        recordCount: number,
        columnList: string,
        filter: (required predicate: (row: T, currentRow: number, query: Query<T>) => boolean) => Query<T>,
        recursive: Query<U><T>
    } & T;

    @type X = {foo: string}

    @type MySchema = {v: string, rec_uid: number, filename: string };
--->

<!--- @type Query<{x: number}><{y: string}> --->
<cfquery name="q">
    select * from foo where bar = baz;
</cfquery>

<cfscript>
    q.
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