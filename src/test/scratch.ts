// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, flattenTree } from "../compiler/utils";

import * as fs from "fs";
import * as path from "path";

/*const fname = path.resolve("./test/mxunit/doc/colddoc/strategy/AbstractTemplateStrategy.cfc");
console.error("parsing: " + fname);
const sourceFile = SourceFile(fname, cfmOrCfc(fname)!, fs.readFileSync(fname));*/

const parser = Parser().setDebug(true);


const sourceFile = NilCfm( `
<cfscript>
    function foo() {
        cgi.
    }
</cfscript>`);

parser.setSourceFile(sourceFile);
const binder = Binder().setDebug(true);

parser.parse();
binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);

let match = binarySearch(
    flatProgram,
    (v) => {
        const target = 12;
        if (v.range.fromInclusive <= target && target < v.range.toExclusive) {
            // match: on or in the target index
            return 0;
        }
        else if (v.range.fromInclusive < target) {
            return -1;
        }
        else {
            return 1;
        }
    });

match = match < 0 ? ~match : match;
const node = binder.NodeMap.get(flatProgram[match].nodeId);

const diagnostics = parser.getDiagnostics();
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}