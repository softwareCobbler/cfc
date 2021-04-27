// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import * as fs from "fs";
import * as path from "path";

// PluginDemoTests\InvalidMarkupTest.cfc
//const fname = path.resolve("./test/mxunit/tests/framework/RemoteFacadeObjectCacheTest.cfc");
//console.error("parsing: " + fname);
//const scanner = Scanner(fs.readFileSync(fname));
const scanner = Scanner(`
<cfcomponent>
<cfset foo = a?.()>
</cfcomponent>
`);


const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfc);

const diagnostics = parser.getDiagnostics();
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}