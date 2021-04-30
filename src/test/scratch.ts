// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser } from "../compiler";
import { CfFileType } from "../compiler/scanner";
//import * as fs from "fs";
//import * as path from "path";

// PluginDemoTests\InvalidMarkupTest.cfc
//const fname = path.resolve("./test/mxunit/generator/listFiles.cfm");
//console.error("parsing: " + fname);
//const scanner = Scanner(fs.readFileSync(fname));
const scanner = Scanner(`<cfset x=4 <!---<!--- --->   >`);


const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfm);

const diagnostics = parser.getDiagnostics();
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}