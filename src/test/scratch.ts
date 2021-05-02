// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder } from "../compiler";
import { CfFileType } from "../compiler/scanner";


/*import * as fs from "fs";
import * as path from "path";

const fname = path.resolve("./test/mxunit/framework/TestCase.cfc");
console.error("parsing: " + fname);
const scanner = Scanner(fs.readFileSync(fname));*/

const scanner = Scanner(`
<cfset var x = 4>
<cffunction name="injectProperty" output="false" access="public" returntype="void" hint="injects properties into the receiving object">
    <cfargument name="receiver" type="any" required="true" hint="the object receiving the method"/>
    <cfargument name="propertyName" type="string" required="true" hint="the property to be overwritten"/>
    <cfargument name="propertyValue" type="any" required="true" hint="the property value to be used">
    <cfargument name="scope" type="string" required="false" default="" hint="the scope in which to set the property. Defaults to variables and this.">

    <cfset var receiver = receiver._mixinProperty(propertyName = arguments.propertyName,
                            property = arguments.propertyValue,
                            scope = arguments.scope)>
</cffunction>
`);


const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

const binder = Binder().setDebug(true);

const tree = parser.parse(CfFileType.cfm);
binder.bindProgram(tree, scanner, parser.getDiagnostics());


const diagnostics = parser.getDiagnostics();
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}