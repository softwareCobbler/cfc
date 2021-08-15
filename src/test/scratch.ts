// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilDCfm, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";
import { Checker } from "../compiler/checker";
import { Project } from "../compiler/project";

import * as fs from "fs";
import * as path from "path";

function projectFiddle() {
    const parser = Parser().setDebug(true);
    const binder = Binder().setDebug(true);
    const checker = Checker();
    const project = Project(["c:/users/anon/dev/cf-ts-compiler/mxunit/"], /*debug*/ true);

    project.addFile("C:\\Users\\anon\\dev\\cf-ts-compiler\\mxunit\\PluginDemoTests\\CompareDialogTest.cfc")
}

projectFiddle();
process.exit();

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

const libPath = path.resolve("./src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm");
const stdLib = SourceFile(libPath , CfFileType.dCfm, fs.readFileSync(libPath));

const sourceFile = fromFile("c:\\Users\\anon\\dev\\cf-ts-compiler\\mxunit\\framework\\TestDecorator.cfc");

/*const sourceFile = NilCfc(`
<cfcomponent>
    <!--- @type Query = <T = any> => {recordCount: number, currentRow: number} & {[key in keyof T]: T[key] & T[key][]} --->
    <cfset final this.lel = {
        m1: 1,
        m2: 2,
        m3: 3
    }>

    <cffunction name="init">
        <cfargument name="arg0" default=#0#>
    </cffunction>

    <cffunction name="foo">
        <cfscript>
            // @type {type: string, descr: string}[]
            final var x = [];
        </cfscript>
    </cffunction>
</cfcomponent>
`);*/

const sourceFile2 = NilCfc(`
<cfcomponent>
    <cffunction name="x">
        <cfscript>
            final var a = new A();
        </cfscript>
    </cffunction>
</cfcomponent>
`);

const parser = Parser().setDebug(true).setParseTypes(true);
const binder = Binder().setDebug(true);
const checker = Checker();

/*parser.setSourceFile(stdLib);
parser.parse();
binder.bind(stdLib, parser.getScanner(), parser.getDiagnostics());
checker.check(stdLib, parser.getScanner(), parser.getDiagnostics());

sourceFile.libRefs.push(stdLib);*/

parser.setSourceFile(sourceFile);
parser.parse();
binder.bind(sourceFile);
checker.check(sourceFile);

parser.setSourceFile(sourceFile2);
parser.parse();
binder.bind(sourceFile2);
checker.check(sourceFile2);

const flatTree = flattenTree(sourceFile);

const diagnostics = sourceFile.diagnostics;
console.log("got ", diagnostics.length + " diagnostics");
for (const diag of diagnostics) {
    //console.log(diag);
}