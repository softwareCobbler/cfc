// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, Parser, Binder, NilCfc, NilCfm, SourceFile } from "../compiler";
import { CfFileType } from "../compiler/scanner";
import { binarySearch, cfmOrCfc, findNodeInFlatSourceMap, flattenTree } from "../compiler/utils";

import * as fs from "fs";
import * as path from "path";

function fromFile(fname: string) {
    const absPath = path.resolve(fname);
    return SourceFile(absPath, cfmOrCfc(fname)!, fs.readFileSync(absPath));
}

//const sourceFile = fromFile("./test/mxunit/tests/mightymock/fixture/Logger.cfc");

const sourceFile = NilCfm(`<cfcomponent>
<cffunction name="hmmmm">
    <cfscript>
    </cfscript>
</cffunction>

<cffunction name="foo">
    <cfargument name="lel">
    <cfquery name="local.q">
        <cfscript>
        </cfscript>
    </cfquery>
    <cfreturn {ok:true}>
</cffunction>

<cftry>
    <cfcatch>
    </cfcatch>
    <cffinally>
        <cftry>
            <cfcatch>
                <cfswitch>
                    <cfcase value=#x#>
                        <cfscript>
                            function foo(haha, ok) {
                                arguments.|<<<<
                                for (var x = 0; x < 10; x++) {

                                }
                            }
                        </cfscript>
                    </cfcase>
                </cfswitch>
            </cfcatch>
        </cftry>
    </cffinally>
</cftry>
</cfcomponent>`);

const parser = Parser().setDebug(true);
parser.setSourceFile(sourceFile);
const binder = Binder().setDebug(true);

parser.parse();
//binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());

const flatProgram = flattenTree(sourceFile);
const diagnostics = parser.getDiagnostics();

console.log("got ", diagnostics.length + " diagnostics");
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}