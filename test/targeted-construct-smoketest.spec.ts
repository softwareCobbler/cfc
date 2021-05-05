import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import {Scanner, Parser, Binder, CfFileType, SourceFile } from "../out/compiler";

const parser = Parser().setDebug(true);
const binder = Binder().setDebug(true);

function assertDiagnosticsCount(text: string, cfFileType: CfFileType, count: number) {
    const sourceFile = SourceFile("", cfFileType, text);
    parser.setSourceFile(sourceFile).parse(cfFileType);
    const diagnostics = parser.getDiagnostics();
    binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());
    assert.strictEqual(diagnostics.length, count, `${count} diagnostics emitted`);
}

describe("general smoke test for particular constructs", () => {
    it("Should accept `new` expression in an expression context", () => {
        assertDiagnosticsCount(`<cfset x = {v: new foo.bar().someMethod()}>`, CfFileType.cfm, 0);
    });
    it("Should accept named arguments with both '=' and ':' as the name/expression delimiter", () => {
        assertDiagnosticsCount(`<cfset x = foo(x=1, y:2)>`, CfFileType.cfm, 0);
    });
    it("Should accept `%=`", () => {
        assertDiagnosticsCount(`<cfset x %= y>`, CfFileType.cfm, 0);
    });
    it("Should accept for-in and normal-for", () => {
        assertDiagnosticsCount(`
            <cfscript>
                for (x in y) {
                    for (i = 0; i < 10; i++) {
            
                    }
                }
            </cfscript>`, CfFileType.cfm, 0);
    });
    it("Should accept the legacy operator `EQV`", () => {
        assertDiagnosticsCount(`<cfif a eqv b></cfif>`, CfFileType.cfm, 0);
    });
    it("Should accept the legacy operator `IMP`", () => {
        assertDiagnosticsCount(`<cfif a imp b></cfif>`, CfFileType.cfm, 0);
    });
    it("Should be OK with multibyte utf16 characters", () => {
        assertDiagnosticsCount(`<cfif 😬 EQ 😬>😅</cfif>`, CfFileType.cfm, 0);
    });
    it("Should accept optional dot access", () => {
        assertDiagnosticsCount(`<cfif a?.b eq c?.d></cfif>`, CfFileType.cfm, 0);
    });
    it("Should issue a diagnostic for an optional bracket access", () => {
        assertDiagnosticsCount(`<cfif a?.deep?.["b"] eq c?.deep?.["d"]></cfif>`, CfFileType.cfm, 2);
    });
    it("Should issue a diagnostic for an optional call", () => {
        assertDiagnosticsCount(`<cfif a?.deep?.() gt b?.deep?.()></cfif>`, CfFileType.cfm, 2);
    });
    it("Should issue diagnostics for sugared aborts not followed by string literals or semicolons", () => {
        assertDiagnosticsCount(`<cfscript>abort function foo() {};</cfscript>`, CfFileType.cfm, 1);
        assertDiagnosticsCount(`<cfscript>abort v</cfscript>`, CfFileType.cfm, 1);
        assertDiagnosticsCount(`<cfscript>abort 1</cfscript>`, CfFileType.cfm, 1);
    });
    it("Should accept sugared aborts followed by string literals or semicolons", () => {
        assertDiagnosticsCount(`<cfscript>abort;</cfscript>`, CfFileType.cfm, 0);
        assertDiagnosticsCount(`<cfscript>abort "v"</cfscript>`, CfFileType.cfm, 0);
        assertDiagnosticsCount(`<cfscript>abort '1'</cfscript>`, CfFileType.cfm, 0);
    });
    it("Should flag var declaration binding-phase errors", () => {
        assertDiagnosticsCount(`
            <cfset var illegal_var_decl_at_top_level = 42>

            <cffunction name="foo">
                <cfargument name="ARGNAME">
                <cfset var ok_var_decl_inside_function = 42>
                <cfset var argname = 42> <!--- can't re-declare a variable that is in arguments scope --->
            </cffunction>

            <cfscript>
                function foo(argName, argName2) {
                    var argName2 = 42;
                }

                f = function(argName) {
                    var argName = 42;

                    function nested(x) {
                        var argName = "ok because the outer arguments scope is not considered";
                    }
                }

                f = (argName) => {
                    var argName = 42;
                    var argName.f.z = 42;

                    argName = 42; // ok, not a redeclaration, just a reassignment
                }
            </cfscript>
        `, CfFileType.cfm, 6);
    });
});