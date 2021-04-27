import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import {Scanner, Parser, CfFileType } from "../out/compiler";

const parser = Parser().setDebug(true);

function assertDiagnosticsCount(text: string, cfFileType: CfFileType, count: number) {
    const scanner = Scanner(text);
    parser.setScanner(scanner).parse(cfFileType);
    assert.strictEqual(parser.getDiagnostics().length, count, `${count} diagnostics emitted`);
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
    it("Should be OK with multibyte utf16 characters`", () => {
        assertDiagnosticsCount(`<cfif 😬 EQ 😬>😅</cfif>`, CfFileType.cfm, 0);
    });
    it("Should accept optional dot access`", () => {
        assertDiagnosticsCount(`<cfif a?.b eq c?.d></cfif>`, CfFileType.cfm, 0);
    });
    it("Should issue a diagnostic for an optional bracket access`", () => {
        assertDiagnosticsCount(`<cfif a?.deep?.["b"] eq c?.deep?.["d"]></cfif>`, CfFileType.cfm, 2);
    });
    it("Should issue a diagnostic for an optional call`", () => {
        assertDiagnosticsCount(`<cfif a?.deep?.() gt b?.deep?.()></cfif>`, CfFileType.cfm, 2);
    });
});