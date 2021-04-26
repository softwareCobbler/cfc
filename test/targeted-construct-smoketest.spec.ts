import * as assert from "assert";
import * as fs from "fs";
import * as path from "path";
import {Scanner, Parser, CfFileType } from "../out/compiler";

const parser = Parser().setDebug(true);

function assertNoDiagnostics(text: string, cfFileType: CfFileType) {
    const scanner = Scanner(text);
    parser.setScanner(scanner).parse(cfFileType);
    assert.strictEqual(parser.getDiagnostics().length, 0, "No diagnostics emitted");
}

describe("general smoke test for particular constructs", () => {
    it("Should accept `new` expression in an expression context", () => {
        assertNoDiagnostics(`<cfset x = {v: new foo.bar().someMethod()}>`, CfFileType.cfm);
    });
    it("Should accept named arguments with both '=' and ':' as the name/expression delimiter", () => {
        assertNoDiagnostics(`<cfset x = foo(x=1, y:2)>`, CfFileType.cfm);
    });
    it("Should accept `%=`", () => {
        assertNoDiagnostics(`<cfset x %= y>`, CfFileType.cfm);
    });
    it("Should accept for-in and normal-for", () => {
        assertNoDiagnostics(`
            <cfscript>
                for (x in y) {
                    for (i = 0; i < 10; i++) {
            
                    }
                }
            </cfscript>`, CfFileType.cfm);
    });
    it("Should accept the legacy operator `EQV`", () => {
        assertNoDiagnostics(`<cfif a eqv b></cfif>`, CfFileType.cfm);
    });
    it("Should accept the legacy operator `IMP`", () => {
        assertNoDiagnostics(`<cfif a imp b></cfif>`, CfFileType.cfm);
    });
});