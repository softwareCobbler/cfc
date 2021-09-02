import * as assert from "assert";

import { Parser, Binder, CfFileType, SourceFile, NilCfm, flattenTree, NilCfc, DebugFileSystem, FileSystem, Project } from "../src/compiler";
import { IndexedAccess, NodeType } from "../src/compiler/node";
import { findNodeInFlatSourceMap, getTriviallyComputableString } from "../src/compiler/utils";
import * as TestLoader from "./TestLoader";
import { getCompletions } from "../src/services/completions";
import { LanguageVersion } from "../src/compiler/project";

const parser = Parser().setDebug(true);
const binder = Binder().setDebug(true);

function assertDiagnosticsCount(text: string, cfFileType: CfFileType, count: number) {
    const sourceFile = SourceFile("", cfFileType, text);
    parser.setSourceFile(sourceFile).parse(cfFileType);
    binder.bind(sourceFile);
    flattenTree(sourceFile); // just checking that it doesn't throw
    assert.strictEqual(sourceFile.diagnostics.length, count, `${count} diagnostics emitted`);
}

function assertDiagnosticsCountWithProject(fs: FileSystem, diagnosticsTargetFile: string, count: number, language = LanguageVersion.acf2018) {
    const project = Project(["/"], fs, {debug: true, parseTypes: true, language});
    project.addFile(diagnosticsTargetFile);
    assert.strictEqual(project.getDiagnostics(diagnosticsTargetFile).length, count, `Expected ${count} errors.`);
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
    it("Should not throw an error during tree flattening", () => {
        const string = `
            <cfscript>
                function foo() {
                    cgi.
                }
            </cfscript>`;
        const sourceFile = NilCfm(string);
        parser.setSourceFile(sourceFile);
        parser.parse();
        flattenTree(sourceFile);
    });
    it("should accept 8 of 9 possible indexed access expression (dot|trivia) configurations", () => {
        assertDiagnosticsCount(`
            <cfscript>
                z = x. y;     // expected a property name | cf engine error is "a variable may not end in '.'"
                z = x . y;    // ok
                z = x .y;     // ok
                z = x[1]. y;  // ok
                z = x[1] . y; // ok
                z = x[1] .y;  // ok
                z = x(). y    // ok
                z = x() . y   // ok
                z = x() .y    // ok
            </cfscript>`,
            CfFileType.cfm, 1);
    });
    it("Should find a dot terminal attached to an indexed access expression with a root scope name of arguments", () => {
        const completionsTestCase = TestLoader.loadCompletionAtTest("./test/sourcefiles/arguments_lookup.cfc");

        const sourceFile = NilCfc(completionsTestCase.sourceText);
        parser.setSourceFile(sourceFile).parse();
        binder.bind(sourceFile);
        const flatSourceMap = flattenTree(sourceFile);
        const nodeMap = binder.getNodeMap();

        const node = findNodeInFlatSourceMap(flatSourceMap, nodeMap, completionsTestCase.index);
        assert.strictEqual(node?.kind, NodeType.terminal, "found node is a terminal");
        assert.strictEqual(node?.parent?.kind, NodeType.indexedAccessChainElement, "found node parent is an indexedAccessChainElement");
        assert.strictEqual(node?.parent?.parent?.kind, NodeType.identifier, "found node parent.parent is an identifier");
        assert.strictEqual(node?.parent?.parent?.parent?.kind, NodeType.indexedAccess, "found node parent.parent.parent is an indexed access");
        assert.strictEqual(getTriviallyComputableString((<IndexedAccess>node?.parent?.parent?.parent).root), "arguments", "indexed access root is arguments scope");
    });
    it("Should not throw error on tree-flatten of arrow function with missing expression after fat arrow", () => {
        const sourceFile = NilCfm("<cfscript>foo = bar((row) => )</cfscript>");
        parser.setSourceFile(sourceFile).parse();
        binder.bind(sourceFile);
        flattenTree(sourceFile);
    });
    it("Should accept spread args in struct/array literals, function definition parameter lists and call expresion argument lists", () => {
        assertDiagnosticsCount(`<cfscript>
            function foo(first, ...rest) {
                final var bar = (...rest) => rest;
                final var struct_check = () => {
                    var base = {x: 1, y:2};
                    var spread_target = {a:0, ...base};
                    spread_target = {...base};
                    spread_target = {a:0, ...base, ...base};
                    spread_target = {...base, ...{x:1}};
                }

                final var array_check = () => {
                    var base = [1,2,3];
                    var spread_target = [42, ...base];
                    spread_target = [...base];
                    spread_target = [42, ...base, ...base];
                    spread_target = [42, ...base, ...base, ...[42]];
                }
            }
        </cfscript>`, CfFileType.cfm, 0);
    });
    it("Should accept slice expressions in bracket access context", () => {
        assertDiagnosticsCount(`<cfscript>
            function foo(first, ...rest) {
                rest[  :   :   ];
                rest[v :   :   ];
                rest[  : v :   ];
                rest[  :   : v ];
                rest[v : v :   ];
                rest[v :   : v ];
                rest[  : v : v ];
                rest[v : v : v ];

                first.x()[3::][4:5:6]().z; // some random expression chain
            }
        </cfscript>`, CfFileType.cfm, 0);
    });
    it("Should accept optional slice expressions in bracket access context but emit diagnostics due to optional bracket access being unsupported", () => {
        assertDiagnosticsCount(`<cfscript>
            function foo(first, ...rest) {
                rest?.[  :   :   ];
                rest?.[v :   :   ];
                rest?.[  : v :   ];
                rest?.[  :   : v ];
                rest?.[v : v :   ];
                rest?.[v :   : v ];
                rest?.[  : v : v ];
                rest?.[v : v : v ];
            }
        </cfscript>`, CfFileType.cfm, 8);
    });
    it("Should accept a function returning a function with functions as arguments", () => {
        assertDiagnosticsCount(`
        component {
            public  function function f1(required function f, function g) attr1=foo { final var internal = (function baz) => baz(f, g); }
            private function function f2(required function f, function g) attr1=foo { final var internal = (function baz) => baz(f, g); }
            package function function f3(required function f, function g) attr1=foo { final var internal = (function baz) => baz(f, g); }
            remote  function function f4(required function f, function g) attr1=foo { final var internal = (function baz) => baz(f, g); }
                    function function f5(required function f, function g) attr1=foo { final var internal = (function baz) => baz(f, g); }
        }`, CfFileType.cfc, 0);
    });
    it("Should disallow `function` as a function name", () => {
        assertDiagnosticsCount(`
            <cfscript>
                // function returns a function taking a function -- that's ok
                // function as the function name -- no good
                function function function(function f) {};
            </cfscript>
        }`, CfFileType.cfm, 1);
    });
    it("Should find containing functions - both function and arrow functions", () => {
        assertDiagnosticsCount(`
            <cfscript>
                a = function() {
                    var x = 1; // illegal top-level var if it doesn't find function as a container
                }
                b = () => {
                    var x = 1; // illegal top-level var if it doesn't find the containing arrow function
                }
            </cfscript>
        }`, CfFileType.cfm, 0);
    });
    it("Should not throw when encountering an unterminated comment inside a tag", () => {
        // expected expression, unterminated tag comment, no matching </cfif> tag
        // but! it shouldn't throw
        assertDiagnosticsCount(`<cfif <!---`, CfFileType.cfm, 3);
    });
    it("should bind and be able to find in the flatmap a script function definition return type", () => {
        const completionsTestCase = TestLoader.loadCompletionAtTest("./test/sourcefiles/tree-flatten-1.cfm");

        const sourceFile = NilCfm(completionsTestCase.sourceText);
        parser.setSourceFile(sourceFile).parse();
        binder.bind(sourceFile);
        const flatSourceMap = flattenTree(sourceFile);
        const nodeMap = binder.getNodeMap();

        const node = findNodeInFlatSourceMap(flatSourceMap, nodeMap, completionsTestCase.index);
        assert.strictEqual(node?.parent?.kind, NodeType.dottedPath);
        assert.strictEqual(node?.parent?.parent?.kind, NodeType.functionDefinition);
    });
    it("Should issue errors on redeclaration of variables already present in arguments scope", () => {
        const dfs = DebugFileSystem([
            ["/a.cfm", `
                <cfscript>
                    function foo(a, b, c) {
                        for (var a in b) {}
                        for (var b = 42; b == 42; g) {}
                        var c = 42;
                    }
                    function bar(a,b,c) {
                        function foo() {
                            for (var a in b) {}
                            for (var b = 42; b == 42; g) {}
                            var c = 42;
                        }
                    }
                    function outer() {
                        var foo = (a,b,c) => {
                            for (var a in b) {}
                            for (var b = 42; b == 42; g) {}
                            var c = 42;
                        }
                        var bar = (a,b,c) => {
                            var inner = () => {
                                for (var a in b) {}
                                for (var b = 42; b == 42; g) {}
                                var c = 42;
                            }
                        }
                    }
                </cfscript>
            `],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 6);
    });
    // eventually we will support this but for now we can't error on what is actually valid code
    it("Should not issue an error on an array member lookup", () => {
        const dfs = DebugFileSystem([
            ["/a.cfm", `
                <cfscript>
                    function foo(array a) {
                        return a.len();
                    }
                </cfscript>
            `],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should offer completions for functions within a CFC", () => {
        const testCase = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfc");

        const fs = DebugFileSystem([["/a.cfc", testCase.sourceText]], "/");
        const project = Project(["/"], fs, {debug: true, parseTypes: true, language: LanguageVersion.acf2018});
        project.addFile("/a.cfc");

        const completions = getCompletions(project, "/a.cfc", testCase.index, null);

        assert.strictEqual(completions.length, 2);
        assert.strictEqual(completions[0].label, "foo");
        assert.strictEqual(completions[1].label, "bar");
    });
    it("Should offer completions for functions of a CFC from a CFM", () => {
        const cfc = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfc");
        const cfm = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfc");

        const fs = DebugFileSystem([
            ["/cfc_function_completion.cfc", cfc.sourceText],
            ["/cfc_function_completion.cfm", cfm.sourceText]
        ], "/");
        const project = Project(["/"], fs, {debug: true, parseTypes: true, language: LanguageVersion.acf2018});
        project.addFile("/cfc_function_completion.cfc");
        project.addFile("/cfc_function_completion.cfm");

        const completions = getCompletions(project, "/cfc_function_completion.cfm", cfm.index, null);

        assert.strictEqual(completions.length, 2);
        assert.strictEqual(completions[0].label, "foo");
        assert.strictEqual(completions[1].label, "bar");
    });
    it("Should accept argumentCollection as meeting the named argument requirements for a function call", () => {
        const dfs = DebugFileSystem([
            ["/a.cfc", `
                component {
                    function foo(required a, required b) {
                        foo(argumentCollection=arguments);
                    }
                }
            `],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should issue diagnostic on call expression with less than the required number of arguments.", () => {
        const dfs = DebugFileSystem([
            ["/a.cfc", `
                component {
                    function foo(required a, required b) {
                        foo(42);
                    }
                }
            `],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 1);
    });
    it("Should accept a decimal number as a function argument", () => {
        const dfs = DebugFileSystem([
            ["/a.cfm", `<cfset foo(.42)>`],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should accept a variable declaration as a loose statement after an if predicate", () => {
        // really the issue we are checking for is that we parse the semicolon after `var y = z`, so that we see the 'else' following it
        const dfs = DebugFileSystem([
            ["/a.cfm", `
            <cfscript>
                function foo() {
                    if (x) var y = z;
                    else 42;
                }
            </cfscript>`],
        ], "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should parse param as a sugared tag expression.", () => {
        const dfs = DebugFileSystem([
            ["/a.cfc", `
                component {
                    param name="foo" default="lel";
                }`]],
            "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should parse catch bindings as either string or dotted path.", () => {
        const dfs = DebugFileSystem([
            ["/a.cfc", `
                component {
                    try {}
                    catch ("a.b.c" e) {}

                    try {}
                    catch (a.b.c e) {}

                    try {}
                    catch ('a.b.c' e) {}

                    try {}
                    catch('#abc#' e) {}

                    try {}
                    catch(e e) {}
                }`]],
            "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should not error on reserved keywords when used in contextually valid positions.", () => {
        const dfs = DebugFileSystem([
            ["/a.cfc", `
                component {
                    s = {
                        final: 42,
                        default: 42
                    };
                    call(final=42, default=42);
                }`]],
            "/");
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
});