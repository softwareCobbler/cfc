import * as assert from "assert";

import { Parser, Binder, CfFileType, SourceFile, NilCfm, flattenTree, NilCfc, DebugFileSystem, FileSystem, Project } from "../src/compiler";
import { IndexedAccess, NodeKind } from "../src/compiler/node";
import { findNodeInFlatSourceMap, getTriviallyComputableString } from "../src/compiler/utils";
import * as TestLoader from "./TestLoader";
import { CompletionItem, getCompletions } from "../src/services/completions";
import { ProjectOptions, FileSystemNode, pushFsNode } from "../src/compiler/project";
import { EngineVersions, EngineVersion } from "../src/compiler/engines";
import { setDebug as setNodeModuleDebug } from "../src/compiler/node";

setNodeModuleDebug();

const options : ProjectOptions = {
    debug: true,
    parseTypes: false,
    withWireboxResolution: false,
    cfConfigProjectRelativePath: null,
    engineVersion: EngineVersions["acf.2018"],
    genericFunctionInference: false,
    checkReturnTypes: false,
    checkFlowTypes: false,
    cancellationToken: {
        cancellationRequested: () => false,
        throwIfCancellationRequested: () => void 0,
    }
}

const parser = Parser(options);
const binder = Binder(options);

function assertDiagnosticsCount(text: string, cfFileType: CfFileType, count: number) {
    const sourceFile = SourceFile("", cfFileType, text);
    parser.parse(sourceFile);
    binder.bind(sourceFile);
    flattenTree(sourceFile); // just checking that it doesn't throw
    assert.strictEqual(sourceFile.diagnostics.length, count, `${count} diagnostics emitted`);
}

function assertDiagnosticsCountWithProject(fs: FileSystem, diagnosticsTargetFile: string, count: number, engineVersion : EngineVersion = EngineVersions["acf.2018"]) {
    const project = Project("/", fs, {...options, engineVersion});
    project.addFile(diagnosticsTargetFile);
    assert.strictEqual(project.getDiagnostics(diagnosticsTargetFile).length, count, `Expected ${count} errors.`);
}

describe("general smoke test for particular constructs", () => {
    const commonProjectOptions : ProjectOptions = {...options};

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
        parser.parse(sourceFile);
        flattenTree(sourceFile);
    });
    it("should accept 9 of 9 possible indexed access expression (dot|trivia) configurations", () => {
        assertDiagnosticsCount(`
            <cfscript>
                z = x. y;     // in adobe, this should be 'expected a property name' | cf engine error is "a variable may not end in '.'"; do this in the binder probably, not at parse time
                z = x . y;    // ok
                z = x .y;     // ok
                z = x[1]. y;  // ok
                z = x[1] . y; // ok
                z = x[1] .y;  // ok
                z = x(). y    // ok
                z = x() . y   // ok
                z = x() .y    // ok
            </cfscript>`,
            CfFileType.cfm, 0);
    });
    it("Should find a dot terminal attached to an indexed access expression with a root scope name of arguments", () => {
        const completionsTestCase = TestLoader.loadCompletionAtTest("./test/sourcefiles/arguments_lookup.cfc");

        const sourceFile = NilCfc(completionsTestCase.sourceText);
        parser.parse(sourceFile);
        binder.bind(sourceFile);
        const flatSourceMap = flattenTree(sourceFile);
        const nodeMap = sourceFile.nodeMap;

        const node = findNodeInFlatSourceMap(flatSourceMap, nodeMap, completionsTestCase.index);
        assert.strictEqual(node?.kind, NodeKind.terminal, "found node is a terminal");
        assert.strictEqual(node?.parent?.kind, NodeKind.indexedAccessChainElement, "found node parent is an indexedAccessChainElement");
        assert.strictEqual(node?.parent?.parent?.kind, NodeKind.identifier, "found node parent.parent is an identifier");
        assert.strictEqual(node?.parent?.parent?.parent?.kind, NodeKind.indexedAccess, "found node parent.parent.parent is an indexed access");
        assert.strictEqual(getTriviallyComputableString((<IndexedAccess>node?.parent?.parent?.parent).root), "arguments", "indexed access root is arguments scope");
    });
    it("Should not throw error on tree-flatten of arrow function with missing expression after fat arrow", () => {
        const sourceFile = NilCfm("<cfscript>foo = bar((row) => )</cfscript>");
        parser.parse(sourceFile);
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
                rest[     ::   ];
                rest[ (v) ::   ]; // ambiguity between '[v::]' meaning 'slice from v to end with stride 1' or 'lookup element v::<missing-static-prop-name>', resolved with (v)
                rest[     :: v ];
                rest[  :   :   ];
                rest[v :   :   ];
                rest[  : v :   ];
                rest[  :   : v ];
                rest[v : v :   ];
                rest[v :   : v ];
                rest[  : v : v ];
                rest[v : v : v ];

                first.x()[(3)::][4:5:6]().z; // some random expression chain
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
        parser.parse(sourceFile);
        binder.bind(sourceFile);
        const flatSourceMap = flattenTree(sourceFile);
        const nodeMap = sourceFile.nodeMap;

        const node = findNodeInFlatSourceMap(flatSourceMap, nodeMap, completionsTestCase.index);
        assert.strictEqual(node?.parent?.kind, NodeKind.dottedPath);
        assert.strictEqual(node?.parent?.parent?.kind, NodeKind.functionDefinition);
    });
    it("Should issue errors on redeclaration of variables already present in arguments scope", () => {
        const dfs = DebugFileSystem({
            "/": {
                "a.cfm": `
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
                `
            }
        });
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 6);
    });
    // eventually we will support this but for now we can't error on what is actually valid code
    it("Should not issue an error on an array member lookup", () => {
        const dfs = DebugFileSystem({
            "/": {
                "a.cfm": `
                <cfscript>
                    function foo(array a) {
                        return a.len();
                    }
                </cfscript>
            `},
        });
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should offer completions for functions within a CFC", () => {
        const testCase = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfc");

        const fs = DebugFileSystem({"/": {"a.cfc": testCase.sourceText}});
        const project = Project("/", fs, commonProjectOptions);
        project.addFile("/a.cfc");

        const completions = getCompletions(project, "/a.cfc", testCase.index, null);

        assert.strictEqual(completions.length, 2);
        const justLabels = completions.map(v => v.label);
        assert.strictEqual(justLabels.includes("foo"), true, "completions includes `foo`");
        assert.strictEqual(justLabels.includes("bar"), true, "completions includes `bar`");
    });
    it("Should offer completions for functions of a CFC from a CFM", () => {
        const cfc = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfc");
        const cfm = TestLoader.loadCompletionAtTest("./test/sourcefiles/cfc_function_completion.cfm");

        const fs = DebugFileSystem({
            "/": {
                "cfc_function_completion.cfc": cfc.sourceText,
                "cfc_function_completion.cfm": cfm.sourceText,
            }
        });
        const project = Project("/", fs, commonProjectOptions);
        project.addFile("/cfc_function_completion.cfc");
        project.addFile("/cfc_function_completion.cfm");

        const completions = getCompletions(project, "/cfc_function_completion.cfm", cfm.index, null);

        assert.strictEqual(completions.length, 2);
        const justLabels = completions.map(v => v.label);
        assert.strictEqual(justLabels.includes("foo"), true, "completions includes `foo`");
        assert.strictEqual(justLabels.includes("bar"), true, "completions includes `bar`");
    });
    it("Should accept argumentCollection as meeting the named argument requirements for a function call", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
                component {
                    function foo(required a, required b) {
                        return bar(argumentCollection=arguments);
                    }

                    function bar(required a, required b, required x) {
                        
                    }
                }`
        );
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should issue diagnostic on call expression with less than the required number of arguments.", () => {
        const dfs = DebugFileSystem({
            "/": {
                "a.cfc": `
                    component {
                        string function foo(required a, required b) {
                            return foo(42);
                        }
                    }`
            }
        });
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 1);
    });
    it("Should accept a decimal number as a function argument", () => {
        const dfs = DebugFileSystem({"/": {"a.cfm": "<cfset foo(.42)>"}})
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should accept a variable declaration as a loose statement after an if predicate", () => {
        // really the issue we are checking for is that we parse the semicolon after `var y = z`, so that we see the 'else' following it
        const dfs = DebugFileSystem({
            "/": {
                "a.cfm": `
                <cfscript>
                    function foo() {
                        if (x) var y = z;
                        else 42;
                    }
                </cfscript>
                `
            }
        })
        assertDiagnosticsCountWithProject(dfs, "/a.cfm", 0);
    });
    it("Should parse param as a sugared tag expression.", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", 
            `
                component {
                    param name="foo" default="lel";
                }`);
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should parse catch bindings as either string or dotted path.", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
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
            }`);
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should not error on reserved keywords when used in contextually valid positions.", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
            component {
                s = {
                    final: 42,
                    default: 42
                };
                call(final=42, default=42);
            }`);
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should error on quoted named call argument names in Adobe; in Lucee it is OK", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
            component {
                someCall("quotedArgName" = 42);
                someCall("quotedArg");
            }`);
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 1, EngineVersions["acf.2018"]);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0, EngineVersions["lucee.5"]);
    });
    it("Should not require a required && defaulted parameter", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
            component {
                function foo(required arg = 0) {}
                foo();
            }`);
        const dfs = DebugFileSystem(fsRoot);
        assertDiagnosticsCountWithProject(dfs, "/a.cfc", 0);
    });
    it("Should not typecheck a call expression to a built-in function", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfm", `
            <cfscript>
                dateFormat(); // should require one param, but we're not type checking this yet

                function foo(string dateFormat) {
                    dateFormat(42); // built-in should be "more visible" in a call-expression position than the argument named dateFormat
                }
            </cfscript>`);
        pushFsNode(fsRoot, "/lib.d.cfm", `@!interface __cfEngine { dateFormat: (date: string, mask?: string) => any }`);

        const dfs = DebugFileSystem(fsRoot);

        const project = Project("/", /*filesystem*/dfs, commonProjectOptions);
        project.addEngineLib("lib.d.cfm");
        project.addFile("a.cfm");
    
        const diagnostics = project.getDiagnostics("a.cfm");

        assert.strictEqual(diagnostics.length, 0);
    });
    it("Should offer completions for methods in parent components", () => {
        const files = TestLoader.loadMultiFileTest("./test/sourcefiles/returnTypeCompletions.cfm");

        // for the completions tests, we want to strip the completions marker before adding it to the compiler host
        const completionsTestTargets = {
            // ...(() => {
            //     const name = "/foo/Child.cfc";
            //     const completionsAt = TestLoader.loadCompletionAtTestFromSource(files[name]);
            //     const check = (completions: CompletionItem[]) => {
            //         assert.strictEqual(completions.length, 1, `${name} :: got exactly 1 completion`);
            //         assert.strictEqual(completions[0].label, "someRootSiblingMethod");            
            //     }
            //     return {[name]: {...completionsAt, check}};
            // })(),
            // ...(() => {
            //     const name = "/foo/Child2.cfc";
            //     const completionsAt = TestLoader.loadCompletionAtTestFromSource(files[name]);
            //     const check = (completions: CompletionItem[]) => {
            //         assert.strictEqual(completions.length, 4, `${name} :: got exactly 4 completions`);
            //         const expectedLabels = [
            //             "someChildMethod",
            //             "shouldBeNotExportedBecauseItIsPrivate", // ok, we're a descendant so its visible
            //             "someBaseMethod",
            //             "shouldReturnRootSibling"
            //         ];
            //         for (const expectedLabel of expectedLabels) {
            //             assert.strictEqual(!!completions.find((v) => v.label === expectedLabel), true, `completions includes '${expectedLabel}`);
            //         }
            //     }
            //     return {[name]: {...completionsAt, check}};
            // })(),
            ...(() => {
                const name = "/foo/Impl.cfm";
                const completionsAt = TestLoader.loadCompletionAtTestFromSource(files[name]);
                const check = (completions: CompletionItem[]) => {
                    assert.strictEqual(completions.length, 3, `${name} :: got exactly 3 completions`);
                    const justNames = completions.map(completion => completion.label);
                    for (const name of ["shouldReturnRootSibling", "someChildMethod", "someBaseMethod"]) {
                        assert.deepStrictEqual(justNames.includes(name), true, `includes a completion for '${name}'`);
                    }
                }
                return {[name]: {...completionsAt, check}};
            })()
        };

        const fsRoot : FileSystemNode = {"/": {}};
        for (const absPath of Object.keys(files)) {
            if (completionsTestTargets.hasOwnProperty(absPath)) pushFsNode(fsRoot, absPath, completionsTestTargets[absPath as keyof typeof completionsTestTargets].sourceText);
            else pushFsNode(fsRoot, absPath, files[absPath]);
        }

        const debugFs = DebugFileSystem(fsRoot);
        const project = Project("/", /*filesystem*/debugFs, commonProjectOptions);
        for (const absPath of Object.keys(files)) project.addFile(absPath);

        for (const absPath of Object.keys(completionsTestTargets) as (keyof typeof completionsTestTargets)[]) {
            const completions = getCompletions(project, absPath, completionsTestTargets[absPath].index, ".");
            completionsTestTargets[absPath].check(completions);
        }
    })
    it("Unparenthesized arrow functions illegal in Lucce", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `component { x = v => 42; }`);
        const luceeProject = Project("/", DebugFileSystem(fsRoot), {...commonProjectOptions, engineVersion: EngineVersions["lucee.5"]});
        const acfProject = Project("/", DebugFileSystem(fsRoot), {...commonProjectOptions, engineVersion: EngineVersions["acf.2018"]});

        luceeProject.addFile("/a.cfc");
        acfProject.addFile("/a.cfc");

        assert.strictEqual(luceeProject.getDiagnostics("/a.cfc").length, 1, "Lucee project got an error");
        assert.strictEqual(acfProject.getDiagnostics("/a.cfc").length, 0, "acf project did not get an error");
    })
    it("Infers the return type of a function returning a struct", () => {
        const completionsAt = TestLoader.loadCompletionAtTest("./test/sourcefiles/inferredStructReturn.cfc");
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/lib.d.cfm", `@!interface Array<T> { PLACEHOLDER: any }`);
        pushFsNode(fsRoot, "/a.cfc", completionsAt.sourceText);
        const luceeProject = Project("/", DebugFileSystem(fsRoot), {...commonProjectOptions, checkReturnTypes: true, engineVersion: EngineVersions["lucee.5"]});

        luceeProject.addEngineLib("/lib.d.cfm");
        luceeProject.addFile("/a.cfc");

        const completions = getCompletions(luceeProject, "/a.cfc", completionsAt.index, ".");
        assert.strictEqual(completions.length, 1, "got one completion");
        assert.strictEqual(completions[0].label, "innerB", "completion is as expected");
    })
    it("Understands a non-composite-function-type annotation", () => {
        const completionsAt = TestLoader.loadCompletionAtTest("./test/sourcefiles/non-composite-function-type-annotation.cfc");
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/lib.d.cfm", `@!interface Array<T> { PLACEHOLDER: any }`);
        pushFsNode(fsRoot, "/a.cfc", completionsAt.sourceText);
        const luceeProject = Project("/", DebugFileSystem(fsRoot), {...commonProjectOptions, parseTypes: true, engineVersion: EngineVersions["lucee.5"]});

        luceeProject.addEngineLib("/lib.d.cfm");
        debugger;
        luceeProject.addFile("/a.cfc");

        const completions = getCompletions(luceeProject, "/a.cfc", completionsAt.index, ".");
        completions.sort((l,r) => l.label < r.label ? -1 : l.label == r.label ? 0 : 1);
        assert.strictEqual(completions.length, 2, "got 2 completions");

        assert.strictEqual(completions[0].label, "aMember", "completion is as expected");
        assert.strictEqual(completions[1].label, "bar", "completion is as expected");
    })
    it("Parses and does some minor binding phase checks for indexed access into string literals", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfm", `
            <cfscript>
                " foo ".trim();
                " foo "["trim"]();
                
                // https://github.com/softwareCobbler/cfc/issues/10
                someCall("#"2022-11-08".lsDateFormat("full")#");
            </cfscript>`
        );

        {
            const dfs = DebugFileSystem(fsRoot);
            const project = Project("/", /*filesystem*/dfs, {...commonProjectOptions, engineVersion: EngineVersions["lucee.5"]});
            project.addFile("a.cfm");
        
            const diagnostics = project.getDiagnostics("a.cfm");
            assert.strictEqual(diagnostics.length, 0);
        }

        {
            const dfs = DebugFileSystem(fsRoot);
            const project = Project("/", /*filesystem*/dfs, {...commonProjectOptions, engineVersion: EngineVersions["acf.2018"]});
            project.addFile("a.cfm");
        
            const diagnostics = project.getDiagnostics("a.cfm");
            assert.strictEqual(diagnostics.length, 1);
            assert.strictEqual(diagnostics[0].msg, `cf engine Adobe/2018 does not support bracket-access into string literals.`)
        }
    })
    it("parses param statements with a dot-indexed target binding", () => {
        const fsRoot : FileSystemNode = {"/": {}};
        pushFsNode(fsRoot, "/a.cfc", `
            // https://github.com/softwareCobbler/cfc/issues/9
            component {
                public static void function sendReceipt(required struct mailParams) {
                    param string mailParams.replyTo = "";
                }
            }`
        );

        {
            const dfs = DebugFileSystem(fsRoot);
            const project = Project("/", /*filesystem*/dfs, {...commonProjectOptions, engineVersion: EngineVersions["acf.2018"]});
            project.addFile("a.cfc");
        
            const diagnostics = project.getDiagnostics("a.cfc");
            assert.strictEqual(diagnostics.length, 0);
        }

        {
            const dfs = DebugFileSystem(fsRoot);
            const project = Project("/", /*filesystem*/dfs, {...commonProjectOptions, engineVersion: EngineVersions["lucee.5"]});
            project.addFile("a.cfc");
        
            const diagnostics = project.getDiagnostics("a.cfc");
            assert.strictEqual(diagnostics.length, 0);
        }
        
    })
});