# cfc
A compiler for Coldfusion, designed primarily for editor tooling support.

The VsCode plugin based on this tool is "cflsp", downloadable from within VsCode or on the [extensions marketplace](https://marketplace.visualstudio.com/items?itemName=DavidRogers.cflsp)

## current goals
* Support ColdFusion 2018+ and Lucee5+
* Recognize syntactic and semantic errors and emit reasonable diagnostics for them.
* Offer autocomplete and navigate-to-symbol
* Extract type information from source text in a gradual, unobtrusive way that permits flagging code that will fail at runtime and that supports an improved in-editor development experience.
* Don't crash on invalid source text, always produce an AST

### Experiments
* Resolve Coldbox modules via Wirebox's `getInstance` and in-cfc property declarations, treating it as the de-facto package manager, ala `npm` in the nodejs world
* Type annotations for functions and variable declarations to support type checking of expressions
* "interface extended engine scopes" &mdash;&nbsp; `@interface variables { x: string }` in a CFC preamble is the equivalent of declaring `x: string` in the global scope (since `variables` always participates in name lookup); the same can be done for `cgi`, `application`, `this`, etc.
* define interfaces, typedefs and decorators with the following syntax in a comment: `@!interface <interface-name> { <interface-structlike-kv-pairs }`, `@!type <type-here>` `@!typedef <name> = <type>` `@!decorate <decorator-name>`

## building

* `git clone https://github.com/softwareCobbler/cfc.git && cd cfc`
* `npm run install-all`
* `npm run build-cflsp`

Then, from within vscode, there is a debug configuration called "Client + Server" which launches the VSCode extension host with the built plugin running, as well as a debug connection to the server. The actual language server glue code is about 99% pulled from [Microsoft's vs-code-extension-samples/lsp-sample](https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample), but the parser is all handrolled recursive descent.

## tests
to run the current tests (just a quick smoke test, hopefully helps check for regressions during development), install the git submodules:
* `git submodule init`
* `git submodule update`

Now the directory `test/mxunit` should be populated, and the test can run:
* `npm run test-all`
