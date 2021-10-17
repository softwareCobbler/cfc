# cfc
A compiler for Coldfusion, designed primarily for editor tooling support

## current goals
* Parse large subset of ColdFusion 2018+ / Lucee5+
* Recognize syntactic and semantic errors and emit reasonable diagnostics for them.
* Offer autocomplete and navigate-to-symbol
* Always produce a valid AST - don't crash on invalid source text, circular extends clauses, and etc.
* Extract type information from source text in a gradual, unobtrusive way that permits flagging code that will fail at runtime and that supports an improved in-editor development experience.

### Experiments
* Resolve Coldbox module via Wirebox's `getInstance`, treating it as the de-facto package manager, ala `npm` in the nodejs world
* Type annotations for functions and variable declarations to support type checking of expressions
* "interface extended engine scopes" - `@interface variables { x: string }` is the equivalent of declaring `x: string` in the global scope (since `variables` always participates in name lookup); the same can be done for `cgi`, `application`, `this`, etc.

### How you can help
* Feature suggestions welcome, we can't promise we'll implement them but we're interested in listening
* Bug reports
* How do you use `new`? How do you map paths? Is it extremely dynamic, using a lot of `expandPath` and friends? Would you be willing to be a little less dynamic, if it meant a better editor experience? Do you use Forgebox/Coldbox/Wirebox?

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
