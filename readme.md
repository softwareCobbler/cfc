# cfc
coldfusion compiler

## current goals
* Parse and understand a large subset of ColdFusion 2018+
* Recognize common syntactic and semantic errors and emit reasonable diagnostics for them.
* Don't crash on invalid source text, circular extends clauses, and etc.
* Extract type information from source text in a gradual, unobtrusive way that permits flagging code that will fail at runtime and that supports an improved in-editor development experience.

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
