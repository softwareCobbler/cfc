# cfc
A compiler for Coldfusion, designed primarily for editor tooling support.

The VsCode plugin based on this tool is "cflsp", downloadable from within VsCode or on the [extensions marketplace](https://marketplace.visualstudio.com/items?itemName=DavidRogers.cflsp)

## current goals
* Support ColdFusion 2018+ and Lucee5+
* Recognize syntactic and some semantic errors and emit reasonable diagnostics for them.
* Parse incrementally to keep response times reasonable
* Offer autocomplete and navigate-to-symbol where possible

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
