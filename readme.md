# cfc
coldfusion compiler

well, really it's just a parser

## current goals
* Parse a large subset of ColdFusion 2018+
* Recognize common errors and emit reasonable diagnostics for them
* Don't crash on invalid source text
* Provide a VSCode language plugin that error-squiggles syntactic errors, with intent of working on top of [KamasamaK's excellent existing CFML plugin](https://github.com/KamasamaK/vscode-cfml)
* Learn about good ways to collect and debug any plugin errors from opted-in end-user machines
* Learn about the VSCode extension deployment workflow

## current non-goals
* Check for or emit diagnostics for semantic errors (`symbol X is not defined`, etc.)

## building

* `git clone https://github.com/softwareCobbler/cfc.git && cd cfc`
* `npm run install-all`
* `npm run build-cflsp`

Then, from within vscode, there is a debug configuration called "Client + Server" which launches the VSCode extension host with the built plugin running, as well as a debug connection to the server. The actual language server glue code is about 99% pulled from [Microsoft's vs-code-extension-samples/lsp-sample](https://github.com/microsoft/vscode-extension-samples/tree/main/lsp-sample), but the parser is all handrolled recursive descent.

## tests
a definite todo.