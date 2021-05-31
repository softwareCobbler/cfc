## cflsp - A ColdFusion Language Plugin

A parser to see what kind of live diagnostics can reasonably be had for ColdFusion2018+. It helps me in my day-to-day in a very minimal way, mostly by pointing out illegal trailing commas in function calls and struct literals.

There is a REAAAAAAAALLY primitive support for flow analysis, hidden behind the boolean configuration option `x_types`. This is difficult with the dynamic nature of CF, since files can generally safely assume (based on the developer's understanding of the actual execution environment) that names are available. So in addition to checking for syntax errors, there is now a go at checking for use-before-definition errors. This will probably lead to many false positives, especially since it doesn't link in parent components via `extends` clauses, and doesn't investigate `<cfinclude>s`. Anyway, 


I really recommend [KamasamaK's current plugin](https://github.com/KamasamaK/vscode-cfml) as the current best CF plugin; this is intended to work on top of that (note there is ZERO affiliation between this plugin and the KamasamaK one) as the other provides nice completions and syntax highlighting. This one just squiggle-underlines syntactic errors.

Thanks for having a look. If it is marking a tag, expression or statement as an error and you know it shouldn't, please let me know [on github](https://github.com/softwareCobbler/cfc). If you can provide the source that would be very helpful because I need a corpus of test material to run against.

![errors from top-level var decl and function-level arguments scope shadowing](./cflsp-vscode/declaration-errors.png)
![example diagnostics, both squiggly-underlined and in the 'problems' panel](./cflsp-vscode/cfls-diagnostics.png)
