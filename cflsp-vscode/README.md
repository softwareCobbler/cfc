## cflsp - A ColdFusion Language Plugin

A small parser to see what kind of live diagnostics can reasonably be had for ColdFusion2018+. It helps me in my day-to-day in a very minimal way, mostly by pointing out illegal trailing commas in function calls and struct literals.

I really recommend [KamasamaK's current plugin](https://github.com/KamasamaK/vscode-cfml) as the current best CF plugin; this is intended to work on top of that (note there is ZERO affiliation between this plugin and the KamasamaK one) as the other provides nice completions and syntax highlighting. This one just squiggle-underlines syntactic errors.

Thanks for having a look. If it is marking a tag, expression or statement as an error and you know it shouldn't, please let me know [on github](https://github.com/softwareCobbler/cfc). If you can provide the source that would be very helpful because I need a corpus of test material to run against.

![errors from top-level var decl and function-level arguments scope shadowing](./cflsp-vscode/declaration-errors.png)
![example diagnostics, both squiggly-underlined and in the 'problems' panel](./cflsp-vscode/cfls-diagnostics.png)
