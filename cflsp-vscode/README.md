## cflsp - A ColdFusion Language Plugin

A parser to see what kind of live diagnostics can reasonably be had for ColdFusion2018+. It helps me in my day-to-day in a very minimal way, mostly by pointing out illegal trailing commas in function calls and struct literals.

There is now REAAAAAAAALLY primitive support for flow analysis ("reaching definitions" per the ol' Dragon Book), hidden behind the boolean configuration option `x_types`. This is difficult with the dynamic nature of CF, since files can generally safely assume (based on the developer's understanding of the actual execution environment) that names are magically visible. Also, CF allows this:

```
foo = "bar";
#foo# = "baz"; // variables.bar = "baz"; analyzable if we had a type system that could track foo as the unique string "bar"

// and:
foo = getRandomValueFromDimensionX();
#foo# = foo; // :( totally opaque to static analysis
```

Anyway, in addition to checking for syntax errors, there is now a go at checking for use-before-definition errors. This will probably result in many false positives, especially since it doesn't link in parent components via `extends` clauses, and doesn't investigate `<cfinclude>s`. It expects that variables are assigned at least once before use, in either the current `local`, `this`, or `variables` scope. Closures expect that outer variables are assigned at any point within any parent scope.

To turn on/off `x_types`:
```
File > Preferences > Settings
(Search bar should be visible, into which you should type:) x_types
(Now x_types option should be visible, toggle it here)
```

I really recommend [KamasamaK's current plugin](https://github.com/KamasamaK/vscode-cfml) as the current best CF plugin; this is intended to work on top of that (note there is ZERO affiliation between this plugin and the KamasamaK one) as the other provides nice completions and syntax highlighting. This one just squiggle-underlines syntactic errors.

I hope this of some utility to you. If it is marking a tag, expression or statement as an error and you know it shouldn't, please let me know [on github](https://github.com/softwareCobbler/cfc).

![errors from top-level var decl and function-level arguments scope shadowing](./cflsp-vscode/declaration-errors.png)
![example diagnostics, both squiggly-underlined and in the 'problems' panel](./cflsp-vscode/cfls-diagnostics.png)
