## cflsp - A ColdFusion Language Plugin

A language plugin to see how far we can push ColdFusion language tooling.

If we are marking a tag, expression or statement as an error and you know it shouldn't (or it isn't and it should), please let me know [on github](https://github.com/softwareCobbler/cfc).

1.0.32
- member function autocompletions for `array` argument types, using an `Array<T>` interface defined in the always visible standard library;
- some generic inference for signatures like `Array.map`, hidden behind
  `x_genericFunctionInference` option; pretty alpha-level quality but it does work in limited circumstances (i.e in straightforward cases we can infer U in `<U>map(m: (e: T) => U) => U[]` and fill in `T` in an inline body of `m` and get the expected autocomplete)
- actually respect "x_" options (experimental features)
- don't reboot the server on configuration change, if the configuration was for some other plugin
- somewhat hidden feature - a "warn-undefined" attribute on a cfc will emit warnings on all symbols in the cfc that we couldn't find
  - this is probably going to light up like crazy on existing projects that expect symbols to be magically visible at runtime,
  - but on files with no `<cfinclude>`'s and a resolved inheritance chain, it works pretty well
- shouldn't crash on bootup?...at least one user hit an OOM death on initial CFC caching
  - logs now show what file we're *about* to process instead of the one we just sucesfully processed

1.0.29
- Issue a diagnostic on unparenthesized single-argument arrow functions when in Lucee language mode

1.0.28
- Experimental coldbox/wirebox module resolution, with some autocomplete support for `getInstance`
  - we parse and understand a subset of the Wirebox configuration file syntax (well, right now we only support the `mapDirectory` initiator, but that can be improved)
  - currently, the resolved-modules list is built at startup, and no listeners are installed
  to check for new / deleted modules; however, all references to resolved modules are live (so a change in a resolved module will affect autocomplete results in another file referencing it).
  - We walk up from caller's path to find all parent `modules` folders during resolution, like `node_modules` during node's module resolution
  - improvements to navigate-to-symbol for function definitions (f12 a function name)
  - experimental "Decorator" feature, which serves to change the visible type of a CFC for autocomplete purposes
    - primary reason for existence is Quick will take properties and add a "where" prefix, or take all methods that start with "scope" and drop the "scope" prefix and first argument
    - right now a Decorator is a magic compiler builtin, but if they are useful in many situations, a user-available language interface can be developed
- support for "interface extensions" of known-scopes, to enable autocomplete for things we statically know will be in scope at runtime
- resolve method return types if they are cfcs for chained method auto completions

![wirebox module resolutions and interface driven scope extensions](./cflsp-vscode/experimentalWireboxResolution.gif)

1.0.27 (Sep 2021)

- Provide option in config `(File >> Preferences >> Settings >> (search for cflsp))` to switch between Lucee and Adobe language mode.
- Misc. fixes to give less false positive errors.

1.0.24

With 1.0.24, we introduce a *very* limited notion of type checking.

Type information is not generally available in ColdFusion, with the exception of in function definitions, where code
might say an argument is a string, number, struct, array, or etc. We can leverage that information and provide a small amount of error
checking; for example, say a function takes an argument of type string, and tries to pass it to a function that accepts an argument of type
struct -- we can flag that as an error.

For the adventurous palate, we offer a very-much-in-alpha option called "x_types", which enables experimental type annotations. This supports Typescript-esque
type annotations for functions and variables, allowing more precise types to be applied to such constructs. A function that returns `struct` can be annotated
so that it instead returns the more descriptive `{x: string, y: string}`. The syntax, subject to change in the future, looks like:

```
// @type (x: string) => {x: number, y: number}
struct function(string x) { ... }
```

The above applies the nearest-preceding type annotation to its following construct, much like JS-Doc.

We also now check function argument list lengths, such that a function that indicates *N* required parameters will be required at its call sites to
accept N parameters. In the case of something like `function foo (function f) { return f(a,b,c); }` we say that `f` can accept 0 or more arguments,
since `f` is just "a function, with unknown signature".

We try to be very conservative with flagging errors; if we cannot be totally certain of a term's type, we say that it is of type "any", for which anything goes (subtract
it, string-concat it, bake_it_in_a_pie()!)

There's a long way to go before types become generally useful, but we think this is a reasonable step towards improving ColdFusion tooling in general.

Usage in its current form is shown below.

![current state of typechecks](./cflsp-vscode/min-type-checks.png)

Big thanks to [KamasamaK](https://github.com/KamasamaK/vscode-cfml) for their plugin which offers, among plenty of other features, syntax coloring.