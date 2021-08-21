## cflsp - A ColdFusion Language Plugin

A language plugin to see how far we can push syntactic and semantic analysis for ColdFusion.

If we are marking a tag, expression or statement as an error and you know it shouldn't (or it isn't and it should), please let me know [on github](https://github.com/softwareCobbler/cfc).

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