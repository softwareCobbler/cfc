<!---
    `some_f` looks like a return type definition for the function on the following line,
    but we should get completions (preferrably type names but we're not at that point...)
    ah we weren't binding return type so it wasn't making it into the binder's flatNodeMap, so looking for it during
    later binary searches yielded `undefined`
--->
component {
    function foo() {
        var inner = () => {
            return {
                a: [{innerA: ""}],
                b: [{innerB: ""}],
                c: [{innerC: ""}]
            }
        }

        inner().b[999].|<<<<
    }
}