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