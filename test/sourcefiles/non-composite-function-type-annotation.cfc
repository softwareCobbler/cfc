/**
* @!typedef SomeType = {foo: {aMember: string[], bar: string}}
*/
component {
    // @!arg targetArg : SomeType
    function foo(firstArg, targetArg, someOtherArg) {
        targetArg.foo.|<<<<
    }
}