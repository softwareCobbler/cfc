//// @file="/A.cfc"
component {
    /**
        @!interface O { fooKey: string, barKey: string, bazKey: "" }
        @!typeparam T extends keyof O
        @!arg arg1 : any
        @!arg arg2 : T
        @!returns any
    */
    function foo(required arg, required arg2) {}

    function bar() {
        foo(arg2= "|<<<<", arg1=somethingelse)
    }
}
