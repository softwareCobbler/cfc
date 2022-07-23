//// @file="/SomeLib.d.cfm"
@!namespace Foo {
    @!namespace Bar {
        @!typedef function_transform = Baz.function_transform
        @!interface Foo {}
    }
}

@!namespace Baz {
    @!typedef function_transform<F> = F["cfname"] extends "scope#infer name#" ? inject<name, () => any> : 0
}

//// @file="/SomeLib2.d.cfm"
@!namespace Foo {
    @!namespace Bar {
        @!typedef function_transform = Bar.function_transform
    }
}

//// @file="/A.cfc"
/**
@!import "SomeLib.d.cfm" qualified MyLib
@!cfc-transform {
    @!typedef functions = MyLib.Foo.Bar.function_transform
}
*/
component {
    function scopeWithFoo(qb, argument_A) {}
    function scopeWithBar(qb, argument_A) {}
}

//// @file="/B.cfc"
component {
    function doit() {
        var a = new A();
        a.|<<<<
    }
}
