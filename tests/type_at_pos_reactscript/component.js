component Foo(foo: number) { return null; }
//        ^
component Bar(bar: string) renders number { return 3; }
//        ^
component Baz(foo: number) renders? number { return 3; }
//        ^
component Qux(bar: string) renders* number { return 3; }
//        ^
