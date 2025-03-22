component Foo(foo: number) { return null; }
//        ^
component Bar(bar: string) renders number { return 3; }
//        ^
component Baz(foo: number) renders? number { return 3; }
//        ^
component Qux(bar: string) renders* number { return 3; }
//        ^
component Poly<T> (foo: T) {return null};
//        ^
type PolyRendersExactlyTypeApp1 = Poly<string>;
//        ^
type PolyRendersExactlyTypeApp1 = Poly<string>;
//                            ^
type ConcretizedPolyComponent = Poly<string> extends React$RendersExactly<infer C> ? C : empty;
//        ^
type RendersExactlyFoo = Foo;
//                        ^

declare const x: component();

type xRendersExactly= x;
//                    ^

component FooForParam(foo: ?number) { return null; }
//                     ^
type TypeofFoo = typeof Foo; // do not print evaluated version - it's the same
//   ^
