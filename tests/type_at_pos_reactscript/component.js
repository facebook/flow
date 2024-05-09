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
type PolyElementTypeApp1 = Poly<string>;
//        ^
type PolyElementTypeApp2 = Poly<string>;
//                          ^
type ConcretizedPolyComponent = Poly<string> extends React$Element<infer C> ? C : empty;
//        ^
type FooElement = Foo;
//                 ^

declare const x: React$AbstractComponent<{}>;

type xElement = x;
//              ^

component FooForParam(foo: ?number) { return null; }
//                     ^
type TypeofFoo = typeof Foo; // TODO do not print evaluated version - it's the same
//   ^
