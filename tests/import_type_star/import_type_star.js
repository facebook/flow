import type * as Foo from './ExportNamed';

// OK: access type exports
type A = Foo.MyType;
var a1: A = 'hello'; // ok
var a2: A = 42; // Error: number ~> string

type B = Foo.MyInterface;
var b1: B = {x: 42};
var b2: B = {x: 'hello'}; // Error: string ~> number

// OK: typeof on value exports
type C = typeof Foo.myValue;
var c1: C = 42; // ok
var c2: C = 'hello'; // Error: string ~> number

type D = typeof Foo.myFunc;
var d1: D = () => 'hello';
var d2: D = () => 42; // Error: number ~> string

// OK: class type (instance type)
type E = Foo.MyClass;
var e1: E = new (require('./ExportNamed').MyClass)();
var e2: E = 42; // Error: number ~> MyClass

// Error: type-only namespace used as value
Foo; // error
Foo.myValue; // error
const x = Foo.myFunc(); // error

// OK: typeof on the whole namespace
type F = typeof Foo;
