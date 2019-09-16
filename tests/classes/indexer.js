// @flow

class A {
  a: number;
  b: number;
}

class B {
  a: string;
  b: string;
}

declare class C {
  [key: string]: number;
}

declare class D {
  [key: string]: string;
}

var a: { [key: string]: number } = new A(); // ok
var b: { [key: string]: number } = new B(); // error, string ~> number
var c: { [key: string]: number } = new C(); // ok
var d: { [key: string]: number } = new D(); // error, string ~> number

declare class E {
  [key: number]: string;
}

var e: { [key: number]: string } = new E(); // ok
var f: { [key: string]: string } = new E(); // error, string ~> number
