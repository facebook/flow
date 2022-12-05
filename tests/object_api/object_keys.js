/* @flow */

var sealed = {one: 'one', two: 'two'};
(Object.keys(sealed): Array<'one'|'two'>);
(Object.keys(sealed): void); // error, Array<string>

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  (k : number) // error: string ~> number
});

var any: Object = {};
(Object.keys(any): Array<number>); // error, Array<string>

class Foo {
  prop: string;
  foo() {}
}
// constructor and foo not enumerable
(Object.keys(new Foo()): Array<'error'>); // error: prop ~> error

class Bar extends Foo {
  bar_prop: string;
  bar() {}
}
// only own enumerable props
(Object.keys(new Bar()): Array<'error'>); // error: bar_prop ~> error

var tests = [
  // dictionary of string literals -> array of string literals
  function(dict: {['hi']: mixed}) {
    (Object.keys(dict): Array<'hi'>);
    (Object.keys(dict): Array<'bye'>); // error
  },

  // dictionary of number literals -> array of generic strings (for now)
  function(dict: {[123]: mixed}) {
    (Object.keys(dict): Array<string>);
    (Object.keys(dict): Array<'123'>); // error: not supported yet
  },
];

// Interfaces
declare var iface: interface {a: number, b: string};
declare var ifaceDict: interface {['x' | 'y']: boolean};
declare var ifaceBoth: interface {['x' | 'y']: boolean, z: number};

(Object.keys(iface): Array<'a' | 'b'>); // OK
(Object.keys(ifaceDict): Array<'x' | 'y'>); // OK
(Object.keys(ifaceBoth): Array<'x' | 'y' | 'z'>); // OK
(Object.keys(ifaceDict): Array<'$value' | '$key'>); // ERROR

// Invalid values
Object.keys(undefined); // ERROR
Object.keys(null); // ERROR
Object.keys(1); // ERROR
Object.keys(true); // ERROR
