var sealed = {one: 'one', two: 'two'};
(Object.keys(sealed): Array<'one'|'two'>);
(Object.keys(sealed): void); // error, Array<string>

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  (k : number) // error: string ~> number
});

{
  // Union dict
  declare const x: {[string | number]: mixed}
  (Object.keys(x): Array<string>); // OK
}

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

// Opaque types
declare opaque type OpaqueKey;
{
  declare const opaqueDict: {[OpaqueKey]: number};
  (Object.keys(opaqueDict)[0]: OpaqueKey); // ERROR
}

declare opaque type OpaqueKeyWithSupertype1: string;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype1]: number};
  (Object.keys(opaqueDict)[0]: OpaqueKeyWithSupertype1); // OK
  (Object.keys(opaqueDict)[0]: empty); // ERROR
}

type A<T> = T;
declare opaque type OpaqueKeyWithSupertype2: A<string>;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype2]: number};
  (Object.keys(opaqueDict)[0]: OpaqueKeyWithSupertype2); // OK
}

declare opaque type OpaqueKeyWithSupertype3: string | boolean;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype3]: number};
  (Object.keys(opaqueDict)[0]: OpaqueKeyWithSupertype3); // ERROR
}

declare opaque type OpaqueKeyWithSupertype4: string | A<string>;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype4]: number};
  (Object.keys(opaqueDict)[0]: OpaqueKeyWithSupertype4); // OK
}

opaque type OpaqueKeyWithSupertypeAndLocal: $Keys<{a: number; b: number}> = 'a';
{
  declare const opaqueDict: {[OpaqueKeyWithSupertypeAndLocal]: number};
  const x = Object.keys(opaqueDict)[0];
  (x: 'a'); // OK
}
