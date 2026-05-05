var sealed = {one: 'one', two: 'two'};
Object.keys(sealed) as Array<'one'|'two'>;
Object.keys(sealed) as void; // error, Array<string>

var dict: { [k: number]: string } = {};
Object.keys(dict).forEach(k => {
  k as number // error: string ~> number
});

{
  // Union dict
  declare const x: {[string | number]: unknown}
  Object.keys(x) as Array<string>; // OK
}

var any: Object = {};
Object.keys(any) as Array<number>; // error, Array<string>

class Foo {
  prop: string;
  foo() {}
}
// constructor and foo not enumerable
Object.keys(new Foo()) as Array<'error'>; // error: prop ~> error

class Bar extends Foo {
  bar_prop: string;
  bar() {}
}
// only own enumerable props
Object.keys(new Bar()) as Array<'error'>; // error: bar_prop ~> error

var tests = [
  // dictionary of string literals -> array of string literals
  function(dict: {['hi']: unknown}) {
    Object.keys(dict) as Array<'hi'>;
    Object.keys(dict) as Array<'bye'>; // error
  },

  // dictionary of number literals -> array of generic strings (for now)
  function(dict: {[123]: unknown}) {
    Object.keys(dict) as Array<string>;
    Object.keys(dict) as Array<'123'>; // error: not supported yet
  },
];

// Interfaces
declare var iface: interface {a: number, b: string};
declare var ifaceDict: interface {['x' | 'y']: boolean};
declare var ifaceBoth: interface {['x' | 'y']: boolean, z: number};

Object.keys(iface) as Array<'a' | 'b'>; // OK
Object.keys(ifaceDict) as Array<'x' | 'y'>; // OK
Object.keys(ifaceBoth) as Array<'x' | 'y' | 'z'>; // OK
Object.keys(ifaceDict) as Array<'$value' | '$key'>; // ERROR

// Invalid values
Object.keys(undefined); // ERROR
Object.keys(null); // ERROR
Object.keys(1); // ERROR
Object.keys(true); // ERROR

// Opaque types
declare opaque type OpaqueKey;
{
  declare const opaqueDict: {[OpaqueKey]: number};
  Object.keys(opaqueDict)[0] as OpaqueKey; // ERROR
}

declare opaque type OpaqueKeyWithSupertype1: string;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype1]: number};
  Object.keys(opaqueDict)[0] as OpaqueKeyWithSupertype1; // OK
  Object.keys(opaqueDict)[0] as empty; // ERROR
}

type A<T> = T;
declare opaque type OpaqueKeyWithSupertype2: A<string>;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype2]: number};
  Object.keys(opaqueDict)[0] as OpaqueKeyWithSupertype2; // OK
}

declare opaque type OpaqueKeyWithSupertype3: string | boolean;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype3]: number};
  Object.keys(opaqueDict)[0] as OpaqueKeyWithSupertype3; // ERROR
}

declare opaque type OpaqueKeyWithSupertype4: string | A<string>;
{
  declare const opaqueDict: {[OpaqueKeyWithSupertype4]: number};
  Object.keys(opaqueDict)[0] as OpaqueKeyWithSupertype4; // OK
}

opaque type OpaqueKeyWithSupertypeAndLocal: keyof {a: number; b: number} = 'a';
{
  declare const opaqueDict: {[OpaqueKeyWithSupertypeAndLocal]: number};
  const x = Object.keys(opaqueDict)[0];
  x as 'a'; // OK
}
