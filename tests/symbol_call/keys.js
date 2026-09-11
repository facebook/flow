// A minted symbol is a property key like any other `unique symbol`: it names a
// member of an object type, of a class, and of a `keyof` key set, and a
// different symbol names none of them.

const key = Symbol('key');
const otherKey = Symbol('otherKey');

declare const annotated: {[key]: number};
annotated[key] as number; // OK
annotated[otherKey]; // ERROR: a different symbol

class C {
  [key]: number;
  static [otherKey]: string;
}
declare const c: C;
c[key] as number; // OK
C[otherKey] as string; // OK
c[otherKey]; // ERROR: declared on the static side, not on an instance

// `typeof` a binding that holds a minted symbol is that same symbol, so it can
// be passed around and written wherever a type is expected.
type Key = typeof key;
declare const viaType: Key;
annotated[viaType] as number; // OK

function read<K extends Key>(o: {[key]: number}, k: K): number {
  return o[k]; // OK: the bound is the key
}

// The key set of an object with a symbol key holds that symbol and nothing
// else.
type Keys = keyof typeof annotated;
key as Keys; // OK
otherKey as Keys; // ERROR: not a key of `annotated`
key as string; // ERROR: a symbol is not a string

// A minted symbol is still a `symbol`.
key as symbol; // OK
const symbols: Array<symbol> = [key, otherKey]; // OK
