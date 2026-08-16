// `keyof` over an object type with `unique symbol` keys includes those symbols
// in the key set, and a matching `unique symbol` satisfies it. A distinct
// `unique symbol` is correctly rejected. This is the module-registry pattern
// used by libraries that key a registry object off `unique symbol`s.

declare const sym1: unique symbol;
declare const sym2: unique symbol;

type Obj = {
  a: number,
  [sym1]: string,
};

// A `keyof` alias mixing a string key and a symbol key.
type Keys = keyof Obj;

// Membership: the symbol key and the string key both satisfy the key set.
sym1 as Keys; // OK
"a" as Keys; // OK

// A distinct `unique symbol` is not a key of `Obj`.
sym2 as Keys; // ERROR: sym2 is not a key of Obj
// An unrelated string is not a key of `Obj`.
"b" as Keys; // ERROR: string literal 'b' is not a key of Obj

// The module-registry pattern: a type parameter bounded by the symbol key set
// accepts the symbol used as its key, and rejects a distinct symbol.
type Wrap<K extends Keys> = {key: K};

declare const w1: Wrap<typeof sym1>; // OK
w1.key as typeof sym1; // OK

declare const w2: Wrap<typeof sym2>; // ERROR: sym2 is not in keyof Obj

// A generic bounded by a `unique symbol` also satisfies the symbol key set.
function viaGeneric<K extends typeof sym1>(k: K): Keys {
  return k; // OK: the bound `typeof sym1` is a key of Obj
}

// `keyof` over an object whose only key is a symbol still yields that symbol.
type SymOnly = {[sym1]: number};
sym1 as keyof SymOnly; // OK
sym2 as keyof SymOnly; // ERROR: sym2 is not a key of SymOnly

// `keyof` must preserve symbol-valued property-map keys when used as the source
// type too. If the keyset dropped symbols here, these return checks would
// incorrectly pass.
const symObj = {[sym1]: 1};
function returnSymObjKey(k: keyof typeof symObj): typeof sym1 {
  return k; // OK
}
function returnSymObjKeyAsString(k: keyof typeof symObj): string {
  return k; // ERROR: symbol key is not a string
}
function returnSymObjKeyAsEmpty(k: keyof typeof symObj): empty {
  return k; // ERROR: keyset is not empty
}
