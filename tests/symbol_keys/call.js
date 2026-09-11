// A `unique symbol` annotation and a call to the symbol constructor each
// introduce a symbol. Written on one declaration they introduce one: the
// annotation names the symbol the binding will hold, and the call is what
// produces it. Written apart they are two, so a symbol minted for one
// declaration is never a key of another's.

declare const declared: unique symbol;
const minted = Symbol();

declare const o: {[declared]: number, [minted]: string};
o[declared] as number; // OK
o[minted] as string; // OK

declare const onlyDeclared: {[declared]: number};
onlyDeclared[minted]; // ERROR: the minted symbol is a different key

// `typeof` a minted symbol names it wherever a type is expected, which is what
// the annotation would otherwise have been reached for.
declare const alias: typeof minted;
o[alias] as string; // OK

// The annotation and the call in one declaration are one symbol, so the
// declaration can be written at all and its symbol keys what the annotation
// keys. TypeScript reads the same pair as one symbol.
const annotated: unique symbol = Symbol();
const registered: unique symbol = Symbol.for('registered');
declare const p: {[annotated]: number, [registered]: string};
p[annotated] as number; // OK
p[registered] as string; // OK

// A class field takes the same pair, on the static side, which is the only side
// a bare `unique symbol` may be written on.
class C {
  static readonly K: unique symbol = Symbol();
}
declare const q: {[C.K]: number};
q[C.K] as number; // OK

// Only a bare `unique symbol` names a symbol the declaration is free to
// introduce. `typeof` names one that another declaration already holds, and no
// call can produce that one.
const viaTypeof: typeof declared = Symbol(); // ERROR

// Only a call to the symbol constructor produces the annotated symbol. Nothing
// else returns a symbol that is not already someone else's.
declare function mk(): symbol;
const fromFunction: unique symbol = mk(); // ERROR
const fromBinding: unique symbol = minted; // ERROR

// Where the annotation is in a position that cannot mean one symbol, that is
// the one thing reported. A second error about the initializer would say
// nothing the first does not.
let rebindable: unique symbol = Symbol(); // ERROR: not `const`
class D {
  readonly I: unique symbol = Symbol(); // ERROR: instance field
}

// A property of an object type is not a declaration of the symbol: many objects
// can inhabit one annotation, each holding a symbol of its own, so a call
// cannot claim to produce the one the annotation names. TypeScript rejects this
// too.
const inObject: {readonly a: unique symbol} = {a: Symbol()}; // ERROR

// An explicitly annotated symbol is non-widening, just like an explicitly
// annotated primitive literal, so its identity survives a mutable container.
const annotatedHolder = {annotated};
declare const annotatedTable: {[annotated]: number};
annotatedTable[annotatedHolder.annotated] as number; // OK
