// A value with a call or construct signature is a function at runtime.

// `Map` and `Symbol` are `declare var`s of constructor interfaces.
const M = typeof Map === 'function' && Map;
M as false; // error: `MapConstructor` ~> `false`
const S = typeof Symbol === 'function' && Symbol;
S as false; // error: `SymbolConstructor` ~> `false`

interface Callable {
  (): number;
  foo: string;
}
interface Plain {
  foo: string;
}
declare const callable: Callable;
declare const plain: Plain;

if (typeof callable === 'function') {
  callable() as number; // ok
} else {
  callable as empty; // ok
}

if (typeof plain === 'function') {
  plain as empty; // ok
} else {
  plain.foo as string; // ok
}

declare const either: Callable | number;
if (typeof either === 'function') {
  either() as number; // ok
} else {
  either as number; // ok
  either as Callable; // error: number ~> Callable
}
