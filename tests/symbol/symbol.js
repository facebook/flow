declare var s: symbol;

// Creation
{
  const x: symbol = Symbol(); // OK
  const y: symbol = Symbol('bar'); // OK

  Symbol('foo', 'bar'); // Error: unused argument
}

// Properties and methods
{
  const x: string = s.toString(); // OK
  const y: ?symbol = s.valueOf(); // OK
  const z: string | void = s.description; // OK
}

// Refinement
{
  const x: symbol | boolean = true;
  if (typeof x === "symbol") {
    (x: symbol); // OK
    (x: boolean); // Error
  } else {
    (x: boolean); // OK
    (x: symbol); // Error
  }
}

// Well-known symbols
{
  (Symbol.hasInstance: symbol); // OK
  (Symbol.isConcatSpreadable: symbol); // OK
  (Symbol.match: symbol); // OK
  (Symbol.matchAll: symbol); // OK
  (Symbol.replace: symbol); // OK
  (Symbol.search: symbol); // OK
  (Symbol.species: symbol); // OK
  (Symbol.split: symbol); // OK
  (Symbol.toPrimitive: symbol); // OK
  (Symbol.toStringTag: symbol); // OK
  (Symbol.unscopables: symbol); // OK

  const x: $SymbolMatch = Symbol.match; // OK
  const y: $SymbolMatch = Symbol.toPrimitive; // Error
}

// Temporary compatibility with `Symbol`,
// before we codemod existing annotations to use `symbol` instead
{
  const x: Symbol = Symbol(); // OK
  const y: Symbol = Symbol.for("b"); // OK
  const z: Symbol = Symbol.match; // OK
  Symbol.keyFor(s); // OK
  Symbol.keyFor(x); // OK
}
