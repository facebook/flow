// A minted symbol crosses the module boundary. The signature records the call
// that minted it, at the same location the checker reads it from, so an
// importer names the same symbol the defining file does.

export const shared = Symbol('shared');
export const registered = Symbol.for('registered');

declare export const table: {[shared]: number, [registered]: string};

// The object literal itself still needs an annotation to be exported, since a
// signature has no way to describe a computed key.
export const untyped = {[shared]: 1}; // ERROR: cannot build a typed interface

// A call reached through a comma mints in neither pipeline. The checker leaves
// it the ordinary `symbol`, as `mint.js` shows, and a signature has no way to
// describe a plain call.
export const throughComma = (0, Symbol()); // ERROR: cannot build a typed interface

// Where a binding takes one value per branch the two do not agree: the checker
// mints once per branch, as `mint.js` shows, and the signature has no way to
// write down either symbol, so such a binding is minted but not exportable.
// Annotating it is no way out, since `typeof` is the only way to name a minted
// symbol.
declare const test: boolean;
export const branched = test ? Symbol() : Symbol(); // ERROR: cannot build a typed interface
export const matched = match (test) { // ERROR: cannot build a typed interface
  true => Symbol(),
  _ => Symbol(),
};
export const ored = test || Symbol(); // ERROR: cannot build a typed interface
