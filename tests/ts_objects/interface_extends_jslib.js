// @flow
// Negative: declaring a fresh interface in a .js file with an object-typed
// `extends` must still error. The gate is consumer-keyed -- the relaxation
// only applies in .ts files. This pins that behavior.

type Base = {a: number, b: string};

interface JsExt extends Omit<Base, "b"> { // ERROR: not inheritable in .js
  c: boolean;
}
