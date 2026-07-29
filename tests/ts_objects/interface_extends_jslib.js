// @flow
// Negative: a Flow interface cannot extend a Flow object type.
// Both sides retain Flow strictness through the inheritance check,
// so the resolved object type is still not inheritable.

type Base = {a: number, b: string};

interface JsExt extends Omit<Base, "b"> { // ERROR: not inheritable in .js
  c: boolean;
}
