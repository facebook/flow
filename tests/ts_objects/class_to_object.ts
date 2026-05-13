// Class instance flowing to an inexact object type.
// In .js files, Flow rejects this with EClassToObject ("Cannot use class
// instance as object"). In .ts files we suppress that error and fall through
// to structural subtyping, matching TS where classes are structural.

declare class Animal {
  name: string;
  age: number;
}

declare const a: Animal;

// Subset of class shape: OK via structural subtyping.
a as {name: string}; // OK
a as {name: string; age: number}; // OK

// Missing-required: still an error -- structural subtyping must reject.
a as {name: string; missing: boolean}; // ERROR: `missing` not in Animal

// Wrong-type prop: structural subtyping still catches type mismatches.
a as {name: number}; // ERROR: number vs string

// Interfaces already worked structurally; verify the same shape with an
// interface still behaves the same (no regression).
interface IAnimal {
  name: string;
}
declare const i: IAnimal;
i as {name: string}; // OK
