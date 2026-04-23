// TS-style declaration merging: a class can coexist with a same-named
// interface (the class remains the canonical type binding; member-level
// merging is future work).
export class A {}
export interface A {} // ok

export interface B {}
export class B {} // ok
