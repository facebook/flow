// class exports both a type and value binding, so it should conflict
// with a type alias of the same name during type-checking
export class A {}
export type A = number; // ERROR

export type B = number;
export class B {} // ERROR
