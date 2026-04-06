// class exports both a type and value binding, so it should conflict
// with an interface of the same name during type-checking
export class A {}
export interface A {} // ERROR

export interface B {}
export class B {} // ERROR
