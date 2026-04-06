// enum exports both a type and value binding, so it should conflict
// with an interface of the same name during type-checking
export enum A { X = 'x' }
export interface A {} // ERROR

export interface B {}
export enum B { X = 'x' } // ERROR
