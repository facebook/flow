// enum exports both a type and value binding, so it should conflict
// with a type alias of the same name during type-checking
export enum A { X = 'x' }
export type A = number; // ERROR

export type B = number;
export enum B { X = 'x' } // ERROR
