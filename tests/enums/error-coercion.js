// @flow

enum E {
  A,
  B,
}

const a: string = E.A; // Error: cannot implicitly coerce enum to its representation type

const b: E = 'B'; // Error: cannot implicitly coerce into enum type

const c: number = E.A; // Error, message does not include suggestion because string isn't a subtype of number

enum B of boolean {
  A = true,
}
const bool: boolean = B.A; // Error, with suggestion
const x: number = B.A; // Error, no suggestion

enum N of number {
  A = 1,
}
const num: number = N.A; // Error, with suggestion
const y: string = N.A; // Error, no suggestion

enum S of symbol {
  A,
}
const sym: symbol = S.A; // Error, with suggestion
const z: boolean = S.A; // Error, no suggestion
