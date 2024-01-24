declare export enum FooGood { // ok
  A,
  B,
}

export enum BarGood { // ok
  A,
  B,
}

declare export enum FooBad { // error
  A,
  C,
}

export enum BarBad1 { // error
  A = 1,
  B = 2,
}

export enum BarBad2 { // error
  A,
  C,
}

enum BazBadNotExported {
  A,
  C,
}

declare export const fooGood: FooGood; // ok
declare export const fooBar: FooBad; // error
declare export const barGood: BarGood;  // ok
declare export const barBad1: BarBad1; // errored at BarBad1 declaration site
declare export const barBad2: BarBad2; // error
declare export const bazBad: BazBadNotExported; // error
