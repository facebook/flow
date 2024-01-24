declare export enum FooGood {// should be ok, but error
  A,
  B,
}

export enum BarGood {// should be ok, but error
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

declare export const fooGood: FooGood; // should be ok, but error
declare export const fooBar: FooBad; // error
declare export const barGood: BarGood;  // should be ok, but error
declare export const barBad1: BarBad1; // error
declare export const barBad2: BarBad2; // error
declare export const bazBad: BazBadNotExported; // error
