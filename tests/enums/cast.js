// @flow

enum B of boolean {
  A = true,
  B = false,
}

enum N of number {
  A = 1,
  B = 2,
}

enum S of string {
  A,
  B,
}

enum Y of symbol {
  A,
  B,
}

enum B2 of boolean {
  A = true,
  B = false,
}

const s: string = S.A as string; // OK
const b: boolean = B.A as boolean; // OK
const n: number = N.A as number; // OK
const y: symbol = Y.A as symbol; // OK

const X = B; // Renaming
X.A as boolean; // OK

S.A as ?string; // Error: if casting to representation type, must cast to exactly it

const ss: S = S.A as S; // OK
const bb: B = B.A as B; // OK
const nn: N = N.A as N; // OK
const yy: Y = Y.A as Y; // OK

S.A as ?S; // OK
S.A as S | B; // OK
S.A as mixed; // OK

type T = string;
S.A as T; // OK

B.A as number; // Error
S.A as boolean; // Error
N.A as boolean; // Error
Y.A as boolean; // Error
X.A as string; // Error

declare var BB: typeof B | typeof B2;
const bba: B | B2 = BB.A;
BB.A as boolean; // OK

declare var bs: B | B2;
bs as boolean; // OK

declare var BS: typeof B | typeof S;
const bsa: B | S = BS.A;
BS.A as string | boolean; // Error

declare var sb: S | B;
sb as string | boolean; // Error

S.A as interface {}; // Error
S.A as empty; // Error

type I<T> = T;
{
  declare const x: I<S>;
  const s: string = x as string; // OK
}
