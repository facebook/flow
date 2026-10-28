// @flow

class C {}
class D {}

type I1 = interface {};
//   ^?
type I2 = interface { (): number };
//   ^?
type I3 = interface { readonly [my_key: string]: number };
//   ^?
type I4 = interface { writeonly [my_key: string]: number };
//   ^?
type I5 = interface { [my_key: string]: number };
//   ^?
type I6 = interface extends C, D { r(): number };
//   ^?
type I7 = interface extends C { (): number };
//   ^?
type I8 = interface { a(): void, b: number, get c(): string, set d(x: string): void };
//   ^?
type I9 = interface { m(this: interface {}): void };
//   ^?
