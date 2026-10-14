// A tuple type can end with a spread of an array, which is its rest element.

type T = [string, ...Array<number>];

// Values
{
  ['a'] as T; // OK
  ['a', 1, 2] as T; // OK
  [] as T; // ERROR: arity
  ['a', 'b'] as T; // ERROR: string ~> number
}

// Access
{
  declare const t: T;
  t[0] as string; // OK
  t[1] as number; // OK
  t[5] as number; // OK
  t[1] as string; // ERROR: number ~> string
  t.length as number; // OK
  t.length as 1; // ERROR: length is just `number`
}

// Subtyping
{
  declare const t: T;
  declare const fixed: [string, number, number];
  fixed as T; // OK
  t as [string, number]; // ERROR: arity
  t as [a: string, b?: number, ...Array<number>]; // OK
  t as [string, ...]; // OK
  t as ReadonlyArray<string | number>; // OK
  t as ReadonlyArray<string>; // ERROR: number ~> string

  declare const longer: [string, number, ...Array<number>];
  longer as T; // OK
  t as [string, number, ...Array<number>]; // ERROR: arity
}

// Spread into calls
{
  declare const t: T;
  declare function f(a: string, ...rest: Array<number>): void;
  f(...t); // OK
  declare function g(a: string, b: boolean): void;
  g(...t); // ERROR: number ~> boolean
}

// Spread into tuple types
{
  type U = [boolean, ...T];
  declare const u: U;
  u[1] as string; // OK
  u[2] as number; // OK
  type V = [...T, boolean]; // ERROR: element after a spread with a rest element
  type W = [...Array<number>];
  declare const w: W;
  w as Array<number>; // OK
}

// `[...A, ...B]` where `B` is an array
{
  declare function partial<A extends ReadonlyArray<unknown>, B extends ReadonlyArray<unknown>>(
    f: (...args: [...A, ...B]) => void,
    ...args: A
  ): (...args: B) => void;
  declare function log(...data: Array<string>): void;
  const logX = partial(log, 'x'); // OK
  logX('y', 'z'); // OK
  logX(1); // ERROR: number ~> string
}

// Destructuring
{
  declare const t: T;
  const [first, ...rest] = t;
  first as string; // OK
  rest as Array<number>; // OK
}

// Readonly
{
  declare const r: Readonly<T>;
  r[1] = 1; // ERROR: not writable
}

// Printing
{
  declare const t: T;
  t as empty; // ERROR
}

// Refinement on an element of the rest element
{
  declare const t: T;
  if (t[1]) {
    t as empty; // ERROR: the element may not exist, so `t` is not refined away
  }
}

// The rest element of a spread of a generic with a read-only bound is read-only
{
  declare const r: [string, ...ReadonlyArray<number>];
  r[1] = 1; // OK
  function g<A extends ReadonlyArray<number>>(x: [string, ...A]): void {
    x[1] = 5; // ERROR: not writable
  }
  function f<A extends ReadonlyArray<number>>(x: [...A]): A {
    x.push(1); // ERROR: read-only array
    return x;
  }
  function h<A extends Array<number>>(x: [...A]): A {
    x.push(1); // OK, like for `x: A`
    return x;
  }
}

// A rest element after an inexact spread
{
  type X = [...[string, ...], ...Array<number>]; // ERROR
  declare const x: X;
  x as empty; // OK: `any`
}

// Variance
type Covariant<out X> = [string, ...Array<X>]; // ERROR: X in an input/output position
