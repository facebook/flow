// An array literal whose last spread is an array has a rest element.

type T = [string, ...Array<number>];

// Array literals
{
  declare const arr: Array<number>;
  ['a', ...arr] as T; // OK
  [1, ...arr] as T; // ERROR: number ~> string
  declare const t: T;
  [...t] as T; // OK
  [true, ...t] as [boolean, ...T]; // OK
}

// Arguments to a rest parameter that is a tuple with a rest element
{
  declare function f(...args: T): void;
  declare const t: T;
  declare const arr: Array<number>;
  f(...t); // OK
  f('a', ...arr); // OK
  f(1, ...arr); // ERROR: number ~> string
  f as (a: string, ...rest: Array<number>) => void; // OK
  f as (a: string, ...rest: Array<string>) => void; // ERROR: string ~> number
}

// Array literals flowing to array literals
{
  declare const arr: Array<number>;
  declare const first: string;
  let parts = [first];
  parts = [first, ...arr]; // ERROR: number ~> string
  let b = ['a', ...arr];
  b = ['b', 'c']; // ERROR: string ~> number
  declare const inexact: [string, number, ...];
  b = [...inexact]; // ERROR: unknown elements of an inexact tuple
}

// Partial application with a spread (a method's `bind`)
{
  declare class C {
    m(...args: T): void;
  }
  declare const c: C;
  declare const arr: Array<number>;
  const g = c.m.bind(c, 'a', ...arr); // OK
  g(1); // OK
  g('x'); // ERROR: string ~> number
}
