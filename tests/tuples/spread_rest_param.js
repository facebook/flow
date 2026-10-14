// A type argument spread after other elements of a tuple type, like `B` in `[...A, ...B]`, is
// solved from what the tuple type must be, minus the elements before it.

declare function partial<A extends ReadonlyArray<unknown>, B extends ReadonlyArray<unknown>>(
  f: (...args: [...A, ...B]) => void,
  ...args: A
): (...args: B) => void;

declare function three(a: string, b: number, c: boolean): void;
const bound = partial(three, 'x', 1); // ok
bound(true); // ok
bound(1); // error: number ~> boolean

declare function one(a: string): void;
partial(one, 'x', 1)(); // ok

type Concat<A extends ReadonlyArray<unknown>, B extends ReadonlyArray<unknown>> = [...A, ...B];
declare function partial_alias<A extends ReadonlyArray<unknown>, B extends ReadonlyArray<unknown>>(
  f: (...args: Concat<A, B>) => void,
  ...args: A
): (...args: B) => void;
const bound_alias = partial_alias(three, 'x', 1); // ok
bound_alias(true); // ok
bound_alias(1); // error: number ~> boolean

declare function partial_after_string<
  A extends ReadonlyArray<unknown>,
  B extends ReadonlyArray<unknown>,
>(
  f: (...args: [string, ...A, ...B]) => void,
  ...args: A
): (...args: B) => void;
const bound_after_string = partial_after_string(three, 1); // ok
bound_after_string(true); // ok
bound_after_string(1); // error: number ~> boolean

type AfterTwo = typeof three extends (...args: [...[string, number], ...infer A]) => unknown
  ? A
  : empty;
declare const after_two: AfterTwo;
after_two as [boolean]; // ok
after_two as [number]; // error: boolean ~> number

const unbound = partial(three); // ok
unbound('x', 1, true); // ok
unbound('x', 1, 1); // error: number ~> boolean

declare function partial_elem<A extends ReadonlyArray<unknown>, B extends ReadonlyArray<unknown>>(
  f: (...args: [...A, ...B]) => void,
  ...args: A
): (x: B[number]) => void;
partial_elem(three, 'x', 1)(true); // ok
partial_elem(three, 'x', 1)(''); // error: string ~> boolean
