// `infer` on a rest param solves it from the params of the matched function.

type AfterSecond<F> = F extends (a: string, b: number, ...args: infer A) => unknown
  ? A
  : empty;

declare function no_rest(a: string): void;
declare const no_rest_after_second: AfterSecond<typeof no_rest>;
no_rest_after_second as []; // ok
no_rest_after_second as [number]; // error: arity

// The params after the matched ones, including the rest param: `[...params, ...rest]`.
type AfterFirst<F> = F extends (a: string, ...args: infer A) => unknown ? A : empty;

declare function tuple_rest(a: string, ...rest: [number, boolean]): void;
declare const after_first: AfterFirst<typeof tuple_rest>;
after_first as [number, boolean]; // ok
after_first as [number]; // error: arity
declare const after_second: AfterSecond<typeof tuple_rest>;
after_second as [boolean]; // ok
after_second as [number]; // error: boolean ~> number

declare function array_rest(a: string, b: number, ...rest: Array<boolean>): void;
declare const array_after_first: AfterFirst<typeof array_rest>;
array_after_first as [number, ...Array<boolean>]; // ok
array_after_first as [number]; // error: arity

function generic_rest<K extends 'a' | 'b'>(
  handler: (...args: {a: [], b: [number, number]}[K]) => void,
) {
  type Args = typeof handler extends (...args: infer A) => unknown ? A : empty;
  declare const args: Args;
  args as {a: [], b: [number, number]}[K]; // ok
}

function generic_rest_after_first<T extends ReadonlyArray<number>>(
  f: (a: string, ...args: T) => void,
) {
  declare const args: AfterFirst<typeof f>;
  args as T; // ok
}

// Trailing optional params are optional elements, which also accept `undefined`.
type Params<F> = F extends (...args: infer A) => unknown ? A : empty;
declare function optional_params(a: string, b?: number): void;
[''] as Params<typeof optional_params>; // ok
['', 1] as Params<typeof optional_params>; // ok
['', undefined] as Params<typeof optional_params>; // ok
[1] as Params<typeof optional_params>; // error: number ~> string
declare function non_trailing_optional(a?: string, b: number): void;
[undefined, 1] as Params<typeof non_trailing_optional>; // ok
[1] as Params<typeof non_trailing_optional>; // error: arity
declare function all_optional(a?: string): void;
[] as Params<typeof all_optional>; // ok
declare function optional_and_rest(a: string, b?: number, ...rest: Array<boolean>): void;
[''] as Params<typeof optional_and_rest>; // ok
['', 1, true] as Params<typeof optional_and_rest>; // ok

// The same solution in a call, where it also flows to other uses of the type argument.
declare function no_rest_two(a: string, b: number): void;
declare function array_rest_one(a: string, ...rest: Array<number>): void;
declare function after_first_call<A extends ReadonlyArray<unknown>>(
  f: (a: string, ...args: A) => void,
  g: (...args: A) => void,
): void;
after_first_call(no_rest_two, (b: number) => {}); // ok
after_first_call(no_rest_two, (b: string) => {}); // error: number ~> string
after_first_call(array_rest_one, (...rest: Array<number>) => {}); // ok
after_first_call(array_rest_one, (b: string) => {}); // error: number ~> string
