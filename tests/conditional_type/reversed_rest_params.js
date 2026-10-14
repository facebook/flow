// `infer` on a rest param solves it from the params of the matched function.

type AfterSecond<F> = F extends (a: string, b: number, ...args: infer A) => unknown
  ? A
  : empty;

declare function no_rest(a: string): void;
declare const no_rest_after_second: AfterSecond<typeof no_rest>;
no_rest_after_second as []; // ok
no_rest_after_second as [number]; // error: arity
