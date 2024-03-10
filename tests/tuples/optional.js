([0, undefined]: [number, ?string]); // Ok, correct arity
([0]: [number, ?string]); // Error, arity is enforced

([]: [?number, string]); // error, since second element is not marked maybe


// Optional elements
type Single = [a: number];
type WithOpt = [a: number, b?: string];
type WithReadOnlyOpt = [a: number, +b?: string];

{
  declare const single: Single;
  (single: WithOpt); // ERROR
  (single: WithReadOnlyOpt); // OK
  (single.length: 1); // OK
}

{
  declare const withOpt: WithOpt;
  (withOpt: Single); // ERROR
  (withOpt: WithReadOnlyOpt); // OK
  (withOpt: [a: number, b?: string]); // OK
  (withOpt.length: 1 | 2); // OK
  (withOpt.length: 2); // ERROR

  (withOpt[0]: number); // OK
  (withOpt[1]: string | void); // OK
  (withOpt[1]: string); // ERROR

  withOpt[1] = undefined; // ERROR
}
{
  declare const withReadOnlyOpt: WithReadOnlyOpt;
  (withReadOnlyOpt: Single); // ERROR
  (withReadOnlyOpt: WithOpt); // ERROR
  withReadOnlyOpt[1] = undefined; // ERROR
}
{
  declare const withWriteOnlyOpt: [a: number, -b?: string | boolean];
  (withWriteOnlyOpt: [a: number]); // ERROR
  (withWriteOnlyOpt: [a: number, b?: string | boolean]); // ERROR
  (withWriteOnlyOpt: [a: number, -b?: string]); // OK
}
{
  declare const f11: (...[+foo?: string]) => number;
  declare const f12: (...[+foo: string | void]) => number;
  declare const f2: (foo?: string) => number;

  f11 as typeof f2; // error
  f12 as typeof f2; // ok
  f2 as typeof f11; // ok
  f2 as typeof f12; // ok
  f11 as typeof f12; // error
  f12 as typeof f11; // ok
  // from above, f12 = f2 <: f11
}

([1]: Single); // OK
([1, undefined]: Single); // ERROR

([1]: WithOpt); // OK
([1, 's']: WithOpt); // OK
([1, undefined]: WithOpt); // ERROR
([1, true]: WithOpt); // ERROR

([1]: WithReadOnlyOpt); // OK
([1, 's']: WithReadOnlyOpt); // OK
([1, undefined]: WithReadOnlyOpt); // ERROR
([1, true]: WithReadOnlyOpt); // ERROR

type InvalidReqAfterOpt = [a: number, b?: string, c: string]; // ERROR
{
  declare const x: mixed;
  (x: InvalidReqAfterOpt); // OK - it's `any`
}
type InvalidReqAfterOptMultiple = [a: number, b?: string, c: string, d: boolean]; // ERROR (on first one)

// Refinements
{
  declare const x: [number] | [a: boolean, b?: string];
  if (x.length === 1) {
    (x: [number] | [a: boolean, b?: string]); // OK
    (x: empty); // ERROR
  } else {
    (x: [a: boolean, b?: string]); // OK
    (x[0]: boolean); // OK
    (x: [number]); // ERROR
  }
}
{
  declare const x: [number] | [a: boolean, b: string, c?: string];
  if (x.length === 2) {
    (x: [a: boolean, b: string, c?: string]); // OK
    (x[0]: boolean); // OK
    (x: [number]); // ERROR
  } else {
    (x: [number] | [a: boolean, b: string, c?: string]); // OK
    (x: empty); // ERROR
  }
}

// Array initializers
{
  const x: [a?: 0] = [];
  const y: [a?: 1] = [1];
}

// Array rest
{
  declare const x: [a: 1, b: 2, c?: 3, d?: 4];

  const [, ...rest0] = x;
  (rest0: [b: 2, c?: 3, d?: 4]); // OK
  (rest0: [a: 1, b: 2, c?: 3, d?: 4]); // ERROR
  (rest0: [a: 1, b?: 3, c?: 4]); // ERROR

  const [,,, ...rest1] = x;
  (rest1: [d?: 4]); // OK
  (rest1: [d?: empty]); // ERROR

  const [,,,, ...rest2] = x;
  (rest2: []); // OK
  (rest2: [1]); // ERROR
}

// `undefined` values
{
  ([1, undefined]: [number, b?: string | void]); // OK
  type O = {a?: string};
  ([1, undefined]: [number, b?: O['a']]); // OK
}

// `Optional` & `Required`
type AllRequired = [number, string];
([]: AllRequired); // ERROR
([]: Partial<AllRequired>); // OK

type AllOptional = [a?: number, b?: string];
([]: AllOptional); // OK
([]: Required<AllOptional>); // ERROR
([1, 's']: Required<AllOptional>); // OK
