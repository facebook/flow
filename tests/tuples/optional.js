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

  (withOpt.length: 1 | 2); // OK
  (withOpt.length: 2); // ERROR
}

{
  declare const withReadOnlyOpt: WithReadOnlyOpt;
  (withReadOnlyOpt: Single); // ERROR
  (withReadOnlyOpt: WithOpt); // ERROR
}

{
  declare const withWriteOnlyOpt: [a: number, -b?: string | boolean];
  (withWriteOnlyOpt: [a: number]); // ERROR
  (withWriteOnlyOpt: [a: number, b?: string | boolean]); // ERROR
  (withWriteOnlyOpt: [a: number, -b?: string]); // OK
}

([1]: Single); // OK
([1, undefined]: Single); // ERROR

([1]: WithOpt); // OK
([1, 's']: WithOpt); // OK
([1, undefined]: WithOpt); // OK
([1, true]: WithOpt); // ERROR

([1]: WithReadOnlyOpt); // OK
([1, 's']: WithReadOnlyOpt); // OK
([1, undefined]: WithReadOnlyOpt); // OK
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
