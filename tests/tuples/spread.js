// Basic
type A = [1, 2];

type Same = [...A]; // OK
{
  [1, 2] as Same; // OK
  declare const x: Same;
  x as [1, 2]; // OK

  [1] as Same; // ERROR: arity
  x as [1, 2, 3]; // ERROR: arity

  [1, 99] as Same; // ERROR: element type
}

type Middle = [0, ...A, 3]; // OK
{
  [0, 1, 2, 3] as Middle; // OK
  declare const x: Middle;
  x as [0, 1, 2, 3]; // OK

  [0, 1, 2, 3, 4] as Middle; // ERROR: arity
  x as [0, 1, 2]; // ERROR: arity

  [0, 1, 99, 3] as Middle; // ERROR: element type
}

// Required after optional
type Optionals = [a: 1, b?: 2];
type ReqAfterOptionalInline = [0, ...Optionals, 3]; // ERROR
type ReqAfterOptionalSpread = [...Optionals, ...A]; // ERROR

// Unions
type Union = [1, 2] | [6];
type FromUnion = [0, ...Union, 9]; // OK
[0, 1, 2, 9] as FromUnion; // OK
[0, 6, 9] as FromUnion; // OK
[0, 9] as FromUnion; // ERROR

// Invalid spread
type InvalidSpread1 = [1, ...2]; // ERROR
{
  declare const x: InvalidSpread1;
  x as empty; // OK
}
type Alias = {};
type InvalidSpread2 = [1, ...Alias]; // ERROR
{
  declare const x: InvalidSpread2;
  x as empty; // OK
}
type ArraySpread = [...Array<mixed>]; // ERROR
{
  declare const x: ArraySpread;
  x as empty; // OK
}
type ROArraySpread = [...$ReadOnlyArray<mixed>]; // ERROR
{
  declare const x: ROArraySpread;
  x as empty; // OK
}

// Spreading `any`
type AnySpread = [1, 2, ...any]; // OK
{
  declare const x: AnySpread;
  x as empty; // OK
}

// Spreading `empty`
type EmptySpread = [1, 2, ...empty]; // OK
{
  declare const x: AnySpread;
  x as empty; // OK
}

// Tuple-like array spread
{
  const x = [1, 2];
  type TupleArrSpread = [0, ...typeof x]; // OK
  [0, 1, 2] as TupleArrSpread; // OK
}

// Generics
type WithTail<T> = [1, ...T];
[1, 2, 3] as WithTail<[2, 3]>; // OK
[1, 2, 66] as WithTail<[2, 3]>; // ERROR

// Tuple-like array spread
{
  const x = [1, 2];
  type TupleArrSpread = [0, ...typeof x]; // OK
  [0, 1, 2] as TupleArrSpread; // OK
}

declare function tail<T>(xs: [1, ...T]): T;
tail<[2, 3]>([ 1, 2, 3]); // OK
{
  const x = tail([1]); // OK
  x as []; // OK
}
{
  const x = tail([1, 2, 3]);
  x as [2, 3]; // OK
  x as [1, 2, 3]; // ERROR
}
tail([666, 2, 3]); // ERROR
tail([]); // ERROR

declare function noReversal1<T>(xs: [1, ...T, 2]): T;
noReversal1([1, 9, 2]); // ERROR

declare function noReversal2<T>(xs: [...T, ...any]): T;
noReversal2([1, 2, 3]); // ERROR
