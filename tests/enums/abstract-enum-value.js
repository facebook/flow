enum E {A, B}

// Concrete enum as LHS
{
  declare const x: E;

  x as EnumValue<>; // OK
  x as EnumValue<string>; // OK
  x as EnumValue<string | number>; // OK

  x as EnumValue<number>; // ERROR
  x as EnumValue<{foo: 1}>; // ERROR
}

// Abstract enum as LHS
{
  declare const x: EnumValue<string>;

  const s: string = x as string; // OK
  x as EnumValue<string | number>; // OK

  x as empty; // ERROR
  x as E; // ERROR
  x as number; // ERROR
}

// Generic representation type
function f<TEnumValue: EnumValue<>>(x: TEnumValue) {
  x as TEnumValue; // OK
}
function g<T: string | number>(x: EnumValue<T>): T {
  x as EnumValue<string | number>; // OK

  x as boolean; // ERROR
  x as EnumValue<boolean>; // ERROR

  return x as T; // OK
}

// Refinement
{
  declare const x: EnumValue<string> | void;

  x as EnumValue<string>; // ERROR

  if (x) {
    x as EnumValue<string>; // OK
  }
  if (typeof x === 'string') {
    x as EnumValue<string>; // OK
    const s: string = x; // ERROR
  }
}

// Different kind of enums as LHS
{
  enum N {A = 1}
  N.A as EnumValue<>; // OK
  N.A as EnumValue<number>; // OK

  enum B {A = true}
  B.A as EnumValue<>; // OK
  B.A as EnumValue<boolean>; // OK

  enum S of symbol {A}
  S.A as EnumValue<>; // OK
  S.A as EnumValue<symbol>; // OK

  enum I {A = 1n}
  I.A as EnumValue<>; // OK
  I.A as EnumValue<bigint>; // OK
}

// Use as representation type
{
  declare const x: EnumValue<string>;
  const a: string = x as string; // OK
  const b: string = x; // ERROR
}
