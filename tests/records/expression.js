// Empty record
{
  record R {}

  const x: R = R {}; // OK

  R {xxx: 0}; // ERROR: extra field `xxx`
}

// Multiple fields, order
{
  record R {
    a: number,
    b: string,
    c: boolean,
  }

  R {a: 1, b: 's', c: true}; // OK
  R {c: true, b: 's', a: 1}; // OK
}

// Optional fields
{
  record R {
    a: number,
    b: boolean = false,
  }

  R {a: 1}; // OK
  R {a: 1, b: true}; // OK
  R {a: 1, b: undefined}; // OK (uses default value)

  R {a: 1, b: null}; // ERROR
}

// Missing required fields
{
  record R {
    a: number,
    b: string,
  }

  R {a: 1}; // ERROR: missing `b`
  R {b: 's'}; // ERROR: missing `a`
  R {}; // ERROR: missing `a` and `b`
}

// Extra fields
{
  record R {
    a: number,
  }

  R {a: 1, b: 's'}; // ERROR: extra field `b`
}

// Type errors
{
  record R {
    a: number,
    b: string,
  }

  R {a: 's', b: 1}; // ERROR: wrong type for `a` and `b`
  R {a: null, b: 's'}; // ERROR: wrong type for `a`
}

// Shorthand
{
  record R {
    a: number,
  }

  const a = 1;
  const x: R = R {a}; // OK
}

// Spread
{
  record R {
    a: number,
    b: string,
  }

  declare const x: R;

  R {...x}; // OK
  R {...x, a: 0}; // OK
}

// Type arguments
{
  record R<T> {
    a: T,
  }

  const x = R {a: 1}; // OK
  x as R<number>; // OK

  R<number> {a: 1}; // OK

  R<1> {a: 1}; // OK
  const z: R<1> = R {a: 1}; // OK
}
{
  record R<T, U> {
    a: T,
    b: U,
  }

  const x = R {a: 1, b: 's'}; // OK
  x as R<number, string>; // OK

  R<1, 's'> {a: 1, b: 's'}; // OK
}

// Nested records
{
  record Inner {
    a: number,
  }

  record Outer {
    inner: Inner,
    b: string,
  }

  const x: Inner = Inner {a: 1}; // OK
  Outer {inner: x, b: 's'}; // OK
  Outer {inner: Inner {a: 2}, b: 's'}; // OK
}
