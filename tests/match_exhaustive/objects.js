// Boolean
{
  declare const x: {foo: boolean, bar: boolean};

  match (x) { // OK
    {foo: true, bar: true} => {}
    {foo: true, bar: false} => {}
    {foo: false, bar: true} => {}
    {foo: false, bar: false} => {}
  }

  match (x) { // ERROR: missing `{foo: false, bar: true}`
    {foo: true, bar: true} => {}
    {foo: true, bar: false} => {}
    {foo: false, bar: false} => {}
  }
}

// Enums
{
  enum E {
    A,
    B,
    C
  }

  declare const x: {foo: E, bar: E};

  match (x) { // OK
    {foo: E.A, bar: E.A} => {}
    {foo: E.A, bar: E.B} => {}
    {foo: E.A, bar: E.C} => {}
    {foo: E.B, bar: E.A} => {}
    {foo: E.B, bar: E.B} => {}
    {foo: E.B, bar: E.C} => {}
    {foo: E.C, bar: E.A} => {}
    {foo: E.C, bar: E.B} => {}
    {foo: E.C, bar: E.C} => {}
  }

  match (x) { // OK
    {foo: E.A | E.B, bar: E.A} => {}
    {foo: E.A | E.B | E.C, bar: E.B | E.C} => {}
    {foo: E.C, bar: E.A} => {}
  }

  match (x) { // ERROR: missing `{foo: E.C, bar: E.A}`
    {foo: E.A, bar: E.A} => {}
    {foo: E.A, bar: E.B} => {}
    {foo: E.A, bar: E.C} => {}
    {foo: E.B, bar: E.A} => {}
    {foo: E.B, bar: E.B} => {}
    {foo: E.B, bar: E.C} => {}
    {foo: E.C, bar: E.B} => {}
    {foo: E.C, bar: E.C} => {}
  }

  match (x) { // ERROR: missing `{foo: E.C, bar: E.A}`
    {foo: E.A | E.B, bar: E.A} => {}
    {foo: E.A | E.B | E.C, bar: E.B | E.C} => {}
  }
}

// Wildcards
{
  declare const x: {foo: boolean, bar: boolean};

  match (x) { // OK
    _ => {}
  }

  match (x) { // OK
    {foo: _, bar: _} => {}
  }

  match (x) { // OK
    {foo: _, bar: true} => {}
    {foo: _, bar: false} => {}
  }

  match (x) { // OK
    {foo: true, bar: _} => {}
    {foo: false, bar: _} => {}
  }

  match (x) { // ERROR: missing `{foo: false, bar: false}`
    {foo: true, bar: _} => {}
    {foo: _, bar: true} => {}
  }
}

// Additional props in pattern
{
  declare const x: {a: boolean, b: boolean};

  match (x) { // ERROR
    {a: _, b: _, c: _} => {} // ERROR
  }
}

// Nested
{
  enum E {
    A,
    B,
  }

  declare const x: {a: {x: boolean, y: E}, b: {v: 1 | 2, w: 'foo' | 'bar'}};

  match (x) { // OK
    {a: {x: true, y: E.A}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: true, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: true, y: E.A}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: true, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: true, y: E.B}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: true, y: E.B}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: true, y: E.B}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: true, y: E.B}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: false, y: E.A}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: false, y: E.B}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: false, y: E.B}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: false, y: E.B}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: false, y: E.B}, b: {v: 2, w: 'bar'}} => {}
  }

  match (x) { // OK
    {a: {x: true, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: true, y: E.A}, b: {v: _, w: 'foo'}} => {}
    {a: {x: true, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: true, y: E.B}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: true, y: E.B}, b: _} => {}
    {a: {x: false, y: _}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: false, y: E.B}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: false, y: E.B}, b: {v: 2, w: _}} => {}
  }

  match (x) { // ERROR: missing `{a: {x: false, y: E.B}, b: {v: 1, w: 'bar'}}`
    {a: {x: true, y: E.A}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: true, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: true, y: E.A}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: true, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: true, y: E.B}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: true, y: E.B}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: true, y: E.B}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: true, y: E.B}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: false, y: E.A}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 1, w: 'bar'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: false, y: E.A}, b: {v: 2, w: 'bar'}} => {}
    {a: {x: false, y: E.B}, b: {v: 1, w: 'foo'}} => {}
    {a: {x: false, y: E.B}, b: {v: 2, w: 'foo'}} => {}
    {a: {x: false, y: E.B}, b: {v: 2, w: 'bar'}} => {}
  }
}

// Errors for missing `...` in pattern
{
  declare const x: {a: boolean, b: string, c: number};

  match (x) { // OK
    {} => {} // ERROR
  }

  match (x) { // OK
    {a: _, b: _} => {} // ERROR
  }

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // OK
    {a: _, b: _, ...} => {} // OK
  }
}

// Inexact object value
{
  declare const x: {a: boolean, b: string, c: number, ...};

  match (x) { // OK
    {} => {} // ERROR
  }

  match (x) { // OK
    {a: _, b: _, c: _} => {} // ERROR
  }

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // OK
    {a: _, b: _, c: _, ...} => {} // OK
  }
}

// Optional properties
{
  declare const x: {a: 1, b?: 1};

  match (x) { // ERROR
    {a: 1, b: 1} => {}
  }

  match (x) { // ERROR
    {a: 1, b: 1 | undefined} => {}
  }

  match (x) { // OK
    {a: 1, ...} => {}
  }
}

// Non-own properties
{
  declare const x: {};

  match (x) { // OK
    {hasOwnProperty: _} => {} // OK
  }
}

// Non-readable
{
  declare const x: {-foo: boolean};

  match (x) { // OK
    {foo: _} => {} // OK
  }

  match (x) { // ERROR
    {foo: true} => {}
  }
}

// Non-readable optional
{
  declare const x: {-foo?: boolean};

  match (x) { // ERROR: missing `{...}`
    {foo: _} => {} // OK
  }
}

// Interface: basic
{
  declare const x: interface {foo: boolean, bar: boolean};

  match (x) { // OK
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: true, ...} => {}
    {foo: false, bar: false, ...} => {}
  }

  match (x) { // ERROR: missing `{bar: true, foo: false, ...}`
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: false, ...} => {}
  }
}

// Class: basic
{
  class C {
    foo: boolean;
    bar: boolean;
  }
  declare const x: C;

  match (x) { // OK
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: true, ...} => {}
    {foo: false, bar: false, ...} => {}
  }

  match (x) { // ERROR: missing `{bar: true, foo: false, ...}`
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: false, ...} => {}
  }
}

// Interface: proto props
{
  declare const x: interface {foo: boolean, m(): void};

  match (x) { // OK
    {foo: true, m: _, ...} => {}
    {foo: false, m: _, ...} => {}
  }
}

// Class: extends
{
  class A {
    foo: boolean;
  }
  class C extends A {
  }
  declare const x: C;

  match (x) { // OK
    {foo: true, ...} => {}
    {foo: false, ...} => {}
  }
}
{
  class A {
    foo: boolean;
  }
  class C extends A {
    bar: boolean;
  }
  declare const x: C;

  match (x) { // OK
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: true, ...} => {}
    {foo: false, bar: false, ...} => {}
  }

  match (x) { // ERROR: missing `{bar: true, foo: false, ...}`
    {foo: true, bar: true, ...} => {}
    {foo: true, bar: false, ...} => {}
    {foo: false, bar: false, ...} => {}
  }
}

// Unused rest
{
  declare const x: {a: boolean};

  match (x) { // OK
    {a: true, ...} => {} // ERROR
    {...} => {} // OK
  }
}

// Indexer
{
  declare const x: {[string]: boolean};

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // ERROR
    {foo: true, ...} => {} // OK
    {foo: false, ...} => {} // OK
  }

  match (x) { // ERROR
    {foo: true, ...} => {} // OK
    {foo: true, ...} => {} // ERROR
  }

  match (x) { // OK
    {foo: 1, ...} => {} // ERROR
    {...} => {}
  }
}
{
  declare const x: {[number]: boolean};

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // ERROR
    {1: true, ...} => {} // OK
  }

  match (x) { // OK
    {1: 1, ...} => {} // ERROR
    {...} => {}
  }
}

// Many wildcard props
{
  declare const x: {a: number, b: string, c: boolean, d: symbol, e: bigint, f: 5};

  match (x) {} // ERROR

  match (x) {
    {a: 0, b: "", c: true, ...} => {}
  }
}

// Error for disjoint object union
{
  declare const x:
    | {type: 'ok', value: number}
    | {type: 'error', value: Error};

  match (x) {} // ERROR: `type` prop values are printed
}

// Deep object left over
{
  declare const x: {foo: {bar: {baz: {zap: {bort: boolean}}}}};

  match (x) {} // ERROR: only brings `foo: _`
}

// Sentinel prop is last
{
  declare const x:
    | {a: 1 | 2, b: 1 | 2, xxx: 'foo'}
    | {a: 2 | 3, b: 2 | 3, xxx: 'bar'}

  match (x) { // ERROR: missing `{xxx: 'bar', a: _, b: _}`
    {a: 1 | 2, b: 1 | 2, xxx: 'foo'} => {}
  }

  match (x) { // ERROR: `xxx` prop is printed first in pattern
    {a: 1, b: 1, xxx: 'foo'} => {}
  }
}

// With invalid patterns
{
  declare const x: {foo: 0}

  match (x) {
    {foo: 0} => {} // OK
    {foo: +1n} => {} // ERROR: invalid pattern (doesn't error for unnecessary pattern)
    {foo: 999} => {} // ERROR: unnecessary pattern
  }
}

// Wildcard matches all
{
  declare const x: {foo: 0};

  match (x) {
    _ => {}
    {foo: 0} => {} // ERROR
  }
}

// Structure prop checked after leaf-only prop
{
  declare const x: {
    a: {foo: 0},
    b: 0 | 1,
  } | {
    a: {foo: 999},
    b: 999,
  };

  match (x) { // ERROR: missing {b: 0 | 1, a: _}
    {a: {foo: 999}, b: 999} => {}
  }
}
