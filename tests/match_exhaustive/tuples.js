// Boolean
{
  declare const x: [boolean, boolean];

  match (x) { // OK
    [true, true] => {}
    [true, false] => {}
    [false, true] => {}
    [false, false] => {}
  }

  match (x) { // ERROR: missing `[false, true]`
    [true, true] => {}
    [true, false] => {}
    [false, false] => {}
  }
}

// Enums
{
  enum E {
    A,
    B,
    C
  }

  declare const x: [E, E];

  match (x) { // OK
    [E.A, E.A] => {}
    [E.A, E.B] => {}
    [E.A, E.C] => {}
    [E.B, E.A] => {}
    [E.B, E.B] => {}
    [E.B, E.C] => {}
    [E.C, E.A] => {}
    [E.C, E.B] => {}
    [E.C, E.C] => {}
  }

  match (x) { // OK
    [E.A | E.B, E.A] => {}
    [E.A | E.B | E.C, E.B | E.C] => {}
    [E.C, E.A] => {}
  }

  match (x) { // ERROR: missing `[E.C, E.A]`
    [E.A, E.A] => {}
    [E.A, E.B] => {}
    [E.A, E.C] => {}
    [E.B, E.A] => {}
    [E.B, E.B] => {}
    [E.B, E.C] => {}
    [E.C, E.B] => {}
    [E.C, E.C] => {}
  }

  match (x) { // ERROR: missing `[E.C, E.A]`
    [E.A | E.B, E.A] => {}
    [E.A | E.B | E.C, E.B | E.C] => {}
  }
}

// Wildcards
{
  declare const x: [boolean, boolean];

  match (x) { // OK
    _ => {}
  }

  match (x) { // OK
    [_, _] => {}
  }

  match (x) { // OK
    [_, true] => {}
    [_, false] => {}
  }

  match (x) { // OK
    [true, _] => {}
    [false, _] => {}
  }

  match (x) { // ERROR: missing `[false, false]`
    [true, _] => {}
    [_, true] => {}
  }
}

// Nested
{
  enum E {
    A,
    B,
  }

  declare const x: [[boolean, E], [1 | 2, 'foo' | 'bar']];

  match (x) { // OK
    [[true, E.A], [1, 'foo']] => {}
    [[true, E.A], [1, 'bar']] => {}
    [[true, E.A], [2, 'foo']] => {}
    [[true, E.A], [2, 'bar']] => {}
    [[true, E.B], [1, 'foo']] => {}
    [[true, E.B], [1, 'bar']] => {}
    [[true, E.B], [2, 'foo']] => {}
    [[true, E.B], [2, 'bar']] => {}
    [[false, E.A], [1, 'foo']] => {}
    [[false, E.A], [1, 'bar']] => {}
    [[false, E.A], [2, 'foo']] => {}
    [[false, E.A], [2, 'bar']] => {}
    [[false, E.B], [1, 'foo']] => {}
    [[false, E.B], [1, 'bar']] => {}
    [[false, E.B], [2, 'foo']] => {}
    [[false, E.B], [2, 'bar']] => {}
  }

  match (x) { // OK
    [[true, E.A], [1, 'bar']] => {}
    [[true, E.A], [_, 'foo']] => {}
    [[true, E.A], [2, 'bar']] => {}
    [[true, E.B], [1, 'foo']] => {}
    [[true, E.B], _] => {}
    [[false, _], [1, 'foo']] => {}
    [[false, E.A], [1, 'bar']] => {}
    [[false, E.A], [2, 'foo']] => {}
    [[false, E.A], [2, 'bar']] => {}
    [[false, E.B], [1, 'bar']] => {}
    [[false, E.B], [2, _]] => {}
  }

  match (x) { // ERROR: missing `[[false, E.B], [1, 'bar']]`
    [[true, E.A], [1, 'foo']] => {}
    [[true, E.A], [1, 'bar']] => {}
    [[true, E.A], [2, 'foo']] => {}
    [[true, E.A], [2, 'bar']] => {}
    [[true, E.B], [1, 'foo']] => {}
    [[true, E.B], [1, 'bar']] => {}
    [[true, E.B], [2, 'foo']] => {}
    [[true, E.B], [2, 'bar']] => {}
    [[false, E.A], [1, 'foo']] => {}
    [[false, E.A], [1, 'bar']] => {}
    [[false, E.A], [2, 'foo']] => {}
    [[false, E.A], [2, 'bar']] => {}
    [[false, E.B], [1, 'foo']] => {}
    [[false, E.B], [2, 'foo']] => {}
    [[false, E.B], [2, 'bar']] => {}
  }
}

// Union of tuples of different lengths
{
  enum E {
    A,
    B,
  }

  declare const x: [boolean] | [1 | 2, E] | [E, 1 | 2, boolean];

  match (x) { // OK
    [true] => {}
    [false] => {}
    [1, E.A] => {}
    [1, E.B] => {}
    [2, E.A] => {}
    [2, E.B] => {}
    [E.A, 1, true] => {}
    [E.A, 1, false] => {}
    [E.A, 2, true] => {}
    [E.A, 2, false] => {}
    [E.B, 1, _] => {}
    [E.B, 2, _] => {}
  }

  match (x) { // OK
    [_] => {}
    [_, _] => {}
    [_, _, _] => {}
  }

  match (x) { // ERROR: missing `[_, _]`
    [_] => {}
    [_, _, _] => {}
  }
}

// Union of tuples with non-tuple
{
  declare const x: ?[boolean];

  match (x) { // OK
    null => {}
    undefined => {}
    [true] => {}
    [false] => {}
  }
}

// Union of tuple with sentinel value
{
  declare const x: ['foo', boolean] | ['bar', 1 | 2];

  match (x) { // OK
    ['foo', true] => {}
    ['foo', false] => {}
    ['bar', 1] => {}
    ['bar', 2] => {}
  }

  match (x) { // ERROR: missing `['bar', _]`
    ['foo', true] => {}
    ['foo', false] => {}
  }
}

// Optional tuple elements
{
  declare const x: [a: 1, b?: 1, c?: 1];

  match (x) { // OK
    [_] => {}
    [_, _] => {}
    [_, _, _] => {}
  }

  match (x) { // ERROR: missing `[_, _]`
    [_] => {}
    [_, _, _] => {}
  }
}

// Inexact tuple patterns
{
  declare const x: [1, 1] | [1, 1, 1];

  match (x) { // OK
    [...] => {}
  }

  match (x) { // OK
    [1, ...] => {}
  }

  match (x) { // OK
    [1, 1, ...] => {}
  }

  match (x) { // ERROR: missing `[_, _]`
    [1, 1, 1, ...] => {} // ERROR: unnecessary `...`
  }
}

// Inexact tuple types
{
  declare const x: [1, ...];

  match (x) { // OK
    [...] => {}
  }

  match (x) { // OK
    [1, ...] => {}
  }

  match (x) { // ERROR: missing `[_, ...]`
    [1] => {} // OK
  }
}

// With inexhaustible
{
  declare const x: [number, boolean];
  match (x) { // OK
    [0, _] => {}, // OK
    [_, true] => {},
    [_, false] => {},
  }
}

// Unnecessary tuples
{
  declare const x: [boolean];

  match (x) {
    null => {} // ERROR
    [true] => {}
    [1] => {} // ERROR
    [false] => {}
    [_] => {} // ERROR
    [true, false] => {} // ERROR
    [true, ...] => {} // ERROR
  }

  match (x) {
    [true | false, ...] => {} // ERROR: unnecessary `...`
  }
}

// Unnecessary inexact tuple values
{

  declare const x: [1, ...];

  match (x) {
    [] => {} // ERROR
    [1] => {} // OK
    [...] => {} // OK
  }
}

// Unnecessary nested
{
  declare const x: [[boolean]];

  match (x) {
    [[true]] => {}
    [[
      | false
      | 1 // ERROR
    ]] => {}
  }
}

// Tuples are objects
{
  declare const x: [boolean];

  match (x) { // OK
    {...} => {} // OK
  }

  match (x) { // ERROR: missing `[false]`
    {0: true} => {} // OK
  }

  match (x) { // OK
    {0: true} => {} // OK
    {0: false} => {} // OK
  }
}
{
  declare const x: [boolean, boolean];

  match (x) { // OK
    {0: true | false, ...} => {} // OK
  }

  match (x) { // OK
    {1: true | false, ...} => {} // OK
  }

  match (x) { // ERROR: missing `[false, _]`
    {0: true, ...} => {} // OK
  }

  match (x) { // ERROR: missing `[_, false]`
    {1: true, ...} => {} // OK
  }

  match (x) { // OK
    {length: _, ...} => {}
  }
}

// Test from paper
{
  declare const x: [boolean, boolean, boolean, boolean]

  match (x) { // OK
    [true, true, true, true] => {}
    [false, false, false, false] => {}
    [_, true, true, true] => {}
    [_, false, false, false] => {}
    [_, _, true, true] => {}
    [_, _, false, false] => {}
    [_, _, _, true] => {}
    [_, _, _, false] => {}
  }

  match (x) { // ERROR: missing 4 cases
    [true, true, true, true] => {}
    [false, false, false, false] => {}
    [_, true, true, true] => {}
    [_, false, false, false] => {}
    [_, _, true, true] => {}
    [_, _, false, false] => {}
    [_, _, _, true] => {}
  }
}

// With invalid patterns
{
  declare const x: [0]

  match (x) {
    [0] => {} // OK
    [+1n] => {} // ERROR: invalid pattern (doesn't error for unnecessary pattern)
    [999] => {} // ERROR: unnecessary pattern
  }
}

// Non-readable
{
  declare const x: [-foo: boolean];

  match (x) { // OK
    [_] => {} // OK
  }

  match (x) { // ERROR
    [true] => {}
  }
}

// Wildcard matches all
{
  declare const x: [0];

  match (x) {
    _ => {}
    [0] => {} // ERROR
  }

  match (x) {
    _ => {}
    [...] => {} // ERROR
  }
}
