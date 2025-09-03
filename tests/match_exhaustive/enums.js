// Basic enum
{
  enum E {
    A,
    B,
  }

  declare const x: E;

  match (x) { // OK
    E.A => {}
    E.B => {}
  }

  match (x) { // OK
    E.A | E.B => {}
  }

  match (x) { // ERROR: missing `E.B`
    E.A => {}
  }
}

// Enum with unknown members
{
  enum E {
    A,
    B,
    ...
  }

  declare const x: E;

  match (x) { // OK
    E.A => {}
    E.B => {}
    _ => {}
  }

  match (x) { // OK
    E.A => {}
    _ => {}
  }

  match (x) { // ERROR: missing wildcard for unknown members
    E.A => {}
    E.B => {}
  }

  // flowlint-next-line require-explicit-enum-checks:error
  match (x) { // OK
    E.A => {}
    E.B => {}
    _ => {}
  }

  // flowlint-next-line require-explicit-enum-checks:error
  match (x) { // ERROR
    E.A => {}
    _ => {}
  }

  enum D {
    M,
    N,
    ...
  }
  declare const y: E | D;
  // flowlint-next-line require-explicit-enum-checks:error
  match (y) { // ERROR
    _ => {}
  }
}

// Mixed with non-enum
{
  enum E {
    A,
    B,
  }

  declare const x: ?E;

  match (x) { // OK
    E.A => {}
    E.B => {}
    null | undefined => {}
  }

  match (x) { // ERROR: missing `E.B`
    E.A => {}
    null | undefined => {}
  }

  match (x) { // ERROR: missing `undefined`
    E.A => {}
    E.B => {}
    null => {}
  }
}

// Invalid member
{
  enum E {
    A,
    B,
  }

  declare const x: E;

  match (x) {
    E.A => {}
    E.B => {}
    E.XXX => {} // ERROR: invalid enum access
  }
}

// Invalid check
{
  enum E {
    A,
    B,
  }

  declare const x: E;

  match (x) { // ERROR
    x => {} // ERROR
  }
}
