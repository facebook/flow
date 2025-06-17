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

  match (x) { // ERROR: missing wildcard for unknown members
    E.A => {}
    E.B => {}
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
