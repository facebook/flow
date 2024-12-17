// Primitive value union
{
  declare const x: 1 | -2 | 3n | 's' | false | null;

  const e1 = match (x) {
    1: 0,
    -2: 0,
    3n: 0,
    's': 0,
    false: 0,
    null: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    false: 0,
    3n: 0,
    null: 0,
    const d: d as empty, // ERROR: not all members checked
  };
}

// Identifier patterns
{
  declare const x: 1 | 2;

  declare const one: 1;
  declare const two: 2;

  const e1 = match (x) {
    one: 0,
    two: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    one: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// `undefined`
{
  declare const x: 1 | void;

  const e1 = match (x) {
    1: 0,
    undefined: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    const d: d as empty, // ERROR: `undefined` not checked
  };
}

// Maybe types
{
  declare const x: ?1;

  const e1 = match (x) {
    1: 0,
    undefined: 0,
    null: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    const d: d as empty, // ERROR: `null` and `undefined` not checked
  };
}

// Member patterns
{
  declare const x: 1 | 2;

  declare const o: {
    one: 1,
    two: 2,
  };

  const e1 = match (x) {
    o.one: 0,
    o.two: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    o.one: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// `as` pattern refines using its pattern
{
  declare const x: 1 | 2;

  const e1 = match (x) {
    1 as a: a as 1, // OK
    2 as a: a as 2, // OK
    const d: d as empty, // OK: all members checked
  };
}


// Top level binding and wildcard
{
  declare const x: 1 | 2;

  const e1 = match (x) {
    1: 0,
    const a: a as 2, // OK
    const d: d as empty, // OK: above binding catches all
  };

  const e2 = match (x) {
    1: 0,
    _: 0, // OK
    const d: d as empty, // OK: above wildcard catches all
  };
}

// Non-ident/member argument still works
{
  declare const f: () => 1 | 2;

  const e1 = match (f()) {
    1: 0,
    2: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (f()) {
    1: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// Or pattern
{
  declare const x: 1 | 2 | 3;

  const e1 = match (x) {
    1 | 2 | 3: true,
    const d: d as empty, // OK
  };

  const e2 = match (x) {
    1 | 2: true,
    const d: d as empty, // ERROR: `3` not checked
  };
}

// Patterns with guard could match or not match
{
  declare const x: 1 | 2;

  declare function f(): boolean;

  const e1 = match (x) {
    1: 0,
    2 if f(): 0,
    const d: d as empty, // ERROR: `2` not checked
  };

  const e2 = match (x) {
    1: 0,
    2 if f(): 0,
    2: 0,
    const d: d as empty, // OK
  };
}
