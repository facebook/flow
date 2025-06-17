declare const cond: boolean;

// Basic
{
  declare const x: 1 | 2;

  match (x) { // ERROR: missing `1`
    1 if (cond) => {}
    2 => {}
  }

  match (x) { // OK
    1 if (cond) => {}
    1 => {} // OK
    2 => {}
  }

  match (x) { // OK
    1 => {}
    1 if (cond) => {} // ERROR
    2 => {}
  }

  match (x) { // OK
    1 | 2 if (cond) => {}
    1 => {} // OK
    2 => {} // OK
  }

  match (x) {
    _ => {}
    1 if (cond) => {} // ERROR
  }

  // Not in input type

  match (x) { // OK
    3 if (cond) => {} // ERROR
    1 | 2 => {} // OK
  }

  match (x) { // OK
    3 | 2 if (cond) => {} // ERROR
    1 | 2 => {} // OK
  }
}

// Object patterns
{
  declare const x: {foo: 1} | {bar: 2};

  match (x) { // ERROR: missing `{foo: 1}`
    {foo: 1} if (cond) => {}
    {bar: 2} => {}
  }

  match (x) { // OK
    {foo: 1} if (cond) => {}
    {foo: 1} => {} // OK
    {bar: 2} => {}
  }

  match (x) { // OK
    {foo: 1} => {}
    {foo: 1} if (cond) => {} // ERROR
    {bar: 2} => {}
  }

  match (x) { // OK
    {foo: 1} | {bar: 2} if (cond) => {}
    {foo: 1} => {} // OK
    {bar: 2} => {} // OK
  }

  match (x) {
    _ => {}
    {foo: 1} if (cond) => {} // ERROR
  }

  // Not in input type

  match (x) { // OK
    {baz: 3} if (cond) => {} // ERROR
    {foo: 1} | {bar: 2} => {} // OK
  }

  match (x) { // OK
    {baz: 3} | {bar: 2} if (cond) => {} // ERROR
    {foo: 1} | {bar: 2} => {} // OK
  }
}

// Tuple patterns
{
  declare const x: [1] | [2];

  match (x) { // ERROR: missing `[1]`
    [1] if (cond) => {}
    [2] => {}
  }

  match (x) { // OK
    [1] if (cond) => {}
    [1] => {} // OK
    [2] => {}
  }

  match (x) { // OK
    [1] => {}
    [1] if (cond) => {} // ERROR
    [2] => {}
  }

  match (x) { // OK
    [1] | [2] if (cond) => {}
    [1] => {} // OK
    [2] => {} // OK
  }

  match (x) {
    _ => {}
    [1] if (cond) => {} // ERROR
  }

  // Not in input type

  match (x) { // OK
    [3] if (cond) => {} // ERROR
    [1] | [2] => {} // OK
  }

  match (x) { // OK
    [3] | [2] if (cond) => {} // ERROR
    [1] | [2] => {} // OK
  }
}

// Wildcards
{
  declare const x: string;

  match (x) { // ERROR: missing `_`
    _ if (cond) => {} // ERROR
  }

  match (x) { // OK
    _ if (cond) => {}
    _ => {} // OK
  }

  match (x) { // OK
    _ => {}
    _ if (cond) => {} // ERROR
  }

  match (x) { // ERROR: missing `_`
    "foo" | _ if (cond) => {} // ERROR
  }

  match (x) { // ERROR: missing `_`
    _ | "foo" if (cond) => {} // ERROR
  }

  match (x) { // OK
    "foo" | _ if (cond) => {} // OK
    _ => {}
  }
}
