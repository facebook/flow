// Member RHS
{
  declare const O: {|
    A: 1,
    B: 2,
  |};
  declare const x: 1 | 2;

  if (x === O.A) {
    x as 1; // OK
  } else if (x === O.B) {
    x as 2; // OK
  } else {
    x as empty; // OK
  }

  if (x === O.A) {
    x as 1; // OK
  } else {
    x as empty; // ERROR: `2` remains
  }
}

// Member LHS
{
  declare const one: 1;
  declare const two: 2;
  declare const O: {|x: 1 | 2|};

  if (O.x === one) {
    O.x as 1; // OK
  } else if (O.x === two) {
    O.x as 2; // OK
  } else {
    O.x as empty; // OK
  }

  if (O.x === one) {
    O.x as 1; // OK
  } else {
    O.x as empty; // ERROR: `2` remains
  }
}

// Negated sentinel regression test
{
  declare const O: {|
    foo: 'foo',
    bar: 'bar',
  |};

  type T = {|type: 'foo', value: number|}
         | {|type: 'bar', value: void|};

  declare const x: T;

  if (x.type !== O.bar) {
    x.value as number; // OK
    x.value as empty; // ERROR
  }
}

// Works in a switch
{
  declare const O: {|
    A: 1,
    B: 2,
  |};

  declare const x: 1 | 2;

  switch (x) {
    case O.A: break;
    case O.B: break;
    default:
      x as empty; // OK
  }
}

// With sentinel refinement in switch
{
  declare const x: {|
    type: 'foo',
    value: string,
  |} | {|
    type: 'bar',
    value: number,
  |};

  declare const O: {|
    foo: 'foo',
    bar: 'bar',
  |};

  switch (x.type) {
    case O.foo:
      x.value as string; // OK
      break;
    case O.bar:
      x.value as number; // OK
      break;
    default:
      x as empty; // OK
  }
}

// Regression test for when RHS and LHS both reference the same object
{
  declare const x: {|
    top: 1,
  |}

  if (x === x.top) {} // OK
}
