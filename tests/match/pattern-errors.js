// Match expressions only allow `const` bindings
{
  declare const x: 0 | 1 | [2] | {a: 3, b: 4};

  const e1 = match (x) {
     [...let a]: 0, // ERROR
     {let a, ...let b}: 0, // ERROR
     0 as let a: 0, // ERROR
     let a: 0, // ERROR
  };

  const e2 = match (x) {
     [...var a]: 0, // ERROR
     {var a, ...var b}: 0, // ERROR
     0 as var a: 0, // ERROR
     var a: 0, // ERROR
  };
}

// Invalid numeric property
{
  declare const x: {1: true};

  const e1 = match (x) {
    {1.1: _}: 0, // ERROR
    _: 0,
  };
}

// Unary pattern on `0` banned
{
  declare const x: 0;

  const e1 = match (x) {
    -0: true, // ERROR
    +0: true, // ERROR
    0: true, // OK
  };
}
