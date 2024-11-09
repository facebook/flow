// @flow

// Bindings
{
  declare const x: number;

  const out = match (x) {
    const a: a,
//        ^
  }
}

{
  declare const x: [number, string];

  const out = match (x) {
    [_, const a]: a,
//            ^
  }
}

// Identifier
{
  declare const x: number;
  declare const foo: 1;
  const out = match (x) {
    foo: 0,
//  ^
    _,
  }
}

{
  declare const x: [number];
  declare const foo: 1;
  const out = match (x) {
    [foo]: 0,
//   ^
    _,
  }
}

// Member
{
  declare const x: number;
  declare const O: {foo: 1, bar: 1};
  const out = match (x) {
    O.foo: 0,
//  ^
    O.bar: 0,
//    ^
    _,
  }
}

{
  declare const x: [number];
  declare const O: {foo: 1, bar: 1};
  const out = match (x) {
    [O.foo]: 0,
//   ^
    [O.bar]: 0,
//     ^
    _,
  }
}
