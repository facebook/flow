// @flow

// Bindings
{
  declare const x: number;

  const out = match (x) {
    const a => a,
//        ^
  }
}

{
  declare const x: [number, string];

  const out = match (x) {
    [_, const a] => a,
//            ^
  }
}

{
  declare const x: {a: number};

  const out = match (x) {
    {const a} => a,
//         ^
  }
}

// Identifier
{
  declare const x: number;
  declare const foo: 1;
  const out = match (x) {
    foo => 0,
//  ^
    _ => 0,
  }
}

{
  declare const x: [number];
  declare const foo: 1;
  const out = match (x) {
    [foo] => 0,
//   ^
    _ => 0,
  }
}

// Member
{
  declare const x: number;
  declare const O: {foo: 1, bar: 1};
  const out = match (x) {
    O.foo => 0,
//  ^
    O.bar => 0,
//    ^
    _ => 0,
  }
}

{
  declare const x: [number];
  declare const O: {foo: 1, bar: 1};
  const out = match (x) {
    [O.foo] => 0,
//   ^
    [O.bar] => 0,
//     ^
    _ => 0,
  }
}

{
  const out = match ([1, 2]) {
    const a => a,
//             ^
// Above should produce a tuple type, not `Array<number>`
  }
}

{
  declare const x: number;
  declare const y: number;

  const out = match ([x, y]) {
    const a => a,
//             ^
// Above should produce a tuple type, not `Array<number>`
  }
}
