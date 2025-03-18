declare const x: number;

const foo = "foo"; // Will not be in autocomplete results below.

const e = match (x) {
  [...const f ] => 0,
//           ^
  {...const f } => 0,
//           ^
  1 as f => 0,
//      ^
  const f => 0,
//       ^

  c
// ^
};
