/**
 * @format
 * @flow
 */

({
  p: 42, // Error: number ~> empty
}) as {
  p: empty,
};

({
  a: 1, // Error: number ~> empty
  b: 2, // Error: number ~> empty
  c: 3, // Error: number ~> empty
}) as {
  a: empty,
  b: empty,
  c: empty,
};
