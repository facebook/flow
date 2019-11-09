// @flow

enum E {
  A,
  B,
}

// Error: accessing non-existent member
const x = E.C;
// As it is the result of an error, `x` is `any`
(x: boolean);
