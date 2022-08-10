// @flow

function myFunc(): string {
  let a = "a string";

  [1].forEach(() => {
    a = 2;
  });

  return a; // a is string
}
