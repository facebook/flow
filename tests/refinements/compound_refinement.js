// @flow

type U = {
  type: "a";
  a: U;
} | {
  type: "b";
  b: mixed;
};

declare var x: U;

if (x.type === 'a' && x.a.type === 'b') {
  x.a.b; // okay
}
