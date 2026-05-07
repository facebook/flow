type U = {
  type: "a";
  a: U;
 ...} | {
  type: "b";
  b: unknown;
 ...};

declare const x: U;

if (x.type === 'a' && x.a.type === 'b') {
  x.a.b; // okay
}
