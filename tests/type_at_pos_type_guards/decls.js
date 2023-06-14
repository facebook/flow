type G = (x: mixed) => x is number;
//   ^
function foo(x: mixed): x is number {
//       ^
  return typeof x === "number";
}
