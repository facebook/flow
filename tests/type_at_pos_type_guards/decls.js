type G = (x: mixed) => x is number;
//   ^
function foo(x: mixed): x is number {
//       ^
  return typeof x === "number";
}

function zeros(vals: $ReadOnlyArray<0 | 1>): $ReadOnlyArray<0> {
  return vals.filter((x): x is 0 => x === 0);
//            ^
}
