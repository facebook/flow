type G = (x: unknown) => x is number;
//   ^
function foo(x: unknown): x is number {
//       ^
  return typeof x === "number";
}

function zeros(vals: $ReadOnlyArray<0 | 1>): $ReadOnlyArray<0> {
  return vals.filter((x): x is 0 => x === 0);
//            ^
}

declare function implies(x: unknown): implies x is number;
//               ^
