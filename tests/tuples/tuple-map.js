// $TupleMap should still work as a lower bound
function foo1(arr: $TupleMap<[number, number], number => string>): [1, 2] {
  return arr; // error
}
// $TupleMap should still work as a upper bound
function foo2(arr: [number, number]): $TupleMap<[number, number], number => string> {
  return arr; // error
}
