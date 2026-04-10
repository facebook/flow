//@flow

function foo(arr: ReadonlyArray<Object>): Array<NonNullable<Object>> {
  return arr
    .map(foo)
    .reduce((acc: Array<NonNullable<Object>>, item) => acc.concat(item), [])
    .filter(Boolean);
}
