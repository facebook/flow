//@flow

function foo(arr: ReadonlyArray<any>): Array<NonNullable<any>> {
  return arr
    .map(foo)
    .reduce((acc: Array<NonNullable<any>>, item) => acc.concat(item), [])
    .filter(Boolean);
}
