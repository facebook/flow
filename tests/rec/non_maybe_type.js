//@flow

function foo(arr: $ReadOnlyArray<Object>): Array<$NonMaybeType<Object>> {
    return arr.map(foo)
        .reduce((acc, item) => acc.concat(item), [])
        .filter(Boolean);
  }
