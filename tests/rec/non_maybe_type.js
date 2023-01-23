//@flow

function foo(arr: $ReadOnlyArray<Object>): Array<$NonMaybeType<Object>> {
    return arr.map(foo)
        .reduce((acc: Array<$NonMaybeType<Object>>, item) => acc.concat(item), [])
        .filter(Boolean);
  }
