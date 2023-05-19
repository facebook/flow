function basic(
  fn: (x: mixed) => x is number,
  y: mixed,
) {
  if (fn(y)) {
    (y: number); // okay
    (y: string); // error number ~> string
  }
}

function nonMaybe(
  fn: <T>(x: ?T) => x is T,
  x1: string,
  x2: string | null,
  x3: string | void,
  x4: ?string,
) {
  if (fn(x1)) {
    (x1: string);
    (x1: number); // error string ~> number
  }
  if (fn(x2)) {
    (x2: string);
    (x2: number); // error string ~> number
  }
  if (fn(x3)) {
    (x3: string);
    (x3: number); // error string ~> number
  }
  if (fn(x4)) {
    (x4: string);
    (x4: number); // error string ~> number
  }
}

function array_filter() {

  declare class ReadOnlyArray_<+T> {
    filter(callbackfn: typeof Boolean): Array<$NonMaybeType<T>>;
    filter<This, S: T>(predicate: (this: This, value: T, index: number, array: $ReadOnlyArray<T>) => value is S, thisArg?: This): Array<S>;
    filter<This>(callbackfn: (this : This, value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg : This): Array<T>;
  }

  declare var arr: ReadOnlyArray_<mixed>;

  const arr1: Array<number> = arr.filter((x: mixed): x is number => { return typeof x === "number"; });
  const arr2: Array<string> = arr.filter((x: mixed): x is number => { return typeof x === "number"; }); // error
  const arr3: Array<string> = arr.filter((x: mixed) => { return typeof x === "number"; }); // error no refinement
}
