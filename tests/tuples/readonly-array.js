// $ReadOnlyArray<T> is the supertype for all tuples
function tupleLength(tup: $ReadOnlyArray<mixed>): number {
  // $ReadOnlyArray can use Array.prototype properties that don't mutate it
  return tup.length;
}
// Array literals with known types can flow to $ReadOnlyArray
tupleLength([1,2,3]);
tupleLength(["a", "b", "c"]);
// Arrays can flow to $ReadOnlyArray
tupleLength(([1, 2, 3]: Array<number>));
// Tuple types can flow to $ReadOnlyArray
tupleLength(([1,2,3]: [1,2,3]));
// $ReadOnlyArray can flow to $ReadOnlyArray
tupleLength(([1,2,3]: $ReadOnlyArray<number>));

const elemCheck = (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup; // error
const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup; // error
const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123); // error

// You can use the $ReadOnlyArray functions
function foo1(x: [1,2]): string { return x.length; } // error

// The ref that $ReadOnlyArray functions provide is a $ReadOnlyArray
function foo2(tup: [1,2], arr: Array<number>): void {
  tup.forEach((value, index, readOnlyRef) => {
    readOnlyRef.push(123); // error
    (readOnlyRef[0]: 1); // error
  });

  arr.forEach((value, index, writeableRef) => {
    writeableRef.push(123);
  });
}

// You can't use Array functions
function foo3(x: [1,2]): number { return x.unshift(); } // error
