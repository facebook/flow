// ReadonlyArray<T> is the supertype for all tuples
function tupleLength(tup: ReadonlyArray<unknown>): number {
  // ReadonlyArray can use Array.prototype properties that don't mutate it
  return tup.length;
}
// Array literals with known types can flow to ReadonlyArray
tupleLength([1,2,3]);
tupleLength(["a", "b", "c"]);
// Arrays can flow to ReadonlyArray
tupleLength([1, 2, 3] as Array<number>);
// Tuple types can flow to ReadonlyArray
tupleLength([1,2,3] as [1,2,3]);
// ReadonlyArray can flow to ReadonlyArray
tupleLength([1,2,3] as ReadonlyArray<number>);

const elemCheck = (tup: ReadonlyArray<number>): ReadonlyArray<string> => tup; // error
const tupleToArray = (tup: ReadonlyArray<number>): Array<number> => tup; // error
const arrayMethods = (tup: ReadonlyArray<number>): void => tup.push(123); // error

// You can use the ReadonlyArray functions
function foo1(x: [1,2]): string { return x.length; } // error

// The ref that ReadonlyArray functions provide is a ReadonlyArray
function foo2(tup: [1,2], arr: Array<number>): void {
  tup.forEach((value, index, readOnlyRef) => {
    readOnlyRef.push(123); // error
    readOnlyRef[0] as 1; // error
  });

  arr.forEach((value, index, writeableRef) => {
    writeableRef.push(123);
  });
}

// You can't use Array functions
function foo3(x: [1,2]): number { return x.unshift(); } // error
