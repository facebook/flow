// @flow

const arr1 = [1];
const result1_0: Array<number> = arr1.flat(); // OK
const result1_1: Array<string> = arr1.flat(); // Error
const result1_2: $ReadOnlyArray<number> = arr1.flat(); // OK
const result1_3: $ReadOnlyArray<string> = arr1.flat(); // Error

const arr2: Array<Array<number>> = [[1]];
const result2_0: Array<number> = arr2.flat(); // OK
const result2_1: Array<string> = arr2.flat(); // Error
const result2_2: Array<Array<number>> = arr2.flat(0); // OK
const result2_3: Array<number> = arr2.flat(0); // Error
const result2_4: Array<number> = arr2.flat(1); // OK
const result2_5: Array<string> = arr2.flat(1); // Error

const arr3: Array<Array<Array<number>>> = [[[1]]];
const result3_0: Array<Array<number>> = arr3.flat(); // OK
const result3_1: Array<number> = arr3.flat(); // Error

// We don't support depth arguments of 2 or greater, other than saying it's `Array<mixed>`
const result3_2: Array<number> = arr3.flat(2); // Error - don't support this
const result3_3: Array<mixed> = arr3.flat(2); // OK

// We don't support depth arguments that are not literal numbers
const x: number = 1;
const result2_6: Array<number> = arr2.flat(x); // Error - don't support this
const result2_7: Array<mixed> = arr2.flat(x); // OK

const arr4 = [1, [2]];
// We don't support array elements which are unions or elements and arrays
const result4_0: Array<number> = arr4.flat(); // Error - don't support this
const result4_1: Array<Array<number>> = arr4.flat(); // Error - don't support this
// But we can say it's a `$ReadOnlyArray<mixed>`
const result4_3: $ReadOnlyArray<mixed> = arr4.flat(); // OK
