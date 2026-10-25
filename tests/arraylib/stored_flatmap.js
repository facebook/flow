declare const nested: number[][];
const flattened = nested.flatMap(values => values);
flattened as number[];
flattened as string[]; // ERROR
flattened.push(1);
flattened.push('wrong'); // ERROR

declare const readonlyNested: ReadonlyArray<ReadonlyArray<number>>;
const readonlyFlattened = readonlyNested.flatMap(values => values);
readonlyFlattened as number[];
readonlyFlattened as string[]; // ERROR

const filtered = [1, 2].flatMap(value => value > 1 ? [value] : []);
filtered as number[];
filtered as string[]; // ERROR

const mixed = [1, 2].flatMap(value => value > 1 ? [value] : value);
mixed as number[];
mixed as string[]; // ERROR
