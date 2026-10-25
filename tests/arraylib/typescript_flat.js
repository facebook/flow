declare const nested: number[][];
const flat = nested.flat();
flat as number[];
flat as string[]; // ERROR
flat.push(1);
flat.push('wrong'); // ERROR

const depth0 = nested.flat(0);
depth0 as number[][];
depth0 as number[]; // ERROR
const depth1 = nested.flat(1);
depth1 as number[];
depth1 as number[][]; // ERROR

declare const deep: number[][][];
deep.flat(2) as number[];
deep.flat(2) as string[]; // ERROR

declare const readonlyNested: ReadonlyArray<ReadonlyArray<number>>;
readonlyNested.flat() as number[];
readonlyNested.flat(0) as Array<ReadonlyArray<number>>;
readonlyNested.flat(1) as number[];
readonlyNested.flat(1) as string[]; // ERROR

declare const helper0: FlatArray<number[][], 0>;
helper0 as number[];
declare const helper1: FlatArray<number[][], 1>;
helper1 as number;
declare const helper2: FlatArray<number[][][], 2>;
helper2 as number;

nested.flatMap(values => values) as number[];
readonlyNested.flatMap(values => values) as number[];
nested.flatMap(function (this: {factor: number, ...}, values) {
  return values.map(value => value * this.factor);
}, {factor: 2}) as number[];
