type O = {a: 1, b: 2};

type T1 = $Values<O>; // ERROR

const values = 1; // OK

type T2 = O[keyof O]; // OK

type T3 = Values<O>; // OK

export type exportedValuesType = Values<O>; // OK
