type O = {a: 1, b: 2};

type T = keyof O; // ERROR

const keyof = 1; // OK
