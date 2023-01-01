type S = [number, string];

type A = [...S]; // ERROR: syntax not supported
type B = [...foo: S]; // ERROR: syntax not supported
