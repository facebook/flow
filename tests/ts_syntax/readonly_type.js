type A = readonly [string, number]; // ERROR
type B = readonly string[]; // ERROR
type C = readonly number; // ERROR
