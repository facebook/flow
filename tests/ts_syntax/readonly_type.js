type A = readonly [string, number]; // ERROR
type B = readonly string[]; // ERROR
type C = readonly number; // ERROR

// The `readonly` array and tuple operators are allowed in library definitions
libReadonlyArray() as ReadonlyArray<string>; // OK
libReadonlyArray().push('a'); // ERROR: read-only
libReadonlyTuple() as Readonly<[string, number]>; // OK
libReadonlyTuple()[0] = 'a'; // ERROR: read-only
