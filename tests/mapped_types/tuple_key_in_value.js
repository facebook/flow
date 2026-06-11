// Mapped type over a tuple that captures the key `K` in the value position.

type Columns = [string, number, boolean];
type Indexed = {[K in keyof Columns]: [K, Columns[K]]};

declare const r: Indexed;

// The key element of each position is that position's numeric index literal.
r[0][0] as 0; // OK
r[1][0] as 1; // OK
r[2][0] as 2; // OK

// The value element of each position is the source tuple's value type.
r[0][1] as string; // OK
r[1][1] as number; // OK
r[2][1] as boolean; // OK

// The key element is the number literal, not the value type: reading it where a
// string is expected reports the key's true type, not a string-vs-string error.
r[0][0] as string; // ERROR: number literal `0` ~> string
r[1][1] as string; // ERROR: number ~> string (sanity check on the value side)
