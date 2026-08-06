// @flow
// A named element-access key on an array reads a named property off the array's
// object shape (`arr['map']`, `arr[Symbol.iterator]`), mirroring object and
// instance access. A numeric key, or a string key holding a safe integer, is an
// array index, and a general `string`/`number` key is not a valid array index.

declare const arr: Array<number>;
declare const i: number;

arr[i] as number; // numeric index
arr[0] as number; // numeric literal index
arr['length'] as number; // named field via bracket
arr['map']((x: number) => x + 1) as Array<number>; // named method call via bracket
const it = arr[Symbol.iterator](); // well-known symbol method call
it.next();

arr['length'] as string; // ERROR: number is not string
declare const k: string;
arr[k]; // ERROR: general string is not an array index

// A named key resolves for write and call actions too, not just reads.
arr['length'] = 3; // OK: length is writable
arr['length'] = 'x'; // ERROR: string into number
arr['map'] = 3; // ERROR: method is not writable

// A numeric-like string key indexes the array and reads the element type.
arr['0'] as number; // numeric-like string index
arr['0'] as string; // ERROR: number is not string
arr['0'] = 5; // OK: element write via numeric-like string

declare const ro: ReadonlyArray<number>;
ro['length'] as number; // OK on a read-only array
ro[Symbol.iterator](); // OK on a read-only array
ro['0'] as number; // OK: numeric-like string index on a read-only array
ro['0'] = 5; // ERROR: cannot write to a read-only array

// On a tuple, a numeric-like string key resolves to the specific element, with
// bounds and writability checks, just like the equivalent numeric index.
declare const tup: [number, string];
tup['0'] as number; // element 0 via numeric-like string
tup['1'] as string; // element 1 via numeric-like string
tup['0'] as string; // ERROR: number is not string
tup['2']; // ERROR: out of bounds
tup['0'] = 4; // OK: writable element
tup['0'] = 'x'; // ERROR: string into number element
tup['length'] as 2; // named field: tuple length
tup['length'] as 3; // ERROR: the length is 2, not 3

// A string key is an index only when it holds a safe integer. Every other string
// is a named key, so it reads a member of the array's object shape or fails to
// find one.
arr['-1'] as number; // safe integer, so an index, just like `arr[-1]`
arr['9007199254740993']; // ERROR: outside the safe-integer range, so a named key
arr['1.5']; // ERROR: not an integer, so a named key
arr['01']; // ERROR: not a canonical integer, so a named key
arr['nope']; // ERROR: no such named member
tup['9007199254740993']; // ERROR: outside the safe-integer range, so a named key
