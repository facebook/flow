// Literal initializers
declare const s = 'foo';
declare const n = 10;
declare const b = true;
declare const bi = 100n;

// Multiple declarations
declare const x: string, y: number;
declare let a: number, bb: string;
declare var c: boolean, d: number;

// Both features combined
declare const p: string, q = 'bar', r: number;

// Type usage - verify types are correct
s as 'foo'; // OK - literal type
n as 10; // OK - literal type
b as true; // OK - literal type
bi as 100n; // OK - literal type
x as string; // OK
y as number; // OK

// Type errors to verify types work
s as 'bar'; // ERROR - 'foo' !== 'bar'
n as 20; // ERROR - 10 !== 20
bi as 200n; // ERROR - 100n !== 200n
s as string; // OK - singleton 'foo' is subtype of string

// Error: both annotation and initializer
declare const e: string = 'hello'; // ERROR

// Error: non-literal initializer
declare const f = 1 + 2; // ERROR

// Error: missing both annotation and initializer
declare const h; // ERROR

// Error: destructuring
declare const [g] = 'foo'; // ERROR
