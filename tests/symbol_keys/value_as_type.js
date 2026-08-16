// A `unique symbol` value used in a type position resolves to its own `unique
// symbol` type. This lets a value-level symbol binding key an object type member
// directly (`{[s]: T}`), with no `typeof`, matching the value-level object
// literal `{[s]: v}`. Distinct symbols stay distinct.

declare const s: unique symbol;
declare const t: unique symbol;
declare const other: unique symbol;

type T = {[s]: number, [t]: string};
declare const o: T;

o[s] as number; // OK
o[t] as string; // OK
o[s] as string; // ERROR: number is not string
o[other]; // ERROR: a distinct `unique symbol` is a distinct key, so it is missing
