// Value-level `unique symbol` computed keys on an interface, resolved within the
// file that declares it. A `unique symbol` value used as a member key resolves
// to its own distinct member, the same way object literals, object types, and
// classes already do, so distinct symbols coexist and a distinct one is missing.

declare const key1: unique symbol;
declare const key2: unique symbol;
declare const key3: unique symbol;

interface I {
  [key1]: number;
  [key2](): string;
  regular: boolean;
}

declare const i: I;

// Each distinct symbol resolves to its own member.
i[key1] as number; // OK
i[key2]() as string; // OK
i.regular as boolean; // OK

// Accessing with the wrong expected type errors.
i[key1] as string; // ERROR: number is incompatible with string
i[key2]() as number; // ERROR: string is incompatible with number

// A distinct `unique symbol` is a distinct key, so it is missing.
i[key3]; // ERROR: property is missing
