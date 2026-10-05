// Printed construct signatures must not show the receiver as a `this` parameter.

declare const plain: interface { new(): number; foo: number };
plain as number; // ERROR: `interface {new(): number; foo: number}`

declare const generic: interface { new<T>(x: T): T };
generic as number; // ERROR: `interface {new<T>(x: T): T}`

declare const overloaded: interface { new(): number; new(x: string): string };
overloaded as number; // ERROR: `interface {new(): number; new(x: string): string}`
