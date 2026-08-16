// Value-level `unique symbol` computed keys on a runtime class declaration.

declare const key1: unique symbol;
declare const key2: unique symbol;
declare const key3: unique symbol;

class C {
  [key1]: number = 1;
  [key2](): string {
    return 's';
  }
  regular: boolean = true;
  static [key1]: string = 'static';
}

declare const c: C;

// Each distinct symbol resolves to its own member.
c[key1] as number; // OK
c[key2]() as string; // OK
c.regular as boolean; // OK
C[key1] as string; // OK: static symbol member is in its own namespace

// Accessing with the wrong expected type errors.
c[key1] as string; // ERROR: number is incompatible with string
c[key2]() as number; // ERROR: string is incompatible with number

// A distinct `unique symbol` is a distinct key, so it is missing.
c[key3]; // ERROR: property is missing
