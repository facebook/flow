type T = never; // OK

declare const x: empty;
x as T; // OK

declare const y: unknown;
y as T; // ERROR: expected - unknown is not empty

