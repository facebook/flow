type T = never; // OK

declare const x: empty;
x satisfies T; // OK

declare const y: unknown;
y satisfies T; // ERROR: expected - unknown is not empty

