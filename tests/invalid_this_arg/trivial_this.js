// A method that accepts any receiver cannot be broken by being handed a
// different one, so the check does not apply when `mixed <: this`.

declare const target: {x: number};

declare function unknownThis(this: unknown, y: number): void;
unknownThis.call(target, 1); // ok
unknownThis.apply(target, [1]); // ok
unknownThis.bind(target); // ok
unknownThis.call(null, 1); // ok

declare function anyThis(this: any, y: number): void;
anyThis.call(target, 1); // ok
anyThis.bind(target); // ok

// No `this` annotation and no `this` in the body: inferred as `unknown`.
function inferred(y: number): void {}
inferred.call(target, 1); // ok

// A union that includes `unknown` is still `unknown`.
declare function unionThis(this: unknown | {x: number}, y: number): void;
unionThis.call(target, 1); // ok

// Contrast: a receiver-sensitive `this` still errors.
declare function objectThis(this: {x: number, ...}, y: number): void;
objectThis.call(target, 1); // error

class A {
  x: number = 0;
}
declare function classThis(this: A, y: number): void;
classThis.call(new A(), 1); // error
