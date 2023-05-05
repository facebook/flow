export type T = [foo: string]; // ERROR
export type S = [...T]; // ERROR
export type R = [number, foo: string, ...S]; // ERROR

declare const x: mixed;
(x: T); // OK: `T` is `any`
(x: S); // OK: `S` is `any`
(x: R); // OK: `R` is `any`
