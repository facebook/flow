type A = T<number>;
type B = T<string>;

declare const a: A;
a as B; // Error

type T<Phantom> = any;

type C = T<number>;
type D = T<string>;

declare const c: C;
c as D; // Error
