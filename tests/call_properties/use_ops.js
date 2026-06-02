type A = { p: {...}, ... }
type B = { readonly p: () => void, ... }
declare const a: A;
a as B; // error HERE and preserve use ops
