// Labeled
type A = [foo: string, bar: number];

(["s", 1]: A); // OK
([true, 1]: A); // ERROR

// Mixed Labeled and non-labeled
type B = [string, bar: number];

(["s", 1]: B); // OK
(["s", true]: B); // ERROR
