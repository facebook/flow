class A {
    x : string
}

type O = {
    x : string
, ...}

interface I {
    x : string
}

type T = interface {
    x : string
}

declare const a : A;
declare const o : O;
declare const i : I;
declare const t : T;

a as O; // error
a as I;
a as T;

o as A; // error
o as I;
o as T;

i as A; // error
i as O; // error
i as T;

t as A; // error
t as O; // error
t as I;


class C { }

C as { ...}; // error
C as interface { };
