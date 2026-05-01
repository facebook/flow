class A {
    x : string
}

type O = {
    x : string
}

interface I {
    x : string
}

type T = interface {
    x : string
}

declare var a : A;
declare var o : O;
declare var i : I;
declare var t : T;

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

C as { }; // error
C as interface { };
