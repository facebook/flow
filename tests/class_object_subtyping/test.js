// @flow

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

(a : O); // error
(a : I);
(a : T);

(o : A); // error
(o : I);
(o : T);

(i : A); // error
(i : O); // error
(i : T);

(t : A); // error
(t : O); // error
(t : I);


class C { }

(C: { }); // error
(C : interface { });
