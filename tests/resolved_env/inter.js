//@flow

interface A extends B {
    foo: C
}

interface B {
    bar: A;
}

declare class D {
    w(): number
}

declare class C extends D {
    m(): this;
}


declare var a: A;
(a.foo.w(): empty);

var c = new C();
(c.m().w(): empty);
