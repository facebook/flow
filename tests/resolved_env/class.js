//@flow

class Annotated {
    x: number;
    foo(x: number): number {
        (x: empty);
        const b = x;
        return b;
    }
}

const a = new Annotated();
(a: empty);
const b = a.x;
(b: empty);
const c = a.foo(b);
(c: empty);

class Unannotated {
    x = 42;
    foo(x: number) {
        (x: empty);
        const b = x;
        return b;
    }
}

const a1 = new Unannotated();
(a1: empty);
const b1 = a1.x;
(b1: empty);
const c1 = a1.foo(b1);
(c1: empty);
