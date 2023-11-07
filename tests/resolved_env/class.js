//@flow

class Annotated {
  x: number;
  foo(x: number): number {
    x as empty;
    const b = x;
    return b;
  }
}

const a = new Annotated();
a as empty;
const b = a.x;
b as empty;
const c = a.foo(b);
c as empty;

class Unannotated {
  x = 42;
  foo(x: number) {
    x as empty;
    const b = x;
    return b;
  }
}

const a1 = new Unannotated();
a1 as empty; // error
const b1 = a1.x;
b1 as empty; // error
const c1 = a1.foo(b1);
c1 as empty; // error in old inference, no error in LTI
