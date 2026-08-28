class A {
    prop : number
    x() : number { return this.prop; }
}

let _1 = {prop : "", method : (new A()).x }.method(); // error: object literal is not an A receiver

class B {
    prop : number
    x() : number { return 3; }
}

let _2 = {prop : "", method : (new B()).x }.method(); // error: object literal is not a B receiver

class C {
    prop : number
    x = function () : number { return 3; }
}

let c = new C();
c.x = (new A).x; // error: A receiver is incompatible with C.x's receiver
(c.x()); // error: C is incompatible with the preserved A receiver

class D {
    x : number
    m(): number { return this.x }
    n() {
        let m = this.m; // ok: receiver type is preserved
        let z = m(); // error: the preserved receiver is missing
    }
}

class E {
    static foo() {}

    bar() {
        this.constructor.foo(); // no error
    }
}

class F {
  foo() {
  }
}

class G {
  foo() {
  }
}
