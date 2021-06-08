// @flow

class A {
    prop : number
    x() : number { return this.prop; }
}

let _1 = {prop : "", method : (new A()).x }.method(); // object incompatible with A

class B {
    prop : number
    x() : number { return 3; }
}

let _2 = {prop : "", method : (new B()).x }.method(); // object incompatible with B

class C {
    prop : number
    x = function () : number { return 3; }
}

let c = new C();
c.x = (new A).x;
(c.x()); // C incompatible with A

class D {
    x : number
    m() { return this.x }
    n() {
        let m = this.m;
        let z = m(); // global object incompatible with D
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
