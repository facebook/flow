class A {
    prop : number
    x() : number { return this.prop; }
}

let _1 = {prop : "", method : (new A()).x }.method(); // method-unbinding, this type becomes any, so no more this typing errors

class B {
    prop : number
    x() : number { return 3; }
}

let _2 = {prop : "", method : (new B()).x }.method(); // method-unbinding, this type becomes any, so no more this typing errors

class C {
    prop : number
    x = function () : number { return 3; }
}

let c = new C();
c.x = (new A).x; // method-unbinding
(c.x()); // method-unbinding above, this type becomes any, so no more this typing errors

class D {
    x : number
    m(): number { return this.x }
    n() {
        let m = this.m; // method-unbinding
        let z = m(); // method-unbinding above, this type becomes any, so no more this typing errors
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
