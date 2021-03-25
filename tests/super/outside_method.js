// @flow

class A {
    a() {}
    b = () => {}
    c = function () {}
    d = () => {}
}

class B extends A {
    a() { }
    a2() { super.a () }
    b = () => { super.b() }
    b2 = () => { }
    c = function () { super.c() }
    c2 = function () { }
    d = super.d
    d2 = () => {}


    e = () => {
        let c = class extends A {
            a() {super.a()}
        }
    }
}
