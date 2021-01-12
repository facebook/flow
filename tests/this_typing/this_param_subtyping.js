// @flow

class A {
    x : number
    y : number
    foo (this : {x : number}) {}
}
class B extends A {
    foo (this : {y : number}) {}
}


class C<T> {
    foo (this : C<T>) {}
}

class D<T> extends C<T> {
    foo (this : D<T>) {}
}

class E<T> extends D<T> {
    foo (this : C<T>) {}
}
