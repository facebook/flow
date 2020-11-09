// @flow

type T = (this : number) => void;

function foo (this : number) : void {}

declare function bar(this : number) : void;

type O = {
    foo (this : number) : void
}

let o = {
    foo (this : number) : void {}
}

declare class A {
    foo(this : number) : void
}

class B {
    foo(this : number) : void {}
}

interface C {
    foo(this : number) : void
}
