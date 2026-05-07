//@flow

class Foo {
    toString : number
}

declare const x : Foo & {};
x.
//^
