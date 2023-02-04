//@flow

function f() {
    var x = this;
}

//@flow
class C {
    foo = function(): void { //require this annot
        (this: empty);
    }
    foo_a: () => void = function(): void { // require this annot
        (this: empty);
    }
    bar = function(): void { }
}
