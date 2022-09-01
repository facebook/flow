// @flow

class A {
    x: Array<number> = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (z) {
            return this.y; // error, function has wrong this
        });
    }
}

class B {
    x: Array<number> = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (z) {
            return this.y; // ok, function gets passed correct this
        }, this);
    }
}

class C {
    x: Array<number> = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(z => {
            return this.y; // ok, arrow binds surrounding context this
        });
    }
}
