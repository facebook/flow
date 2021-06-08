// @flow

class A {
    x = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (z) {
            this.y; // error, function has wrong this
        });
    }
}

class B {
    x = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (z) {
            this.y; // ok, function gets passed correct this
        }, this);
    }
}

class C {
    x = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(z => {
            this.y; // ok, arrow binds surrounding context this
        });
    }
}
