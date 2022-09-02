// @flow

class A {
    x: Array<number> = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (this: void, z) {
            return this.y; // error, function has wrong this
        });
    }
}

class B {
    x: Array<number> = [1, 2, 3];
    y = 4;
    foo() {
        this.x = this.x.map(function (this: B, z) {
            return this.y; // ok, function has correct this annotation
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
