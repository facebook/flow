// @flow

class A {
    constructor(x: any) {}
}

class B extends A {
    constructor() {
        super(
            super(0)
        );
    }
}
