class A {

    b: string;

    c(d: string = this.b) { // ok - can use `this` in function default parameter values

    }

    e(): string {
        return this.b;
    }

    f(g: string = this.e()) { // ok - can use `this` in function default parameter values

    }

    h(i: number = this.b) { // error

    }

}
