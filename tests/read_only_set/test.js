/* @flow */

class Bar<T> {
    good (x: number) : ReadonlySet<T> {
        return new Set(); // Fine, Invariant in invariant position
    }
}

class Foo<+T> {
    bad (x: number) : ReadonlySet<T> { // Error: T in invariant position
        return new Set();
    }
}

class Baz<-T> {
    bad (x: number) : ReadonlySet<T> {// Error: T in invariant position
        return new Set();
    }
}
