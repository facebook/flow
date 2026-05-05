/* @flow */

class Bar<T extends {}> {
    good (x: number) : $ReadOnlyWeakSet<T> { // Fine, invariant in invariant position
        return new WeakSet();
    }
}

class Foo<+T extends {}> {
    bad (x: number) : $ReadOnlyWeakSet<T> { // Error: T in invariant position
        return new WeakSet();
      }
}

class Baz<-T extends {}> {
    bad (x: number) : $ReadOnlyWeakSet<T> {// Error: T in invariant position
        return new WeakSet();
    }
}

{
    class Bar<T extends {}> {
        good (x: number) : ReadonlyWeakSet<T> { // Fine, invariant in invariant position
            return new WeakSet();
        }
    }

    class Foo<+T extends {}> {
        bad (x: number) : ReadonlyWeakSet<T> { // Error: T in invariant position
            return new WeakSet();
        }
    }

    class Baz<-T extends {}> {
        bad (x: number) : ReadonlyWeakSet<T> {// Error: T in invariant position
            return new WeakSet();
        }
    }
}
