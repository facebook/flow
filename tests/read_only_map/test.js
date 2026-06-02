/* @flow */

class Bar<K, V> {
    good (x: number) : ReadonlyMap<K, V> {
        return new Map(); // Fine, Invariant can flow to covariant
    }
}

class FooK<out K, V> {
    bad (x: number) : ReadonlyMap<K, V> { // Error: K in invariant position
        return new Map();
    }
}

class FooV<K, out V> {
    bad (x: number) : ReadonlyMap<K, V> { // Fine, V in covariant position
        return new Map();
    }
}

class BazK<in K, V> {
    bad (x: number) : ReadonlyMap<K, V> {// Error: K in invariant position
        return new Map();
    }
}

class BazV<K, in V> {
    bad (x: number) : ReadonlyMap<K, V> {// Error: V in invariant position
        return new Map();
    }
}
