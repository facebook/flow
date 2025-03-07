declare class MyMap<K, V> {
    @@iterator(): Iterator<[K, V]>;
    constructor(iterable: ?Iterable<[K, V]>): void;
    set(key: K, value: V): Map<K, V>;
}
