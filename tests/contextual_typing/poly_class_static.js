declare export class Map<K, out V> {
    static <K, V>(_: void): Map<K, V>;
    static <K extends string, V>(object: {readonly [k: K]: V, ...}): Map<K, V>;
}

const _ = Map<_, ?Map<string, number>>({
  foo: Map(), // okay
});
