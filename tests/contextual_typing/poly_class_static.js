// @flow

declare export class Map<K, +V> {
    static <K, V>(_: void): Map<K, V>;
    static <K: string, V>(object: {+[k: K]: V, ...}): Map<K, V>;
}

const _ = Map<_, ?Map<string, number>>({
  foo: Map(), // okay
});
