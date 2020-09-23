// @flow

declare export class Iterable<K,V> {
  has(key: K): boolean;
}

declare export class Seq<K,V> extends Iterable<K,V> {}

declare export class IndexedIterable<T> extends Iterable<number,T> {}

declare export class IndexedSeq<T> extends Seq<number,T> mixins IndexedIterable<T> {}
