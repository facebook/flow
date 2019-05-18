// @flow

// Part from immutable.js

declare export class Seq<K, +V> {
  static isSeq: typeof isSeq;
  size: number | void;
}

declare export function isSeq(
  maybeSeq: mixed
): boolean /*::%checks(maybeSeq instanceof Seq)*/;

declare export class KeyedCollection<K, +V> {
  flatten(depth?: number): KeyedCollection<V, K>;
  flatten(shallow?: boolean): KeyedCollection<any, any>;
}

// If you remove `mixins` clause it works
declare export class KeyedSeq<K, +V> extends Seq<K, V> mixins KeyedCollection<K, V> {
  flatten(depth?: number): KeyedSeq<any, any>;
  flatten(shallow?: boolean): KeyedSeq<any, any>;
}
