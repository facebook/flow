// @flow
type immutable$JSIterable<+T> = $Iterable<T, void, void>;

declare export class List<+T> {
  static <T>(iter: immutable$JSIterable<T>): List<T>;
  static <T>(iter: Iterator<T>): List<T>;
}

declare export class Map<K, +V> {
  static <K, V>(iter: immutable$JSIterable<[K, V]>): Map<K, V>;
  static <K: string, V>(object: { +[k: K]: V, ... }): Map<K, V>;
}

declare var unused_var: unbound_type; // sanity check: should surface this error

List([Map({name: "abc"})]); // expected error
