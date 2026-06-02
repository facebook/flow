type immutable$JSIterable<out T> = $Iterable<T, void, void>;

declare export class List<out T> {
  static <T>(iter: immutable$JSIterable<T>): List<T>;
  static <T>(iter: Iterator<T>): List<T>;
}

declare export class Map<K, out V> {
  static <K, V>(iter: immutable$JSIterable<[K, V]>): Map<K, V>;
  static <K extends string, V>(object: { readonly [k: K]: V, ... }): Map<K, V>;
}

declare const unused_var: unbound_type; // sanity check: should surface this error

List([Map({name: "abc"})]); // no error
