//@flow

declare class Map<K, V> {
  static <K, V>(_: void): Map<K, V>;
}
const x: Map<mixed, mixed> = Map<mixed, mixed>();

declare class OneTarg<T> {
  static <K, V>(): OneTarg<K, V>;
}

const y = OneTarg<string>(); // error, incorrect amount of args
const z = OneTarg<string, number>(); // no error, correct amount of args
