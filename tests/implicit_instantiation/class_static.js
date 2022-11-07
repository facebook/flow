//@flow

declare class Map<K, V> {
  static <K, V>(_: void): Map<K, V>;
}
const x1: Map<mixed, mixed> = Map<mixed, mixed>();
const x2: Map<mixed, mixed> = Map();

declare class OneTarg<T> {
  static <K, V>(): OneTarg<K, V>;
}

const y = OneTarg<string>(); // error, incorrect amount of args
const z1 = OneTarg<string, number>(); // no error, correct amount of args
