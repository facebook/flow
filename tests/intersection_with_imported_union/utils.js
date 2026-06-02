declare class IterableClass<out K, out V> {}

export type LogMap<K, V> =
  | {readonly [key: K]: V, ...}
  | IterableClass<K, V>
  | ReadonlyMap<K, V>;
