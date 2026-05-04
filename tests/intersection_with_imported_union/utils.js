declare class IterableClass<+K, +V> {}

export type LogMap<K, V> =
  | {+[key: K]: V, ...}
  | IterableClass<K, V>
  | ReadonlyMap<K, V>;
