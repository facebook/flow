declare interface Array<T> {
  length: number;
  push(...items: Array<T>): number;
}

declare class Map<K, out V> {
  constructor(iterable?: ?ReadonlyArray<[K, V]>): void;
}
