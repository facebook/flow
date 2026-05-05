declare interface Array<T> {
  length: number;
  push(...items: Array<T>): number;
}

declare class Map<K, +V> {
  constructor(iterable?: ?ReadonlyArray<[K, V]>): void;
}
