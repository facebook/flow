// Intentionally bad shim for ts names to demonstrate that it will not be read if `experimental.ts_syntax` is off
type ReadonlyMap<K, +V> = string;
