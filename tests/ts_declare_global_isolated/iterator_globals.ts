export {};

declare global {
  interface ScopedIterator<T> {
    next(): T;
  }

  interface ScopedIteratorObject<T> extends ScopedIterator<T> {}
}
