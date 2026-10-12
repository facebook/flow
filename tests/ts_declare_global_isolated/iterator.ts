export {};

declare abstract class ScopedIterator<T> {
  abstract next(): T;
}

interface ScopedIterator<T> extends globalThis.ScopedIteratorObject<T> {}

type ScopedIteratorConstructor = typeof ScopedIterator;

declare global {
  interface ScopedIteratorObject<T> {
    map<U>(callback: (value: T) => U): ScopedIteratorObject<U>;
  }

  interface ScopedIteratorFactory extends ScopedIteratorConstructor {
    from<T>(value: ScopedIterator<T>): ScopedIteratorObject<T>;
  }

  var ScopedIterator: ScopedIteratorFactory;
}
