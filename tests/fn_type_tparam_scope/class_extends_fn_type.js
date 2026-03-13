// Test that method type params in interfaces don't leak and overwrite
// the interface's own type params when they share the same names.

interface Base<T> {
  get(): T;
  // $FlowFixMe[incompatible-variance]
  toArray(): Array<T>;
}

// This interface's reduce method has its own <T> type param that shadows
// the class's <T>. Without the fix, after processing reduce, the class's
// T in env.tparams_map gets overwritten, causing filter's T to resolve wrong.
interface Derived<T> extends Base<T> {
  reduce<T>(reducer: (acc: T, value: T) => T, init: T): T;
  filter(pred: (value: T) => boolean): Derived<T>;
  // $FlowFixMe[incompatible-variance]
  toArray(): Array<T>;
}
