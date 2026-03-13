// Test that type parameters from class methods don't leak into
// the enclosing class scope, overwriting the class's own type parameters.
// Bug: when a method has type params with the same name as the class's
// type params, they get inserted into env.tparams_map, overwriting
// the class's type params. Without save/restore, subsequent method
// processing sees the method's type vars (now out of scope -> unknown).

declare class Base<K, +V> {
  get(key: K): V | void;
  // $FlowFixMe[incompatible-variance]
  toJSON(): {[key: string]: V, ...};
  // $FlowFixMe[incompatible-variance]
  toArray(): Array<V>;
}

declare class Child<K, +V> extends Base<K, V> {
  // This static method has type params <K, V> that shadow the class's <K, +V>.
  // Without the fix, the class's K,V in env.tparams_map get overwritten,
  // causing subsequent properties to resolve V as unknown.
  static <K, V>(values?: Iterable<[K, V]>): Child<K, V>;

  // $FlowFixMe[incompatible-variance]
  toJSON(): {[key: string]: V, ...};
  // $FlowFixMe[incompatible-variance]
  toArray(): Array<V>;
}
