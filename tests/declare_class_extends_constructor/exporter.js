// `Error`, `Map` and `Promise` are interfaces plus a `declare var` of their
// constructor interface, so the class extends the constructor value.

declare class ErrorSubclass extends Error {}
declare class MapSubclass<K, V> extends Map<K, V> {}
declare class PromiseSubclass<R> extends Promise<R> {}

// A constructor interface with a `prototype` but no construct signature, like
// TypeScript's `SymbolConstructor`. The class extends the `prototype` instance.
interface PrototypeOnlyInstance {
  instanceProp: number;
}
interface PrototypeOnlyConstructor {
  readonly prototype: PrototypeOnlyInstance;
  (): PrototypeOnlyInstance;
  staticProp: string;
}
declare var PrototypeOnly: PrototypeOnlyConstructor;
declare class PrototypeOnlySubclass extends PrototypeOnly {}

module.exports = {
  ErrorSubclass,
  MapSubclass,
  PromiseSubclass,
  PrototypeOnlySubclass,
};
