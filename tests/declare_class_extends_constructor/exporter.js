// `Error`, `Map` and `Promise` are interfaces plus a `declare var` of their
// constructor interface, so the class extends the constructor value.

declare class ErrorSubclass extends Error {}
declare class MapSubclass<K, V> extends Map<K, V> {}
declare class PromiseSubclass<R> extends Promise<R> {}

module.exports = {ErrorSubclass, MapSubclass, PromiseSubclass};
