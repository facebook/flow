// `ThisType<T>` shapes that appear in real .d.ts files we want to import.
// In .ts/.d.ts files, `ThisType<T>` is a structurally-empty interface
// marker; the type argument is parsed but not used (Flow does not implement
// TS's contextual-`this` rewiring -- declaration-only positions don't need
// it).

// lib.es5.d.ts: `ThisType<any>` is essentially a no-op marker on the
// descriptor parameter to Object.defineProperty / defineProperties.
export interface PropertyDescriptorMap {
  [key: string]: { value?: unknown; writable?: boolean };
}
export declare function defineProperties<T>(
  o: T,
  properties: PropertyDescriptorMap & ThisType<any>,
): T;

// Vue-style: ThisType<D & M> on the methods bag.
export type Options<D, M> = {
  data: D;
  methods: M & ThisType<D & M>;
};
export declare function makeObject<D, M>(opts: Options<D, M>): D & M;

// Vue props: ThisType<void> -- says "do not rewire `this` here".
export declare function withVoidThis<P>(props: P & ThisType<void>): P;

// TypeORM Repository.extend pattern: ThisType<this & C>.
export declare class Repo {
  extend<C>(customs: C & ThisType<this & C>): this & C;
}

// Signature-path discriminator: a non-generic parameter `M & ThisType<T>` where
// `M` has required props. Only meaningful cross-file if the signature path
// reduces `ThisType<T>` to an empty interface (so the param is `M`); if it
// degraded to `any`, callers passing the wrong shape would be wrongly accepted.
export declare function needsBox(
  arg: { tag: "box"; size: number } & ThisType<unknown>,
): void;
