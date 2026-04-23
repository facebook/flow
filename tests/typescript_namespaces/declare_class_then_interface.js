// `declare class A {}; interface A {}` is the canonical declaration-merging
// shape from TypeScript. The bindings coexist (the merge itself is future work).
declare class D { x: number; }
interface D { y?: number; }

declare var d: D;
