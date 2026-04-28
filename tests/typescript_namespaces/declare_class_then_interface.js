// `declare class A {}; interface A {}` is the canonical declaration-merging
// shape from TypeScript. Members from the interface fold into the class type.
declare class D { x: number; }
interface D { y?: number; }

declare const d: D;
d.x as number;
d.y as number | void;
d.x as string; // ERROR
d.y as string; // ERROR
