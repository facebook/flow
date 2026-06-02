// Library module — exports both `abstract new () => Base` and the
// non-abstract `new () => Base`. The abstract bit must survive being
// observed across this module boundary via type_sig.

export class Base {}

export type AbstractCtor = abstract new () => Base;
export type ConcreteCtor = new () => Base;
