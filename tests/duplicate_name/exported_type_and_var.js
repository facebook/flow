// TS-style cross-namespace coexistence: interface (type namespace) and
// let/type alias (value/type namespace respectively) coexist when they
// don't overlap.
export interface A {}
export let A: string; // ok: cross-namespace

export let B: string;
export type B = number; // ok: cross-namespace
