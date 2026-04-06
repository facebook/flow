export interface A {}
export let A: string; // ERROR: we haven't yet implemented type-checking support

export let B: string;
export type B = number; // ERROR: we haven't yet implemented type-checking support
