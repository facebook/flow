// @flow

// Declaration merging must carry the forward-reference bound order across the
// merged declarations (type_sig_parse merge_tparams preserves bound_order).

// Two interfaces with forward-referencing bounds.
interface Merged<A extends B, B> {
  a: A;
}
interface Merged<A extends B, B> {
  b: B;
}

declare var okMerged: Merged<string, string>; // OK: forward bound holds
okMerged.a as string; // OK: member from the first declaration
okMerged.b as string; // OK: member from the second declaration

declare var badMerged: Merged<number, string>; // ERROR: `number` does not satisfy the merged forward bound `A extends B`

// declare class first, then a same-name interface folded into the class; the
// forward bound survives the class/interface merge.
declare class Combined<A extends B, B> {
  a: A;
}
interface Combined<A extends B, B> {
  b: B;
}
declare var combined: Combined<string, string>; // OK: forward bound survives the class/interface merge
combined.a as string; // OK: member from the declared class
combined.b as string; // OK: member from the interface
