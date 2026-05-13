// TS allows assigning {readonly value: string} into {value: string}
// (a known TS soundness gap). Flow currently emits EPropPolarityMismatch
// here; after the .ts gate lands this file should produce no errors.

type RO = {readonly value: string};
type RW = {value: string};

declare const ro: RO;
const rw: RW = ro; // OK in .ts (no polarity error)
