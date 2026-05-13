// Exercises both ETupleElementPolarityMismatch emit sites
// (flow_js.ml and subtyping_kit.ml). In .ts mode only the safe
// direction (mutable -> readonly) is suppressed; the unsafe
// direction (readonly -> mutable) must still error, matching tsc
// (TS4104).

declare const ro: readonly [string];
declare const rw: [string];

// mutable -> readonly: safe direction, accepted in .ts.
const a: readonly [string] = rw;

// readonly -> mutable: unsafe direction, MUST still error.
const must_error: [string] = ro; // ERROR: readonly cannot flow into mutable

// Real element-type mismatch must still error -- the polarity gate
// must not paper over genuine incompatibilities.
declare const roNum: readonly [number];
const c: readonly [string] = roNum; // ERROR: number vs string
