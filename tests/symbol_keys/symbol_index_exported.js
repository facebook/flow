// An exported object type with a `symbol` index signature, and its key set. Both
// are computed by the annotation/signature pipeline, so the consumer exercises
// the cross-module path in addition to the within-file checker.
export type Idx = {[k: symbol]: number};
export type Key = keyof Idx;
