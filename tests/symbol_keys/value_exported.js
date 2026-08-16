import {s, t} from './keys';

// An exported object type keyed by imported `unique symbol` values. Its
// signature is computed by the annotation/signature pipeline, so this exercises
// the cross-module path in addition to the within-file checker.
export type O = {[s]: number, [t]: string};
