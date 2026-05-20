// Barrel re-exports the interface from `barrel_base.js`. The polymorphic
// `this` shape must survive the re-export — see
// `Pack.ExportTypeFrom` / `Pack.ExportFrom` handling in
// `type_sig_merge.ml`'s `merge_type_export` / `merge_export`.
export type { IBarrel } from './barrel_base';
