// Two-step relay: a local `import type` followed by a local `export type`.
// This codifies as `Pack.ExportTypeRef (Pack.RemoteRef ...)` in the type-sig
// IR. Compare with `barrel.js`, which uses `export type { X } from './base'`
// (single-step `Pack.ExportTypeFrom`).
import type { IBarrel } from './barrel_base';
export type { IBarrel };
