(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Pack = Type_sig_pack
open Type_sig_collections

type exports =
  | CJSExports of {
      type_exports: (ALoc.t option * Type.t) Lazy.t SMap.t;
      exports: Type.t Lazy.t option;
      type_stars: (ALoc.t * Module_refs.index) list;
      strict: bool;
    }
  | ESExports of {
      type_exports: (ALoc.t option * Type.t) Lazy.t SMap.t;
      exports: (ALoc.t option * Type.t) Lazy.t SMap.t;
      type_stars: (ALoc.t * Module_refs.index) list;
      stars: (ALoc.t * Module_refs.index) list;
      strict: bool;
    }

type file = {
  key: File_key.t;
  cx: Context.t;
  dependencies: (string * (ALoc.t -> Type.t)) Module_refs.t;
  exports: Type.t;
  local_defs: (ALoc.t * string * Type.t) Lazy.t Local_defs.t;
  remote_refs: (ALoc.t * string * Type.t) Lazy.t Remote_refs.t;
  patterns: Type.t Lazy.t Patterns.t;
  pattern_defs: Type.t Lazy.t Pattern_defs.t;
}

type tparams_map = Type.t SMap.t

val def_reason : (ALoc.t, 'b) Type_sig.def -> Reason.t

val remote_ref_reason : ALoc.t Pack.remote_ref -> Reason.t

val merge_pattern : file -> ALoc.t Pack.pattern -> Type.t

val merge_remote_ref : file -> Reason.t -> ALoc.t Pack.remote_ref -> Type.t

val merge_export : file -> ALoc.t Pack.export -> ALoc.t option * Type.t

val merge_type_export : file -> Reason.t -> ALoc.t Pack.type_export -> ALoc.t option * Type.t

val merge_exports : file -> Reason.t -> exports -> Type.t

val merge_def : file -> Reason.t -> (ALoc.t, ALoc.t Pack.packed) Type_sig.def -> Type.t

val merge_resource_module_t : Context.t -> string -> ALoc.t -> Type.t

val merge : tparams_map -> file -> ALoc.t Pack.packed -> Type.t
