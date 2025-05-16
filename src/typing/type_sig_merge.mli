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
      type_exports: Type.named_symbol Lazy.t SMap.t;
      exports: (ALoc.t option * Type.t) Lazy.t option;
      type_stars: (ALoc.t * Module_refs.index) list;
      strict: bool;
      platform_availability_set: Platform_set.t option;
    }
  | ESExports of {
      type_exports: Type.named_symbol Lazy.t SMap.t;
      exports: Type.named_symbol Lazy.t SMap.t;
      type_stars: (ALoc.t * Module_refs.index) list;
      stars: (ALoc.t * Module_refs.index) list;
      strict: bool;
      platform_availability_set: Platform_set.t option;
    }

type file = {
  cx: Context.t;
  dependencies: (Flow_import_specifier.userland * Context.resolved_require Lazy.t) Module_refs.t;
  exports: unit -> (Type.moduletype, Type.t) result;
  local_defs: (ALoc.t * string * Type.t Lazy.t * Type.t Lazy.t) Lazy.t Local_defs.t;
  remote_refs: (ALoc.t * string * Type.t) Lazy.t Remote_refs.t;
  patterns: Type.t Lazy.t Patterns.t;
  pattern_defs: Type.t Lazy.t Pattern_defs.t;
}

type tparams_map = Type.t SMap.t

val def_reason : (ALoc.t, 'b) Type_sig.def -> Reason.t

val remote_ref_reason : ALoc.t Pack.remote_ref -> Reason.t

val merge_pattern : file -> ALoc.t Pack.pattern -> Type.t

val merge_remote_ref : file -> Reason.t -> ALoc.t Pack.remote_ref -> Type.t

val merge_export : file -> ALoc.t Pack.export -> Type.named_symbol

val merge_type_export : file -> Reason.t -> ALoc.t Pack.type_export -> Type.named_symbol

val merge_exports : file -> Reason.t -> exports -> Type.moduletype Lazy.t

val merge_def :
  const_decl:bool -> file -> Reason.t -> (ALoc.t, ALoc.t Pack.packed) Type_sig.def -> Type.t

val merge_resource_module_t : Context.t -> File_key.t -> string -> Reason.t * Type.moduletype Lazy.t

val merge : tparams_map -> file -> ALoc.t Pack.packed -> Type.t

val merge_cjs_export_t : file -> ALoc.t Pack.packed -> ALoc.t option * Type.t

val merge_builtins :
  Context.t ->
  File_key.t ->
  Loc.t Locs.t ->
  Locs.index Packed_type_sig.Builtins.t ->
  (ALoc.t * Type.t) lazy_t SMap.t
  * (ALoc.t * Type.t) lazy_t SMap.t
  * (Reason.t * Type.moduletype Lazy.t) Lazy.t SMap.t
