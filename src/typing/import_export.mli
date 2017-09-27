(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_module_t : Context.t -> Reason.t -> Type.t
val mk_commonjs_module_t :
  Context.t -> Reason.t -> Reason.t -> Type.t -> Type.t
val mk_resource_module_t : Context.t -> Loc.t -> string -> Type.t
val require : Context.t -> SMap.key -> Loc.t -> Type.t
val import :
  ?reason:Reason.t -> Context.t -> SMap.key -> Loc.t -> Type.t
val import_ns : Context.t -> Reason.t -> SMap.key -> Loc.t -> Type.t
val add_require_tvar : Context.t -> string -> Loc.t -> unit
val module_t_of_cx : Context.t -> Type.t
val set_module_t : Context.t -> Reason.t -> (Type.t -> unit) -> unit
val set_module_kind : Context.t -> Loc.t -> Context.module_kind -> unit
val nameify_default_export_decl :
  Loc.t * Loc.t Ast.Statement.t' ->
  Loc.t * Loc.t Ast.Statement.t'
val warn_or_ignore_export_star_as : Context.t -> (Loc.t * 'a) option -> unit
val get_module_exports : Context.t -> Loc.t -> Type.t
val set_module_exports : Context.t -> Loc.t -> Type.t -> unit
