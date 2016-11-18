(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val mk_module_t : Context.t -> Reason.t -> Type.t
val mk_commonjs_module_t :
  Context.t -> Reason.t -> Reason.t -> Type.t -> Type.t
val mk_resource_module_t : Context.t -> Loc.t -> string -> Type.t
val require : Context.t -> ?internal:bool -> SMap.key -> Loc.t -> Type.t
val import :
  ?reason:Reason.t -> Context.t -> SMap.key -> Loc.t -> Type.t
val import_ns : Context.t -> Reason.t -> SMap.key -> Loc.t -> Type.t
val module_t_of_cx : Context.t -> Type.t
val module_t_of_name : Context.t -> SMap.key -> Reason.t -> Type.t
val set_module_t : Context.t -> Reason.t -> (Type.t -> unit) -> unit
val set_module_kind :
  Context.t -> Reason.t -> Context.module_kind -> unit
val nameify_default_export_decl :
  Loc.t * Spider_monkey_ast.Statement.t' ->
  Loc.t * Spider_monkey_ast.Statement.t'
val warn_or_ignore_export_star_as : Context.t -> (Loc.t * 'a) option -> unit
val get_module_exports : Context.t -> Reason.t -> Type.t
val set_module_exports : Context.t -> Reason.t -> Type.t -> unit
