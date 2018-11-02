(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_module_t : Context.t -> Reason.t -> Type.t
val mk_commonjs_module_t :
  Context.t -> Reason.t -> Reason.t -> Type.t -> Type.t
val mk_resource_module_t : Context.t -> ALoc.t -> string -> Type.t
val require : Context.t -> (ALoc.t * string) -> ALoc.t -> Type.t
val import : Context.t -> (ALoc.t * string) -> ALoc.t -> Type.t
val import_ns : Context.t -> Reason.t -> (ALoc.t * string) -> ALoc.t -> Type.t
val module_t_of_cx : Context.t -> Type.t
val set_module_t : Context.t -> Reason.t -> (Type.t -> unit) -> unit
val set_module_kind : Context.t -> ALoc.t -> Context.module_kind -> unit
val nameify_default_export_decl :
  ('M, 'M) Flow_ast.Statement.t ->
  ('M, 'M) Flow_ast.Statement.t * (('N, 'U) Flow_ast.Statement.t -> ('N, 'U) Flow_ast.Statement.t)
val warn_or_ignore_export_star_as : Context.t -> (ALoc.t * 'a) option -> unit
val get_module_exports : Context.t -> ALoc.t -> Type.t
val set_module_exports : Context.t -> ALoc.t -> Type.t -> unit
