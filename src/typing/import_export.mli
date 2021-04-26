(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_module_t : Context.t -> Reason.t -> Type.t

val mk_resource_module_t : Context.t -> ALoc.t -> string -> Type.t

val require : Context.t -> ALoc.t * string -> ALoc.t -> Type.t

val import : Context.t -> ALoc.t * string -> Type.t

val import_ns : Context.t -> Reason.t -> ALoc.t * string -> Type.t

val get_module_exports : Context.t -> ALoc.t -> Type.t

val set_module_exports : Context.t -> ALoc.t -> Type.t -> unit

val cjs_clobber : Context.t -> ALoc.t -> Type.t -> unit

val export : Context.t -> Reason.name -> ALoc.t -> Type.t -> unit

val export_type : Context.t -> Reason.name -> ALoc.t option -> Type.t -> unit

val export_binding : Context.t -> Reason.name -> ALoc.t -> Flow_ast.Statement.export_kind -> unit

val export_star : Context.t -> ALoc.t -> Type.t -> unit

val export_type_star : Context.t -> ALoc.t -> Type.t -> unit
