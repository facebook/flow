(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs

module Funs : module type of Typing_heap.Funs
module Classes : module type of Typing_heap.Classes
module Typedefs : module type of Typing_heap.Typedefs
module GConsts : module type of Typing_heap.GConsts

type fake_members = {
  last_call : Pos.t option;
  invalid : SSet.t;
  valid : SSet.t;
}
type expression_id = Ident.t
type local = locl ty list * locl ty * expression_id
type local_env = fake_members * local IMap.t
type env = {
  pos : Pos.t;
  tenv : locl ty IMap.t;
  subst : int IMap.t;
  lenv : local_env;
  genv : genv;
  todo : tfun list;
  in_loop : bool;
  grow_super : bool;
}
and genv
and anon = env -> locl fun_params -> env * locl ty
and tfun = env -> env
val fresh : unit -> int
val fresh_type : unit -> locl ty
val add_subst : env -> int -> int -> env
val get_var : env -> int -> env * int
val rename : env -> int -> int -> env
val add : env -> int -> locl ty -> env
val get_type : env -> int -> env * locl ty
val get_type_unsafe : env -> int -> env * locl ty
val expand_type : env -> locl ty -> env * locl ty
val expand_type_recorded : env -> ISet.t -> locl ty -> env * ISet.t * locl ty
val make_ft : Pos.t -> decl fun_params -> decl ty -> decl fun_type
val get_shape_field_name : Nast.shape_field_name -> string
val debugl : ISet.t -> env -> locl ty list -> unit
val debug : env -> locl ty -> unit
val empty_fake_members : fake_members
val empty_local : local_env
val empty : TypecheckerOptions.t -> Relative_path.t -> env
val add_class : Classes.key -> Classes.t -> unit
val add_typedef : Typedefs.key -> Typing_heap.Typedef.tdef -> unit
val is_typedef : Typedefs.key -> bool
val get_enum : Classes.key -> Classes.t option
val is_enum : Classes.key -> bool
val get_enum_constraint : Classes.key -> decl ty option
val add_typedef_error : Typedefs.key -> unit
val add_fun : Funs.key -> Funs.t -> unit
val add_wclass : env -> string -> unit
val fresh_tenv : env -> (env -> unit) -> unit
val get_class : env -> Classes.key -> Classes.t option
val get_typedef : env -> Typedefs.key -> Typedefs.t option
val add_extends_dependency : env -> string -> unit
val get_class_dep : env -> Classes.key -> Classes.t option
val get_const : env -> class_type -> string -> class_elt option
val get_gconst : env -> GConsts.key -> GConsts.t option
val get_static_member : bool -> env -> class_type -> string -> class_elt option
val suggest_static_member :
  bool -> class_type -> string -> (Pos.t * string) option
val get_member : bool -> env -> class_type -> string -> class_elt option
val suggest_member : bool -> class_type -> string -> (Pos.t * string) option
val get_construct : env -> class_type -> class_elt option * bool
val get_todo : env -> tfun list
val get_return : env -> locl ty
val set_return : env -> locl ty -> env
val with_return : env -> (env -> env) -> env
val is_static : env -> bool
val grow_super : env -> bool
val invert_grow_super : env -> (env -> env) -> env
val get_self : env -> locl ty
val get_self_id : env -> string
val is_outside_class : env -> bool
val get_parent : env -> decl ty
val get_fn_kind : env -> Ast.fun_kind
val get_file : env -> Relative_path.t
val get_fun : env -> Funs.key -> Funs.t option
val set_fn_kind : env -> Ast.fun_kind -> env
val add_todo : env -> tfun -> env
val add_anonymous : env -> anon -> env * int
val set_anonymous : env -> int -> anon -> env
val get_anonymous : env -> int -> anon option
val set_self_id : env -> string -> env
val set_self : env -> locl ty -> env
val set_parent : env -> decl ty -> env
val set_static : env -> env
val set_mode : env -> FileInfo.mode -> env
val set_root : env -> Typing_deps.Dep.variant -> env
val get_mode : env -> FileInfo.mode
val is_strict : env -> bool
val is_decl : env -> bool
val get_options: env -> TypecheckerOptions.t
val get_last_call : env -> Pos.t
val lost_info : string -> ISet.t -> env -> locl ty -> env * locl ty
val forget_members : env -> Pos.t -> env
module FakeMembers :
  sig
    val make_id : Nast.expr -> string -> string
    val make_static_id : Nast.class_id -> string -> string
    val get : env -> Nast.expr -> string -> int option
    val is_invalid : env -> Nast.expr -> string -> bool
    val get_static : env -> Nast.class_id -> string -> int option
    val is_static_invalid : env -> Nast.class_id -> string -> bool
    val make : Pos.t -> env -> Nast.expr -> string -> env * int
    val make_static : Pos.t -> env -> Nast.class_id -> string -> env * int
  end
val unbind : env -> locl ty -> env * locl ty
val set_local : env -> Ident.t -> locl ty -> env
val get_local : env -> Ident.t -> env * locl ty
val set_local_expr_id : env -> Ident.t -> expression_id -> env
val get_local_expr_id : env -> Ident.t -> expression_id option
val freeze_local_env : env -> env
val anon : local_env -> env -> (env -> env * locl ty) -> env * locl ty
val in_loop : env -> (env -> env) -> env
