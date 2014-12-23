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

module Class : sig type t = class_type val prefix : Prefix.t end
module Fun : sig type t = fun_type val prefix : Prefix.t end
module Typedef :
  sig
    type visibility = Public | Private
    type tdef =
        visibility * tparam list * ty option *
        ty * Pos.t
    type tdef_or_error = Error | Ok of tdef
    type t = tdef_or_error
    val prefix : Prefix.t
  end
module GConst : sig type t = ty val prefix : Prefix.t end

module Funs : module type of SharedMem.WithCache (String) (Fun)
module Classes : module type of SharedMem.WithCache (String) (Class)
module Typedefs : module type of SharedMem.WithCache (String) (Typedef)
module GConsts : module type of SharedMem.WithCache (String) (GConst)

type funs = Funs.t
type classes = Classes.t
type fake_members = {
  last_call : Pos.t option;
  invalid : SSet.t;
  valid : SSet.t;
}
type local = ty list * ty
type local_env = fake_members * local IMap.t
type env = {
  pos : Pos.t;
  tenv : ty IMap.t;
  subst : int IMap.t;
  lenv : local_env;
  genv : genv;
  todo : tfun list;
}
and genv
and anon = env -> fun_params -> env * ty
and tfun = env -> env
val fresh : unit -> int
val fresh_type : unit -> ty
val add_subst : env -> int -> int -> env
val get_var : env -> int -> env * int
val rename : env -> int -> int -> env
val add : env -> int -> ty -> env
val get_type : env -> int -> env * ty
val get_type_unsafe : env -> int -> env * ty
val expand_type : env -> ty -> env * ty
val expand_type_recorded : env -> ISet.t -> ty -> env * ISet.t * ty
val has_type : env -> int -> bool
val make_ft : Pos.t -> fun_params -> ty -> fun_type
val get_shape_field_name : Nast.shape_field_name -> string
val debugl : ISet.t -> env -> ty list -> unit
val debug : env -> ty -> unit
val empty_fake_members : fake_members
val empty_local : local_env
val empty : Relative_path.t -> env
val add_class : Classes.key -> Classes.t -> unit
val add_typedef : Typedefs.key -> Typedef.tdef -> unit
val is_typedef : Typedefs.key -> bool
val get_enum : Classes.key -> Classes.t option
val is_enum : Classes.key -> bool
val get_enum_constraint : Classes.key -> ty option
val add_typedef_error : Typedefs.key -> unit
val add_fun : Funs.key -> Funs.t -> unit
val add_wclass : env -> string -> unit
val fresh_tenv : env -> (env -> unit) -> unit
val get_class : env -> Classes.key -> Classes.t option
val get_typedef : env -> Typedefs.key -> Typedefs.t option
val class_exists : Classes.key -> bool
val add_extends_dependency : env -> string -> unit
val get_class_dep : env -> Classes.key -> Classes.t option
val get_const : env -> class_type -> string -> class_elt option
val get_typeconst_type : env -> class_type -> string -> ty option
val get_gconst : env -> GConsts.key -> GConsts.t option
val get_static_member : bool -> env -> class_type -> string -> class_elt option
val suggest_static_member :
  bool -> class_type -> string -> (Pos.t * string) option
val method_exists : class_type -> string -> bool
val get_member : bool -> env -> class_type -> string -> class_elt option
val suggest_member : bool -> class_type -> string -> (Pos.t * string) option
val get_construct : env -> class_type -> class_elt option * bool
val get_todo : env -> tfun list
val get_return : env -> ty
val set_return : env -> ty -> env
val with_return : env -> (env -> env) -> env
val allow_null_as_void : env -> bool
val is_static : env -> bool
val get_self : env -> ty
val get_self_id : env -> string
val get_parent : env -> ty
val get_fn_kind : env -> Nast.fun_kind
val get_file : env -> Relative_path.t
val get_fun : env -> Funs.key -> Funs.t option
val set_allow_null_as_void : ?allow:bool -> env -> env
val set_fn_kind : env -> Nast.fun_kind -> env
val add_todo : env -> tfun -> env
val add_anonymous : env -> anon -> env * int
val get_anonymous : env -> int -> anon option
val set_self_id : env -> string -> env
val set_self : env -> ty -> env
val set_parent : env -> ty -> env
val set_static : env -> env
val set_mode : env -> Ast.mode -> env
val set_root : env -> Typing_deps.Dep.variant -> env
val get_mode : env -> Ast.mode
val is_strict : env -> bool
val is_decl : env -> bool
val get_last_call : env -> Pos.t
val lost_info : string -> ISet.t -> env -> ty -> env * ty
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
val unbind : env -> ty -> env * ty
val set_local : env -> Ident.t -> ty -> env
val get_local : env -> Ident.t -> env * ty
val freeze_local_env : env -> env
val anon : local_env -> env -> (env -> env * ty) -> env * ty
