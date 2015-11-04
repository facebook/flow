(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

let binop_hooks:
  (
    Pos.t ->
    Ast.bop ->
    Typing_defs.locl Typing_defs.ty ->
    Typing_defs.locl Typing_defs.ty ->
    unit
  ) list ref
  = ref []

let assign_hooks: (Pos.t -> Typing_defs.locl Typing_defs.ty ->
                   Typing_env.env -> unit) list ref = ref []

let (id_hooks: (Pos.t * string -> Typing_env.env -> unit) list ref) = ref []

let (smethod_hooks: (Typing_defs.class_type -> Pos.t * string ->
                     Typing_env.env -> Nast.class_id option -> is_method:bool ->
                     unit) list ref) = ref []

let (cmethod_hooks: (Typing_defs.class_type -> Pos.t * string ->
                     Typing_env.env -> Nast.class_id option -> is_method:bool ->
                     unit) list ref) = ref []

let (lvar_hooks: (Pos.t * Ident.t -> Typing_env.env ->
                  unit) list ref) = ref []

let (fun_call_hooks: ((string option * Typing_defs.locl Typing_defs.ty) list ->
                      Pos.t list -> Typing_env.env -> unit) list ref) = ref []

let (new_id_hooks: (Nast.class_id-> Typing_env.env ->
                    Pos.t -> unit) list ref) = ref []

let (fun_id_hooks: (Pos.t * string -> unit) list ref) = ref []

let (parent_construct_hooks: (Typing_env.env ->
                    Pos.t -> unit) list ref) = ref []

let (constructor_hooks: (Typing_defs.class_type ->
                         Typing_env.env -> Pos.t -> unit) list ref) = ref []

let (class_id_hooks: (Pos.t * string ->
                      (Pos.t * string) option -> unit) list ref) = ref []

let (infer_ty_hooks: (Typing_defs.phase_ty -> Pos.t ->
                      Typing_env.env -> unit) list ref) = ref []

let (enter_method_def_hooks: (Nast.method_ -> unit) list ref) = ref []

let (exit_method_def_hooks: (Nast.method_ -> unit) list ref) = ref []

let (enter_fun_def_hooks: (Nast.fun_ -> unit) list ref) = ref []

let (exit_fun_def_hooks: (Nast.fun_ -> unit) list ref) = ref []

let (enter_class_def_hooks: (Nast.class_ -> Typing_defs.class_type
                            -> unit) list ref) = ref []

let (exit_class_def_hooks: (Nast.class_ -> Typing_defs.class_type
                            -> unit) list ref) = ref []

let attach_smethod_hook hook =
  smethod_hooks := hook :: !smethod_hooks

let attach_cmethod_hook hook =
  cmethod_hooks := hook :: !cmethod_hooks

let attach_binop_hook hook =
  binop_hooks := hook :: !binop_hooks

let attach_assign_hook hook =
  assign_hooks := hook :: !assign_hooks

let attach_id_hook hook =
  id_hooks := hook :: !id_hooks

let attach_lvar_hook hook =
  lvar_hooks := hook :: !lvar_hooks

let attach_fun_call_hook hook =
  fun_call_hooks := hook :: !fun_call_hooks

let attach_new_id_hook hook =
  new_id_hooks := hook :: !new_id_hooks

let attach_fun_id_hook hook =
  fun_id_hooks := hook :: !fun_id_hooks

let attach_parent_construct_hook hook =
  parent_construct_hooks := hook :: !parent_construct_hooks

let attach_constructor_hook hook =
  constructor_hooks := hook :: !constructor_hooks

let attach_class_id_hook hook =
  class_id_hooks := hook :: !class_id_hooks

let attach_infer_ty_hook hook =
  infer_ty_hooks := hook :: !infer_ty_hooks

let attach_method_def_hook enter_hook exit_hook =
  assert (enter_hook <> None || exit_hook <> None);
  (match enter_hook with
  | Some hook ->
      enter_method_def_hooks := hook :: !enter_method_def_hooks
  | None -> ());
  match exit_hook with
  | Some hook ->
      exit_method_def_hooks := hook :: !exit_method_def_hooks
  | None -> ()

let attach_fun_def_hook enter_hook exit_hook =
  assert (enter_hook <> None || exit_hook <> None);
  (match enter_hook with
  | Some hook ->
      enter_fun_def_hooks := hook :: !enter_fun_def_hooks
  | None -> ());
  match exit_hook with
  | Some hook ->
      exit_fun_def_hooks := hook :: !exit_fun_def_hooks
  | None -> ()

let attach_class_def_hook enter_hook exit_hook =
  assert (enter_hook <> None || exit_hook <> None);
  (match enter_hook with
  | Some hook ->
      enter_class_def_hooks := hook :: !enter_class_def_hooks
  | None -> ());
  match exit_hook with
  | Some hook ->
      exit_class_def_hooks := hook :: !exit_class_def_hooks
  | None -> ()

let dispatch_binop_hook p bop ty1 ty2 =
  List.iter !binop_hooks begin fun hook -> hook p bop ty1 ty2 end

let dispatch_assign_hook p ty2 env =
  List.iter !assign_hooks (fun hook -> hook p ty2 env)

let dispatch_id_hook id env =
  List.iter !id_hooks begin fun hook -> hook id env end

let dispatch_smethod_hook class_ id env cid ~is_method =
  List.iter !smethod_hooks (fun hook -> hook class_ id env cid ~is_method)

let dispatch_cmethod_hook class_ id env cid ~is_method =
  List.iter !cmethod_hooks (fun hook -> hook class_ id env cid ~is_method)

let dispatch_lvar_hook id env =
  List.iter !lvar_hooks begin fun hook -> hook id env end

let dispatch_fun_call_hooks ft_params posl env =
  List.iter !fun_call_hooks begin fun hook -> hook ft_params posl env end

let dispatch_new_id_hook cid env p =
  List.iter !new_id_hooks begin fun hook -> hook cid env p end

let dispatch_fun_id_hook id =
  List.iter !fun_id_hooks begin fun hook -> hook id end

let dispatch_parent_construct_hook env p =
  List.iter !parent_construct_hooks begin fun hook -> hook env p end

let dispatch_constructor_hook c env p =
  List.iter !constructor_hooks begin fun hook -> hook c env p end

let dispatch_class_id_hook c_id m_id_optional =
  List.iter !class_id_hooks begin fun hook -> hook c_id m_id_optional end

let dispatch_infer_ty_hook ty pos env =
  List.iter !infer_ty_hooks begin fun hook -> hook ty pos env end

let dispatch_enter_method_def_hook method_ =
  List.iter !enter_method_def_hooks begin fun hook -> hook method_ end

let dispatch_exit_method_def_hook method_ =
  List.iter !exit_method_def_hooks begin fun hook -> hook method_ end

let dispatch_enter_fun_def_hook fun_ =
  List.iter !enter_fun_def_hooks begin fun hook -> hook fun_ end

let dispatch_exit_fun_def_hook fun_ =
  List.iter !exit_fun_def_hooks begin fun hook -> hook fun_ end

let dispatch_enter_class_def_hook cls cls_type =
  List.iter !enter_class_def_hooks begin fun hook -> hook cls cls_type end

let dispatch_exit_class_def_hook cls cls_type =
  List.iter !exit_class_def_hooks begin fun hook -> hook cls cls_type end

let remove_all_hooks () =
  binop_hooks := [];
  assign_hooks := [];
  id_hooks := [];
  cmethod_hooks := [];
  smethod_hooks := [];
  lvar_hooks := [];
  fun_call_hooks := [];
  new_id_hooks := [];
  fun_id_hooks := [];
  constructor_hooks := [];
  class_id_hooks := [];
  infer_ty_hooks := [];
  enter_method_def_hooks := [];
  exit_method_def_hooks := [];
  enter_fun_def_hooks := [];
  exit_fun_def_hooks := [];
  enter_class_def_hooks := [];
  exit_class_def_hooks := []
