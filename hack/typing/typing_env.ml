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
open Nast

module SN = Naming_special_names
module Dep = Typing_deps.Dep

(* The following classes are used to make sure we make no typing
 * mistake when interacting with the database. The database knows
 * how to associate a string to a string. We need to deserialize
 * the string and make sure the type is correct. By using these
 * modules, the places where there could be a typing mistake are
 * very well isolated.
*)

module Funs = Typing_heap.Funs
module Classes = Typing_heap.Classes
module Typedefs = Typing_heap.Typedefs
module GConsts = Typing_heap.GConsts

type fake_members = {
  last_call : Pos.t option;
  invalid   : SSet.t;
  valid     : SSet.t;
}
(* Along with a type, each local variable has a expression id associated with
 * it. This is used when generating expression dependent types for the 'this'
 * type. The idea is that if two local variables have the same expression_id
 * then they refer to the same late bound type, and thus have compatible
 * 'this' types.
 *)
type expression_id = Ident.t
type local = locl ty list * locl ty * expression_id
type local_env = fake_members * local IMap.t

type env = {
  pos     : Pos.t      ;
  tenv    : locl ty  IMap.t ;
  subst   : int IMap.t ;
  lenv    : local_env  ;
  genv    : genv       ;
  todo    : tfun list  ;
  in_loop : bool       ;
  (* when encountering Tunresolved in the supertype, do we allow it to grow?
   * if false, this allows the opposite, i.e. Tunresolved can grow in the
   * subtype. *)
  grow_super : bool      ;
}

and genv = {
  tcopt   : TypecheckerOptions.t;
  mode    : FileInfo.mode;
  return  : locl ty;
  parent  : decl ty;
  self_id : string;
  self    : locl ty;
  static  : bool;
  fun_kind : Ast.fun_kind;
  anons   : anon IMap.t;
  droot   : Typing_deps.Dep.variant option;
  file    : Relative_path.t;
}

(* An anonymous function
 * the environment + the fun parameters + the captured identifiers
*)
and anon = env -> locl fun_params -> env * locl ty
and tfun = env -> env

let fresh () =
  Ident.tmp()

let fresh_type () =
  Reason.none, Tvar (Ident.tmp())

let add_subst env x x' =
  if x <> x'
  then { env with subst = IMap.add x x' env.subst }
  else env

let rec get_var env x =
  let x' = IMap.get x env.subst in
  (match x' with
  | None -> env, x
  | Some x' ->
      let env, x' = get_var env x' in
      let env = add_subst env x x' in
      env, x'
  )

let rename env x x' =
  let env, x = get_var env x in
  let env, x' = get_var env x' in
  let env = add_subst env x x' in
  env

let add env x ty =
  let env, x = get_var env x in
  { env with tenv = IMap.add x ty env.tenv }

let get_type env x =
  let env, x = get_var env x in
  let ty = IMap.get x env.tenv in
  match ty with
  | None -> env, (Reason.none, Tany)
  | Some ty -> env, ty

let get_type_unsafe env x =
  let ty = IMap.get x env.tenv in
  match ty with
  | None ->
      env, (Reason.none, Tany)
  | Some ty -> env, ty

let expand_type env x =
  match x with
  | _, Tvar x -> get_type env x
  | x -> env, x

let expand_type_recorded env set ty =
  match ty with
  | r, Tvar x -> begin
    let env, ty =
      if ISet.mem x set then env, (r, Tany) else expand_type env ty in
    let set = ISet.add x set in
    env, set, ty
  end
  | x -> env, set, x

let make_ft p params ret_ty =
  let arity = List.length params in
  {
    ft_pos      = p;
    ft_deprecated = None;
    ft_abstract = false;
    ft_arity    = Fstandard (arity, arity);
    ft_tparams  = [];
    ft_params   = params;
    ft_ret      = ret_ty;
  }

let get_shape_field_name = function
  | SFlit (_, s) -> s
  | SFclass_const ((_, s1), (_, s2)) -> s1^"::"^s2

(* When printing out types (by hh_show()), TVars are printed with an
 * associated identifier. We reindex them (in the order of appearance) to be
 * consecutive integers starting from zero, because the internal identifier
 * can change due to unrelated reasons, which breaks tests. *)
let printable_tvar_ids = ref IMap.empty

let get_printable_tvar_id x =
  match IMap.get x !printable_tvar_ids with
    | None ->
        let res = (IMap.cardinal !printable_tvar_ids) + 1 in
        printable_tvar_ids := IMap.add
          x
          res
          !printable_tvar_ids;
       res
    | Some v -> v

let rec debug stack env (r, ty) =
  let o = print_string in
  (match r with Reason.Rlost_info _ -> o "~lost" | _ -> ());
  match ty with
  | Tunresolved tyl -> o "intersect("; debugl stack env tyl; o ")"
  | Ttuple _ -> o "tuple"
  | Tarray (None, None) -> o "array"
  | Tarray (Some x, None) -> o "array<"; debug stack env x; o ">"
  | Tarray (Some x, Some y) -> o "array<"; debug stack env x; o ", ";
      debug stack env y; o ">"
  | Tarray _ -> assert false
  | Tmixed -> o "mixed"
  | Tabstract (AKnewtype (x, argl), _)
  | Tclass ((_, x), argl) ->
      Printf.printf "App %s" x;
      o "<"; List.iter (fun x -> debug stack env x; o ", ") argl;
      o ">"
  | Tany -> o "X"
  | Tanon _ -> o "anonymous"
  | Tfun ft ->
      o "fun ";
      List.iter (fun (_, x) -> debug stack env x; o ", ") ft.ft_params;
      o " -> ";
      debug stack env ft.ft_ret
  | Toption ty -> o "option("; debug stack env ty; o ")"
  | Tprim p ->
      (match p with
      | Tvoid -> o "Tvoid"
      | Tint -> o "Tint"
      | Tbool -> o "Tbool"
      | Tfloat -> o "Tfloat"
      | Tstring -> o "Tstring"
      | Tclassname s -> o "Tclassname<"; o s; o ">"
      | Tnum -> o "Tnum"
      | Tresource -> o "Tresource"
      | Tarraykey -> o "Tarraykey"
      | Tnoreturn -> o "Tnoreturn"
      )
  | Tabstract (AKgeneric(s, super), cstr_opt) ->
      o "generic ";
      o s;
      (match cstr_opt, super with
      | None, None -> ()
      | Some x, _ -> o " as <"; debug stack env x; o ">"
      | _, Some x -> o " super <"; debug stack env x; o ">"
      )
  | Tabstract (ak, cstr) ->
     o "[";  o (AbstractKind.to_string ak); o "]";
     Option.iter cstr ~f:(debug stack env)
  | Tvar x ->
      let env, x = get_var env x in
      if ISet.mem x stack
      then o (Ident.debug ~normalize:get_printable_tvar_id x)
      else
        let stack = ISet.add x stack in
        let _, y = get_var env x in
        o "["; o (string_of_int (get_printable_tvar_id y)); o "]";
        (match get_type env x with
        | _, (_, Tany) -> o (Ident.debug ~normalize:get_printable_tvar_id x)
        | _, ty -> debug stack env ty)
  | Tobject -> o "object"
  | Tshape (fields_known, fdm) ->
      o "shape<";
      begin match fields_known with
        | FieldsFullyKnown -> o "FieldsFullyKnown";
        | FieldsPartiallyKnown unset_fields -> begin
            o "FieldsPartiallyKnown(unset fields:";
              ShapeMap.iter begin fun k _ ->
                o (get_shape_field_name k); o " "
              end unset_fields;
            o ")"
          end
      end;
      o ">(";
      ShapeMap.iter begin fun k v ->
        o (get_shape_field_name k); o " => "; debug stack env v
      end fdm;
      o ")"

and debugl stack env x =
  let o = print_string in
  match x with
  | [] -> ()
  | [x] -> debug stack env x
  | x :: rl -> debug stack env x; o ", "; debugl stack env rl

let debug env ty = debug ISet.empty env ty; print_newline()

let empty_fake_members = {
    last_call = None;
    invalid   = SSet.empty;
    valid     = SSet.empty;
}

let empty_local = empty_fake_members, IMap.empty

let empty tcopt file = {
  pos     = Pos.none;
  tenv    = IMap.empty;
  subst   = IMap.empty;
  lenv    = empty_local;
  todo    = [];
  in_loop = false;
  grow_super = true;
  genv    = {
    tcopt   = tcopt;
    mode    = FileInfo.Mstrict;
    return  = fresh_type();
    self_id = "";
    self    = Reason.none, Tany;
    static  = false;
    parent  = Reason.none, Tany;
    fun_kind = Ast.FSync;
    anons   = IMap.empty;
    droot   = None;
    file    = file;
  }
}

let add_class x y =
  Classes.add x y

let add_typedef x y =
  Typedefs.add x (Typing_heap.Typedef.Ok y)

let is_typedef x =
  match Typedefs.get x with
  | None -> false
  | Some _ -> true

let get_enum x =
  match Classes.get x with
  | Some tc when tc.tc_enum_type <> None -> Some tc
  | _ -> None

let is_enum x = get_enum x <> None

let get_enum_constraint x =
  match Classes.get x with
  | None -> None
  | Some tc ->
    match tc.tc_enum_type with
      | None -> None
      | Some e -> e.te_constraint

let add_typedef_error x =
  Typedefs.add x Typing_heap.Typedef.Error

(* Adds a new function (global) *)
let add_fun x ft =
  Funs.add x ft

let add_wclass env x =
  let dep = Dep.Class x in
  Typing_deps.add_idep env.genv.droot dep;
  ()

(* When we want to type something with a fresh typing environment *)
let fresh_tenv env f =
  let genv = env.genv in
  f { env with todo = []; tenv = IMap.empty; genv = genv; in_loop = false }

let get_class env x =
  add_wclass env x;
  Classes.get x

let get_typedef env x =
  add_wclass env x;
  Typedefs.get x

let add_extends_dependency env x =
  let dep = Dep.Class x in
  Typing_deps.add_idep env.genv.droot dep;
  Typing_deps.add_idep env.genv.droot (Dep.Extends x);
  ()

let get_class_dep env x =
  add_wclass env x;
  add_extends_dependency env x;
  Classes.get x

(* Used to access class constants. *)
let get_const env class_ mid =
  add_wclass env class_.tc_name;
  let dep = Dep.Const (class_.tc_name, mid) in
  Typing_deps.add_idep env.genv.droot dep;
  SMap.get mid class_.tc_consts

(* Used to access "global constants". That is constants that were
 * introduced with "const X = ...;" at topelevel, or "define('X', ...);"
 *)
let get_gconst env cst_name =
  Typing_deps.add_idep env.genv.droot (Dep.GConst cst_name);
  GConsts.get cst_name

let get_static_member is_method env class_ mid =
  add_wclass env class_.tc_name;
  let dep = if is_method then Dep.SMethod (class_.tc_name, mid)
  else Dep.SProp (class_.tc_name, mid) in
  Typing_deps.add_idep env.genv.droot dep;
  if is_method then SMap.get mid class_.tc_smethods
  else SMap.get mid class_.tc_sprops

let suggest_member members mid =
  let members = SMap.fold begin fun x ce acc ->
    let pos = Reason.to_pos (fst ce.ce_type) in
    SMap.add (String.lowercase x) (pos, x) acc
  end members SMap.empty
  in
  SMap.get mid members

let suggest_static_member is_method class_ mid =
  let mid = String.lowercase mid in
  let members = if is_method then class_.tc_smethods else class_.tc_sprops in
  suggest_member members mid

let get_member is_method env class_ mid =
  add_wclass env class_.tc_name;
  let dep = if is_method then Dep.Method (class_.tc_name, mid)
  else Dep.Prop (class_.tc_name, mid) in
  Typing_deps.add_idep env.genv.droot dep;
  if is_method then (SMap.get mid class_.tc_methods)
  else SMap.get mid class_.tc_props

let suggest_member is_method class_ mid =
  let mid = String.lowercase mid in
  let members = if is_method then class_.tc_methods else class_.tc_props in
  suggest_member members mid

let get_construct env class_ =
  add_wclass env class_.tc_name;
  let dep = Dep.Cstr (class_.tc_name) in
  Typing_deps.add_idep env.genv.droot dep;
  class_.tc_construct

let get_todo env =
  env.todo

let grow_super env =
  env.grow_super

let invert_grow_super env f =
  let old = env.grow_super in
  let env = { env with grow_super = not old } in
  let env = f env in
  let env = { env with grow_super = old } in
  env

let get_return env =
  env.genv.return

let set_return env x =
  let genv = env.genv in
  let genv = { genv with return = x } in
  { env with genv = genv }

let with_return env f =
  let ret = get_return env in
  let env = f env in
  set_return env ret

let is_static env = env.genv.static
let get_self env = env.genv.self
let get_self_id env = env.genv.self_id
let is_outside_class env = (env.genv.self_id = "")
let get_parent env = env.genv.parent

let get_fn_kind env = env.genv.fun_kind

let get_file env = env.genv.file

let get_fun env x =
  let dep = Dep.Fun x in
  Typing_deps.add_idep env.genv.droot dep;
  Funs.get x

let set_fn_kind env fn_type =
  let genv = env.genv in
  let genv = { genv with fun_kind = fn_type } in
  { env with genv = genv }

let add_todo env x =
  { env with todo = x :: env.todo }

let add_anonymous env x =
  let genv = env.genv in
  let anon_id = Ident.tmp() in
  let genv = { genv with anons = IMap.add anon_id x genv.anons } in
  { env with genv = genv }, anon_id

let set_anonymous env anon_id x =
  let genv = env.genv in
  let genv = { genv with anons = IMap.add anon_id x genv.anons } in
  { env with genv = genv }

let get_anonymous env x =
  IMap.get x env.genv.anons

let set_self_id env x =
  let genv = env.genv in
  let genv = { genv with self_id = x } in
  { env with genv = genv }

let set_self env x =
  let genv = env.genv in
  let genv = { genv with self = x } in
  { env with genv = genv }

let set_parent env x =
  let genv = env.genv in
  let genv = { genv with parent = x } in
  { env with genv = genv }

let set_static env =
  let genv = env.genv in
  let genv = { genv with static = true } in
  { env with genv = genv }

let set_mode env mode =
  let genv = env.genv in
  let genv = { genv with mode = mode } in
  { env with genv = genv }

let set_root env root =
  let genv = env.genv in
  let genv = { genv with droot = Some root } in
  { env with genv = genv }

let get_mode env = env.genv.mode

let is_strict env = get_mode env = FileInfo.Mstrict
let is_decl env = get_mode env = FileInfo.Mdecl

let get_options env = env.genv.tcopt

(*
let debug_env env =
  Classes.iter begin fun cid class_ ->
    Printf.printf "Type of class %s:" cid;
    Printf.printf "{ ";
    SMap.iter begin fun m _ ->
      Printf.printf "%s " m;
    end class_.tc_methods;
    Printf.printf "}\n"
  end env.genv.classes
*)
(*****************************************************************************)
(* This is used when we want member variables to be treated like locals
 * We want to handle the following:
 * if($this->x) {
 *   ... $this->x ...
 * }
 * The trick consists in replacing $this->x with a "fake" local. So that
 * all the logic that normally applies to locals is applied in cases like
 * this. Hence the name: FakeMembers.
 * All the fake members are thrown away at the first call.
 * We keep the invalidated fake members for better error messages.
 *)
(*****************************************************************************)

let get_last_call env =
  match (fst env.lenv).last_call with
  | None -> assert false
  | Some pos -> pos

let rec lost_info fake_name stack env ty =
  let info r = Reason.Rlost_info (fake_name, r, get_last_call env) in
  match ty with
  | _, Tvar v when ISet.mem v stack -> env, ty
  | _, Tvar v ->
      let stack = ISet.add v stack in
      let env, v' = get_var env v in
      (match IMap.get v' env.tenv with
      | None ->
          env, ty
      | Some ty ->
          let env, ty = lost_info fake_name stack env ty in
          let env = add env v ty in
          env, ty
      )
  | r, Tunresolved tyl ->
      let env, tyl = lfold (lost_info fake_name stack) env tyl in
      env, (info r, Tunresolved tyl)
  | r, ty ->
      env, (info r, ty)

let forget_members env call_pos =
  let fake_members, locals = env.lenv in
  let old_invalid = fake_members.invalid in
  let new_invalid = fake_members.valid in
  let new_invalid = SSet.union new_invalid old_invalid in
  let fake_members = {
    last_call = Some call_pos;
    invalid = new_invalid;
    valid = SSet.empty;
  } in
  { env with lenv = fake_members, locals }

module FakeMembers = struct

  let make_id obj_name member_name =
    let obj_name =
      match obj_name with
      | _, This -> this
      | _, Lvar (_, x) -> x
      | _ -> assert false
    in
    string_of_int obj_name^"->"^member_name

  let make_static_id cid member_name =
    let class_name = class_id_to_str cid in
    class_name^"::"^member_name

  let get env obj member_name =
    match obj with
    | _, This
    | _, Lvar _ ->
        let id = make_id obj member_name in
        if SSet.mem id (fst env.lenv).valid
        then Some (Hashtbl.hash id)
        else None
    | _ -> None

  let is_invalid env obj member_name =
    match obj with
    | _, This
    | _, Lvar _ ->
        SSet.mem (make_id obj member_name) (fst env.lenv).invalid
    | _ -> false

  let get_static env cid member_name =
    let name = make_static_id cid member_name in
    if SSet.mem name (fst env.lenv).valid
    then Some (Hashtbl.hash name)
    else None

  let is_static_invalid env cid member_name =
    SSet.mem (make_static_id cid member_name) (fst env.lenv).invalid

  let add_member env fake_id =
    let fake_members, locals = env.lenv in
    let valid = SSet.add fake_id fake_members.valid in
    let fake_members = { fake_members with valid = valid } in
    { env with lenv = fake_members, locals }

  let make _ env obj_name member_name =
    let my_fake_local_id = make_id obj_name member_name in
    let env = add_member env my_fake_local_id in
    env, Hashtbl.hash my_fake_local_id

 let make_static _ env class_name member_name =
   let my_fake_local_id = make_static_id class_name member_name in
   let env = add_member env my_fake_local_id in
   env, Hashtbl.hash my_fake_local_id

end


(*****************************************************************************)
(* Locals *)
(*****************************************************************************)

(* We want to "take a picture" of the current type
 * that is, the current type shouldn't be affected by a
 * future unification.
 *)

let rec unbind seen env ty =
  let env, ty = expand_type env ty in
  if List.mem ty seen
  then env, ty
  else
    let seen = ty :: seen in
    match ty with
    | r, Tunresolved tyl ->
        let env, tyl = lfold (unbind seen) env tyl in
        env, (r, Tunresolved tyl)
    | ty -> env, ty

let unbind = unbind []

(* We maintain 3 states for a local, all the types that the
 * local ever had (cf integrate in typing.ml), the type
 * that the local currently has, and an expression_id generated from
 * the last assignment to this local.
 *)
let set_local env x new_type =
  let fake_members, locals = env.lenv in
  let env, new_type = unbind env new_type in
  let all_types, expr_id =
    match IMap.get x locals with
    | None -> [], Ident.tmp()
    | Some (x, _, y) -> x, y
  in
  let all_types =
    if List.exists (fun x -> x = new_type) all_types
    then all_types
    else new_type :: all_types
  in
  let local = all_types, new_type, expr_id in
  let locals = IMap.add x local locals in
  let env = { env with lenv = fake_members, locals } in
  env

let get_local env x =
  let lcl = IMap.get x (snd env.lenv) in
  match lcl with
  | None -> env, (Reason.Rnone, Tany)
  | Some (_, x, _) -> env, x

let set_local_expr_id env x new_eid =
  let fake_members, locals = env.lenv in
  match IMap.get x locals with
  | Some (all_types, type_, eid) when eid <> new_eid ->
      let local = all_types, type_, new_eid in
      let locals = IMap.add x local locals in
      let env = { env with lenv = fake_members, locals } in
      env
  | _ -> env

let get_local_expr_id env x =
  let lcl = IMap.get x (snd env.lenv) in
  Option.map lcl ~f:(fun (_, _, x) -> x)

(*****************************************************************************)
(* This function is called when we are about to type-check a block that will
 * later be fully_integrated (cf Typing.fully_integrate).
 * Integration is about keeping track of all the types that a local had in
 * its lifetime. It's necessary to correctly type-check catch blocks.
 * After we type-check a block, we want to take all the types that the local
 * had in this block, and add it to the list of possible types.
 *
 * However, we are not interested in the types that the local had *before*
 * we started typing the block.
 *
 * A concrete example:
 *
 * $x = null;
 *
 * $x = 'hello'; // the type of $x is string
 *
 * while (...) {
 *   $x = 0;
 * }
 *
 * The type of $x is string or int, NOT string or int or ?_.
 * We don't really care about the fact that $x could be null before the
 * block.
 *
 * This is what freeze_local does, just before we start type-checking the
 * while loop, we "freeze" the type of locals to the current environment.
 *)
(*****************************************************************************)

let freeze_local_env env =
  let (members, locals) = env.lenv in
  let locals = IMap.map (fun (_, type_, eid) -> [type_], type_, eid) locals in
  let lenv = members, locals in
  { env with lenv = lenv }

(*****************************************************************************)
(* Sets up/cleans up the environment when typing an anonymous function. *)
(*****************************************************************************)

let anon anon_lenv env f =
  (* Setting up the environment. *)
  let old_lenv = env.lenv in
  let old_return = get_return env in
  let outer_fun_kind = get_fn_kind env in
  let env = { env with lenv = anon_lenv } in
  (* Typing *)
  let env, result = f env in
  (* Cleaning up the environment. *)
  let env = { env with lenv = old_lenv } in
  let env = set_return env old_return in
  let env = set_fn_kind env outer_fun_kind in
  env, result

let in_loop env f =
  let old_in_loop = env.in_loop in
  let env = { env with in_loop = true } in
  let env = f env in
  { env with in_loop = old_in_loop }
