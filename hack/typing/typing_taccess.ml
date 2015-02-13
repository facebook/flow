(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs
open Utils
open Nast

module TUtils = Typing_utils
module Reason = Typing_reason
module Env = Typing_env
module Inst = Typing_instantiate
module SN = Naming_special_names
module TGen = Typing_generic

let fill_type_hole env ty hole_ty =
  let subst = Inst.make_subst
    [Ast.Invariant, (Pos.none, SN.Typehints.type_hole), None]
    [hole_ty] in
  Inst.instantiate subst env ty

let rec expand env reason (root, ids) =
  try
    let env, (r, ty_), seen = expand_ env [] root ids in
    let ty_str = Typing_print.error (Taccess (root, ids)) in
    let reason = Reason.Rtype_access (reason, ty_str, List.rev seen, r) in
    let reason =
      match snd root with
      (* If it is a expression dependent type we want to explain what expression
       * it was derived from. When we substitute an expression dependent type
       * it will be a Tabstract. Since all other Tabstract we create are from
       * newtype defs, if it isn't a typedef then we know it has to be an
       * expression dependent type
       *)
      | Tabstract ((p, x), [], Some _) when not (Env.is_typedef x) ->
          Reason.Rexpr_dep_type (reason, p, x)
      | _ -> reason
    in
    (* check for potential cycles *)
    Errors.try_with_error
      (fun () ->
        check_tconst seen env (reason, ty_);
        env, (reason, ty_)
      )
      (fun () -> env, (r, Tany))
  with
  | Exit ->
      env, (reason, Tany)

and expand_ env seen root_ty ids =
  match ids with
  | [] ->
      env, root_ty, seen
  | _ ->
      let generic_names, (class_pos, class_name), (root_ty, ids) =
        root_to_class_name env root_ty (root_ty, ids) in
      let (pos, tconst), rest = List.hd ids, List.tl ids in
      let root_class =
        match Env.get_class env class_name with
        | None ->
            Errors.unbound_name_typing class_pos class_name;
            raise Exit
        | Some c -> c
      in
      let typeconst =
        match SMap.get tconst root_class.tc_typeconsts with
        | None ->
            Errors.smember_not_found `class_typeconst
              pos (root_class.tc_pos, class_name) tconst `no_hint;
            raise Exit
        | Some tc -> tc
      in
      let type_ n ty =
        let name = (strip_ns n)^"::"^tconst in
        Reason.Rwitness (fst typeconst.ttc_name), Tgeneric (name, ty)
      in
      let tconst_ty =
        match generic_names, typeconst with
        | _, {ttc_type = Some ty; ttc_constraint = None; _}
        | [], {ttc_type = Some ty; _} -> ty
        | names, {ttc_constraint = ty; _} ->
            List.fold_right (fun n acc -> type_ n (Some acc))
              names (type_ class_name ty)
      in
      let env, tconst_ty = fill_type_hole env tconst_ty root_ty in
      (* check for cycles before expanding further *)
      let seen =
        let cur_tconst = List.fold_left (fun acc (_, s) -> acc^"::"^s)
          (strip_ns class_name) ids in
        if List.mem cur_tconst seen
        then (
          let seen = List.rev (cur_tconst :: seen) in
          Errors.cyclic_typeconst (fst typeconst.ttc_name) seen;
          raise Exit
        )
        else cur_tconst :: seen
      in
      expand_ env seen tconst_ty rest

(* The root of a type access is a type. When expanding a type access this type
 * needs to resolve to the name of a class so we can look up if a given type
 * constant is defined in the class.
 *
 * We also need to track the name of the generics we stumble across. This is
 * so we do not expand different generics to the same type. For instance if we
 * have the following type accesses:
 *
 *   Taccess (Tgeneric ("this", Some( Tapply "C")), ["T"])
 *   Taccess (Tgeneric ("X", Some( Tapply "C")), ["T"])
 *
 * resolve_to_class_name will yield:
 *
 *  (["this"], "C")
 *  (["X"], "C")
 *
 * And will ultimately expand to:
 *
 *  Tgeneric ("this::T", Some(Tgeneric "C::T"))
 *  Tgeneric ("X::T", Some(Tgeneric "C::T"))
 *)
 and root_to_class_name env (r, ty) (root_ty, ids) =
  let root_to_cname env type_ = root_to_class_name env type_ (root_ty, ids) in
  match ty with
  | Tapply ((_, tdef), tyl) when Env.is_typedef tdef ->
      let env, ty = TUtils.expand_typedef env r tdef tyl in
      root_to_cname env ty
  | Tapply (x, _) -> [], x, (root_ty, ids)
  (* instead of expanding this Taccess, we update the root and ids that we
   * are working on and pass it down to the caller
   *)
  | Taccess (root, ids2) ->
      root_to_class_name env root (root, ids2 @ ids)
  | Toption ty ->
      root_to_cname env ty
  (* If we haven't filled the type hole at this point we fill it with self *)
  | Tgeneric (x, _) when x = SN.Typehints.type_hole ->
      let names, class_name, taccess = root_to_cname env (Env.get_self env) in
      SN.Typehints.this::names, class_name, taccess
  | Tabstract ((_, x), _, Some ty) | Tgeneric (x, Some ty) ->
      let names, class_name, taccess = root_to_cname env ty in
      x::names, class_name, taccess
  | Tany
  | Tvar _ | Tunresolved _
  | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
  | Tarray (_, _) | Tfun _ | Tabstract (_, _, _) | Tgeneric (_, _) ->
      let pos, tconst = List.hd ids in
      let ty = Typing_print.error ty in
      Errors.non_object_member tconst (Reason.to_pos r) ty pos;
      raise Exit

(* Following code checks for cycles that may occur when expanding a Taccess.
 * This is mainly copy-pasta from Typing_tdef.ml. We should see if its possible
 * to provide a more generic cycle detection utility function.
 * *)
and check_tconst seen env (r, t) =
  match t with
  | Tany -> ()
  | Tmixed -> ()
  | Tarray (ty1, ty2) ->
      check_tconst_opt seen env ty1;
      check_tconst_opt seen env ty2;
      ()
  | Tgeneric (_, ty) ->
      check_tconst_opt seen env ty
  | Toption ty -> check_tconst seen env ty
  | Tprim _ -> ()
  | Tvar _ -> ()
  | Tfun fty -> check_fun_tconst seen env fty
  | Tapply ((_, tdef), tyl) when Typing_env.is_typedef tdef ->
      let env, ty = TUtils.expand_typedef env r tdef tyl in
      check_tconst seen env ty
  | Tabstract (_, tyl, cstr) ->
      check_tconst_list seen env tyl;
      check_tconst_opt seen env cstr
  | Tapply (_, tyl)
  | Ttuple tyl ->
      check_tconst_list seen env tyl
  | Taccess (root, ids) ->
      let env, ty, seen = expand_ env seen root ids in
      check_tconst seen env ty
  | Tanon _ -> assert false
  | Tunresolved _ -> assert false
  | Tobject -> ()
  | Tshape tym ->
      Nast.ShapeMap.iter (fun _ v -> check_tconst seen env v) tym

and check_tconst_list seen env x =
  List.iter (check_tconst seen env) x

and check_fun_tconst seen env ft =
  check_tconst_tparam_list seen env ft.ft_tparams;
  check_tconst_fun_param_list seen env ft.ft_params;
  (match ft.ft_arity with
    | Fvariadic (_, p) -> check_tconst_fun_param seen env p
    | _ -> ());
  check_tconst seen env ft.ft_ret;
  ()

and check_tconst_fun_param_list seen env x =
  List.iter (check_tconst_fun_param seen env) x

and check_tconst_fun_param seen env (_, ty) =
  check_tconst seen env ty

and check_tconst_tparam_list seen env x =
  List.iter (check_tconst_tparam seen env) x

and check_tconst_tparam seen env (_, _, x) =
  check_tconst_opt seen env x

and check_tconst_opt seen env = function
  | None -> ()
  | Some x -> check_tconst seen env x

(* A type access "this::T" is translated to "<this>::T" during the
 * naming phase. While typing a body, "<this>" is a type hole that needs to
 * be filled with a final concrete type. Resolution is specified in typing.ml,
 * here is a high level break down:
 *
 * 1) When a class member "bar" is accessed via "[CID]->bar" or "[CID]::bar"
 * we resolves "<this>" in the type of "bar" to "<[CID]>"
 *
 * 2) When typing a method, we resolve "<this>" in the return type to
 * "this"
 *
 * 3) When typing a method, we resolve "<this>" in parameters of the
 * function to "<static>" in static methods or "<$this>" in non-static
 * methods
 *
 * More specific details are explained inline
 *)
let fill_with_class_id env cid cid_ty ty =
  (* we use <.*> to indicate an expression dependent type *)
  let fill_name n = "<"^n^">" in
  let pos = Reason.to_pos (fst cid_ty) in
  let env, cid, cid_ty =
    match cid with
    (* In a non-static context, <parent>::T and <static>::T should be
     * compatible with <$this>::T. This is because $this, static, and
     * parent (maybe?) all refer to the same late static bound type.
     *)
    | CIparent | CIstatic when not (Env.is_static env) ->
        let cid = CIvar (pos, This) in
        let env, ty = Env.get_local env this in
        let this_ty =
          Reason.Rwitness pos, Tgeneric (SN.Typehints.this, Some ty) in
        let env, cid_ty = Inst.instantiate_this env cid_ty this_ty in
        env, cid, cid_ty
    | cid -> env, cid, cid_ty
  in
  let filling_ty =
    match cid with
    | CIself | CI _ ->
        cid_ty
    (* For (almost) all expressions we generate a new identifier. In the future,
     * we might be able to do some local analysis to determine if two given
     * expressions refer to the same Late Static Bound Type, but for now we do
     * this since it is easy and sound.
     *)
    | CIvar (p, x) when x <> This ->
        let name = "expr#"^string_of_int(Ident.tmp()) in
        Reason.Rwitness p, Tabstract ((p, fill_name name), [], Some cid_ty)
    | CIvar (p, This) ->
        Reason.Rwitness p, Tabstract ((p, fill_name "$this"), [], Some cid_ty)
    | _ ->
      let name = class_id_to_str cid in
      Reason.Rwitness pos, Tabstract ((pos, fill_name name), [], Some cid_ty)
  in
  fill_type_hole env ty filling_ty

let fill_with_expr env expr expr_ty ty =
  let cid = CIvar expr in
  fill_with_class_id env cid expr_ty ty
