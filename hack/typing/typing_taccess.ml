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

let rec expand env (root, ids) =
  try expand_ env root ids with
  | Exit ->
      env, (Reason.none, Tany)

and expand_ env root_ty ids =
  let root_pos = Reason.to_pos (fst root_ty) in
  match ids with
  | [] ->
      env, root_ty
  | (pos, tconst) :: rest ->
      let generic_names, class_name = root_to_class_name env root_ty ids in
      let root_class =
        match Env.get_class env class_name with
        | None ->
            Errors.unbound_name_typing (root_pos) class_name;
            raise Exit
        | Some c -> c
      in
      let typeconst =
        match SMap.get tconst root_class.tc_typeconsts with
        | None ->
            Errors.smember_not_found
              `class_typeconst pos (root_pos, class_name) tconst `no_hint;
            raise Exit
        | Some tc -> tc
      in
      let tconst_ty =
        match generic_names, typeconst with
        | _, {ttc_type = Some ty; ttc_constraint = None; _}
        | [], {ttc_type = Some ty; _} -> ty
        | names, {ttc_constraint = ty; _} ->
            List.fold_right (fun n acc ->
              Reason.Rnone, Tgeneric (n^"::"^tconst, Some acc)
            ) names (Reason.Rnone, Tgeneric (class_name^"::"^tconst, ty))
      in
      let env, tconst_ty = fill_type_hole env tconst_ty root_ty in
      let tconst_ty = Reason.Rwitness (Pos.btw root_pos pos), snd tconst_ty in
      expand_ env tconst_ty rest

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
 and root_to_class_name env (r, ty) ids =
  let root_to_cname env type_ = root_to_class_name env type_ ids in
  match ty with
  | Tapply ((_, tdef), tyl) when Env.is_typedef tdef ->
      let env, ty = TUtils.expand_typedef env r tdef tyl in
      root_to_cname env ty
  | Tapply ((_, class_name), _) -> [], class_name
  | Taccess (root, ids) ->
      let env, fty = expand_ env root ids in
      root_to_cname env fty
  | Toption ty | Tabstract ((_, _), _, Some ty) ->
      root_to_cname env ty
  (* If we haven't filled the type hole at this point we fill it with self *)
  | Tgeneric (x, _) when x = SN.Typehints.type_hole ->
      let names, class_name = root_to_cname env (Env.get_self env) in
      SN.Typehints.this::names, class_name
  | Tgeneric (x, Some ty) ->
      let names, class_name = root_to_cname env ty in
      x::names, class_name
  | Tany
  | Tvar _ | Tunresolved _
  | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
  | Tarray (_, _) | Tfun _ | Tabstract (_, _, _) | Tgeneric (_, _) ->
      let pos = Reason.to_pos r in
      let str = Typing_print.full env (r, ty) in
      Errors.unbound_name_typing pos str;
      raise Exit

(* A type access "this::T" is translated to "[unknown]::T" during the
 * naming phase. While typing a body, "[unknown]" is a type hole that needs to
 * be filled with a final concrete type. Resolution is specified in typing.ml,
 * here is a high level break down:
 *
 * 1) When a class member "bar" is accessed via "[CID]->bar" or "[CID]::bar"
 * we resolves "[unknown]" in the type of "bar" to "[CID]"
 *
 * 2) When typing a method, we resolve "[unknown]" in the return type to
 * "this"
 *
 * 3) When typing a method, we resolve "[unknown]" in parameters of the
 * function to "static" in static methods or "$this" in non-static methods
 *
 * More specific details are explained inline
 *)
let fill_with_class_id env cid cid_ty ty =
  let pos = Reason.to_pos (fst cid_ty) in
  let env, cid, cid_ty =
    match cid with
    (* In a non-static context, parent::T and static::T should be compatible
     * with $this::T. This is because $this, static, and parent (maybe?) all
     * refer to the same late static bound type.
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
        let name = "$"^string_of_int(Ident.tmp()) in
        Reason.Rwitness p, Tgeneric (name, Some cid_ty)
    | _ ->
      let name = class_id_to_str cid in
      Reason.Rwitness pos, Tgeneric (name, Some cid_ty)
  in
  fill_type_hole env ty filling_ty

let fill_with_expr env expr expr_ty ty =
  let cid = CIvar expr in
  fill_with_class_id env cid expr_ty ty
