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
open Typing_dependent_type
open Utils

module TUtils = Typing_utils
module Reason = Typing_reason
module Env = Typing_env
module Inst = Typing_instantiate
module SN = Naming_special_names
module TGen = Typing_generic
module Phase = Typing_phase

type env = {
  tenv : Env.env;
  ety_env : Phase.env;
  (* A trail of all the type constants we have expanded. Used primarily for
   * error reporting
   *)
  trail : dependent_type list;
  (* Keep track of any Tvars we see to check for potential recursive Tvars *)
  seen_tvar : ISet.t;
  (* A list of dependent we have encountered while expanding a type constant.
   * After expanding a type constant we can choose either the assigned type or
   * the constrained type. If we choose the assigned type, the result will not
   * be expression dependent so this list will be set to empty. However, if it
   * is the constrained type then the final type will also be a dependent type.
   *)
  dep_tys : (Reason.t * dependent_type) list;
  (* The remaining type constants we need to expand *)
  ids : Nast.sid list;
}

let empty_env env ety_env ids = {
  tenv = env;
  ety_env = ety_env;
  trail = [];
  seen_tvar = ISet.empty;
  dep_tys = [];
  ids = ids;
}

let rec expand_with_env ety_env env reason root ids =
  let expand_env = empty_env env ety_env ids in
  let env, ty = expand_ expand_env root in
  let reason = Reason.Rtype_access(
    reason,
    env.trail
    |> List.rev
    |> List.map (compose strip_ns ExprDepTy.to_string ),
    fst ty
  ) in
  let ty = reason, snd ty in
  expand_env.tenv, (expand_env.ety_env, ty)

and expand env r (root, ids) =
  let ety_env = Phase.env_with_self env in
  let env, (ety_env, root) = Phase.localize_with_env ~ety_env env root in
  let env, (_, ty) = expand_with_env ety_env env r root ids in
  env, ty

(* The root of a type access is a type. When expanding a type access this type
 * needs to resolve to the name of a class so we can look up if a given type
 * constant is defined in the class.
 *
 * We also need to track what expansions have already taken place to make sure
 * we do not recurse infinitely.
 *)
and expand_ env (root_reason, root_ty as root) =
  match env.ids with
  | [] ->
      env, make_dependent_type env.dep_tys root
  | head::tail -> begin match root_ty with
      | Tany -> env, root
      | Tabstract (AKnewtype (_, _), Some ty) | Toption ty -> expand_ env ty
      | Tclass ((class_pos, class_name), _) ->
          let env, ty =
            create_root_from_type_constant
              env class_pos class_name root head in
          expand_ { env with ids = tail } ty
      | Tabstract (AKgeneric (s, _), Some ty) ->
          (* Expanding a generic creates a dependent type *)
          let dep_ty = `cls s, [] in
          let env =
            { env with
              dep_tys = (root_reason, dep_ty)::env.dep_tys } in
          expand_ env ty
      | Tabstract (AKdependent dep_ty, Some ty) ->
          let env =
            { env with
              dep_tys = (root_reason, dep_ty)::env.dep_tys } in
          expand_ env ty
      | Tunresolved tyl ->
          let env, tyl = lfold begin fun prev_env ty ->
            let env, ty = expand_ env ty in
            { prev_env with tenv = env.tenv }, ty
          end env tyl in
          env, (root_reason, Tunresolved tyl)
      | Tvar _ ->
          let tenv, seen, ty =
            Env.expand_type_recorded env.tenv env.seen_tvar root in
          let env = { env with tenv = tenv; seen_tvar = seen } in
          expand_ env ty
      | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
      | Tarray (_, _) | Tfun _ | Tabstract (_, _) ->
          let pos, tconst = head in
          let ty = Typing_print.error root_ty in
          Errors.non_object_member tconst (Reason.to_pos root_reason) ty pos;
          env, (root_reason, Tany)
     end

(* The function takes a "step" forward in the expansion. We look up the type
 * constant associated with the given class_name and create a new root type.
 * A type constant has both a constraint type and assigned type. Which one we
 * choose depends on if there are any dependent types set in the environment.
 * If the root type is not a dependent type then we choose the assigned type,
 * otherwise we choose the constraint type. If there is no constraint type then
 * we choose the assigned type.
 *)
and create_root_from_type_constant env class_pos class_name root_ty (pos, tconst) =
  match get_typeconst env class_pos class_name pos tconst with
  | None -> env, (fst root_ty, Tany)
  | Some (ety_env, typeconst) ->
      let env =
        { env with
          trail = (`cls class_name, List.map snd env.ids)::env.trail } in
      let ety_env = { ety_env with this_ty = root_ty; from_class = None } in
      begin
        match typeconst with
        | { ttc_type = Some ty; _ }
            when typeconst.ttc_constraint = None || env.dep_tys = [] ->
            (* We are choosing the assigned type. The assigned type itself
             * may contain a 'this' type so we need to change the this_ty in
             * the environment to be the root as a dependent type.
             *)
            let ety_env =
              { ety_env with
                this_ty = make_dependent_type env.dep_tys root_ty;
                from_class = None; } in
            let tenv, ty = Phase.localize ~ety_env env.tenv ty in
            { env with dep_tys = []; tenv = tenv }, ty
        | {ttc_constraint = Some cstr; _} ->
            let tenv, cstr = Phase.localize ~ety_env env.tenv cstr in
            let dep_ty = Reason.Rwitness (fst typeconst.ttc_name),
                         (`cls class_name, [tconst]) in
            (* Append the name of the expanded type constant to each dependent
             * type.
             *)
            let dep_tys =
              List.map (fun (r, (d, s)) -> r, (d, s @ [tconst])) env.dep_tys in
            { env with dep_tys = dep_ty::dep_tys; tenv = tenv }, cstr
        | _ ->
            let ty =
              Reason.Rwitness (fst typeconst.ttc_name),
              Tabstract (AKdependent (`cls class_name, [tconst]), None) in
            let dep_tys =
              List.map (fun (r, (d, s)) -> r, (d, s @ [tconst])) env.dep_tys in
            { env with dep_tys = dep_tys }, ty
      end

(* Looks up the type constant within the given class. This also checks for
 * potential cycles by examining the expansions we have already performed.
 *)
and get_typeconst env class_pos class_name pos tconst =
  try
    let class_ = match Env.get_class env.tenv class_name with
      | None ->
          Errors.unbound_name_typing class_pos class_name;
          raise Exit
      | Some c -> c in
    let typeconst = match SMap.get tconst class_.tc_typeconsts with
      | None ->
          Errors.smember_not_found
            `class_typeconst pos (class_.tc_pos, class_name) tconst `no_hint;
          raise Exit
      | Some tc -> tc in
    (* Check for cycles. We do this by combining the name of the current class
     * with the remaining ids that we need to expand. If we encounter the same
     * class name + ids that means we have entered a cycle.
     *)
    let cur_tconst = `cls class_name, List.map snd env.ids in
    let seen = ExprDepTy.to_string cur_tconst in
    let expansions = (pos, seen)::env.ety_env.type_expansions in
    if List.mem seen (List.map snd env.ety_env.type_expansions) then
      begin
        let seen = expansions |> List.map snd |> List.rev in
        Errors.cyclic_typeconst (fst typeconst.ttc_name) seen;
        raise Exit
      end;
    Some ({ env.ety_env with type_expansions = expansions }, typeconst)
  with
    Exit -> None

and make_dependent_type dep_tys ty =
  List.fold_left begin fun ty (r, dep_ty) ->
    (* If it is a expression dependent type we want to explain what
     * expression it was derived from.
     *)
    let reason = match fst dep_ty with
      | `expr _ | `static ->
          let pos = Reason.to_pos r in
          let name = ExprDepTy.to_string (fst dep_ty, []) in
          Reason.Rexpr_dep_type (fst ty, pos, name)
      | _ ->
          fst ty in
    reason, Tabstract (AKdependent dep_ty, Some ty)
  end ty dep_tys

(*****************************************************************************)
(* Exporting *)
(*****************************************************************************)

let () = Typing_utils.expand_typeconst_ref := expand_with_env
