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
  (* Keeps track of all the expansions that occur when expanding a Taccess.
   * This is necessary to check for potential cycles while expanding, as well
   * as providing detailed information in the Reason.t of the resulting type.
   *)
  expansions : string list;
  (* Original reason for the Taccess. This will contain a Pos.t to the place
   * where the Taccess is declared
   *)
  orig_reason : Reason.t;
  (* Whether or not we are expanding a generic type such as "this::T".
   * If we are expanding a generic then when choosing the type of a
   * type constant we use the constraint type rather than the assigned type.
   *)
  is_generic : bool;
  (* Keep track of any Tvars we see to check for potential recursive Tvars *)
  seen_tvar : ISet.t;
  (* This is used when expanding generics so we know what ids need to
   * be appended for the new generic that we derive. See expand_generic for
   * a more detailed example.
   *)
  ids : string list;
}

let empty_env env ety_env reason = {
  tenv = env;
  ety_env = ety_env;
  expansions = [];
  orig_reason = reason;
  is_generic = false;
  seen_tvar = ISet.empty;
  ids = [];
}

let rec expand_with_env ety_env env reason (root, ids) =
  let expand_env = empty_env env ety_env reason in
  let expand_env, ty = expand_ expand_env root ids in
  expand_env.tenv, (expand_env.ety_env, ty)

and expand env r t =
  let ety_env = Phase.env_with_self env in
  let env, (_, ty) = expand_with_env ety_env env r t in
  env, ty

(* The root of a type access is a type. When expanding a type access this type
 * needs to resolve to the name of a class so we can look up if a given type
 * constant is defined in the class.
 *
 * We also need to track additional information in the enviornment as we expand.
 * See the declaration of the type "env" above for all the information we need.
 *)
and expand_ env root_ty ids =
  match ids with
  | [] ->
      (* check for potential cycles *)
      Errors.try_with_error
      (fun () ->
        check_tconst env root_ty;
        let reason = Reason.Rtype_access(
          env.orig_reason,
          List.rev env.expansions,
          fst root_ty
        ) in
        env, (reason, snd root_ty)
      )
      (fun () -> env, (Reason.none, Tany))
  | head :: tail -> begin
      match snd root_ty with
      | Tany ->
          env, root_ty
      | Taccess (root, ids2) ->
          expand_ env root (ids2 @ ids)
      | Toption ty ->
          expand_ env ty ids
      | Tclass ((class_pos, class_name), _) ->
          begin
            try
              let env, ty, ety_env = create_root_from_type_constant
                env class_pos class_name root_ty head in
              (* check for cycles before expanding further *)
              let seen =
                let cur_tconst = List.fold_left (fun acc (_, s) -> acc^"::"^s)
                  (strip_ns class_name) ids in
                if List.mem cur_tconst env.expansions
                then (
                  let seen = List.rev (cur_tconst :: env.expansions) in
                  Errors.cyclic_typeconst (Reason.to_pos (fst ty)) seen;
                  raise Exit
                )
                else cur_tconst :: env.expansions in
              let env =
                if tail = []
                then {
                  env with
                  expansions = seen;
                  ety_env = ety_env;
                }
                else {
                  env with
                  expansions = seen
                } in
              expand_ env ty tail
            with
              Exit -> env, (Reason.none, Tany)
          end
      | Tabstract (ak, Some ty) ->
          let name = strip_ns @@ AbstractKind.to_string ak in
          let env, ty = expand_generic env ak name ty ids in
          (* If it is a expression dependent type we want to explain what
           * expression it was derived from.
           *)
          let reason =
            match ak with
            | AKdependent ((`static | `expr _), _) ->
                let pos = Reason.to_pos @@ fst root_ty in
                Reason.Rexpr_dep_type (fst ty, pos, name)
            | AKgeneric (_, _) | AKnewtype (_, _) | AKdependent (_, _) ->
                fst ty in
          env, (reason, snd ty)
      | Tunresolved tyl ->
          let tyl =
            List.map begin fun ty ->
              snd (expand_ env ty ids)
            end tyl in
          env, (env.orig_reason, Tunresolved tyl)
      | Tvar n ->
          if ISet.mem n env.seen_tvar
          then env, (Reason.none, Tany)
          else (
            let tenv, ty = Env.expand_type env.tenv root_ty in
            let env = { env with
              tenv = tenv;
              seen_tvar = ISet.add n env.seen_tvar;
            } in
            expand_ env ty ids
          )
      | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
      | Tarray (_, _) | Tfun _ | Tabstract (_, _) ->
          let pos, tconst = head in
          let ty = Typing_print.error (snd root_ty) in
          Errors.non_object_member tconst (Reason.to_pos (fst root_ty)) ty pos;
          env, (Reason.none, Tany)
  end

(* When expanding the constraint of a Tabstract we need to modify
 * the environment. While expanding an abstract type we will always choose the
 * constraint of a type constant (the "as" defined type) over the assigned type
 * (the "=" defined type). This is because the type constant could've been
 * overridden by a subclass so we only have the gurantee that the constraint is
 * satisified.
 *
 * We also need to do extra bookkeeping as well. Each time we use the constraint
 * of a type constant we add it to the list "env.ids". This is to ensure we
 * construct the correct generic result. For example:
 *
 * interface I {
 *   const type TnoGeneric = I;
 *   abstract const type TGeneric as I;
 *
 *   public function f(): this::TnoGeneric;
 *   public function g(): this::TGeneric;
 * }
 *
 * When expanding "this::TnoGeneric" the root will be a generic "this as I".
 * We will call 'expand_generic env "this" Tapply "I" ["TnoGeneric"]'. We will
 * continue expanding "I::TnoGeneric". When we look up the type constant we
 * will use the assigned type since there is no constraint. The resulting type
 * will be "I" after expansion is complete.
 *
 * However when expanding "this::TGeneric" there is a constraint on the type
 * constant "I::TGeneric". Since we are in a generic context we must use the
 * constraint type and also store "TGeneric" in "env.ids". When we get back to
 * expand_generic we see that "env.ids" is non-empty so we construct a new
 * generic type by appending the ids in "env.ids" to the name of generic.
 * The resulting type should be
 *
 *    this::TGeneric as I::TGeneric as I
 *)
and expand_generic env ak name root ids =
  let env = { env with
    is_generic = true;
    expansions = List.fold_left (fun acc (_, s) -> acc^"::"^s) name ids
      :: env.expansions;
  } in
  let env, ty = expand_ env root ids in
  if env.ids = [] then
    env, ty
  else
    let dt =
      match ak with
      | AKgeneric (x, _) | AKnewtype (x, _) -> `cls x
      | AKdependent (dt, _) -> dt in
    env, (fst ty, Tabstract (AKdependent (dt, env.ids), Some ty))

(* The function takes a "step" forward in the expansion. We look up the type
 * constant associated with the given class_name and create a new root type.
 * A type constant has both a constraint type and assigned type. The constraint
 * type is always used if env.is_generic is true, otherwise we use the assigned
 * type if one exists.
 *)
and create_root_from_type_constant env class_pos class_name root_ty (pos, tconst) =
  let class_ =
    match Env.get_class env.tenv class_name with
    | None ->
        Errors.unbound_name_typing class_pos class_name;
        raise Exit
    | Some c -> c in
  let typeconst =
    match SMap.get tconst class_.tc_typeconsts with
    | None ->
        Errors.smember_not_found `class_typeconst
          pos (class_.tc_pos, class_name) tconst `no_hint;
        raise Exit
    | Some tc -> tc in
  let ety_env = { env.ety_env with this_ty = root_ty } in
  (match typeconst with
  | { ttc_type = Some ty; _ }
    when typeconst.ttc_constraint = None || (not env.is_generic) ->
      (* It is important to clear the fields in the environment so we don't
       * accidently wrap this into a generic type in expand_generic
       *)
      let tenv, (ety_env, ty) =
        Phase.localize_with_env ~ety_env env.tenv ty in
      { env with ids = []; is_generic = false; tenv = tenv }, ty, ety_env
  | {ttc_constraint = cstr; _} ->
      let tenv, ety_env, cstr =
        match opt (Phase.localize_with_env ~ety_env) env.tenv cstr with
        | tenv, Some (ety_env, cstr) -> tenv, ety_env, Some cstr
        | tenv, None -> tenv, ety_env, None in
      let ty =
        Reason.Rwitness (fst typeconst.ttc_name),
        Tabstract (AKdependent (`cls class_name, [tconst]), cstr) in
      { env with ids = tconst :: env.ids; tenv = tenv }, ty, ety_env
  )

(* Following code checks for cycles that may occur when expanding a Taccess.
 * This is mainly copy-pasta from Typing_tdef.ml. We should see if its possible
 * to provide a more generic cycle detection utility function.
 * *)
and check_tconst env (_, ty) =
  match ty with
  | Tany -> ()
  | Tmixed -> ()
  | Tarray (ty1, ty2) ->
      check_tconst_opt env ty1;
      check_tconst_opt env ty2;
      ()
  | Toption ty -> check_tconst env ty
  | Tprim _ -> ()
  | Tvar _ -> ()
  | Tfun fty -> check_fun_tconst env fty
  | Tabstract (AKgeneric (_, super), cstr) ->
      check_tconst_opt env super;
      check_tconst_opt env cstr
  | Tabstract (AKnewtype (_, tyl), cstr) ->
      check_tconst_list env tyl;
      check_tconst_opt env cstr
  | Tabstract (AKdependent (_, _), cstr) ->
      check_tconst_opt env cstr
  | Ttuple tyl ->
      check_tconst_list env tyl
  | Tunresolved tyl ->
      check_tconst_list env tyl
  | Taccess (root, ids) ->
      let env, ty = expand_ env root ids in
      check_tconst env ty
  | Tanon _ -> assert false
  | Tclass (_, tyl) ->
     check_tconst_list env tyl
  | Tobject -> ()
  | Tshape (_, tym) ->
      Nast.ShapeMap.iter (fun _ v -> check_tconst env v) tym

and check_tconst_list env x =
  List.iter (check_tconst env) x

and check_fun_tconst env ft =
  check_tconst_fun_param_list env ft.ft_params;
  (match ft.ft_arity with
    | Fvariadic (_, p) -> check_tconst_fun_param env p
    | _ -> ());
  check_tconst env ft.ft_ret;
  ()

and check_tconst_fun_param_list env x =
  List.iter (check_tconst_fun_param env) x

and check_tconst_fun_param env (_, ty) =
  check_tconst env ty

and check_tconst_opt env = function
  | None -> ()
  | Some x -> check_tconst env x
