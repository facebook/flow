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

module N = Nast
module SN = Naming_special_names
module Reason = Typing_reason
module Env = Typing_env
module ShapeMap = Nast.ShapeMap

(*****************************************************************************)
(* Importing what is necessary *)
(*****************************************************************************)

let not_implemented _ = failwith "Function not implemented"

type expand_typedef =
    expand_env -> Env.env -> Reason.t -> string -> locl ty list -> Env.env * ety
let (expand_typedef_ref : expand_typedef ref) = ref not_implemented
let expand_typedef x = !expand_typedef_ref x

type unify = Env.env -> locl ty -> locl ty -> Env.env * locl ty
let (unify_ref: unify ref) = ref not_implemented
let unify x = !unify_ref x

type sub_type = Env.env -> locl ty -> locl ty -> Env.env
let (sub_type_ref: sub_type ref) = ref not_implemented
let sub_type x = !sub_type_ref x

type expand_typeconst =
  expand_env -> Env.env -> Reason.t -> locl ty -> Nast.sid list ->
  Env.env * ety
let (expand_typeconst_ref: expand_typeconst ref) = ref not_implemented
let expand_typeconst x = !expand_typeconst_ref x

(* Convenience function for creating `this` types *)
let this_of ty = Tabstract (AKdependent (`this, []), Some ty)

(*****************************************************************************)
(* Returns true if a type is optional *)
(*****************************************************************************)

let rec is_option env ty =
  let _, ety = Env.expand_type env ty in
  match snd ety with
  | Toption _ -> true
  | Tunresolved tyl ->
      List.exists (is_option env) tyl
  | _ -> false

let is_class ty = match snd ty with
  | Tclass _ -> true
  | _ -> false

(*****************************************************************************)
(* Gets the base type of an abstract type *)
(*****************************************************************************)

let rec get_base_type ty = match snd ty with
  | Tabstract (AKnewtype (classname, _), _) when
      classname = SN.Classes.cClassname -> ty
  | Tabstract (_, Some ty) -> get_base_type ty
  | _ -> ty

(*****************************************************************************)
(* Unification error *)
(*****************************************************************************)
let uerror r1 ty1 r2 ty2 =
  let ty1 = Typing_print.error ty1 in
  let ty2 = Typing_print.error ty2 in
  Errors.unify_error
    (Reason.to_string ("This is " ^ ty1) r1)
    (Reason.to_string ("It is incompatible with " ^ ty2) r2)

(* We attempt to simplify the unification error to see if it can be
 * explained without referring to dependent types.
 *)
let simplified_uerror env ty1 ty2 =
  (* Need this check to ensure we don't enter an infiinite loop *)
  let simplify = match snd ty1, snd ty2 with
    | Tabstract (AKdependent (`static, []), _), Tclass _
    | Tclass _, Tabstract (AKdependent (`static, []), _) -> false
    | Tabstract (AKdependent _, Some _), _
    | _, Tabstract (AKdependent _, Some _) -> true
    | _, _ -> false in
  (* We unify the base types to see if that produces an error, if not then
   * we use the standard unification error
   *)
  if simplify then
    Errors.must_error
      (fun _ -> ignore @@ unify env (get_base_type ty1) (get_base_type ty2))
      (fun _ -> uerror (fst ty1) (snd ty1) (fst ty2) (snd ty2))
  else
    uerror (fst ty1) (snd ty1) (fst ty2) (snd ty2)

let process_static_find_ref cid mid =
  match cid with
  | Nast.CI c ->
    Typing_hooks.dispatch_class_id_hook c (Some mid);
  | _ -> ()

(* Find the first defined position in a list of types *)
let rec find_pos p_default tyl =
  match tyl with
  | [] -> p_default
  | (r, _) :: rl ->
      let p = Reason.to_pos r in
      if p = Pos.none
      then find_pos p_default rl
      else p

(*****************************************************************************)
(* Applies a function to 2 shapes simultaneously, raises an error if
 * the second argument has less fields than the first.
 *)
(*****************************************************************************)

let get_printable_shape_field_name = Env.get_shape_field_name

(* This is used in subtyping and unification. *)
let apply_shape ~on_common_field ~on_missing_optional_field (env, acc)
  (r1, fields_known1, fdm1) (r2, fields_known2, fdm2) =
  begin match fields_known1, fields_known2 with
    | FieldsFullyKnown, FieldsPartiallyKnown _  ->
        let pos1 = Reason.to_pos r1 in
        let pos2 = Reason.to_pos r2 in
        Errors.shape_fields_unknown pos2 pos1
    | FieldsPartiallyKnown unset_fields1,
      FieldsPartiallyKnown unset_fields2 ->
        ShapeMap.iter begin fun name unset_pos ->
          match ShapeMap.get name unset_fields2 with
            | Some _ -> ()
            | None ->
                let pos2 = Reason.to_pos r2 in
                Errors.shape_field_unset unset_pos pos2
                  (get_printable_shape_field_name name);
        end unset_fields1
    | _ -> ()
  end;
  ShapeMap.fold begin fun name ty1 (env, acc) ->
    match ShapeMap.get name fdm2 with
    | None when is_option env ty1 ->
        let can_omit = match fields_known2 with
          | FieldsFullyKnown -> true
          | FieldsPartiallyKnown unset_fields ->
              ShapeMap.mem name unset_fields in
        if can_omit then
          on_missing_optional_field (env, acc) name ty1
        else
          let pos1 = Reason.to_pos r1 in
          let pos2 = Reason.to_pos r2 in
          Errors.missing_optional_field pos2 pos1
            (get_printable_shape_field_name name);
          (env, acc)
    | None ->
        let pos1 = Reason.to_pos r1 in
        let pos2 = Reason.to_pos r2 in
        Errors.missing_field pos2 pos1 (get_printable_shape_field_name name);
        (env, acc)
    | Some ty2 ->
        on_common_field (env, acc) name ty1 ty2
  end fdm1 (env, acc)

and shape_field_name p field =
  let open Nast in match field with
    | String name -> SFlit name
    | Class_const (CI x, y) -> SFclass_const (x, y)
    | _ -> Errors.invalid_shape_field_name p;
      SFlit (p, "")

(*****************************************************************************)
(* Try to unify all the types in a intersection *)
(*****************************************************************************)
let flatten_unresolved env ty acc =
  let env, ety = Env.expand_type env ty in
  let res = match ety with
    (* flatten Tunresolved[Tunresolved[...]] *)
    | (_, Tunresolved tyl) -> tyl @ acc
    | _ -> ty :: acc in
  env, res

let rec member_inter env ty tyl acc =
  match tyl with
  | [] -> env, ty :: acc
  | x :: rl ->
      Errors.try_
        begin fun () ->
          let env, ty = unify env x ty in
          env, List.rev_append acc (ty :: rl)
        end
        begin fun _ ->
          member_inter env ty rl (x :: acc)
        end

and normalize_inter env tyl1 tyl2 =
  match tyl1 with
  | [] -> env, tyl2
  | x :: rl ->
      let env, tyl2 = member_inter env x tyl2 [] in
      normalize_inter env rl tyl2

(*****************************************************************************)
(* *)
(*****************************************************************************)

let in_var env ty =
  let res = Env.fresh_type() in
  let env, res = unify env ty res in
  env, res

let unresolved_tparam env (_, (pos, _), _) =
  let reason = Reason.Rwitness pos in
  in_var env (reason, Tunresolved [])

(*****************************************************************************)
(*****************************************************************************)

(* Try to unify all the types in a intersection *)
let fold_unresolved env ty =
  let env, ety = Env.expand_type env ty in
  match ety with
  | r, Tunresolved [] -> env, (r, Tany)
  | _, Tunresolved [x] -> env, x
  | _, Tunresolved (x :: rl) ->
      (try
        let env, acc =
          List.fold_left begin fun (env, acc) ty ->
            Errors.try_ (fun () -> unify env acc ty) (fun _ -> raise Exit)
          end (env, x) rl in
        env, acc
      with Exit ->
        env, ty
      )
  | _ -> env, ty

(*****************************************************************************)
(* *)
(*****************************************************************************)

let string_of_visibility = function
  | Vpublic  -> "public"
  | Vprivate _ -> "private"
  | Vprotected _ -> "protected"

let unresolved env ty =
  let env, ety = Env.expand_type env ty in
  match ety with
  | _, Tunresolved _ -> in_var env ety
  | _ -> in_var env (fst ty, Tunresolved [ty])

(*****************************************************************************)
(* Function checking if an array is used as a tuple *)
(*****************************************************************************)

let is_array_as_tuple env ty =
  let env, ety = Env.expand_type env ty in
  let env, ety = fold_unresolved env ety in
  match ety with
  | _, Tarray (Some elt_type, None) ->
      let env, normalized_elt_ty = Env.expand_type env elt_type in
      let _env, normalized_elt_ty = fold_unresolved env normalized_elt_ty in
      (match normalized_elt_ty with
      | _, Tunresolved _ -> true
      | _ -> false
      )
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _
    | Tvar _ | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
    | Tfun _ | Tunresolved _ | Tobject | Tshape _) -> false

(*****************************************************************************)
(* Keep the most restrictive visibility (private < protected < public).
 * This is useful when dealing with unresolved types.
 * When there are several candidates for a given visibility we need to be
 * conservative and consider the most restrictive one.
 *)
(*****************************************************************************)

let min_vis vis1 vis2 =
  match vis1, vis2 with
  | x, Vpublic | Vpublic, x -> x
  | Vprotected _, x | x, Vprotected _ -> x
  | Vprivate _ as vis, Vprivate _ -> vis

let min_vis_opt vis_opt1 vis_opt2 =
  match vis_opt1, vis_opt2 with
  | None, x | x, None -> x
  | Some (pos1, x), Some (pos2, y) ->
      let pos = if pos1 = Pos.none then pos2 else pos1 in
      Some (pos, min_vis x y)

(*****************************************************************************)
(* Check if a type is not fully constrained *)
(*****************************************************************************)

module HasTany : sig
  val check: locl ty -> bool
end = struct
  let visitor =
    object(this)
      inherit [bool] TypeVisitor.type_visitor
      method! on_tany _ = true
      method! on_tarray acc ty1_opt ty2_opt =
        (* Check for array without its value type parameter specified *)
        (match ty2_opt with
        | None -> true
        | Some ty -> this#on_type acc ty) ||
        (Option.fold ~f:this#on_type ~init:acc ty1_opt)
    end
  let check ty = visitor#on_type false ty
end
