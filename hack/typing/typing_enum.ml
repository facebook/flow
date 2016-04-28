(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Module used to enforce that Enum subclasses are used reasonably.
 * Exports the Enum type as the type of all constants, checks that constants
 * have the proper type, and restricts what types can be used for enums.
 *)
(*****************************************************************************)
open Core
open Nast
open Typing_defs
open Utils

module SN = Naming_special_names
module Phase = Typing_phase

let member_type env member_ce =
  let default_result = member_ce.ce_type in
  if not member_ce.ce_is_xhp_attr then default_result
  else match default_result with
    | _, Tapply (enum_id, _)->
      (* XHP attribute type transform is necessary to account for
       * non-first class Enums:

       * attribute MyEnum x; // declaration: MyEnum
       * $this->:x;          // usage: MyEnumType
       *)
      let maybe_enum = Typing_env.get_class env (snd enum_id) in
      (match maybe_enum with
        | None -> default_result
        | Some tc ->
          (match Decl_enum.is_enum (tc.tc_pos, tc.tc_name)
              tc.tc_enum_type tc.tc_ancestors with
                | None -> default_result
                | Some (_base, (_, enum_ty), _constraint) ->
                  let ty = (fst default_result), enum_ty in
                  ty
          ))
    | _ -> default_result

(* Check that a type is something that can be used as an array index
 * (int or string), blowing through typedefs to do it. Takes a function
 * to call to report the error if it isn't. *)
let check_valid_array_key_type f_fail ~allow_any:allow_any env p t =
  let ety_env = Phase.env_with_self env in
  let env, (r, t'), trail =
    Typing_tdef.force_expand_typedef ~ety_env env t in
  (match t' with
    | Tprim (Tint | Tstring) -> ()
    (* Enums have to be valid array keys *)
    | Tabstract (AKenum _, _) -> ()
    | Tany when allow_any -> ()
    | Tany | Tmixed | Tarraykind _ | Tprim _ | Toption _
      | Tvar _ | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
      | Tfun _ | Tunresolved _ | Tobject | Tshape _ ->
        f_fail p (Reason.to_pos r) (Typing_print.error t') trail);
  env

let enum_check_const ty_exp env (_, (p, _), _) t =
  (* Constants need to be subtypes of the enum type *)
  let env = Typing_ops.sub_type p Reason.URenum env ty_exp t in
  (* Make sure the underlying type of the constant is an int
   * or a string. This matters because we need to only allow
   * int and string constants (since only they can be array
   * indexes). *)
  (* Need to allow Tany, since we might not have the types *)
  check_valid_array_key_type
    Errors.enum_constant_type_bad
    ~allow_any:true env p t

(* If a class is a subclass of Enum<T>, check that the types of all of
 * the constants are compatible with T.
 * Also make sure that T is either int, string, or mixed (or an
 * abstract type that is one of those under the hood), that all
 * constants are ints or strings when T is mixed, and that any type
 * hints are compatible with the type. *)
let enum_class_check env tc consts const_types =
  let enum_info_opt =
    Decl_enum.is_enum (tc.tc_pos, tc.tc_name) tc.tc_enum_type
    tc.tc_ancestors in
  match enum_info_opt with
    | Some (ty_exp, _, ty_constraint) ->
        let ety_env = Phase.env_with_self env in
        let env, ty_exp = Phase.localize ~ety_env env ty_exp in
        let env, (r, ty_exp'), trail =
          Typing_tdef.force_expand_typedef ~ety_env env ty_exp in
        (match ty_exp' with
          (* We disallow first-class enums from being non-exact types, because
           * a switch on such an enum can lead to very unexpected results,
           * since switch uses == equality. *)
          | Tmixed | Tprim Tarraykey when tc.tc_enum_type <> None ->
              Errors.enum_type_bad (Reason.to_pos r)
                (Typing_print.error ty_exp') trail
          (* We disallow typedefs that point to mixed *)
          | Tmixed when snd ty_exp <> Tmixed ->
              Errors.enum_type_typedef_mixed (Reason.to_pos r)
          | Tmixed -> ()
          | Tprim Tint | Tprim Tstring | Tprim Tarraykey -> ()
          (* Allow enums in terms of other enums *)
          | Tabstract (AKenum _, _) -> ()
          (* Don't tell anyone, but we allow type params too, since there are
           * Enum subclasses that need to do that *)
          | Tabstract (AKgeneric _, _) -> ()
          | Tany | Tarraykind _ | Tprim _ | Toption _ | Tvar _
            | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
            | Tunresolved _ | Tobject | Tfun _ | Tshape _ ->
              Errors.enum_type_bad (Reason.to_pos r)
                (Typing_print.error ty_exp') trail);

        (* Make sure that if a constraint was given that the base type is
         * actually a subtype of it. *)
        let env = (match ty_constraint with
          | Some ty ->
             let env, ty = Phase.localize ~ety_env env ty in
             Typing_ops.sub_type tc.tc_pos Reason.URenum_cstr env ty ty_exp
          | None -> env) in

        List.fold2_exn ~f:(enum_check_const ty_exp) ~init:env consts const_types

    | None -> env

let get_constant tc (seen, has_default) = function
  | Default _ -> (seen, true)
  | Case ((pos, Class_const (CI (_, cls), (_, const))), _) ->
    if cls <> tc.tc_name then
      (Errors.enum_switch_wrong_class pos (strip_ns tc.tc_name) (strip_ns cls);
       (seen, has_default))
    else
      (match SMap.get const seen with
        | None -> (SMap.add const pos seen, has_default)
        | Some old_pos ->
          Errors.enum_switch_redundant const old_pos pos;
          (seen, has_default))
  | Case ((pos, _), _) ->
    Errors.enum_switch_not_const pos;
    (seen, has_default)

let check_enum_exhaustiveness pos tc caselist =
  let (seen, has_default) =
    List.fold_left ~f:(get_constant tc) ~init:(SMap.empty, false) caselist in
  let consts = SMap.remove SN.Members.mClass tc.tc_consts in
  let all_cases_handled = SMap.cardinal seen = SMap.cardinal consts in
  match (all_cases_handled, has_default) with
    | false, false ->
      let const_list = SMap.keys consts in
      let unhandled =
        List.filter const_list (function k -> not (SMap.mem k seen)) in
      Errors.enum_switch_nonexhaustive pos unhandled tc.tc_pos
    | true, true -> Errors.enum_switch_redundant_default pos tc.tc_pos
    | _ -> ()
