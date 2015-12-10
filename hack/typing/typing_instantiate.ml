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
open Typing_defs
open Utils

module SN     = Naming_special_names
module TSubst = Typing_subst
module TUtils = Typing_utils
module Phase  = Typing_phase

type subst = decl ty SMap.t

let make_subst tparams tyl =  TSubst.make tparams tyl

(*****************************************************************************)
(* Code dealing with instantiation. *)
(*****************************************************************************)

let rec instantiate subst (r, ty: decl ty) =
  (* PERF: If subst is empty then instantiation is a no-op. We can save a
   * significant amount of CPU by avoiding recursively deconstructing the ty
   * data type.
   *)
  if SMap.is_empty subst then (r, ty) else
  match ty with
  | Tgeneric (x, cstr_opt) ->
      (match SMap.get x subst with
      | Some x_ty ->
          (Reason.Rinstantiate (fst x_ty, x, r), snd x_ty)
      | None -> begin
          match cstr_opt with
          | Some (ck, ty) ->
              let ty = instantiate subst ty in
              (r, Tgeneric (x, Some (ck, ty)))
          | None -> (r, ty)
        end
      )
  | _ ->
      let ty = instantiate_ subst ty in
      (r, ty)

and instantiate_ subst x =
  match x with
  | Tgeneric _ -> assert false
  (* IMPORTANT: We cannot expand Taccess during instantiation because this can
   * be called before all type consts have been declared and inherited
   *)
  | Taccess (ty, ids) ->
      let ty = instantiate subst ty in
      Taccess (ty, ids)
  | Tarray (ty1, ty2) ->
      let ty1 = Option.map ty1 (instantiate subst) in
      let ty2 = Option.map ty2 (instantiate subst) in
      Tarray (ty1, ty2)
  | Tthis -> Tthis
  | Tmixed -> Tmixed
  | Tany
  | Tprim _ as x -> x
  | Ttuple tyl ->
      let tyl = List.map tyl (instantiate subst) in
      Ttuple tyl
  | Toption ty ->
      let ty = instantiate subst ty in
      (* we want to avoid double option: ??T *)
      (match ty with
      | _, Toption _ -> snd ty
      | _ -> Toption ty
      )
  | Tfun ft ->
      let subst = List.fold_left ~f:begin fun subst (_, (_, x), _) ->
        SMap.remove x subst
      end ~init:subst ft.ft_tparams in
      let params = List.map ft.ft_params begin fun (name, param) ->
        let param = instantiate subst param in
        (name, param)
      end in
      let arity = match ft.ft_arity with
        | Fvariadic (min, (name, var_ty)) ->
          let var_ty = instantiate subst var_ty in
          Fvariadic (min, (name, var_ty))
        | Fellipsis _ | Fstandard _ as x -> x
      in
      let ret = instantiate subst ft.ft_ret in
      Tfun { ft with ft_arity = arity; ft_params = params; ft_ret = ret }
  | Tapply (x, tyl) ->
      let tyl = List.map tyl (instantiate subst) in
      Tapply (x, tyl)
  | Tshape (fields_known, fdm) ->
      let fdm = Nast.ShapeMap.map (instantiate subst) fdm in
      Tshape (fields_known, fdm)

let instantiate_ce subst ({ ce_type = x; _ } as ce) =
  let x = instantiate subst x in
  { ce with ce_type = x }

let instantiate_typeconst subst (
  { ttc_constraint = x; ttc_type = y; _ } as tc) =
    let x = Option.map x (instantiate subst) in
    let y = Option.map y (instantiate subst) in
    { tc with ttc_constraint = x; ttc_type = y }
