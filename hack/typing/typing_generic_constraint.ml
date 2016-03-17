(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

module TUtils = Typing_utils
module N = Nast
module Reason = Typing_reason
module Env = Typing_env

let check_constraint env ck cstr_ty ty =
  let env, ety = Env.expand_type env ty in
  let env, ecstr_ty = Env.expand_type env cstr_ty in
  match snd ecstr_ty, snd ety with
  | _, Tany ->
      (* This branch is only reached when we have an unbound type variable,
       * when this is the case, the constraint should always succeed.
       *)
      env
  | Tany, _ -> fst (TUtils.unify env cstr_ty ty)
  | (Tmixed | Tarraykind _ | Tprim _ | Toption _ | Tvar _
    | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tunresolved _ | Tobject | Tshape _
    ), _ -> begin
        match ck with
        | Ast.Constraint_as ->
            TUtils.sub_type env cstr_ty ty
        | Ast.Constraint_super ->
            (* If cstr_ty is a Tvar, we don't want to unify that Tvar with
             * ty; we merely want the constraint itself to be added to the
             * ty's list of unresolved types. Thus we pass the expanded
             * constraint type. *)
            TUtils.sub_type env ty ecstr_ty
      end

let add_check_constraint_todo env reason generic ck cstr_ty ty =
  Env.add_todo env begin fun env ->
    Errors.try_
      (fun () -> check_constraint env ck cstr_ty ty)
      (fun l ->
       Reason.explain_generic_constraint env.Env.pos reason generic l;
       env
      )
  end
