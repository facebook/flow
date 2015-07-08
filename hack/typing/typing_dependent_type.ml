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

module ExprDepTy = struct
  module N = Nast
  module Env = Typing_env

  let to_string dt = AbstractKind.to_string (AKdependent dt)

  let new_() = `expr (Ident.tmp())

  let from_cid env pos cid =
    let p, dep = match cid with
      | N.CIparent ->
          (match Env.get_parent env with
           | _, Tapply ((_, cls), _) -> pos, `cls cls
           | _, _ -> pos, new_()
          )
      | N.CIself ->
          (match Env.get_self env with
           | _, Tclass ((_, cls), _) -> pos, `cls cls
           | _, _ -> pos, new_()
          )
      | N.CI (p, cls) -> p, `cls cls
      | N.CIstatic -> pos, `static
      | N.CIvar (p, N.This) -> p, `static
          (* For almost all expressions we generate a new identifier. In the
           * future, we might be able to do some local analysis to determine
           * if two given expressions refer to the same Late Static Bound Type,
           * but for now we do this since it is easy and sound.
           *)
      | N.CIvar (p, _) -> p, new_() in
    Reason.Rwitness p, (dep, [])

  (* Takes the given list of dependent types and applies it to the given
   * locl ty to create a new locl ty
   *)
  let apply dep_tys ty =
    List.fold_left begin fun ty (r, dep_ty) ->
      (* If it is a expression dependent type we want to explain what
       * expression it was derived from.
       *)
      let reason = match fst dep_ty with
        | `expr _ | `static ->
            (match r with
             | Reason.Rexpr_dep_type (_, pos, name) ->
                 Reason.Rexpr_dep_type (fst ty, pos, name)
             | _ ->
                 let pos = Reason.to_pos r in
                 let name = to_string (fst dep_ty, []) in
                 Reason.Rexpr_dep_type (fst ty, pos, name)
            )
        | _ ->
            fst ty in
      reason, Tabstract (AKdependent dep_ty, Some ty)
    end ty dep_tys

  (* We do not want to create a new expression dependent type if the type is
   * already expression dependent. However if the type is Tunresolved that
   * contains different expression dependent types then we will want to
   * generate a new dependent type. For example:
   *
   *  if ($cond) {
   *    $x = new A(); // Dependent type (`cls '\A')
   *  } else {
   *    $x = new B(); // Dependent type (`cls '\B')
   *  }
   *  $x; // Tunresolved[(`cls '\A', `cls '\B')
   *
   *  // When we call the function below, we need to generate
   *  // A new expression dependent type since
   *  // (`cls '\A') <> (\cls '\B')
   *  $x->expression_dependent_function();
   *)
  let rec should_apply ?(seen=ISet.empty) env (_, ty_ as ty) = match ty_ with
    | Toption ty
    | Tabstract (
        (AKgeneric _
        | AKnewtype _
        | AKdependent (`this, [])
        ), Some ty) ->
        should_apply ~seen env ty
    | Tabstract (AKdependent _, Some _) ->
        false
    | Tvar _ ->
        let env, seen, ty = Env.expand_type_recorded env seen ty in
        should_apply ~seen env ty
    | Tunresolved tyl ->
        List.exists (should_apply ~seen env) tyl
    | Tclass _ ->
        true
    | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
    | Tarray _ | Tfun _ | Tabstract (_, None) | Tany ->
        false

  (****************************************************************************)
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
  (****************************************************************************)
  let make env cid cid_ty =
    let pos = Reason.to_pos (fst cid_ty) in
    if should_apply env cid_ty then
      apply [from_cid env pos cid] cid_ty
    else
      cid_ty
end
