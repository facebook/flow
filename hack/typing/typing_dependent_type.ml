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

  let new_() =
    let eid = Ident.tmp() in
    Reason.ERexpr eid, `expr eid

  let from_cid env reason cid =
    let pos = Reason.to_pos reason in
    let pos, expr_dep_reason, dep = match cid with
      | N.CIparent ->
          (match Env.get_parent env with
          | _, Tapply ((_, cls), _) ->
              pos, Reason.ERparent cls, `cls cls
          | _, _ ->
              let ereason, dep = new_() in
              pos, ereason, dep
          )
      | N.CIself ->
          (match Env.get_self env with
          | _, Tclass ((_, cls), _) ->
              pos, Reason.ERself cls, `cls cls
          | _, _ ->
              let ereason, dep = new_() in
              pos, ereason, dep
          )
      | N.CI (p, cls) ->
          p, Reason.ERclass cls, `cls cls
      | N.CIstatic ->
          pos, Reason.ERstatic, `static
      | N.CIvar (p, N.This) ->
          p, Reason.ERstatic, `static
      (* If it is a local variable then we look up the expression id associated
       * with it. If one doesn't exist we generate a new one. We are being
       * conservative here because the new expression id we create isn't
       * added to the local enviornment.
       *)
      | N.CIvar (p, N.Lvar (_, x)) ->
          let ereason, dep = match Env.get_local_expr_id env x with
            | Some eid -> Reason.ERexpr eid, `expr eid
            | None -> new_() in
          p, ereason, dep
      (* If all else fails we generate a new identifier for our expression
       * dependent type.
       *)
      | N.CIvar (p, _) ->
          let ereason, dep = new_() in
          p, ereason, dep in
    (Reason.Rexpr_dep_type (reason, pos, expr_dep_reason), (dep, []))

  (* Takes the given list of dependent types and applies it to the given
   * locl ty to create a new locl ty
   *)
  let apply dep_tys ty =
    List.fold_left begin fun ty (r, dep_ty) ->
      r, Tabstract (AKdependent dep_ty, Some ty)
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
    | Tclass ((_, x), _) ->
        let class_ = Env.get_class env x in
        Option.value_map class_
          ~default:false
          ~f:(fun {tc_final; _} -> not tc_final)
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
    if should_apply env cid_ty then
      apply [from_cid env (fst cid_ty) cid] cid_ty
    else
      cid_ty
end
