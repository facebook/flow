(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs

module TUtils = Typing_utils
module N = Nast
module SN = Naming_special_names
module Reason = Typing_reason
module Env = Typing_env
module ShapeMap = Nast.ShapeMap

(*****************************************************************************)
(* Builds a substitution out of a list of type parameters and a list of types.
 *
 * Typical use-case:
 *   class Y<T> { ... }
 *   class X extends Y<int>
 *
 * To build the type of X, we need to replace all the occurrences of T in Y by
 * int. The function make_subst, builds the substitution (the map associating
 * types to a type parameter name), in this case, it would build the map(T =>
 * int).
 *)
(*****************************************************************************)

let make tparams tyl =
  (* We tolerate missing types in silent_mode. When that happens, we bind
   * all the parameters we can, and bind the remaining ones to "Tany".
   *)
  let make_subst_tparam subst tyl (_, (_, tparam_name), _) =
    let ty =
      match !tyl with
      | [] -> Reason.Rnone, Tany
      | ty :: rl -> tyl := rl; ty
    in
    subst := SMap.add tparam_name ty !subst
  in
  let subst = ref SMap.empty in
  let tyl = ref tyl in
  List.iter (make_subst_tparam subst tyl) tparams;
  !subst

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
  | (Tmixed | Tarray (_, _) | Tprim _ | Toption _ | Tvar _
    | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tunresolved _ | Tobject | Tshape _
    ), _ -> begin
        match ck with
        | Ast.Constraint_as ->
            TUtils.sub_type env cstr_ty ty
        | Ast.Constraint_super ->
            (* invert_grow_super is intentionally not used here. Consider
             * the following:
             *
             * class A {}
             * class B extends A {}
             * class C extends A {}
             * class Foo<T> {
             *   public function bar<Tu super T>(Tu $x): Tu {}
             * }
             * function f(Foo<C> $x, B $y): A {
             *   return $x->bar($y);
             * }
             *
             * C is not a supertype of B. However, this doesn't mean the
             * constraint is violated: All's good if Tu = A. Figuring out the
             * most specific supertype is expensive, though, so we just put the
             * constraint into a Tunresolved. (Tunresolved only grows if
             * grow_super is true.) The return type hint provides the
             * supertype we want, and we just have to check that the hint is
             * consistent with all the types in the Tunresolved.
             *
             * Note that since Tu = mixed is always a solution, a `super`
             * constraint itself should never create a type error. That is,
             * it should only result in the type growing as a Tunresolved.
             * Errors will only arise when we encounter conflicting type hints.
             * Thus we ensure that ty is wrapped in a Tunresolved here.
             *)
            let env = match ty, ety with
              | (_, Tvar _), (_, Tunresolved _) -> env
              | (_, Tvar n), (r, _) ->
                  let ety = r, Tunresolved [ety] in
                  let env = Env.add env n ety in
                  env
              | _ ->
                  (* I don't think ty will ever be anything but a Tvar...
                   * might be able to assert false here *)
                  env in
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
