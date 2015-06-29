(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Gets rid of all the type variables,
 * this is only useful when declaring class constants.
 * The thing is, we don't want any type variable left in
 * type declarations, (it would force us to maintain a global
 * substitution, which would be way too big).
 *)
(*****************************************************************************)
open Utils
open Typing_defs

module Env = Typing_env

let rec fully_expand seen env (r, ty) =
  match ty with
  | Tvar n when ISet.mem n seen -> r, Tany
  | Tvar n ->
      let seen = ISet.add n seen in
      let _, ty = Env.get_type env n in
      fully_expand seen env ty
  | ty ->
      r, fully_expand_ seen env ty

and fully_expand_ seen env = function
  | Tvar _ -> assert false
  | Tmixed | Tany | Tanon _ | Tprim _ as x -> x
  | Tarray (ty1, ty2) ->
      let ty1 = fully_expand_opt seen env ty1 in
      let ty2 = fully_expand_opt seen env ty2 in
      Tarray (ty1, ty2)
  | Ttuple tyl ->
      Ttuple (List.map (fully_expand seen env) tyl)
  | Tunresolved tyl ->
      Tunresolved (List.map (fully_expand seen env) tyl)
  | Toption ty ->
      let ty = fully_expand seen env ty in
      Toption ty
  | Tfun ft ->
      let expand_param (name, ty) = name, fully_expand seen env ty in
      let params = List.map expand_param ft.ft_params in
      let ret  = fully_expand seen env ft.ft_ret in
      let arity = match ft.ft_arity with
        | Fvariadic (min, (p_n, p_ty)) ->
          Fvariadic (min, (p_n, fully_expand seen env p_ty))
        | x -> x
      in
      Tfun { ft with ft_params = params; ft_arity = arity; ft_ret = ret }
  | Tabstract (AKgeneric (x, super), cstr) ->
      let super = fully_expand_opt seen env super in
      let cstr = fully_expand_opt seen env cstr in
      Tabstract (AKgeneric (x, super), cstr)
  | Tabstract (AKnewtype (x, tyl), cstr) ->
      let tyl = List.map (fully_expand seen env) tyl in
      let cstr = fully_expand_opt seen env cstr in
      Tabstract (AKnewtype (x, tyl), cstr)
  | Tabstract (ak, cstr) ->
      let cstr = fully_expand_opt seen env cstr in
      Tabstract (ak, cstr)
  | Tclass (x, tyl) ->
     let tyl = List.map (fully_expand seen env) tyl in
     Tclass (x, tyl)
  | Tobject as x -> x
  | Tshape (fields_known, fdm) ->
      Tshape (fields_known, (Nast.ShapeMap.map (fully_expand seen env) fdm))

and fully_expand_opt seen env x = opt_map (fully_expand seen env) x

(*****************************************************************************)
(* External API *)
(*****************************************************************************)

let fully_expand = fully_expand ISet.empty
