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

module Reason = Typing_reason
module Type   = Typing_ops
module Env    = Typing_env
module TUtils = Typing_utils
module SN     = Naming_special_names

let enforce_not_awaitable env p ty =
  let _, ety = Env.expand_type env ty in
  match ety with
  (* Match only a single unresolved -- this isn't typically how you
   * look into an unresolved, but the single list case is all we care
   * about since that's all you can get in this case (I think). *)
  | _, Tunresolved [r, Tclass ((_, awaitable), _)]
  | r, Tclass ((_, awaitable), _) when
      awaitable = SN.Classes.cAwaitable ->
    Errors.discarded_awaitable p (Reason.to_pos r)
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _
    | Tvar _ | Tfun _ | Tabstract (_, _) | Tclass (_, _) | Ttuple _
    | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _) -> ()

(* We would like to pretend that the wait_for*() functions are overloaded like
 * function wait_for<T>(Awaitable<T> $a): _AsyncWaitHandle<T>
 * function wait_for<T>(?Awaitable<T> $a): _AsyncWaitHandle<?T>
 * function wait_forv<T>(array<Awaitable<T>> $a): _AsyncWaitHandle<array<T>>
 * function wait_forv<T>(array<?Awaitable<T>> $a): _AsyncWaitHandle<array<?T>>
 *
 * Basically we check if the argument to wait_for*() looks option-y and decide
 * the expected types based on that.
 *)
let rec overload_extract_from_awaitable env p opt_ty_maybe =
  let type_var = Env.fresh_type() in
  let r = Reason.Rwitness p in
  let env, e_opt_ty = Env.expand_type env opt_ty_maybe in
  (match e_opt_ty with
  | _, Tunresolved tyl ->
    (* If we cannot fold the intersection into a single type, we need to look at
     * all the types *)
    let env, rtyl = List.fold_right begin fun ty (env, rtyl) ->
      let env, rty = overload_extract_from_awaitable env p ty in
      (* We have the invariant we'll never have Tunresolved[Tunresolved], but
       * the recursive call above can remove a layer of Awaitable, so we need
       * to flatten any Tunresolved that may have been inside. *)
      TUtils.flatten_unresolved env rty rtyl
    end tyl (env, []) in
    env, (r, Tunresolved rtyl)
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _
    | Tvar _ | Tfun _ | Tabstract (_, _) | Tclass (_, _) | Ttuple _
    | Tanon (_, _) | Tobject | Tshape _) ->
    let expected_opt_type = r, Toption (r, Tclass ((p, SN.Classes.cAwaitable), [type_var])) in
    let expected_non_opt_type = r, Tclass ((p, SN.Classes.cAwaitable), [type_var]) in
    let expected_type, return_type = (match e_opt_ty with
      | _, Toption _ ->
        expected_opt_type, (r, Toption type_var)
      | _, Tany ->
        expected_non_opt_type, (r, Tany)
      | _, (Tmixed | Tarray (_, _) | Tprim _ | Tvar _ | Tfun _
        | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
        | Tunresolved _ | Tobject | Tshape _)->
        expected_non_opt_type, type_var) in
    let env = Type.sub_type p Reason.URawait env expected_type opt_ty_maybe in
    env, return_type
  )

let overload_extract_from_awaitable_list env p tyl =
  List.fold_right begin fun ty (env, rtyl) ->
    let env, rty =  overload_extract_from_awaitable env p ty in
    env, rty::rtyl
  end tyl (env, [])

let gena env p ty =
  match snd (TUtils.fold_unresolved env ty) with
  | _, Tarray (None, None) ->
    env, ty
  | r, Tarray (Some ty1, None) ->
    let env, ty1 = overload_extract_from_awaitable env p ty1 in
    env, (r, Tarray (Some ty1, None))
  | r, Tarray (Some ty1, Some ty2) ->
    let env, ty2 = overload_extract_from_awaitable env p ty2 in
    env, (r, Tarray (Some ty1, Some ty2))
  | r, Ttuple tyl ->
    let env, tyl =
      overload_extract_from_awaitable_list env p tyl in
    env, (r, Ttuple tyl)
  | r, ty ->
    (* Oh well...let's at least make sure it is array-ish *)
    let expected_ty = r, Tarray (None, None) in
    let env =
      Errors.try_
        (fun () -> Type.sub_type p Reason.URawait env expected_ty (r, ty))
        (fun _ ->
          let ty_str = Typing_print.error ty in
          Errors.gena_expects_array p (Reason.to_pos r) ty_str;
          env
        )
    in
    env, expected_ty

let genva env p tyl =
  let env, rtyl =
    overload_extract_from_awaitable_list env p tyl in
  let inner_type = (Reason.Rwitness p, Ttuple rtyl) in
  env, inner_type

let rec gen_array_rec env p ty =
  let rec is_array env ty = begin
    let env, ety = Env.expand_type env ty in
    match snd (TUtils.fold_unresolved env ety) with
      | _, Ttuple _
      | _, Tarray _ -> gen_array_rec env p ety
      | r, Tunresolved tyl -> begin
        (* You can run gen_array_rec on heterogeneous arrays, like this one:
         * array(
         *   'foo' => cached_result(1),
         *   'bar' => array(
         *     'baz' => cached_result(2),
         *   ),
         * )
         *
         * In this case the value type in the array will be unresolved; we need
         * to check all the types in the unresolved. *)
        let env, rtyl = List.fold_right begin fun ty (env, rtyl) ->
          let env, ty = is_array env ty in
          env, ty::rtyl
        end tyl (env, []) in
        env, (r, Tunresolved rtyl)
      end
      | _, (Tany | Tmixed | Tprim _ | Toption _ | Tvar _
        | Tfun _ | Tabstract (_, _) | Tclass (_, _) | Tanon (_, _) | Tobject
        | Tshape _
           ) -> overload_extract_from_awaitable env p ety
  end in
  match snd (TUtils.fold_unresolved env ty) with
  | r, Tarray (Some vty, None) ->
    let env, vty = is_array env vty in
    env, (r, Tarray (Some vty, None))
  | r, Tarray (kty, Some vty) ->
    let env, vty = is_array env vty in
    env, (r, Tarray (kty, Some vty))
  | _, Ttuple tyl -> gen_array_va_rec env p tyl
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _
    | Tvar _ | Tfun _ | Tabstract (_, _) | Tclass (_, _)
    | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
       ) -> gena env p ty

and gen_array_va_rec env p tyl =
  (* For each item in the type list, treat it differently *)
  let rec gen_array_va_rec' env ty =
  (* Unwrap option types (hopefully we won't have option options *)
    (match snd (TUtils.fold_unresolved env ty) with
    | r, Toption opt_ty ->
      let env, opt_ty = gen_array_va_rec' env opt_ty in
      env, (r, Toption opt_ty)
    | _, Tarray _ -> gen_array_rec env p ty
    | _, Ttuple tyl -> genva env p tyl
    | _, (Tany | Tmixed | Tprim _ | Tvar _ | Tfun _
      | Tabstract (_, _) | Tclass (_, _) | Tanon (_, _) | Tunresolved _
      | Tobject | Tshape _) ->
      overload_extract_from_awaitable env p ty) in

  let env, rtyl = List.fold_right begin fun ty (env, rtyl) ->
    let env, ty = gen_array_va_rec' env ty in
    env, ty::rtyl
  end tyl (env, []) in
  env, (Reason.Rwitness p, Ttuple rtyl)
