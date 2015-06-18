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

module Env = Typing_env
module ShapeMap = Nast.ShapeMap


(* Module checking if a type is generic, I like to use an exception for this sort
 * of things, the code is more readable (subjective :-), and the exception never
 * escapes anyway.
*)
module IsGeneric: sig

  (* Give back the name and position of a generic if found *)
  val ty: locl ty -> string option
end = struct

  exception Found of string

  let rec ty (_, x) = ty_ x
  and ty_ = function
    | Tgeneric ("this", Some (_, x)) -> ty x
    | Tgeneric (x, _) -> raise (Found x)
    | Tanon _ | Taccess _
    | Tany | Tmixed | Tprim _ -> ()
    | Tarray (ty1, ty2) ->
        ty_opt ty1; ty_opt ty2
    | Tvar _ -> assert false (* Expansion got rid of Tvars ... *)
    | Toption x -> ty x
    | Tfun fty ->
        List.iter ty (List.map snd fty.ft_params);
        ty fty.ft_ret;
        (match fty.ft_arity with
          | Fvariadic (_min, (_name, var_ty)) -> ty var_ty
          | _ -> ())
    | Tabstract (_, tyl, x) ->
        List.iter ty tyl; ty_opt x
    | Ttuple tyl -> List.iter ty tyl
    | Tclass (_, tyl)
    | Tunresolved tyl -> List.iter ty tyl
    | Tobject -> ()
    | Tshape (_, fdm) ->
        ShapeMap.iter (fun _ v -> ty v) fdm

  and ty_opt = function None -> () | Some x -> ty x

  let ty x = try ty x; None with Found x -> Some x

end

let rename env old_name new_name ty_to_rename =
  let rec ty env (r, t) = (match t with
    | Tgeneric (x, cstr_opt) ->
        let name = if x = old_name then new_name else x in
        let env, cstr_opt = match cstr_opt with
          | Some (ck, t) ->
              let env, t = ty env t in
              env, Some (ck, t)
          | None -> env, None in
        env, (r, Tgeneric (name, cstr_opt))
    | Tanon _
    | Tany | Tmixed | Tprim _-> env, (r, t)
    | Tarray (ty1, ty2) ->
        let env, ty1 = ty_opt env ty1 in
        let env, ty2 = ty_opt env ty2 in
        env, (r, Tarray (ty1, ty2))
    | Tvar n ->
        let env, t = Env.get_type env n in
        let n' = Env.fresh() in
        let env = Env.rename env n n' in
        let env, t = ty env t in
        let env = Env.add env n' t in
        env, (r, Tvar n')
    | Toption x ->
        let env, x = ty env x in
        env, (r, Toption x)
    | Tfun fty ->
        let env, params = List.fold_right (fun (s_opt, t) (env, params) ->
          let env, t= ty env t in
          env, (s_opt, t)::params
        ) fty.ft_params (env, []) in
        let env, ret = ty env fty.ft_ret in
        let env, arity = match fty.ft_arity with
          | Fvariadic (min, (s_opt, t)) ->
            let env, t = ty env t in
            env, Fvariadic (min, (s_opt, t))
          | x -> env, x
        in
        env, (r, Tfun { fty with
          ft_arity = arity;
          ft_params = params;
          ft_ret = ret;
        })
    | Tabstract (id, l, x) ->
        let env, l = tyl env l in
        let env, x = ty_opt env x in
        env, (r, Tabstract (id, l, x))
    | Tclass (cls, l) ->
       let env, l = tyl env l in
       env, (r, Tclass(cls, l))
    | Taccess (x, ids) ->
        let env, x = ty env x in
        env, (r, Taccess(x, ids))
    | Ttuple l ->
        let env, l = tyl env l in
        env, (r, Ttuple l)
    | Tunresolved l ->
        let env, l = tyl env l in
        env, (r, Tunresolved l)
    | Tobject -> env, (r, Tobject)
    | Tshape (fields_known, fdm) ->
        let env, fdm = ShapeMap.fold (fun k v (env, fdm) ->
          let env, v = ty env v in
          env, ShapeMap.add k v fdm
        ) fdm (env, ShapeMap.empty) in
        env, (r, Tshape (fields_known, fdm) ))

  and ty_opt env = function
    | None -> env, None
    | Some x ->
        let env, x = ty env x in
        env, Some x

  and tyl env l = List.fold_right (fun t (env, l) ->
    let env, t = ty env t in
    env, t::l
  ) l (env, [])

  in ty env ty_to_rename

(* Function making sure that a type can be generalized, in our case it just
 * means the type should be monomorphic
*)
let no_generic p local_var_id env =
  let env, ty = Env.get_local env local_var_id in
  let ty = Typing_expand.fully_expand env ty in
  match IsGeneric.ty ty with
  | None -> env
  | Some x ->
      Errors.generic_static p x;
      env
