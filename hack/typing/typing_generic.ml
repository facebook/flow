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
    | Tabstract ((AKdependent (_, _) | AKenum _), cstr) -> ty_opt cstr
    | Tabstract (AKgeneric (x, _), _) -> raise (Found x)
    | Tanon _ | Tany | Tmixed | Tprim _ -> ()
    | Tarraykind akind ->
      begin match akind with
        | AKany -> ()
        | AKempty -> ()
        | AKvec tv -> ty tv
        | AKmap (tk, tv) -> ty tk; ty tv
        | AKshape fdm ->
            ShapeMap.iter (fun _ (tk, tv) -> ty tk; ty tv) fdm
        | AKtuple fields ->
            Utils.IMap.iter (fun _ tv -> ty tv) fields
      end
    | Tvar _ -> assert false (* Expansion got rid of Tvars ... *)
    | Toption x -> ty x
    | Tfun fty ->
        List.iter (List.map fty.ft_params snd) ty;
        ty fty.ft_ret;
        (match fty.ft_arity with
          | Fvariadic (_min, (_name, var_ty)) -> ty var_ty
          | _ -> ())
    | Tabstract (AKnewtype (_, tyl), x) ->
        List.iter tyl ty; ty_opt x
    | Ttuple tyl -> List.iter tyl ty
    | Tclass (_, tyl)
    | Tunresolved tyl -> List.iter tyl ty
    | Tobject -> ()
    | Tshape (_, fdm) ->
        ShapeMap.iter (fun _ v -> ty v) fdm

  and ty_opt = function None -> () | Some x -> ty x

  let ty x = try ty x; None with Found x -> Some x

end

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
