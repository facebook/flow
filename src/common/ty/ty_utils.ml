(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


(* Set and map based on type variables *)

module TVarSet = struct
  include ISet
  let append = union
end

(* Free variables
 *
 * Decide if a type variable appears free inside a type. This is useful for:
 *
 * - Deciding well-formedness: a type variable should not appear free in a
 *   top-level type.
 *
 * - Computing recursive types: we decide if a type is recursive, we will need
 *   to know if it appears free in its expansion. (More can be found in the type
 *   normalizer module.)
 *)

type t = {
  is_top: bool;
  skip: TVarSet.t;
}

(* Computes the set of variables appearing free in the input. *)
let free_vars_of_t : is_top:bool -> Ty.t -> TVarSet.t =
  let open Ty in
  let o = object (self)
    inherit [_] reduce_ty as super
    method zero = TVarSet.empty
    method plus = TVarSet.union

    method! on_t env t =
      match t with
      | TVar (RVar i, _) when not (TVarSet.mem i env.skip) ->
        TVarSet.singleton i
      | TypeAlias { ta_tparams; ta_type = Some t_body; _ } ->
        let env' = { env with is_top = false } in
        let acc = self#on_option (self#on_list self#on_type_param) env' ta_tparams in
        (* If the type alias is the top-level constructor, then the body of the alias
         * will be useful and so we descend into that type expression and collect
         * variables. Otherwise we avoid collecting variables from the body as it is
         * typically not be exposed through a type query. *)
        if env.is_top
        then self#plus acc (self#on_t env' t_body)
        else acc
      | Mu (v, t) ->
        let env = { env with skip = TVarSet.add v env.skip } in
        super#on_t env t
      | t ->
        super#on_t env t
  end in
  fun ~is_top t ->
    o#on_t { is_top; skip = TVarSet.empty } t

(* The reason we require the is_top parameter is to determine if the TypeAlias
 * body will be walked over. Typically the body is only useful when TypeAlias
 * appears as the top-level constructor, and is ignored otherwise. *)
let appears_in_t ~is_top v t =
  TVarSet.mem v (free_vars_of_t ~is_top t)

let size_of_t : Ty.t -> int =
  let open Ty in
  let o = object (_self)
    inherit [_] reduce_ty as super
    method zero = 0
    method plus a b = a + b
    method! on_t env (t: Ty.t) = 1 + super#on_t env t
  end in
  fun t -> o#on_t () t

let symbols_of_t : Ty.t -> Ty_symbol.symbol list =
  let open Ty in
  let o = object (_self)
    inherit [_] reduce_ty as _super
    method zero = []
    method plus = List.rev_append
    method! on_symbol _env s = [s]
  end in
  fun t -> o#on_t () t
