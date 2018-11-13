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

module Size: sig
  type bounded_int = Exactly of int | GreaterThan of int
  val of_type: ?max:int -> Ty.t -> bounded_int
  val size_to_string: bounded_int -> string
end = struct
  exception SizeCutOff

  type bounded_int =
    | Exactly of int
    | GreaterThan of int

  let size_to_string = function
    | Exactly x -> Utils_js.spf "%d" x
    | GreaterThan x -> Utils_js.spf "(Greater than %d)" x

  let of_type =
    let open Ty in
    let size = ref 0 in
    let o = object
      inherit [_] iter_ty as super
      method! on_t max (t: Ty.t) =
        size := !size + 1;
        if !size > max then
          raise SizeCutOff;
        super#on_t max t
    end in
    fun ?(max=10000) t ->
      size := 0;
      match o#on_t max t with
      | exception SizeCutOff -> GreaterThan max
      | () -> Exactly !size
end

let symbols_of_t : Ty.t -> Ty_symbol.symbol list =
  let open Ty in
  let o = object (_self)
    inherit [_] reduce_ty as _super
    method zero = []
    method plus = List.rev_append
    method! on_symbol _env s = [s]
  end in
  fun t -> o#on_t () t

(* Simplify union/intersection types

   This visitor:
   - removes identical nodes from union and intersection types. (At the moment
     the comparison used is `Pervasives.compare`, but perhaps something more
     clever can replace this.)
   - removes the neutral element for union (resp. intersection) types, which
     is the bottom (resp. top) type.

   The Any state of this visitor is used to capture any change to the type
   structure.
*)
let simplify_unions_inters =
  let open Ty in
  let simplify_zero_one ~zero ~one =
    let rec simplify_aux acc = function
    | [] -> acc
    | t::ts ->
      if t = zero then [t]
      else if t = one then simplify_aux acc ts
      else simplify_aux (t::acc) ts
    in
    simplify_aux []
  in
  let o = object (self)
    inherit [_] endo_ty
    method private simplify env ~break ~zero ~one ~make ts =
      let ts' = self#on_list self#on_t env ts in
      let ts' = List.concat (List.map break ts') in
      let ts' = List.sort Pervasives.compare ts' in
      let ts' = ListUtils.uniq ts' in
      let ts' = simplify_zero_one ~zero ~one ts' in
      if List.length ts = List.length ts'
      then None (* no change *)
      else Some (make ts')

    method! on_t env t =
      match t with
      | Union (t0,t1,ts) ->
        let opt = self#simplify ~break:Ty.bk_union ~zero:Ty.Top
          ~one:Ty.Bot ~make:Ty.mk_union env (t0::t1::ts) in
        Option.value ~default:t opt
      | Inter (t0,t1,ts) ->
        let opt = self#simplify ~break:Ty.bk_inter ~zero:Ty.Bot
          ~one:Ty.Top ~make:Ty.mk_inter env (t0::t1::ts) in
        Option.value ~default:t opt
      (* WARNING: do not descend to other constructors or this will get slow *)
      | _ -> t
  end in
  let rec go t =
    let t' = o#on_t () t in
    if t == t' then t else go t'
  in
  fun t -> go t
