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


(********************)
(* Free Variables   *)
(********************)

(* This module decides if a type variable appears free inside a type. Here are
   some cases where this information can be useful:

   - Deciding well-formedness: a type variable should not appear free in a
     top-level type.

   - Computing recursive types: we decide if a type is recursive, we will need
     if it appears free in its expansion. (More can be found in the type
     normalizer module.)
*)
module FreeVars : sig
  (* The reason we require the is_top parameter is to determine if the TypeAlias
     body will be walked over. Typically the body is only useful when TypeAlias
     appears as the top-level constructor, and is ignored otherwise.
  *)
  val is_free_in : is_top:bool -> int -> Ty.t -> bool
end = struct

  (* In computing the set of free variables in a type we use a set of variables
     that are in scope, i.e. bound through a Mu constructor, and add them to the
     skip set of the environment.

    The environment also includes a flag to denote if the constructor is
    top-level, since we special type-aliases based on that.
  *)
  module Env = struct
    type t = {
      is_top: bool;
      skip: TVarSet.t;
    }

    (* After descending at least one level, is_top becomes false. *)
    let descend _ e = { e with is_top = false }

  end

  (* The visitor is a writer monad where the sate is the set of variables that
     have been deemed as appearing free.
  *)
  module Visitor = Ty_visitor.Make(TVarSet)(Env)
  open Visitor

  (* Computes the set of variables appearing free in the input. *)
  let from_type =
    let open Env in
    (* The visitor class is also a mapper. This visitor does not alter its input. *)
    let free_vars_visitor = object(self) inherit visitor as super
      method! type_ env = function
      | Ty.TVar (Ty.RVar i, _) as t when not (TVarSet.mem i env.skip) ->
        tell (TVarSet.singleton i) >>= fun _ ->
        super#type_ env t >>| fun _ ->
        t

      | Ty.TypeAlias { Ty.ta_tparams; ta_type=Some t_body; _ } as t
        ->
        let env' = Env.descend t env in
        opt (mapM (self#param_t env')) ta_tparams >>= fun _ ->
        begin
        if env.is_top then
          (* The type alias is the top-level constructor: the body of
             the alias will be useful and so we descends into
            that type expression and collects variables.
          *)
          self#type_ env' t_body >>= fun _ ->
          return ()
        else
          (* The type alias is not the top-level constructor: we avoid
             collecting variables from the body as it is typically not be
             exposed through a type query.
          *)
          return ()
        end >>| fun _ -> t

      | Ty.Mu (v, t) ->
        let env = { env with skip = TVarSet.add v env.skip } in
        super#type_ env t

      | t ->
        super#type_ env t
    end
    in fun ~is_top t ->
    snd (free_vars_visitor#type_ { is_top; skip = TVarSet.empty} t)

  let is_free_in ~is_top v t = TVarSet.mem v (from_type ~is_top t)

end


(*************)
(* Ty.t size *)
(*************)

module Size : sig
  val calculate : Ty.t -> int
end = struct
  open Ty_visitor.CountVisitor

  let size_visitor = object(_) inherit visitor as super
    method! type_ env t =
      tell 1 >>= fun _ ->
      super#type_ env t
  end

  let calculate t = snd (size_visitor#type_ () t)
end
