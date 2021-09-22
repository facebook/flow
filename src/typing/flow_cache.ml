(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type
open TypeUtil
module ImplicitTypeArgument = Instantiation_utils.ImplicitTypeArgument

(* Cache that remembers pairs of types that are passed to __flow. *)
module FlowConstraint = struct
  let rec toplevel_use_op = function
    | Frame (_frame, use_op) -> toplevel_use_op use_op
    | Op (Speculation use_op) -> toplevel_use_op use_op
    | use_op -> use_op

  (* attempt to read LB/UB pair from cache, add if absent *)
  let get cx (l, u) =
    match (l, u) with
    (* Don't cache constraints involving type variables, since the
       corresponding typing rules are already sufficiently robust. *)
    | (OpenT _, _)
    | (_, UseT (_, OpenT _))
    | (_, ReposUseT _) ->
      false
    | _ ->
      (* Use ops are purely for better error messages: they should have no
         effect on type checking. However, recursively nested use ops can pose
         non-termination problems. To ensure proper caching, we hash use ops
         to just their toplevel structure. *)
      let u = mod_use_op_of_use_t toplevel_use_op u in
      let cache = Context.constraint_cache cx in
      let cache' = FlowSet.add l u !cache in
      let found = cache' == !cache in
      if not found then
        cache := cache'
      else if Context.is_verbose cx then
        prerr_endlinef
          "%sFlowConstraint cache hit on (%s, %s)"
          (Context.pid_prefix cx)
          (string_of_ctor l)
          (string_of_use_ctor u);
      found
end

(* Cache that limits instantiation of polymorphic definitions. Intuitively,
   for each operation on a polymorphic definition, we remember the type
   arguments we use to specialize the type parameters. An operation is
   identified by its reason, and possibly the reasons of its arguments. We
   don't use the entire operation for caching since it may contain the very
   type variables we are trying to limit the creation of with the cache (e.g.,
   those representing the result): the cache would be useless if we considered
   those type variables as part of the identity of the operation. *)
module PolyInstantiation = struct
  let find cx reason_tapp typeparam op_reason =
    let cache = Context.instantiation_cache cx in
    try Hashtbl.find cache (reason_tapp, typeparam.reason, op_reason) with
    | _ ->
      let t = ImplicitTypeArgument.mk_targ cx typeparam (Nel.hd op_reason) reason_tapp in
      Hashtbl.add cache (reason_tapp, typeparam.reason, op_reason) t;
      t
end

module Eval = struct
  let id cx t defer_use =
    let (eval_id_cache, id_cache) = Context.eval_id_cache cx in
    match t with
    | EvalT (_, d, i) when d = defer_use ->
      (match Hashtbl.find_opt eval_id_cache i with
      | Some t -> t
      | None ->
        let i = Type.Eval.generate_id () in
        Hashtbl.add eval_id_cache i t;
        EvalT (t, defer_use, i))
    | _ ->
      let cache_key = (t, defer_use) in
      let id =
        match Hashtbl.find_opt id_cache cache_key with
        | Some i -> i
        | None ->
          let i = Type.Eval.generate_id () in
          Hashtbl.add id_cache cache_key i;
          i
      in
      EvalT (t, defer_use, id)

  let find_repos cx t defer_use id =
    let repos_cache = Context.eval_repos_cache cx in
    let cache_key = (t, defer_use, id) in
    Hashtbl.find_opt repos_cache cache_key

  let add_repos cx t defer_use id tvar =
    let repos_cache = Context.eval_repos_cache cx in
    let cache_key = (t, defer_use, id) in
    Hashtbl.add repos_cache cache_key tvar
end

module Fix = struct
  let find cx is_this i =
    let cache = Context.fix_cache cx in
    let cache_key = (is_this, i) in
    Hashtbl.find_opt cache cache_key

  let add cx is_this i tvar =
    let cache = Context.fix_cache cx in
    let cache_key = (is_this, i) in
    Hashtbl.add cache cache_key tvar
end

let stats_poly_instantiation cx =
  let cache = Context.instantiation_cache cx in
  Hashtbl.stats cache

(* debug util: please don't dead-code-eliminate *)
(* Summarize flow constraints in cache as ctor/reason pairs, and return counts
   for each group. *)
let summarize_flow_constraint cx =
  let group_counts =
    FlowSet.fold
      (fun (l, u) map ->
        let key =
          spf
            "[%s] %s => [%s] %s"
            (string_of_ctor l)
            (string_of_reason (reason_of_t l))
            (string_of_use_ctor u)
            (string_of_reason (reason_of_use_t u))
        in
        match SMap.find_opt key map with
        | None -> SMap.add key 0 map
        | Some i -> SMap.add key (i + 1) map)
      !(Context.constraint_cache cx)
      SMap.empty
  in
  SMap.elements group_counts |> List.sort (fun (_, i1) (_, i2) -> Stdlib.compare i1 i2)
