(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type
module ImplicitTypeArgument = Instantiation_utils.ImplicitTypeArgument

module FlowSet = struct
  let empty = TypeMap.empty

  let add_not_found l us setr =
    setr := TypeMap.add l us !setr;
    false

  let cache (l, u) setr =
    match TypeMap.get l !setr with
    | None -> add_not_found l (UseTypeSet.singleton u) setr
    | Some us ->
      (* add returns ref eq set if found *)
      let us' = UseTypeSet.add u us in
      us' == us || add_not_found l us' setr

  let fold f = TypeMap.fold (fun l -> UseTypeSet.fold (fun u -> f (l, u)))
end

(* Cache that remembers pairs of types that are passed to __flow. *)
module FlowConstraint = struct
  let cache = ref FlowSet.empty

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
    | (_, UseT (_, OpenT _)) ->
      false
    | _ ->
      (* Use ops are purely for better error messages: they should have no
         effect on type checking. However, recursively nested use ops can pose
         non-termination problems. To ensure proper caching, we hash use ops
         to just their toplevel structure. *)
      let u = mod_use_op_of_use_t toplevel_use_op u in
      let found = FlowSet.cache (l, u) cache in
      if found && Context.is_verbose cx then
        prerr_endlinef
          "%sFlowConstraint cache hit on (%s, %s)"
          (Context.pid_prefix cx)
          (string_of_ctor l)
          (string_of_use_ctor u);
      found
end

(* Cache that maps TypeApp(Poly (...id), ts) to its result. *)
module Subst = struct
  type cache_key = int * Type.t list

  type cache_value_el =
    | ETooFewTypeArgs of ALoc.t Reason.virtual_reason * int
    | ETooManyTypeArgs of ALoc.t Reason.virtual_reason * int

  let cache : (cache_key, cache_value_el list * Type.t) Hashtbl.t = Hashtbl.create 0

  let find = Hashtbl.find_opt cache

  let add = Hashtbl.add cache
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
  type cache_key = ALoc.t * reason * op_reason

  and op_reason = reason Nel.t

  let cache : (cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

  let find cx reason_tapp typeparam op_reason =
    let loc = def_aloc_of_reason reason_tapp in
    try Hashtbl.find cache (loc, typeparam.reason, op_reason)
    with _ ->
      let t = ImplicitTypeArgument.mk_targ cx typeparam (Nel.hd op_reason) reason_tapp in
      Hashtbl.add cache (loc, typeparam.reason, op_reason) t;
      t
end

let repos_cache = ref Repos_cache.empty

module Eval = struct
  type id_cache_key = Type.t * Type.defer_use_t

  type repos_cache_key = Type.t * Type.defer_use_t * int

  let eval_id_cache : (int, Type.t) Hashtbl.t = Hashtbl.create 0

  let id_cache : (id_cache_key, int) Hashtbl.t = Hashtbl.create 0

  let repos_cache : (repos_cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

  let id t defer_use =
    match t with
    | EvalT (_, d, i) when d = defer_use ->
      (match Hashtbl.find_opt eval_id_cache i with
      | Some t -> t
      | None ->
        let i = mk_id () in
        Hashtbl.add eval_id_cache i t;
        EvalT (t, defer_use, i))
    | _ ->
      let cache_key = (t, defer_use) in
      let id =
        match Hashtbl.find_opt id_cache cache_key with
        | Some i -> i
        | None ->
          let i = mk_id () in
          Hashtbl.add id_cache cache_key i;
          i
      in
      EvalT (t, defer_use, id)

  let find_repos t defer_use id =
    let cache_key = (t, defer_use, id) in
    Hashtbl.find_opt repos_cache cache_key

  let add_repos t defer_use id tvar =
    let cache_key = (t, defer_use, id) in
    Hashtbl.add repos_cache cache_key tvar
end

module Fix = struct
  type cache_key = reason * Type.t

  let cache : (cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

  let find reason i =
    let cache_key = (reason, i) in
    Hashtbl.find_opt cache cache_key

  let add reason i tvar =
    let cache_key = (reason, i) in
    Hashtbl.add cache cache_key tvar
end

let clear () =
  FlowConstraint.cache := FlowSet.empty;
  Hashtbl.clear Subst.cache;
  Hashtbl.clear PolyInstantiation.cache;
  repos_cache := Repos_cache.empty;
  Hashtbl.clear Eval.eval_id_cache;
  Hashtbl.clear Eval.id_cache;
  Hashtbl.clear Eval.repos_cache;
  Hashtbl.clear Fix.cache;
  ()

let stats_poly_instantiation () = Hashtbl.stats PolyInstantiation.cache

(* debug util: please don't dead-code-eliminate *)
(* Summarize flow constraints in cache as ctor/reason pairs, and return counts
   for each group. *)
let summarize_flow_constraint () =
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
        match SMap.get key map with
        | None -> SMap.add key 0 map
        | Some i -> SMap.add key (i + 1) map)
      !FlowConstraint.cache
      SMap.empty
  in
  SMap.elements group_counts |> List.sort (fun (_, i1) (_, i2) -> Pervasives.compare i1 i2)
