(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Constraint
open Type

module LocMap = Utils_js.LocMap
module P = Type.Polarity

(** Garbage collection (GC) for graphs refers to the act of "marking" reachable
    type variables from a given set of "roots," by following links between type
    variables and traversing their concrete bounds.

    We mark only those dependencies that may contribute to errors. In
    particular, only type variables that are indirectly reachable via concrete
    bounds are marked; directly reachable type variables via links are not
    marked, since Flow's algorithm ensures that their concrete bounds are
    already propagated.

    This is useful for pruning the graph, i.e., removing type variables in a
    graph that make no difference when the graph is merged with other graphs
    through its requires and exports. **)

(* GC can be made more precise by respecting "polarity," which is just a fancy
   name that indicates the direction of walking: when a type variable is
   reached, we can walk only its lower bounds or its upper bounds based on the
   direction of the walk at that point.

   However, a directed walk requires determining the polarity of every part of
   every type. For some types, like those for functions, objects, arrays,
   etc. this is fairly standard. But for several other types, it is non-trivial
   to determine polarity: to do so, we need to carefully analyze how they appear
   in the flow rules, and whether their parts switch sides when those rules are
   simplified. Determining the wrong polarity in even one case can lead to
   hard-to-find bugs: at best, things crash because a type variable is reached
   that was marked unreachable, leading to a crash; at worst, a dependency is
   missed, leading to missed errors.

   The type visitor has an in-progress, conservative polarity calculation. It is
   conservative in that any "unknown" or unimplemented polarity is treated as
   Neutral, so we will preserve both lower bounds and upper bounds. *)

module Marked : sig
  type t
  val empty: t
  val add: int -> Type.polarity -> t -> (Type.polarity * t) option
  val get: int -> t -> Type.polarity option
  val mem: int -> Type.polarity -> t -> bool
  val exclude: int -> t -> t
end = struct
  type t = Type.polarity IMap.t

  let empty = IMap.empty

  let add id p x =
    match IMap.get id x with
    | None -> Some (p, IMap.add id p x)
    | Some p' ->
      match p, p' with
      | Positive, Negative
      | Negative, Positive -> Some (p, IMap.add id Neutral x)
      | Neutral, Negative -> Some (Positive, IMap.add id p x)
      | Neutral, Positive -> Some (Negative, IMap.add id p x)
      | _ -> None

  let get = IMap.get

  let mem id p x =
    match IMap.get id x with
    | None -> false
    | Some p' -> P.compat (p', p)

  let exclude id x = IMap.add id Neutral x
end

let gc = object (self)
  inherit [Marked.t] Type_visitor.t as super

  val depth = ref 0;

  method! type_ cx pole marked t =
    Option.iter ~f:(fun { Verbose.depth = verbose_depth; indent} ->
      let pid = Context.pid_prefix cx in
      let indent = String.make (!depth * indent) ' ' in
      Utils_js.prerr_endlinef "\n%s%sGC (%s): %s" indent pid
        (Polarity.string pole)
        (Debug_js.dump_t cx ~depth:verbose_depth t)
    ) (Context.verbose cx);
    incr depth;
    let marked = super#type_ cx pole marked t in
    decr depth;
    marked

  method! tvar cx pole marked r id =
    match Marked.add id pole marked with
    | None -> marked
    | Some (pole, marked) ->
      let root_id, constraints = Context.find_constraints cx id in
      if id != root_id then
        self#tvar cx pole marked r root_id
      else
        match constraints with
        | Resolved t -> self#type_ cx pole marked t
        | Unresolved bounds ->
          let marked =
            if P.compat (pole, Positive) then
              let marked = TypeMap.fold (fun l _ acc ->
                self#type_ cx Positive acc l
              ) bounds.lower marked in
              let marked = IMap.fold (fun id _ acc ->
                self#tvar_bounds cx Positive acc id
              ) bounds.lowertvars marked in
              marked
            else marked
          in
          let marked =
            if P.compat (pole, Negative) then
              let marked = UseTypeMap.fold (fun u _ acc ->
                self#use_type_ cx acc u
              ) bounds.upper marked in
              let marked = IMap.fold (fun id _ acc ->
                self#tvar_bounds cx Negative acc id
              ) bounds.uppertvars marked in
              marked
            else marked
          in
          marked

  method private tvar_bounds cx pole marked id =
    match Marked.add id pole marked with
    | None -> marked
    | Some (pole, marked) ->
      let root_id, constraints = Context.find_constraints cx id in
      if id != root_id then
        self#tvar_bounds cx pole marked root_id
      else
        match constraints with
        | Resolved _ -> marked
        | Unresolved bounds ->
          let marked =
            if P.compat (pole, Positive) then
              IMap.fold (fun id _ acc ->
                self#tvar_bounds cx Positive acc id
              ) bounds.lowertvars marked
            else marked
          in
          let marked =
            if P.compat (pole, Negative) then
              IMap.fold (fun id _ acc ->
                self#tvar_bounds cx Negative acc id
              ) bounds.uppertvars marked
            else marked
          in
          marked
end

(* Keep a reachable type variable around. *)
let live cx marked p id =
  let constraints = Context.find_graph cx id in
  match constraints with
  | Resolved _ -> ()
  | Unresolved bounds ->
    let lower, lowertvars =
      if P.compat (p, Positive) then
        bounds.lower,
        IMap.filter (fun id _ -> Marked.mem id Positive marked) bounds.lowertvars
      else
        TypeMap.empty, IMap.empty
    in
    let upper, uppertvars =
      if P.compat (p, Negative) then
        bounds.upper,
        IMap.filter (fun id _ -> Marked.mem id Negative marked) bounds.uppertvars
      else
        UseTypeMap.empty, IMap.empty
    in
    bounds.lower <- lower;
    bounds.upper <- upper;
    bounds.lowertvars <- lowertvars;
    bounds.uppertvars <- uppertvars;
    ()

(* Kill an unreachable type variable. *)
let die cx id =
  Context.remove_tvar cx id

(* Prune the graph given a GC state contained marked type variables. *)
let cleanup ~master_cx cx marked =
  let graph = Context.graph cx in
  let master_graph = Context.graph master_cx in
  IMap.iter (fun id _ ->
    (* Don't collect tvars from the master cx, which are explicitly excluded
     * from GC. Because the master cx is part of every cx, we can simply remove
     * all parts of the master from any cx. This is done separately in merge. *)
    if IMap.mem id master_graph then () else
    match Marked.get id marked with
    | None -> die cx id
    | Some p -> live cx marked p id
  ) graph

(* Main entry point for graph pruning. *)
let do_gc ~master_cx cx =
  Marked.empty
  (* Exclude tvars from the master cx. Adding these ids to the marked set
   * prevents the visitor from walking their bounds. *)
  |> IMap.fold (fun id _ acc -> Marked.exclude id acc) (Context.graph master_cx)
  (* Mark tvars reachable from imports. *)
  |> LocMap.fold (fun _ t acc -> gc#type_ cx Negative acc t) (Context.require_map cx)
  (* Mark tvars reachable from exports. *)
  |> SMap.fold (fun _ t acc -> gc#type_ cx Positive acc t) (Context.module_map cx)
  (* Collect unmarked tvars from the graph. *)
  |> cleanup ~master_cx cx
