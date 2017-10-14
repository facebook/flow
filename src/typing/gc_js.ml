(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Constraint
open Type

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

   Thus, do a conservative version of GC for now, that is undirected. *)

let gc = object (self)
  inherit [ISet.t] Type_visitor.t

  method! tvar cx pole marked r id =
    let root_id, constraints = Flow_js.find_constraints cx id in
    if id == root_id then
      let marked' = ISet.add id marked in
      if marked == marked'
      then marked (* already marked *)
      else (
        match constraints with
        | Resolved t -> self#type_ cx pole marked' t
        | Unresolved bounds -> marked'
          |> TypeMap.fold (fun l _ acc -> self#type_ cx pole acc l) bounds.lower
          |> UseTypeMap.fold (fun u _ acc -> self#use_type_ cx acc u) bounds.upper
      )
    else
      self#tvar cx pole (ISet.add id marked) r root_id
end

(* Keep a reachable type variable around. *)
let live cx marked id =
  let constraints = Flow_js.find_graph cx id in
  match constraints with
  | Resolved _ -> ()
  | Unresolved bounds ->
    bounds.uppertvars <-
      bounds.uppertvars |> IMap.filter (fun id _ -> ISet.mem id marked);
    bounds.lowertvars <-
      bounds.lowertvars |> IMap.filter (fun id _ -> ISet.mem id marked)

(* Kill an unreachable type variable. *)
let die cx id = Context.remove_tvar cx id

(* Prune the graph given a GC state contained marked type variables. *)
let cleanup ~master_cx cx marked =
  let graph = Context.graph cx in
  let master_graph = Context.graph master_cx in
  IMap.iter (fun id _ ->
    (* Don't collect tvars from the master cx, which are explicitly excluded
     * from GC. Because the master cx is part of every cx, we can simply remove
     * all parts of the master from any cx. This is done separately in merge. *)
    if IMap.mem id master_graph then ()
    else if ISet.mem id marked then live cx marked id
    else die cx id
    ) graph

(* Main entry point for graph pruning. *)
let do_gc ~master_cx cx =
  ISet.empty
  (* Exclude tvars from the master cx. Adding these ids to the marked set
   * prevents the visitor from walking their bounds. *)
  |> IMap.fold (fun id _ acc -> ISet.add id acc) (Context.graph master_cx)
  (* Mark tvars reachable from imports. *)
  |> SMap.fold (fun _ t acc -> gc#type_ cx Negative acc t) (Context.require_map cx)
  (* Mark tvars reachable from exports. *)
  |> SMap.fold (fun _ t acc -> gc#type_ cx Positive acc t) (Context.module_map cx)
  (* Collect unmarked tvars from the graph. *)
  |> cleanup ~master_cx cx
