(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Constraint_js
open Type
open Utils

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

(* State carried by GC, which includes most importantly a set of type variables
   marked as reachable. *)
class gc_state = object(this)
  val mutable _markedset = ISet.empty

  method markedset =
    _markedset

  method marked id =
    ISet.mem id _markedset

  method mark id =
    if this#marked id then false
    else (
      _markedset <- _markedset |> ISet.add id;
      true
    )
end

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

   Thus, do a conservative version of GC for now, that is undirected.
*)
let rec gc cx state = function
  | OpenT(_, id) ->
      gc_id cx state id

  (** def types **)

  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | TaintT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
      -> ()

  | FunT(_, static, prototype, funtype) ->
      gc cx state funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx state);
      gc cx state funtype.return_t;
      gc cx state prototype;
      gc cx state static

  | ObjT(_, objtype) ->
      let id = objtype.props_tmap in
      Flow_js.iter_props cx id (fun _ -> gc cx state);
      (match objtype.dict_t with
        | None -> ()
        | Some { key; value; _ } ->
          gc cx state key;
          gc cx state value;
      );
      gc cx state objtype.proto_t

  | ArrT(_, t, ts) ->
      gc cx state t;
      ts |> List.iter (gc cx state);

  | ClassT(t) ->
      gc cx state t

  | InstanceT(_, static, super, instance) ->
      instance.type_args |> SMap.iter (fun _ -> gc cx state);
      Flow_js.iter_props cx instance.fields_tmap (fun _ -> gc cx state);
      Flow_js.iter_props cx instance.methods_tmap (fun _ -> gc cx state);
      gc cx state static;
      gc cx state super

  | OptionalT t ->
      gc cx state t

  | RestT t ->
      gc cx state t

  | AbstractT t ->
      gc cx state t

  | PolyT (typeparams, t) ->
      typeparams |> List.iter (gc_typeparam cx state);
      gc cx state t

  | TypeAppT (t, ts) ->
      gc cx state t;
      ts |> List.iter (gc cx state)

  | BoundT typeparam ->
      gc_typeparam cx state typeparam

  | ExistsT _ -> ()

  | MaybeT t ->
      gc cx state t

  | IntersectionT (_, ts) ->
      ts |> List.iter (gc cx state)

  | UnionT (_, ts) ->
      ts |> List.iter (gc cx state)

  | UpperBoundT (t) ->
      gc cx state t

  | LowerBoundT (t) ->
      gc cx state t

  | AnyObjT _
  | AnyFunT _
      -> ()

  | ShapeT t ->
      gc cx state t

  | DiffT (t1, t2) ->
      gc cx state t1;
      gc cx state t2;

  | KeysT (_, t) ->
      gc cx state t

  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _
      -> ()

  | TypeT (_, t) ->
      gc cx state t

  | AnnotT (t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | SpeculativeMatchFailureT (_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | ModuleT (_, {exports_tmap; cjs_export}) ->
      Flow_js.iter_props cx exports_tmap (fun _ -> gc cx state);
      (match cjs_export with
        | Some t -> gc cx state t
        | None -> ()
      )

  | ReposUpperT (_, t) ->
      gc cx state t

  (** use types **)

  | SummarizeT (_, t) ->
      gc cx state t

  | BindT(_, funtype)
  | CallT(_, funtype) ->
      gc cx state funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx state);
      gc cx state funtype.return_t

  | ApplyT(_, l, funtype) ->
      gc cx state l;
      gc cx state funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx state);
      gc cx state funtype.return_t

  | MethodT(_, _, funtype) ->
      gc cx state funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx state);
      gc cx state funtype.return_t

  | ReposLowerT (_, t) ->
      gc cx state t

  | SetPropT(_, _, t) ->
      gc cx state t

  | GetPropT(_, _, t) ->
      gc cx state t

  | SetElemT(_, i, t) ->
      gc cx state i;
      gc cx state t

  | GetElemT(_, i, t) ->
      gc cx state i;
      gc cx state t

  | ConstructorT(_, params, t) ->
      params |> List.iter (gc cx state);
      gc cx state t

  | SuperT(_, instance) ->
      instance.type_args |> SMap.iter (fun _ -> gc cx state);
      Flow_js.iter_props cx instance.fields_tmap (fun _ -> gc cx state);
      Flow_js.iter_props cx instance.methods_tmap (fun _ -> gc cx state)

  | ExtendsT (ts, t1, t2) ->
      ts |> List.iter (gc cx state);
      gc cx state t1;
      gc cx state t2

  | AdderT(_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | ComparatorT(_, t) ->
      gc cx state t

  | PredicateT (pred, t) ->
      gc_pred cx state pred;
      gc cx state t

  | EqT (_, t) ->
      gc cx state t

  | AndT (_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | OrT (_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | NotT (_, t) ->
      gc cx state t

  | SpecializeT (_, _, ts, t) ->
      ts |> List.iter (gc cx state);
      gc cx state t

  | LookupT (_, _, ts, _, t) ->
      ts |> List.iter (gc cx state);
      gc cx state t

  | ObjAssignT (_, t1, t2, _, _) ->
      gc cx state t1;
      gc cx state t2

  | ObjFreezeT (_, t) ->
      gc cx state t

  | ObjRestT (_, _, t) ->
      gc cx state t

  | ObjSealT (_, t) ->
      gc cx state t

  | ObjTestT (_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | ArrRestT (_, _, t) ->
      gc cx state t

  | UnaryMinusT (_, t) ->
      gc cx state t

  | UnifyT (t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | BecomeT (_, t) ->
      gc cx state t

  | ConcretizeT (t1, ts1, ts2, t2) ->
      gc cx state t1;
      ts1 |> List.iter (gc cx state);
      ts2 |> List.iter (gc cx state);
      gc cx state t2

  | ConcreteT (t) ->
      gc cx state t

  | GetKeysT (_, t) ->
      gc cx state t

  | HasOwnPropT _ -> ()

  | ElemT (_, t1, t2) ->
      gc cx state t1;
      gc cx state t2

  | ImportModuleNsT (_, t)
  | ImportDefaultT (_, _, t)
  | ImportNamedT (_, _, t)
  | ImportTypeT (_, t)
  | ImportTypeofT (_, t) ->
      gc cx state t

  | CJSRequireT (_, t) ->
      gc cx state t

  | CJSExtractNamedExportsT (_, t, t_out) ->
      gc cx state t;
      gc cx state t_out

  | SetNamedExportsT (_, t_smap, t_out) ->
      List.iter (gc cx state) (SMap.values t_smap);
      gc cx state t_out

  | SetStarExportsT (_, target_module, t_out) ->
      gc cx state target_module;
      gc cx state t_out;

and gc_id cx state id =
  let root_id, constraints = Flow_js.find_constraints cx id in (
    if state#mark id then (
      match constraints with
      | Resolved t -> gc cx state t
      | Unresolved bounds ->
          bounds.lower |> TypeMap.iter (fun t _ -> gc cx state t);
          bounds.upper |> TypeMap.iter (fun t _ -> gc cx state t);
    )
  );
  state#mark root_id |> ignore

and gc_typeparam cx state typeparam =
  gc cx state typeparam.bound

and gc_pred cx state = function

  | AndP (p1,p2)
  | OrP (p1,p2) ->
      gc_pred cx state p1;
      gc_pred cx state p2

  | LeftP (_, t)
  | RightP (_, t) ->
      gc cx state t

  | NotP (p) ->
      gc_pred cx state p

  | TrueP
  | FalseP
  | ExistsP
  | NullP
  | MaybeP
  | BoolP
  | FunP
  | NumP
  | ObjP
  | StrP
  | VoidP
  | ArrP
      -> ()

(* Keep a reachable type variable around. *)
let live cx state id =
  let constraints = Flow_js.find_graph cx id in
  match constraints with
  | Resolved _ -> ()
  | Unresolved bounds -> (
      bounds.uppertvars <-
        bounds.uppertvars |> IMap.filter (fun id _ -> state#marked id);
      bounds.lowertvars <-
        bounds.lowertvars |> IMap.filter (fun id _ -> state#marked id);
    )

(* Kill an unreachable type variable. *)
let die cx id = Context.remove_tvar cx id

(* flag controls in-module GC *)
let cleanup_enabled = ref true

(* Prune the graph given a GC state contained marked type variables. *)
let cleanup cx state =
  if !cleanup_enabled then (
    cx |> Context.graph |> IMap.iter (fun id _ ->
      if state#marked id
      then live cx state id
      else die cx id
    );
  )

(* Main entry point for graph pruning. *)
let do_gc cx ms =
  if Context.is_checked cx then (
    let state = new gc_state in
    List.iter
      (gc cx state)
      ((Flow_js.builtins cx)::(List.map (Flow_js.lookup_module cx) ms));
    cleanup cx state;
  )
