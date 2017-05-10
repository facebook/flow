(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

  | AbstractT (_, t) -> gc cx state t
  | AnnotT t -> gc cx state t
  | DefT (_, AnyFunT) -> ()
  | DefT (_, AnyObjT) -> ()
  | DefT (_, AnyT) -> ()
  | AnyWithLowerBoundT (t) -> gc cx state t
  | AnyWithUpperBoundT (t) -> gc cx state t
  | DefT (_, ArrT arraytype) ->
      gc_arraytype cx state arraytype
  | DefT (_, BoolT _) -> ()
  | BoundT typeparam -> gc_typeparam cx state typeparam
  | ChoiceKitT _ -> ()
  | DefT (_, ClassT t) -> gc cx state t
  | CustomFunT _ -> ()
  | DefT (_, NumT _)
  | DefT (_, StrT _)
    -> ()
  | DiffT (t1, t2) ->
      gc cx state t1;
      gc cx state t2;
  | DefT (_, EmptyT) -> ()
  | EvalT (t, defer_use_t, id) ->
      gc cx state t;
      gc_defer_use cx state defer_use_t;
      Flow_js.visit_eval_id cx id (gc cx state)
  | ExactT (_, t) -> gc cx state t
  | ExistsT _ -> ()
  | ExtendsT (_, ts, t1, t2) ->
      ts |> List.iter (gc cx state);
      gc cx state t1;
      gc cx state t2
  | FunProtoApplyT _ -> ()
  | FunProtoBindT _ -> ()
  | FunProtoCallT _ -> ()
  | FunProtoT _ -> ()
  | DefT (_, FunT (static, prototype, ft)) ->
      gc_funtype cx state ft;
      gc cx state prototype;
      gc cx state static
  | DefT (_, MixedT _) -> ()
  | IdxWrapper (_, t) -> gc cx state t
  | DefT (_, InstanceT(static, super, implements, instance)) ->
      instance.type_args |> SMap.iter (fun _ -> gc cx state);
      Context.find_props cx instance.fields_tmap
        |> Properties.iter_t (gc cx state);
      Context.find_props cx instance.methods_tmap
        |> Properties.iter_t (gc cx state);
      implements |> List.iter (gc cx state);
      gc cx state static;
      gc cx state super
  | DefT (_, IntersectionT rep) -> InterRep.members rep |> List.iter (gc cx state)
  | KeysT (_, t) -> gc cx state t
  | DefT (_, MaybeT t) -> gc cx state t
  | ModuleT (_, exporttypes) -> gc_exporttypes cx state exporttypes
  | DefT (_, NullT) -> ()
  | ObjProtoT _ -> ()
  | DefT (_, ObjT objtype) ->
      let id = objtype.props_tmap in
      Context.iter_props cx id (fun _ ->
        Property.iter_t (gc cx state));
      Option.iter objtype.dict_t (gc_dicttype cx state);
      gc cx state objtype.proto_t
  | OpenPredT (_, t, p_map, n_map) ->
      gc cx state t;
      gc_pred_map cx state p_map;
      gc_pred_map cx state n_map
  | DefT (_, OptionalT t) -> gc cx state t
  | DefT (_, PolyT (typeparams, t)) ->
      typeparams |> List.iter (gc_typeparam cx state);
      gc cx state t
  | ShapeT t -> gc cx state t
  | DefT (_, SingletonBoolT _) -> ()
  | DefT (_, SingletonNumT _) -> ()
  | DefT (_, SingletonStrT _) -> ()
  | ReposT (_, t) -> gc cx state t
  | ReposUpperT (_, t) -> gc cx state t
  | TaintT _ -> ()
  | ThisClassT (_, t) -> gc cx state t
  | ThisTypeAppT (_, t, this, ts) ->
      gc cx state t;
      gc cx state this;
      List.iter (gc cx state) ts
  | DefT (_, TypeAppT (t, ts)) ->
      gc cx state t;
      ts |> List.iter (gc cx state)
  | TypeMapT (_, _, t1, t2) ->
      gc cx state t1;
      gc cx state t2
  | DefT (_, TypeT t) -> gc cx state t
  | DefT (_, UnionT rep) -> UnionRep.members rep |> List.iter (gc cx state)
  | DefT (_, VoidT) -> ()


and gc_defer_use cx state = function
  | DestructuringT (_, s) ->
    gc_selector cx state s

  | TypeDestructorT (_, d) ->
    gc_destructor cx state d

and gc_dicttype cx state dicttype =
  gc cx state dicttype.key;
  gc cx state dicttype.value

and gc_funtype cx state funtype =
  gc cx state funtype.this_t;
  funtype.params_tlist |> List.iter (gc cx state);
  Option.iter
    ~f:(fun (_, _, t) -> gc cx state t)
    funtype.rest_param;
  gc cx state funtype.return_t

and gc_funcalltype cx state funcalltype =
  gc cx state funcalltype.call_this_t;
  funcalltype.call_args_tlist
    |> List.iter (function Arg t | SpreadArg t -> gc cx state t);
  gc cx state funcalltype.call_tout;

and gc_use cx state = function

  | UseT (_, t) ->
      gc cx state t

  (** use types **)

  | AdderT(_, t1, t2) -> gc cx state t1; gc cx state t2
  | AndT (_, t1, t2) -> gc cx state t1; gc cx state t2
  | ArrRestT (_, _, t) -> gc cx state t
  | AssertArithmeticOperandT _ -> ()
  | AssertBinaryInLHST _ -> ()
  | AssertBinaryInRHST _ -> ()
  | AssertForInRHST _ -> ()
  | AssertImportIsValueT _ -> ()
  | AssertRestParamT _ -> ()
  | BecomeT (_, t) -> gc cx state t
  | BindT(_, funcalltype, _) -> gc_funcalltype cx state funcalltype
  | CallLatentPredT (_, _, _, t1, t2) -> gc cx state t1; gc cx state t2
  | CallOpenPredT (_, _, _, t1, t2) -> gc cx state t1; gc cx state t2
  | CallElemT (_, _, i, fct) -> gc cx state i; gc_funcalltype cx state fct
  | CallT(_, funcalltype) -> gc_funcalltype cx state funcalltype
  | ChoiceKitUseT (_, choice_use_tool) ->
      gc_choice_use_tool cx state choice_use_tool
  | CJSExtractNamedExportsT (_, (_, exporttypes), t_out) ->
      gc_exporttypes cx state exporttypes;
      gc cx state t_out
  | CJSRequireT (_, t) -> gc cx state t
  | ComparatorT(_, t) -> gc cx state t
  | ConstructorT(_, params, t) ->
      List.iter (function Arg t | SpreadArg t -> gc cx state t) params;
      gc cx state t
  | CopyNamedExportsT (_, target_module, t_out) ->
      gc cx state target_module;
      gc cx state t_out;
  | CopyTypeExportsT (_, target_module, t_out) ->
      gc cx state target_module;
      gc cx state t_out;
  | DebugPrintT _ -> ()
  | ElemT (_, t1, action) ->
      gc cx state t1;
      gc_elem_action cx state action
  | EqT (_, t) -> gc cx state t
  | ExportNamedT (_, _, t_smap, t_out) ->
      List.iter (gc cx state) (SMap.values t_smap);
      gc cx state t_out
  | ExportTypeT (_, _, _, t, t_out) ->
      gc cx state t;
      gc cx state t_out
  | GetElemT(_, i, t) -> gc cx state i; gc cx state t
  | GetKeysT (_, t) -> gc cx state t
  | GetPropT(_, _, t) -> gc cx state t
  | GetStaticsT(_, t) -> gc cx state t
  | GuardT (pred, t1, t2) ->
      gc_pred cx state pred;
      gc cx state t1;
      gc cx state t2
  | HasOwnPropT _ -> ()
  | IdxUnMaybeifyT (_, t_out) -> gc cx state t_out
  | IdxUnwrap (_, t_out) -> gc cx state t_out
  | ImplementsT t -> gc cx state t
  | ImportDefaultT (_, _, _, t) -> gc cx state t
  | ImportModuleNsT (_, t) -> gc cx state t
  | ImportNamedT (_, _, _, t) -> gc cx state t
  | ImportTypeofT (_, _, t) -> gc cx state t
  | ImportTypeT (_, _, t) -> gc cx state t
  | IntersectionPreprocessKitT (_, intersection_preprocess_tool) ->
      gc_intersection_preprocess_tool cx state intersection_preprocess_tool
  | LookupT (_, _, ts, _, action) ->
      ts |> List.iter (gc cx state);
      (match action with
      | RWProp (t, _) ->
        gc cx state t
      | LookupProp (_, p)
      | SuperProp p ->
        Property.iter_t (gc cx state) p)
  | MakeExactT (_, k) -> gc_cont cx state k
  | MapTypeT (_, _, t, k) -> gc cx state t; gc_cont cx state k
  | MethodT(_, _, _, funcalltype) -> gc_funcalltype cx state funcalltype
  | MixinT (_, t) -> gc cx state t
  | NotT (_, t) -> gc cx state t
  | ObjAssignToT (_, t1, t2, _, _) -> gc cx state t1; gc cx state t2
  | ObjAssignFromT (_, t1, t2, _, _) -> gc cx state t1; gc cx state t2
  | ObjFreezeT (_, t) -> gc cx state t
  | ObjRestT (_, _, t) -> gc cx state t
  | ObjSealT (_, t) -> gc cx state t
  | ObjTestT (_, t1, t2) -> gc cx state t1; gc cx state t2
  | OrT (_, t1, t2) -> gc cx state t1; gc cx state t2
  | PredicateT (pred, t) -> gc_pred cx state pred; gc cx state t
  | ReactKitT (_, tool) -> gc_react_kit cx state tool
  | RefineT (_, pred, t) -> gc_pred cx state pred; gc cx state t
  | ReposLowerT (_, u) -> gc_use cx state u
  | ReposUseT (_, _, t) -> gc cx state t
  | SentinelPropTestT (t, _, _, t_out) -> gc cx state t; gc cx state t_out
  | SetElemT(_, i, t) -> gc cx state i; gc cx state t
  | SetPropT(_, _, t) -> gc cx state t
  | SpecializeT (_, _, _, ts, t) -> List.iter (gc cx state) ts; gc cx state t
  | ObjSpreadT (_, tool, state', t) ->
      gc_object_spread cx state tool state';
      gc cx state t
  | SubstOnPredT (_, _, t) -> gc cx state t
  | SuperT (_, instance) -> gc_insttype cx state instance
  | TestPropT(_, _, t) -> gc cx state t
  | ThisSpecializeT (_, this, t) -> gc cx state this; gc cx state t
  | UnaryMinusT (_, t) -> gc cx state t
  | UnifyT (t1, t2) -> gc cx state t1; gc cx state t2
  | VarianceCheckT (_, ts, _) -> List.iter (gc cx state) ts
  | TypeAppVarianceCheckT (_, _, targs) ->
    List.iter (fun (t1, t2) ->
      gc cx state t1;
      gc cx state t2
    ) targs

  | ResolveSpreadT (_, {
    rrt_resolved;
    rrt_unresolved;
    rrt_resolve_to;
  }) ->
      List.iter (function
        | ResolvedArg t -> gc cx state t
        | ResolvedSpreadArg (_, arraytype) ->
            gc_arraytype cx state arraytype
        | ResolvedAnySpreadArg _ -> ()
      ) rrt_resolved;
      List.iter (function
        | UnresolvedArg t
        | UnresolvedSpreadArg t -> gc cx state t
      ) rrt_unresolved;
      gc_spread_resolve cx state rrt_resolve_to


and gc_insttype cx state instance =
  instance.type_args |> SMap.iter (fun _ -> gc cx state);
  Context.iter_props cx instance.fields_tmap (fun _ ->
    Property.iter_t (gc cx state));
  Context.iter_props cx instance.methods_tmap (fun _ ->
    Property.iter_t (gc cx state))

and gc_arraytype cx state = function
| ArrayAT (elemt, None)
| ROArrayAT (elemt) ->
    gc cx state elemt;
| ArrayAT (elemt, Some tuple_types)
| TupleAT (elemt, tuple_types) ->
    gc cx state elemt;
    List.iter (gc cx state) tuple_types;
| EmptyAT -> ()

and gc_id cx state id =
  let root_id, constraints = Flow_js.find_constraints cx id in (
    if state#mark id then (
      match constraints with
      | Resolved t -> gc cx state t
      | Unresolved bounds ->
          bounds.lower |> TypeMap.iter (fun t _ -> gc cx state t);
          bounds.upper |> UseTypeMap.iter (fun t _ -> gc_use cx state t);
    )
  );
  state#mark root_id |> ignore

and gc_typeparam cx state typeparam =
  gc cx state typeparam.bound

and gc_selector cx state = function
  | Prop _ -> ()
  | Elem key -> gc cx state key
  | ObjRest _ -> ()
  | ArrRest _ -> ()
  | Default -> ()
  | Become -> ()
  | Refine _ -> ()

and gc_destructor cx state = function
  | NonMaybeType -> ()
  | PropertyType _ -> ()
  | Bind t -> gc cx state t
  | SpreadType (_, ts) -> List.iter (gc cx state) ts

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
  | SingletonBoolP _
  | SingletonStrP _
  | SingletonNumP _
  | PropExistsP _
      -> ()

  | LatentP (t, _) ->
      gc cx state t

and gc_pred_map cx state pred_map =
  Key_map.iter (fun _ p -> gc_pred cx state p) pred_map

and gc_object_spread =
  let open ObjectSpread in
  let gc_slice cx state (_, props, dict, _) =
    SMap.iter (fun _ (t, _) -> gc cx state t) props;
    Option.iter dict (gc_dicttype cx state)
  in
  let gc_resolved cx state xs =
    Nel.iter (gc_slice cx state) xs
  in
  let gc_resolve cx state = function
    | Next -> ()
    | List0 (todo, _) ->
      Nel.iter (gc cx state) todo
    | List (todo, acc, _) ->
      List.iter (gc cx state) todo;
      Nel.iter (gc_resolved cx state) acc
  in
  let gc_tool cx state = function
    | Resolve tool -> gc_resolve cx state tool
    | Super (slice, tool) ->
      gc_slice cx state slice;
      gc_resolve cx state tool
  in
  fun cx state tool {todo_rev; acc; make_exact=_} ->
    gc_tool cx state tool;
    List.iter (gc cx state) todo_rev;
    List.iter (gc_resolved cx state) acc

and gc_cont cx state = function
  | Lower t -> gc cx state t
  | Upper u -> gc_use cx state u

and gc_choice_use_tool cx state = function
  | FullyResolveType _ -> ()
  | TryFlow (_, spec) -> gc_spec cx state spec

and gc_spec cx state = function
  | UnionCases (t, ts) ->
    gc cx state t;
    List.iter (gc cx state) ts
  | IntersectionCases (ts, u) ->
    List.iter (gc cx state) ts;
    gc_use cx state u

and gc_intersection_preprocess_tool cx state = function
  | ConcretizeTypes (ts1, ts2, t, u) ->
    List.iter (gc cx state) ts1;
    List.iter (gc cx state) ts2;
    gc cx state t;
    gc_use cx state u
  | SentinelPropTest (_, _, t1, t2, t3) ->
    gc cx state t1;
    gc cx state t2;
    gc cx state t3
  | PropExistsTest (_, _, t1, t2) ->
    gc cx state t1;
    gc cx state t2

and gc_exporttypes cx state
  { exports_tmap; cjs_export; has_every_named_export } =
  ignore has_every_named_export;
  Context.find_exports cx exports_tmap
    |> SMap.iter (fun _ -> gc cx state);
  match cjs_export with
  | Some t -> gc cx state t
  | None -> ()

and gc_elem_action cx state = function
  | ReadElem t | WriteElem t -> gc cx state t
  | CallElem (_, fct) -> gc_funcalltype cx state fct

and gc_spread_resolve cx state = function
| ResolveSpreadsToArray (tout)
| ResolveSpreadsToArrayLiteral (_, tout)
| ResolveSpreadsToTuple (_, tout) ->
  gc cx state tout
| ResolveSpreadsToMultiflowCallFull (_, ft)
| ResolveSpreadsToMultiflowSubtypeFull (_, ft) ->
  gc_funtype cx state ft
| ResolveSpreadsToMultiflowPartial (_, ft, _, tout) ->
  gc_funtype cx state ft;
  gc cx state tout
| ResolveSpreadsToCallT (fct, tin) ->
  gc_funcalltype cx state fct;
  gc cx state tin

and gc_react_kit cx state =
  let open React in
  let gc_tlist = List.iter (gc cx state) in
  let gc_resolved_object (_, props, dict, _) =
    Properties.iter_t (gc cx state) props;
    Option.iter dict (gc_dicttype cx state)
  in
  let gc_resolve_array = function
  | ResolveArray -> ()
  | ResolveElem (todo, done_rev) ->
    gc_tlist todo;
    gc_tlist done_rev
  in
  let gc_resolve_object = function
  | ResolveObject -> ()
  | ResolveDict (dicttype, todo, acc) ->
    gc_dicttype cx state dicttype;
    Properties.iter_t (gc cx state) todo;
    gc_resolved_object acc
  | ResolveProp (_, todo, acc) ->
    Properties.iter_t (gc cx state) todo;
    gc_resolved_object acc
  in
  let gc_simplify_prop_type = SimplifyPropType.(function
  | ArrayOf | InstanceOf | ObjectOf -> ()
  | OneOf tool | OneOfType tool -> gc_resolve_array tool
  | Shape tool -> gc_resolve_object tool
  ) in
  let gc_create_class = CreateClass.(
    let iter_known f = function Known x -> f x | Unknown _ -> () in
    let iter_nullable f = function NotNull x -> f x | Null _ -> () in
    let gc_spec spec =
      let {
        obj; statics; prop_types;
        get_default_props; get_initial_state;
        unknown_mixins = _;
      } = spec in
      gc_resolved_object obj;
      Option.iter statics (iter_known gc_resolved_object);
      Option.iter prop_types (iter_known gc_resolved_object);
      gc_tlist get_default_props;
      gc_tlist get_initial_state
    in
    let gc_stack_head (obj, spec) =
      gc_resolved_object obj;
      gc_spec spec;
    in
    let gc_stack_tail = List.iter (fun (head, todo, mixins) ->
      gc_stack_head head;
      gc_tlist todo;
      List.iter (iter_known gc_spec) mixins
    ) in
    let gc_stack (head, tail) =
      gc_stack_head head;
      gc_stack_tail tail
    in
    let tool = function
    | Spec stack' -> gc_stack_tail stack'
    | Mixins stack -> gc_stack stack
    | Statics stack -> gc_stack stack
    | PropTypes (stack, tool) ->
      gc_stack stack;
      gc_resolve_object tool
    | DefaultProps (todo, default_props) ->
      gc_tlist todo;
      Option.iter default_props (iter_known gc_resolved_object)
    | InitialState (todo, initial_state) ->
      gc_tlist todo;
      Option.iter initial_state (iter_known (iter_nullable gc_resolved_object))
    in
    let knot {this; static; state_t; default_t} =
      gc cx state this;
      gc cx state static;
      gc cx state state_t;
      gc cx state default_t
    in
    fun t k -> tool t; knot k
  ) in
  function
  | CreateElement (t, t_out) ->
      gc cx state t;
      gc cx state t_out
  | SimplifyPropType (tool, t_out) ->
      gc_simplify_prop_type tool;
      gc cx state t_out
  | CreateClass (tool, knot, t_out) ->
      gc_create_class tool knot;
      gc cx state t_out

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
