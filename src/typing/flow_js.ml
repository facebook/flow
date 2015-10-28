(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module describes the subtyping algorithm that forms the core of
   typechecking. The algorithm (in its basic form) is described in Francois
   Pottier's thesis. The main data structures maintained by the algorithm are:
   (1) for every type variable, which type variables form its lower and upper
   bounds (i.e., flow in and out of the type variable); and (2) for every type
   variable, which concrete types form its lower and upper bounds. Every new
   subtyping constraint added to the system is deconstructed into its subparts,
   until basic flows between type variables and other type variables or concrete
   types remain; these flows are then viewed as links in a chain, bringing
   together further concrete types and type variables to participate in
   subtyping. This process continues till a fixpoint is reached---which itself
   is guaranteed to exist, and is usually reached in very few steps. *)

open Utils
open Utils_js
open Modes_js
open Reason_js
open Constraint_js
open Type

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static =
  AnyObjT (reason_of_string "object type for statics")

let dummy_prototype =
  MixedT (reason_of_string "empty prototype object")

let dummy_this =
  MixedT (reason_of_string "global object")

let mk_methodtype this tins ?params_names tout = {
  this_t = this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = 0;
  changeset = Changeset.empty
}

let mk_methodtype2 this tins ?params_names tout j = {
  this_t = this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = j;
  changeset = Changeset.empty
}

let mk_functiontype tins ?params_names tout = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = 0;
  changeset = Changeset.empty
}

let mk_functiontype2 tins ?params_names tout j = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = j;
  changeset = Changeset.empty
}

(* An object type has two flags, sealed and exact. A sealed object type cannot
   be extended. An exact object type accurately describes objects without
   "forgeting" any properties: so to extend an object type with optional
   properties, the object type must be exact. Thus, as an invariant, "not exact"
   logically implies "sealed" (and by contrapositive, "not sealed" implies
   "exact"; in other words, exact and sealed cannot both be false).

   Types of object literals are exact, but can be sealed or unsealed. Object
   type annotations are sealed but not exact. *)

let default_flags = {
  sealed = UnsealedInFile None;
  exact = true;
  frozen = false;
}

let mk_objecttype ?(flags=default_flags) dict map proto = {
  flags;
  dict_t = dict;
  props_tmap = map;
  proto_t = proto
}

(**************************************************************)

(* for now, we do speculative matching by setting this global.
   it's set to true when trying the arms of intersection or
   union types.
   when it's true, the error-logging function throws instead
   of logging, and the speculative harness catches.
   TODO
 *)
let throw_on_error = ref false

(* we keep a stack of reasons representing the operations
   taking place when flows are performed. the top op reason
   is used in messages for errors that take place during its
   residence.
 *)
module Ops : sig
  val clear : unit -> reason list
  val push : reason -> unit
  val pop : unit -> unit
  val peek : unit -> reason option
  val get : unit -> reason list
  val set : reason list -> unit
end = struct
  let ops = ref []
  let clear () = let orig = !ops in ops := []; orig
  let push r = ops := r :: !ops
  let pop () = ops := List.tl !ops
  let peek () = match !ops with r :: rs -> Some r | [] -> None
  let get () = !ops
  let set _ops = ops := _ops
end

let silent_warnings = false

exception FlowError of Errors_js.message list

let add_output cx error_kind ?op ?(trace_reasons=[]) message_list =
  if !throw_on_error
  then (
    raise (FlowError message_list)
  ) else (
    (if Context.is_verbose cx then
      prerr_endlinef "\nadd_output cx.file = %S\n%s"
        (string_of_filename (Context.file cx))
        (message_list
        |> List.map (fun message ->
             let loc, s = Errors_js.to_pp message in
             spf "%s: s = %S" (string_of_loc loc) s
           )
        |> String.concat "\n"));
    let error = Errors_js.({
      kind = error_kind;
      messages = message_list;
      op;
      trace = trace_reasons
    }) in

    (* catch no-loc errors early, before they get into error map *)
    Errors_js.(
      if Loc.source (loc_of_error error) = None
      then assert_false (spf "add_output: no source for error: %s"
        (Hh_json.json_to_multiline (json_of_errors [error])))
    );

    if error_kind = Errors_js.ParseError ||
       error_kind = Errors_js.InferError ||
       not silent_warnings
    then Context.add_error cx error
  )

(* tvars *)

let mk_tvar cx reason =
  let tvar = mk_id () in
  let graph = Context.graph cx in
  Context.add_tvar cx tvar (Constraint_js.new_unresolved_root ());
  (if Context.is_verbose cx then prerr_endlinef
    "TVAR %d (%d): %s" tvar (IMap.cardinal graph)
    (string_of_reason reason));
  OpenT (reason, tvar)

let mk_tvar_where cx reason f =
  let tvar = mk_tvar cx reason in
  f tvar;
  tvar

(* This function is used in lieu of mk_tvar_where or mk_tvar when the reason
   must be marked internal. This has the effect of not forcing annotations where
   this type variable appears. See `assume_ground` and `assert_ground`. *)
let mk_tvar_derivable_where cx reason f =
  let reason = derivable_reason reason in
  mk_tvar_where cx reason f

(* Find the constraints of a type variable in the graph.

   Recall that type variables are either roots or goto nodes. (See
   constraint_js.ml for details.) If the type variable is a root, the
   constraints are stored with the type variable. Otherwise, the type variable
   is a goto node, and it points to another type variable: a linked list of such
   type variables must be traversed until a root is reached. *)
let rec find_graph cx id =
  let _, constraints = find_constraints cx id in
  constraints

and find_constraints cx id =
  let root_id, root = find_root cx id in
  root_id, root.constraints

(* Find the root of a type variable, potentially traversing a chain of type
   variables, while short-circuiting all the type variables in the chain to the
   root during traversal to speed up future traversals. *)
and find_root cx id =
  match IMap.get id (Context.graph cx) with
  | Some (Goto next_id) ->
      let root_id, root = find_root cx next_id in
      if root_id != next_id then replace_node cx id (Goto root_id) else ();
      root_id, root

  | Some (Root root) ->
      id, root

  | None ->
      let msg = spf "find_root: tvar %d not found in file %s" id
        (string_of_filename (Context.file cx))
      in
      failwith msg

(* Replace the node associated with a type variable in the graph. *)
and replace_node cx id node = Context.set_tvar cx id node

(* Check that id1 is not linked to id2. *)
let not_linked (id1, bounds1) (id2, bounds2) =
  (* It suffices to check that id1 is not already in the lower bounds of
     id2. Equivalently, we could check that id2 is not already in the upper
     bounds of id1. *)
  not (IMap.mem id1 bounds2.lowertvars)

(**********)
(* frames *)
(**********)

(* note: this is here instead of Env_js because of circular deps:
  Env_js is downstream of Flow_js due general utility funcs such as
  Flow_js.mk_tvar and builtins services. If the flow algorithm can
  be split away from these, then Env_js can be moved upstream and
  this code can be merged into it. *)

(* background:
   - each scope has an id. scope ids are unique, mod cloning
   for path-dependent analysis.
   - an environment is a scope list
   - each context holds a map of environment snapshots, keyed
   by their topmost scope ids
   - every function type contains a frame id, which maps to
   the environment in which it was defined; as well as a
   changeset containing its reads/writes/refinements on
   closed-over variables

   Given frame ids for calling function and called function and
   the changeset of the called function, here we retrieve the
   environment snapshots for the two functions, find the prefix
   of scopes they share, and havoc the variables in the called
   function's write set which live in those scopes.
 *)
let havoc_call_env = Scope.(

  let overlapped_call_scopes func_env call_env =
    let rec loop = Scope.(function
      | func_scope :: func_scopes, call_scope :: call_scopes
          when func_scope.id = call_scope.id ->
        call_scope :: loop (func_scopes, call_scopes)
      | _ -> []
    ) in
    loop (List.rev func_env, List.rev call_env)
  in

  let havoc_entry cx scope ((_, name, _) as entry_ref) =
    (if Context.is_verbose cx then
      prerr_endlinef "%d havoc_entry %s %s" (Unix.getpid ())
        (Changeset.string_of_entry_ref entry_ref)
        (Debug_js.string_of_scope cx scope)
      );
    match get_entry name scope with
    | Some _ ->
      havoc_entry (fun gen -> gen) name scope;
      Changeset.(if is_active () then add_var_change entry_ref)
    | None ->
      (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
      if is_global scope then ()
      else assert_false (spf "missing entry %S in scope %d: { %s }"
        name scope.id (String.concat ", "
          (SMap.fold (fun n _ acc -> n :: acc) scope.entries [])))
  in

  let havoc_refi cx scope ((_, key, _) as refi_ref) =
    (if Context.is_verbose cx then
      prerr_endlinef "%d havoc_refi %s" (Unix.getpid ())
        (Changeset.string_of_refi_ref refi_ref));
    match get_refi key scope with
    | Some _ ->
      havoc_refi key scope;
      Changeset.(if is_active () then add_refi_change refi_ref)
    | None ->
      (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
      if is_global scope then ()
      else assert_false (spf "missing refi %S in scope %d: { %s }"
        (Key.string_of_key key) scope.id
        (String.concat ", " (KeyMap.fold (
          fun k _ acc -> (Key.string_of_key k) :: acc) scope.refis [])))
  in

  fun cx func_frame call_frame changeset ->
    if func_frame = 0 || call_frame = 0 || Changeset.is_empty changeset
    then ()
    else
      let func_env = IMap.find_unsafe func_frame (Context.envs cx) in
      let call_env = IMap.find_unsafe call_frame (Context.envs cx) in
      overlapped_call_scopes func_env call_env |>
        List.iter (fun ({ id; _ } as scope) ->
          Changeset.include_scopes [id] changeset |>
            Changeset.iter_writes
              (havoc_entry cx scope)
              (havoc_refi cx scope)
      )
)

(***************)
(* print utils *)
(***************)

let lib_reason r =
  let loc = loc_of_reason r in
  Loc.(match loc.source with
  | Some LibFile _ -> true
  | Some Builtins -> true
  | Some SourceFile _ -> false
  | None -> false)

let ordered_reasons l u =
  let rl = reason_of_t l in
  let ru = reason_of_t u in
  if is_use u ||
      loc_of_t l = Loc.none ||
      (lib_reason rl && not (lib_reason ru))
  then ru, rl
  else rl, ru

let reasons_overlap r1 r2 = Loc.contains (loc_of_reason r1) (loc_of_reason r2)

(* format an error or warning and add it to flow's output.
   here preformatted trace output is passed directly as an argument *)
let prmsg_flow_trace_reasons cx level trace_reasons msg (r1, r2) = Errors_js.(
  let op, info = match Ops.peek () with
  | Some r when not (reasons_overlap r r1 && reasons_overlap r r2) ->
    (* NOTE: We include the operation's reason in the error message, unless it
       overlaps *both* endpoints. *)
    Some (message_of_reason r), []
  | _ ->
    if lib_reason r1 && lib_reason r2
    then
      (* Since pointing to endpoints in the library without any information on
         the code that uses those endpoints inconsistently is useless, we point
         to the file containing that code instead. Ideally, improvements in
         error reporting would cause this case to never arise. *)
      let loc = Loc.({ none with source = Some (Context.file cx) }) in
      None, [BlameM (loc, "inconsistent use of library definitions")]
    else
      None, []
  in
  let message_list = [
    message_of_reason r1;
    message_of_string msg;
    message_of_reason r2
  ]
  in
  add_output cx level ?op ~trace_reasons (info @ message_list)
)

(* format a trace into list of (reason, desc) pairs used
   downstream for obscure reasons *)
let make_trace_reasons trace =
  if modes.traces = 0 then [] else
    Trace.reasons_of_trace ~level:modes.traces trace
    |> List.map Errors_js.message_of_reason

(* format an error or warning and add it to flow's output.
   here we gate trace output on global settings *)
let prmsg_flow cx level trace msg (r1, r2) =
  let trace_reasons = make_trace_reasons trace in
  prmsg_flow_trace_reasons cx level trace_reasons msg (r1, r2)

let prmsg_flow_prop_not_found cx trace (r1, r2) =
  prmsg_flow cx Errors_js.InferError trace "Property not found in" (r1, r2)

(* format an error and add it to flow's output.
   here we print the full trace, ignoring global settings.
   Notes
   1. since traces can be gigantic, we set the per-level
   indentation to 0 when printing them here.
   2. as an optimization, we never accumulate traces beyond
   a single level if the global setting disables them.
   Being downstream of that throttle, we are subject to that
   restriction here.
*)
let prerr_flow_full_trace cx trace msg l u =
  let trace_reasons =
    Trace.reasons_of_trace ~level:(Trace.trace_depth trace + 1) trace
    |> List.map Errors_js.message_of_reason
  in
  prmsg_flow_trace_reasons cx
    Errors_js.InferError
    trace_reasons
    msg
    (ordered_reasons l u)

(* format an error and add it to flow's output *)
let prerr_flow cx trace msg l u =
  prmsg_flow cx
    Errors_js.InferError
    trace
    msg
    (ordered_reasons l u)

(* format a warning and add it to flow's output *)
let prwarn_flow cx trace msg l u =
  prmsg_flow cx
    Errors_js.InferWarning
    trace
    msg
    (ordered_reasons l u)

let tweak_output list =
  List.concat (
    List.map (fun (reason, msg) -> Errors_js.(
      [message_of_reason reason]
      @ (if msg = "" then [] else [message_of_string msg])
    )) list
  )

let add_msg cx ?trace level list =
  let trace_reasons = match trace with
  | Some trace -> Some (make_trace_reasons trace)
  | None -> None
  in
  add_output cx level ?trace_reasons (tweak_output list)

(* for outside calls *)
let new_warning list = Errors_js.({
  kind = InferWarning;
  messages = tweak_output list;
  op = None;
  trace = []
})

let add_warning cx ?trace list =
  add_msg cx ?trace Errors_js.InferWarning list

let new_error list = Errors_js.({
  kind = InferError;
  messages = tweak_output list;
  op = None;
  trace = []
})

let add_error cx ?trace list =
  add_msg cx ?trace Errors_js.InferError list

(********************************************************************)

(* Since type maps use the built-in compare function to compare types,
   we need to be careful to keep the shape of types within the boundaries
   of that function. In particular, comparison behaves in unexpected ways
   for references. To get around these issues, we denote references with
   indices in types, and maintain side tables of those indices to the
   denoted references. *)

let mk_propmap cx pmap =
  let id = mk_id () in
  Context.add_property_map cx id pmap;
  id

let find_props cx id = Context.find_props cx id

let has_prop cx id x =
  find_props cx id |> SMap.mem x

let read_prop cx id x =
  find_props cx id |> SMap.find_unsafe x

(* suitable replacement for has_prop; read_prop *)
let read_prop_opt cx id x =
  find_props cx id |> SMap.get x

let read_and_delete_prop cx id x =
  find_props cx id |> (fun pmap ->
    let t = SMap.find_unsafe x pmap in
    let pmap = SMap.remove x pmap in
    Context.add_property_map cx id pmap;
    t
  )

let write_prop cx id x t =
  let pmap = find_props cx id in
  let pmap = SMap.add x t pmap in
  Context.add_property_map cx id pmap

let iter_props cx id f =
  find_props cx id
  |> SMap.iter f

let iter_props_ cx id f =
  find_props cx id
  |> SMap.filter (fun x _ -> not (is_internal_name x))
  |> SMap.iter f

(***************)
(* strict mode *)
(***************)

(* For any constraints, return a list of def types that form either the lower
   bounds of the solution, or a singleton containing the solution itself. *)
let types_of constraints =
  match constraints with
  | Unresolved { lower; _ } -> TypeMap.keys lower
  | Resolved t -> [t]

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of (find_graph cx id)

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

(* Gather the possible types of the tvar associated with the given id, and group
   them by their reason's description. Return a map from those descriptions to
   one such matching type and a count.

   NOTE: here we rely on the unchecked invariant that types are reliably tagged
   by their reason descriptions, i.e., that any two nontrivially different types
   will always have different reasons.  Given the number of places where reasons
   are freely created in code, this is a vulnerability.
   TODO: find a way to guarantee, or collate types on some other basis.
*)
let distinct_possible_types cx id =
  let types = possible_types cx id in
  List.fold_left (fun map t ->
    let desc = desc_of_reason (reason_of_t t) in
    let info = match SMap.get desc map with
      | Some (t0, count) -> (t0, count + 1)
      | None -> (t, 1)
    in
    SMap.add desc info map
  ) SMap.empty types

let suggested_type_cache = ref IMap.empty

let rec list_map2 f ts1 ts2 = match (ts1,ts2) with
  | ([],_) | (_,[]) -> []
  | (t1::ts1,t2::ts2) -> (f (t1,t2)):: (list_map2 f ts1 ts2)

module TypeSet = Set.Make(Type)

let create_union ts =
  UnionT (reason_of_string "union", ts)

let rec merge_type cx = function
  | (NumT _, (NumT _ as t))
  | (StrT _, (StrT _ as t))
  | (BoolT _, (BoolT _ as t))
  | (NullT _, (NullT _ as t))
  | (VoidT _, (VoidT _ as t))
  | (TaintT _, ((TaintT _) as t))
      -> t

  | (AnyT _, t) | (t, AnyT _) -> t

  | (EmptyT _, t) | (t, EmptyT _) -> t
  | (_, (MixedT _ as t)) | ((MixedT _ as t), _) -> t

  | (NullT _, (MaybeT _ as t)) | ((MaybeT _ as t), NullT _)
  | (VoidT _, (MaybeT _ as t)) | ((MaybeT _ as t), VoidT _) ->
      t

  | (FunT (_,_,_,ft1), FunT (_,_,_,ft2)) ->
      let tins =
        try
          List.map2 (fun t1 t2 -> merge_type cx (t1,t2))
            ft1.params_tlist ft2.params_tlist
        with _ ->
          [AnyT.t] (* TODO *)
      in
      let tout = merge_type cx (ft1.return_t, ft2.return_t) in
      (* TODO: How to merge parameter names? *)
      FunT (
        reason_of_string "function",
        dummy_static, dummy_prototype,
        mk_functiontype tins tout
      )

  | (ObjT (_,ot1), ObjT (_,ot2)) ->
      (* TODO: How to merge indexer names? *)
      let dict = match ot1.dict_t, ot2.dict_t with
        | None, None -> None
        | Some dict, None | None, Some dict -> Some dict
        | Some dict1, Some dict2 ->
            Some {
              dict_name = None;
              key = merge_type cx (dict1.key, dict2.key);
              value = merge_type cx (dict1.value, dict2.value);
            }
      in
      let pmap =
        let map1 = find_props cx ot1.props_tmap in
        let map2 = find_props cx ot2.props_tmap in
        let map =
          SMap.merge
            (fun x t1_opt t2_opt -> match (t1_opt,t2_opt) with
              | (None,None) -> None
              | (Some t, None) | (None, Some t) -> Some t
              | (Some t1, Some t2) -> Some (merge_type cx (t1, t2))
            ) map1 map2 in
        mk_propmap cx map
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        mk_objecttype dict pmap proto
      )

  | (ArrT (_,t1,ts1), ArrT (_,t2,ts2)) ->
      ArrT (
        reason_of_string "array",
        merge_type cx (t1, t2),
        list_map2 (merge_type cx) ts1 ts2
      )

  | (MaybeT t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | (MaybeT t1, t2)
  | (t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | (UnionT (_, ts1), UnionT (_, ts2)) ->
      create_union (List.rev_append ts1 ts2)

  | (UnionT (_, ts), t)
  | (t, UnionT (_, ts)) ->
      create_union (t :: ts)

  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)

  | (t1, t2) ->
      create_union [t1; t2]

(* This function does not only resolve every OpenT recursively, but also
   replaces the reasons of types with a uniform ones. It is a left-over bit
   from the old ground_type behavior. *)
and ground_type_impl cx ids t = match t with
  | BoundT _ -> t
  | OpenT (reason, id) ->
      lookup_type cx ids id

  | NumT _ -> NumT.t
  | StrT _ -> StrT.t
  | BoolT _ -> BoolT.t
  | EmptyT _ -> EmptyT.t
  | NullT _ -> NullT.t
  | VoidT _ -> VoidT.t
  | MixedT _ -> MixedT.t
  | AnyT _ -> AnyT.t

  | TaintT _ -> TaintT (reason_of_string "taint")

  | SingletonStrT (_, s) ->
    SingletonStrT (reason_of_string "string singleton", s)
  | SingletonNumT (_, n) ->
    SingletonNumT (reason_of_string "number singleton", n)
  | SingletonBoolT (_, b) ->
    SingletonBoolT (reason_of_string "boolean singleton", b)

  | FunT (_, _, _, ft) ->
      let tins = List.map (ground_type_impl cx ids) ft.params_tlist in
      let params_names = ft.params_names in
      let tout = ground_type_impl cx ids ft.return_t in
      FunT (
        reason_of_string "function",
        dummy_static, dummy_prototype,
        mk_functiontype tins ?params_names tout
      )

  | ObjT (_, ot) ->
      let dict = match ot.dict_t with
        | None -> None
        | Some dict ->
            Some { dict with
              key = (ground_type_impl cx ids dict.key);
              value = (ground_type_impl cx ids dict.value);
            }
      in
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (ground_type_impl cx ids)
        |> mk_propmap cx
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        mk_objecttype dict pmap proto
      )

  | ArrT (_, t, ts) ->
      ArrT (reason_of_string "array",
            ground_type_impl cx ids t,
            ts |> List.map (ground_type_impl cx ids))

  | MaybeT t ->
      MaybeT (ground_type_impl cx ids t)

  | PolyT (xs, t) ->
      PolyT (xs, ground_type_impl cx ids t)

  | ClassT t ->
      ClassT (ground_type_impl cx ids t)

  | TypeT (_, t) ->
      TypeT (reason_of_string "type",
             ground_type_impl cx ids t)

  | InstanceT (_, static, super, it) ->
      t (* nominal type *)

  | RestT t ->
      RestT (ground_type_impl cx ids t)

  | OptionalT t ->
      OptionalT (ground_type_impl cx ids t)

  | TypeAppT (c, ts) ->
      let c = ground_type_impl cx ids c in
      let ts = List.map (ground_type_impl cx ids) ts in
      TypeAppT (c, ts)

  | IntersectionT (_, ts) ->
      IntersectionT (
        reason_of_string "intersection",
        List.map (ground_type_impl cx ids) ts
      )

  | UnionT (_, ts) ->
      create_union (List.map (ground_type_impl cx ids) ts)

  | LowerBoundT t ->
      LowerBoundT (ground_type_impl cx ids t)

  | UpperBoundT t ->
      UpperBoundT (ground_type_impl cx ids t)

  | AnyObjT _ -> AnyObjT (reason_of_string "any object")
  | AnyFunT _ -> AnyFunT (reason_of_string "any function")

  | ShapeT t ->
      ShapeT (ground_type_impl cx ids t)
  | DiffT (t1, t2) ->
      DiffT (ground_type_impl cx ids t1, ground_type_impl cx ids t2)

  | AnnotT (t1, t2) ->
      AnnotT (ground_type_impl cx ids t1, ground_type_impl cx ids t2)

  | KeysT (_, t) ->
      KeysT (reason_of_string "key set", ground_type_impl cx ids t)

  | _ ->
    (** TODO **)
    failwith (spf "Unsupported type in ground_type_impl: %s" (string_of_ctor t))

and lookup_type_ cx ids id =
  if ISet.mem id ids then assert false
  else
    let ids = ISet.add id ids in
    let types = possible_types cx id in
    try
      List.fold_left
        (fun u t -> merge_type cx (ground_type_impl cx ids t, u))
        EmptyT.t types
    with _ ->
      AnyT.t

and lookup_type cx ids id =
  match IMap.get id !suggested_type_cache with
  | None ->
      let t = lookup_type_ cx ids id in
      suggested_type_cache := !suggested_type_cache |> IMap.add id t;
      t
  | Some t -> t

and resolve_type cx = function
  | OpenT (_, id) ->
      let ts = possible_types cx id in
      (* The list of types returned by possible_types is often empty, and the
         most common reason is that we don't have enough type coverage to
         resolve id. Thus, we take the unit of merging to be `any`. (Something
         similar happens when summarizing exports in ContextOptimizer below.)

         In the future, we might report errors in some cases where
         possible_types returns an empty list: e.g., when we detect unreachable
         code, or even we don't have enough type coverage. Irrespective of these
         changes, the above decision would continue to make sense: as errors
         become stricter, type resolution should become even more lenient to
         improve failure tolerance.  *)
      List.fold_left (fun u t ->
        merge_type cx (t, u)
      ) AnyT.t ts
  | t -> t

let rec normalize_type cx t =
  match t with
  | FunT (r, static, proto, ft) ->
      let ft =
        { ft with
          return_t = normalize_type cx ft.return_t;
          params_tlist = List.map (normalize_type cx) ft.params_tlist; } in
      FunT (r,
        normalize_type cx static,
        normalize_type cx proto,
        ft)

  | ObjT (r, ot) ->
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (normalize_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = (match ot.dict_t with
          | None -> None
          | Some dict ->
              Some { dict with
                key = normalize_type cx dict.key;
                value = normalize_type cx dict.value;
              });
          proto_t = normalize_type cx ot.proto_t;
          props_tmap = pmap; })

  | UnionT (r, ts) ->
      normalize_union cx r ts

  | IntersectionT (r, ts) ->
      normalize_intersection cx r ts

  | MaybeT t ->
      let t = normalize_type cx t in
      (match t with
      | MaybeT _ -> t
      | _ -> MaybeT t)

  | OptionalT t ->
      OptionalT (normalize_type cx t)

  | RestT t ->
      RestT (normalize_type cx t)

  | ArrT (r, t, ts) ->
      ArrT (r, normalize_type cx t, List.map (normalize_type cx) ts)

  | PolyT (xs, t) ->
      PolyT (xs, normalize_type cx t)

  | TypeAppT (c, ts) ->
      TypeAppT (
        normalize_type cx c,
        List.map (normalize_type cx) ts
      )

  | LowerBoundT t ->
      LowerBoundT (normalize_type cx t)

  | UpperBoundT t ->
      UpperBoundT (normalize_type cx t)

  | ShapeT t ->
      ShapeT (normalize_type cx t)
  | DiffT (t1, t2) ->
      DiffT (normalize_type cx t1, normalize_type cx t2)

  (* TODO: Normalize all types? *)
  | t -> t

(* TODO: This is not an exhaustive list of normalization steps for unions.
   For example, we might want to get rid of AnyT in the union similar to how
   merge_type gets rid of AnyT. Decide on rules like these and implement them
   if required. *)
and normalize_union cx r ts =
  let ts = List.map (normalize_type cx) ts in
  let ts = collect_union_members ts in
  let (ts, has_void, has_null) =
    TypeSet.fold (fun t (ts, has_void, has_null) ->
      match t with
      | MaybeT (UnionT (_, tlist)) ->
          let ts = List.fold_left (fun acc t -> TypeSet.add t acc) ts tlist in
          (ts, true, true)
      | MaybeT t -> (TypeSet.add t ts, true, true)
      | VoidT _ -> (ts, true, has_null)
      | NullT _ -> (ts, has_void, true)
      (* TODO: We should only get EmptyT here when a completely open type
         variable has been in the union before grounding it. This happens when
         "null" is passed to a function parameter. We throw this out because
         it gives no information at all. merge_type also ignores EmptyT. *)
      | EmptyT _ -> (ts, has_void, has_null)
      | _ -> (TypeSet.add t ts, has_void, has_null)
    ) ts (TypeSet.empty, false, false) in
  let ts =
    match (has_void, has_null) with
    | (true, false) -> TypeSet.add VoidT.t ts
    | (false, true) -> TypeSet.add NullT.t ts
    | _ ->
        (* We should never get an empty set at this point but better safe than
           sorry. Stripping out EmptyT above might be unsafe. *)
        if TypeSet.is_empty ts
        then TypeSet.singleton EmptyT.t
        else ts
  in
  let ts = TypeSet.elements ts in
  let t =
    match ts with
    | [t] -> t
    | _ -> UnionT (r, ts)
  in
  if has_void && has_null
  then MaybeT t
  else t

and collect_union_members ts =
  List.fold_left (fun acc x ->
      match x with
      | UnionT (_, ts) -> TypeSet.union (collect_union_members ts) acc
      | _ -> TypeSet.add x acc
    ) TypeSet.empty ts

(* TODO: This does not do any real normalization yet, it only flattens the
   intesection. Think about normalization rules and implement them when there
   is need for that. *)
and normalize_intersection cx r ts =
  let ts = List.map (normalize_type cx) ts in
  let ts = collect_intersection_members ts in
  let ts = TypeSet.elements ts in
  match ts with
  | [t] -> t
  | _ -> IntersectionT (r, ts)

and collect_intersection_members ts =
  List.fold_left (fun acc x ->
      match x with
      | IntersectionT (_, ts) ->
          TypeSet.union acc (collect_intersection_members ts)
      | _ ->
          TypeSet.add x acc
    ) TypeSet.empty ts

let ground_type cx type_ =
  ground_type_impl cx ISet.empty type_

let rec printify_type cx t =
  match t with
  | FunT (r, static, proto, ft) ->
      let ft =
        { ft with
          return_t = printify_type cx ft.return_t;
          params_tlist = List.map (printify_type cx) ft.params_tlist; } in
      FunT (r,
        printify_type cx static,
        printify_type cx proto,
        ft)

  | ObjT (r, ot) ->
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (printify_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = (match ot.dict_t with
          | None -> None
          | Some dict ->
              Some { dict with
                key = printify_type cx dict.key;
                value = printify_type cx dict.value;
              });
          proto_t = printify_type cx ot.proto_t;
          props_tmap = pmap; })

  | UnionT (r, ts) ->
      let (ts, add_maybe) =
        List.fold_left (fun (ts, add_maybe) t ->
            let t = printify_type cx t in
            match t with
            | NullT _ -> (ts, true)
            | _ -> (t :: ts, add_maybe)
          ) ([], false) ts
      in
      (* strictly speaking this is a combination of normalization and
         transformation for printability, but it allows us to get rid of
         another normalize_type call. *)
      let t =
        match ts with
        | [t] -> t
        | _ -> UnionT (r, ts)
      in
      if add_maybe
      then MaybeT t
      else t

  | IntersectionT (r, ts) ->
      IntersectionT (r, List.map (printify_type cx) ts)

  | MaybeT t ->
      (* strictly speaking this is a combination of normalization and
         transformation for printability, but it allows us to get rid of
         another normalize_type call. *)
      let t = printify_type cx t in
      (match t with
      | MaybeT _ -> t
      | _ -> MaybeT t)

  | OptionalT t ->
      OptionalT (printify_type cx t)

  | RestT t ->
      RestT (printify_type cx t)

  | ArrT (r, t, ts) ->
      ArrT (r, printify_type cx t, List.map (printify_type cx) ts)

  | PolyT (xs, t) ->
      PolyT (xs, printify_type cx t)

  | TypeAppT (c, ts) ->
      TypeAppT (
        printify_type cx c,
        List.map (printify_type cx) ts
      )

  | LowerBoundT t ->
      LowerBoundT (printify_type cx t)

  | UpperBoundT t ->
      UpperBoundT (printify_type cx t)

  | ShapeT t ->
      ShapeT (printify_type cx t)
  | DiffT (t1, t2) ->
      DiffT (printify_type cx t1, printify_type cx t2)

  | t -> t

let printified_type cx t =
  let t = ground_type cx t in
  let t = normalize_type cx t in
  printify_type cx t

(* Check whether any of the def types describing a solution for a type variable
   satisfy a predicate. *)
let check_types cx id f =
  let types = possible_types cx id in
  List.exists f types

(** The following functions do "shallow" walks over types, respectively from
    requires and from exports, in order to report missing annotations. There are
    some opportunities for future work:

    - Rewrite these functions using a type visitor class.

    - Consider using gc to crawl the graph further down from requires, and
    maybe also up from exports. Preliminary experiments along those lines
    suggest that a general walk doesn't always give expected results. As an
    example in one direction, the signature of a class is reachable from a
    `require`d superclass, but the corresponding constraint simply checks for
    consistency of overrides, and should not relax reporting missing annotations
    in the signature. As an example in the other direction, an exported function
    may have an open `this` type that we cannot expect to be annotated.
**)

(* To avoid complaining about "missing" annotations where external types are
   used in the exported type, we mark requires and their uses as types. *)

(* TODO: All said and done, this strategy to avoid complaining about missing
   annotations that depend on requires is a hack intended to achieve the ideal
   of being able to "look up" annotations in required modules, when they're
   already provided. The latter should be possible if we switch reporting
   missing annotations from early (during the "infer" phase) to late (during
   the "merge" phase). *)

let rec assume_ground cx ids = function
  | OpenT(_,id) ->
      assume_ground_id cx ids id

  (* The subset of operations to crawl. The type variables denoting the results
     of these operations would be ignored by the is_required check in
     `assert_ground`.

     These are intended to be exactly the operations that might be involved when
     extracting (parts of) requires/imports. As such, they need to be kept in
     sync as module system conventions evolve. *)

  | ImportModuleNsT(_,t)
  | CJSRequireT(_,t)
  | ImportTypeT(_,t)
  | ImportTypeofT(_,t)
  | ReposLowerT (_, t)

  (* Other common operations that might happen immediately after extracting
     (parts of) requires/imports. *)

  | GetPropT(_,_,t)
  | CallT(_,{return_t = t; _})
  | MethodT(_,_,{return_t = t; _})
  | ConstructorT(_,_,t)

      -> assume_ground cx ids t

  | _ -> ()

and assume_ground_id cx ids id =
  if not (ISet.mem id !ids) then (
    ids := !ids |> ISet.add id;
    let constraints = find_graph cx id in
    match constraints with
    | Unresolved { upper; uppertvars; _ } ->
      upper |> TypeMap.iter (fun t _ ->
        assume_ground cx ids t
      );
      uppertvars |> IMap.iter (fun id _ ->
        assume_ground_id cx ids id
      )
    | Resolved t ->
      assume_ground cx ids t
  )

(**************)
(* builtins *)
(**************)

(* Every context has a local reference to builtins (along with local references
   to other modules that are discovered during type checking, such as modules
   required by it, the module it provides, and so on). *)
let mk_builtins cx =
  let builtins = mk_tvar cx (builtin_reason "module") in
  Context.add_module cx Files_js.lib_module builtins

(* Local references to modules can be looked up. *)
let lookup_module cx m = Context.find_module cx m

(* The builtins reference is accessed just like references to other modules. *)
let builtins cx =
  lookup_module cx Files_js.lib_module

let restore_builtins cx b =
  Context.add_module cx Files_js.lib_module b

(* new contexts are prepared here, so we can install shared tvars *)
let fresh_context metadata file module_name =
  let cx = Context.make metadata file module_name in
  (* add types for pervasive builtins *)
  mk_builtins cx;
  cx

(* created in the master process (see Server.preinit), populated and saved to
   ContextHeap. forked workers will have an empty replica from the master, but
   it's useless. should only be accessed through ContextHeap. *)
let master_cx, restore_master_cx =
  let cx_ = ref None in
  let master_cx () =
    match !cx_ with
    | None ->
      let cx = fresh_context { Context.
        checked = false;
        weak = false;
        munge_underscores = false;
        verbose = None;
        is_declaration_file = false;
      } Loc.Builtins (Modulename.String Files_js.lib_module) in
      cx_ := Some cx;
      cx
    | Some cx -> cx in
  let restore_master_cx cx = cx_ := Some cx in
  master_cx, restore_master_cx

(********)

module Union(M: MapSig) =
  struct
    (* This function is used to merge two maps. It is parametric on the key
       type. When the maps overlap, the second map overrides the first map. *)
    let union f map1 map2 =
      M.fold (fun k v map ->
        if M.mem k map then map else M.add k (f v) map
      ) map1 map2
  end

module UnionIMap = Union(IMap)
module UnionTypeMap = Union(TypeMap)

let debug_count =
  let count = ref 0 in
  fun f ->
    incr count;
    prerr_endlinef "[%d] %s" !count (f())

let debug_flow (l,u) =
  spf "%s ~> %s" (string_of_ctor l) (string_of_ctor u)

(***********************)
(* instantiation utils *)
(***********************)

module ImplicitTypeArgument = struct
  (* helpers *)
  let add_typeparam_prefix s = spf "type parameter%s" s
  let has_typeparam_prefix s =
    (String.length s) >= 14 && (String.sub s 0 14) = "type parameter"

  (* Make a type argument for a given type parameter, given a reason. Note that
     not all type arguments are tvars; the following function is used only when
     polymorphic types need to be implicitly instantiated, because there was no
     explicit instantiation (via a type application), or when we want to cache a
     unique instantiation and unify it with other explicit instantiations. *)
  let mk_targ cx (typeparam, reason_op) =
    let prefix_desc = add_typeparam_prefix (spf " `%s` of " typeparam.name) in
    mk_tvar cx (prefix_reason prefix_desc reason_op)

  (* Abstract a type argument that is created by implicit instantiation
     above. Sometimes, these type arguments are involved in type expansion loops,
     so we abstract them to detect such loops. *)
  let abstract_targ tvar =
    let reason, _ = open_tvar tvar in
    let desc = desc_of_reason reason in
    if has_typeparam_prefix desc
    then Some (OpenT (reason_of_string desc, 0))
    else None
end

(* We maintain a stack of entries representing type applications processed
   during calls to flow, for the purpose of terminating unbounded expansion of
   type applications. Intuitively, we may have a potential infinite loop when
   processing a type application leads to another type application with the same
   root, but expanding type arguments. The entries in a stack contain
   approximate measurements that allow us to detect such expansion.

   An entry representing a type application with root C and type args T1,...,Tn
   is of the form (C, [A1,...,An]), where each Ai is a list of the roots of type
   applications nested in Ti. We consider a stack to indicate a potential
   infinite loop when the top of the stack is (C, [A1,...,An]) and there is
   another entry (C, [B1,...,Bn]) in the stack, such that each Bi is non-empty
   and is contained in Ai. *)

module TypeAppExpansion : sig
  type entry
  val push_unless_loop : Context.t -> (Type.t * Type.t list) -> bool
  val pop : unit -> unit
  val get : unit -> entry list
  val set : entry list -> unit
end = struct
  type entry = Type.t * TypeSet.t list
  let stack = ref ([]: entry list)

  (* visitor to collect roots of type applications nested in a type *)
  class roots_collector = object
    inherit [TypeSet.t] Type_visitor.t as super

    method! type_ cx acc t = match t with
    | TypeAppT (c, _) -> super#type_ cx (TypeSet.add c acc) t
    | OpenT _ -> (match ImplicitTypeArgument.abstract_targ t with
      | None -> acc
      | Some t -> TypeSet.add t acc
      )
    | _ -> super#type_ cx acc t
  end
  let collect_roots cx = (new roots_collector)#type_ cx TypeSet.empty

  (* Util to stringify a list, given a separator string and a function that maps
     elements of the list to strings. Should probably be moved somewhere else
     for general reuse. *)
  let string_of_list list sep f =
    list |> List.map f |> String.concat sep

  (* show entries in the stack *)
  let dump_stack () =
    string_of_list !stack "\n" (fun (c, tss) ->
      spf "%s<%s>" (desc_of_t c) (
        string_of_list tss "," (fun ts ->
          spf "[%s]" (string_of_list (TypeSet.elements ts) ";" desc_of_t)
        )))

  (* Detect whether pushing would cause a loop. Push only if no loop is
     detected, and return whether push happened. *)
  let push_unless_loop =
    let contains ts1 ts2 =
      not (TypeSet.is_empty ts1) && TypeSet.subset ts1 ts2
    in
    fun cx (c, ts) ->
      let tss = List.map (collect_roots cx) ts in
      let loop = !stack |> List.exists (fun (prev_c, prev_tss) ->
        c = prev_c && (List.for_all2 contains prev_tss tss)
      ) in
      if loop then false
      else (stack := (c, tss) :: !stack; true)

  let pop () = stack := List.tl !stack
  let get () = !stack
  let set _stack = stack := _stack
end

module Cache = struct
  (* Cache that remembers pairs of types that are passed to __flow__. *)
  module FlowConstraint = struct
    let cache = Hashtbl.create 0

    let mem (l,u) = match l,u with
      (* Don't cache constraints involving type variables, since the
         corresponding typing rules are already sufficiently robust. *)
      | OpenT _, _ | _, OpenT _ -> false
      | _ ->
        begin
          try
            Hashtbl.find cache (l,u);
            true
          with _ ->
            Hashtbl.add cache (l,u) ();
            false
        end
  end

  (* Cache that limits instantiation of polymorphic definitions. Intuitively,
     for each operation on a polymorphic definition, we remember the type
     arguments we use to specialize the type parameters. An operation is
     identified by its reason: we don't use the entire operation for caching
     since it may contain the very type variables we are trying to limit the
     creation of with the cache. In other words, the cache would be useless if
     we considered those type variables as part of the identity of the
     operation. *)
  module PolyInstantiation = struct
    let cache = Hashtbl.create 0

    let find cx (typeparam, reason_op) =
      try
        Hashtbl.find cache (typeparam.reason, reason_op)
      with _ ->
        let t = ImplicitTypeArgument.mk_targ cx (typeparam, reason_op) in
        Hashtbl.add cache (typeparam.reason, reason_op) t;
        t
  end

  let clear () =
    Hashtbl.clear FlowConstraint.cache;
    Hashtbl.clear PolyInstantiation.cache
end

(*********************************************************************)

(********************)
(* subtype relation *)
(********************)

(* Sometimes we expect types to be def types. For example, when we see a flow
   constraint from type l to type u, we expect l to be a def type. As another
   example, when we see a unification constraint between t1 and t2, we expect
   both t1 and t2 to be def types. *)
let expect_def t =
  if is_use t
  then assert_false (spf "Expected def type, but got: %s" (string_of_ctor t))

(* Recursion limiter. We proxy recursion depth with trace depth,
   which is either equal or pretty close.
   When check is called with a trace whose depth exceeds a constant
   limit, we throw a LimitExceeded exception.
 *)
module RecursionCheck : sig
  exception LimitExceeded of Trace.t
  val check: Trace.t -> unit

end = struct
  exception LimitExceeded of Trace.t
  let limit = 10000

  (* check trace depth as a proxy for recursion depth
     and throw when limit is exceeded *)
  let check trace =
    if Trace.trace_depth trace >= limit
    then raise (LimitExceeded trace)
end

(* Sometimes we don't expect to see type parameters, e.g. when they should have
   been substituted away. *)
let not_expect_bound t = match t with
  | BoundT _ -> assert_false (spf "Did not expect %s" (string_of_ctor t))
  | _ -> ()

(** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
let rec __flow cx (l, u) trace =
  begin match Context.verbose cx with
  | Some num_indent ->
    let indent = String.make ((Trace.trace_depth trace - 1) * num_indent) ' ' in
    let pid = Unix.getpid () in
    prerr_endlinef
      "\n%s[%d] %s (%s) ~>\n%s[%d] %s (%s)"
      indent pid
      (dump_reason (reason_of_t l)) (string_of_ctor l)
      indent pid
      (dump_reason (reason_of_t u)) (string_of_ctor u)
  | None -> ()
  end;

  if not (ground_subtype (l,u) || Cache.FlowConstraint.mem (l,u)) then (
    (* limit recursion depth *)
    RecursionCheck.check trace;

    (* Expect that l is a def type. On the other hand, u may be a use type or a
       def type: the latter typically when we have annotations. *)
    expect_def l;
    (* Type parameters should always be substituted out, and as such they should
       never appear "exposed" in flows. (They can still appear bound inside
       polymorphic definitions.) *)
    not_expect_bound l; not_expect_bound u;

    match (l,u) with

    (******************)
    (* process X ~> Y *)
    (******************)

    | (OpenT(reason1,tvar1),OpenT(reason2,tvar2)) ->
      let id1, constraints1 = find_constraints cx tvar1 in
      let id2, constraints2 = find_constraints cx tvar2 in

      (match constraints1, constraints2 with
      | Unresolved bounds1, Unresolved bounds2 ->
          if not_linked (id1, bounds1) (id2, bounds2) then (
            add_upper_edges cx trace (id1, bounds1) (id2, bounds2);
            add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
            flows_across cx trace bounds1.lower bounds2.upper;
          );

      | Unresolved bounds1, Resolved t2 ->
          edges_and_flows_to_t cx trace (id1, bounds1) t2

      | Resolved t1, Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t1, Resolved t2 ->
          rec_flow cx trace (t1, t2)
      );

    (******************)
    (* process Y ~> U *)
    (******************)

    | (OpenT(reason,tvar), t2) ->
      let id1, constraints1 = find_constraints cx tvar in
      (match constraints1 with
      | Unresolved bounds1 ->
          edges_and_flows_to_t cx trace (id1, bounds1) t2

      | Resolved t1 ->
          rec_flow cx trace (t1, t2)
      );

    (******************)
    (* process L ~> X *)
    (******************)

    | (t1, OpenT(reason,tvar)) ->
      let id2, constraints2 = find_constraints cx tvar in
      (match constraints2 with
      | Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t2 ->
          rec_flow cx trace (t1, t2)
      );

    (************)
    (* tainting *)
    (************)

    | (TaintT _, TaintT _) ->
      ()

    | (TaintT _, u) when taint_op u ->
      begin match result_of_taint_op u with
      | Some u -> rec_flow cx trace (l, u)
      | None -> ()
      end

    (*************************)
    (* repositioning, part 1 *)
    (*************************)

    (* Repositions a concrete upper bound using the reason stored in the lower
       bound. This can be used to store a reason as it flows through a tvar. *)
    | (ReposUpperT (reason, l), u) ->
      rec_flow cx trace (l, reposition cx reason u)

    (***************)
    (* annotations *)
    (***************)

    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)

    | _, AnnotT (sink_t, _) ->
      let reason = reason_of_t sink_t in
      rec_flow cx trace (ReposUpperT (reason, l), sink_t)

    (* The source component of an annotation flows out of the annotated
       site to downstream uses. *)

    | AnnotT (_, source_t), u ->
      let reason = reason_of_t source_t in
      rec_flow cx trace (reposition ~trace cx reason source_t, u)

    (****************************************************************)
    (* BecomeT unifies a tvar with an incoming concrete lower bound *)
    (****************************************************************)
    | _, BecomeT (reason, t) ->
      rec_unify cx trace (reposition ~trace cx reason l) t

    (************************************************)
    (* bound variables are equal only to themselves *)
    (************************************************)
    | (BoundT _, _) | (_, BoundT _) when l = u ->
      ()

    (***********************)
    (* guarded unification *)
    (***********************)

    (** Utility to unify a pair of types based on a trigger. Triggers are
        commonly type variables that are set up to record when certain
        operations have been processed: until then, they remain latent. For
        example, we can respond to events such as "a property is added," "a
        refinement succeeds," etc., by setting up unification constraints that
        are processed only when the corresponding triggers fire. *)

    | (_, UnifyT(t,t_other)) ->
      rec_unify cx trace t t_other

    (*********************************************************************)
    (* `import type` creates a properly-parameterized type alias for the *)
    (* remote type -- but only for particular, valid remote types.       *)
    (*********************************************************************)
    | (ClassT(inst), ImportTypeT(reason, t)) ->
      rec_flow cx trace (TypeT(reason, inst), t)

    | (PolyT(typeparams, ClassT(inst)), ImportTypeT(reason, t)) ->
      rec_flow cx trace (PolyT(typeparams, TypeT(reason, inst)), t)

    | (FunT(_, _, prototype, _), ImportTypeT(reason, t)) ->
      rec_flow cx trace (TypeT(reason, prototype), t)

    | (PolyT(typeparams, FunT(_, _, prototype, _)), ImportTypeT(reason, t)) ->
      rec_flow cx trace (PolyT(typeparams, TypeT(reason, prototype)), t)

    | (TypeT _, ImportTypeT(reason, t))
    | (PolyT(_, TypeT _), ImportTypeT(reason, t))
      -> rec_flow cx trace (l, t)

    (**
     * TODO: Delete this once the legacy export-type hacks have been eliminated
     *       in favor of the newer, first class export-type feature.
     *
     *       TODO(jeffmo) Task(6860853)
     *)
    | (ObjT _, ImportTypeT(reason, t)) ->
      rec_flow cx trace (l, t)

    | (_, ImportTypeT(reason, t)) ->
      add_error cx [
        reason,
        "`import type` only works on exported classes, functions, and type aliases!"
      ]

    (************************************************************************)
    (* `import typeof` creates a properly-parameterized type alias for the  *)
    (* "typeof" the remote export.                                          *)
    (************************************************************************)
    | (PolyT(typeparams, ((ClassT _ | FunT _) as lower_t)), ImportTypeofT(reason, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace lower_t in
      rec_flow cx trace (PolyT(typeparams, TypeT(reason, typeof_t)), t)

    | ((TypeT _ | PolyT(_, TypeT _)), ImportTypeofT(reason, t)) ->
      add_error cx [
        reason,
        "`import typeof` can not be used on type-only exports! If you " ^
        "intended to import a type alias, please use `import type` instead."
      ]

    | (_, ImportTypeofT(reason, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace l in
      rec_flow cx trace (TypeT(reason, typeof_t), t)

    (**************************************************************************)
    (* Module exports                                                         *)
    (*                                                                        *)
    (* Flow supports both CommonJS and standard ES modules as well as some    *)
    (* interoperability semantics for communicating between the two module    *)
    (* systems in both directions.                                            *)
    (*                                                                        *)
    (* In order to support both systems at once, Flow abstracts the notion of *)
    (* module exports by storing a type map for each of the exports of a      *)
    (* given module, and for each module there is a ModuleT that maintains    *)
    (* this type map. The exported types are then considered immutable once   *)
    (* the module has finished inference.                                     *)
    (*                                                                        *)
    (* When a type is set for the CommonJS exports value, we store it         *)
    (* separately from the normal named exports tmap that ES exports are      *)
    (* stored within. This allows us to distinguish CommonJS modules from ES  *)
    (* modules when interpreting an ES import statement -- which is important *)
    (* because ES ModuleNamespace objects built from CommonJS exports are a   *)
    (* little bit magic.                                                      *)
    (*                                                                        *)
    (* For example: If a CommonJS module exports an object, we will extract   *)
    (* each of the properties of that object and consider them as "named"     *)
    (* exports for the purposes of an import statement elsewhere:             *)
    (*                                                                        *)
    (*   // CJSModule.js                                                      *)
    (*   module.exports = {                                                   *)
    (*     someNumber: 42                                                     *)
    (*   };                                                                   *)
    (*                                                                        *)
    (*   // ESModule.js                                                       *)
    (*   import {someNumber} from "CJSModule";                                *)
    (*   var a: number = someNumber;                                          *)
    (*                                                                        *)
    (* We also map CommonJS export values to the "default" export for         *)
    (* purposes of import statements in other modules:                        *)
    (*                                                                        *)
    (*   // CJSModule.js                                                      *)
    (*   module.exports = {                                                   *)
    (*     someNumber: 42                                                     *)
    (*   };                                                                   *)
    (*                                                                        *)
    (*   // ESModule.js                                                       *)
    (*   import CJSDefaultExport from "CJSModule";                            *)
    (*   var a: number = CJSDefaultExport.someNumber;                         *)
    (*                                                                        *)
    (* Note that the ModuleT type is not intended to be surfaced to any       *)
    (* userland-visible constructs. Instead it's meant as an internal         *)
    (* construct that is only *mapped* to/from userland constructs (such as a *)
    (* CommonJS exports object or an ES ModuleNamespace object).              *)
    (**************************************************************************)

    (* ES exports *)
    | (ModuleT(_, exports), SetNamedExportsT(reason, tmap, t_out)) ->
      SMap.iter (write_prop cx exports.exports_tmap) tmap;
      rec_flow cx trace (l, t_out)

    (**
     * ObjT CommonJS export values have their properties turned into named
     * exports
     *)
    | (ObjT(_, {props_tmap; proto_t; _;}),
       CJSExtractNamedExportsT(reason, module_t, t_out)) ->

      (* Copy props from the prototype *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (proto_t, CJSExtractNamedExportsT(reason, module_t, t))
      ) in

      (* Copy own props *)
      rec_flow cx trace (module_t, SetNamedExportsT(
        reason,
        find_props cx props_tmap,
        t_out
      ))

    (**
     * InstanceT CommonJS export values have their properties turned into named
     * exports
     *)
    | (InstanceT(_, _, super, {fields_tmap; methods_tmap; _;}),
       CJSExtractNamedExportsT(reason, module_t, t_out)) ->

      (* Copy fields *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (module_t, SetNamedExportsT(
          reason,
          find_props cx fields_tmap,
          t
        ))
      ) in

      (* Copy methods *)
      rec_flow cx trace (module_t, SetNamedExportsT(
        reason,
        find_props cx methods_tmap,
        t_out
      ))

    (**
     * All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way.
     *)
    | (_, CJSExtractNamedExportsT(_, module_t, t_out)) ->
      rec_flow cx trace (module_t, t_out)

    (**************************************************************************)
    (* Module imports                                                         *)
    (*                                                                        *)
    (* The process of importing from a module consists of reading from the    *)
    (* foreign ModuleT type and generating a user-visible construct from it.  *)
    (*                                                                        *)
    (* For CommonJS imports (AKA 'require()'), if the foreign module is an ES *)
    (* module we generate an object whose properties correspond to each of    *)
    (* the named exports of the foreign module. If the foreign module is also *)
    (* a CommonJS module, use the type of the foreign CommonJS exports value  *)
    (* directly.                                                              *)
    (*                                                                        *)
    (* For ES imports (AKA `import` statements), simply generate a model of   *)
    (* an ES ModuleNamespace object from the individual named exports of the  *)
    (* foreign module. This object can then be passed up to "userland"        *)
    (* directly (via `import * as`) or it can be used to extract individual   *)
    (* exports from the foreign module (via `import {}` and `import X from`). *)
    (**************************************************************************)

    (* require('SomeModule') *)
    | (ModuleT(_, exports), CJSRequireT(reason, t)) ->
      let cjs_exports = (
        match exports.cjs_export with
        | Some t ->
          (* reposition the export to point at the require(), like the object
             we create below for non-CommonJS exports *)
          reposition ~trace cx reason t
        | None ->
          let proto = MixedT(reason) in
          let props_smap = find_props cx exports.exports_tmap in
          mk_object_with_map_proto cx reason
            ~sealed:true ~frozen:true props_smap proto
      ) in
      rec_flow cx trace (cjs_exports, t)

    (* import [...] from 'SomeModule'; *)
    | (ModuleT(_, exports), ImportModuleNsT(reason, t)) ->
      let exports_tmap = find_props cx exports.exports_tmap in
      let ns_obj_tmap = (
        match exports.cjs_export with
        | Some(t) -> SMap.add "default" t exports_tmap
        | None -> exports_tmap
      ) in
      let proto = MixedT(reason) in
      let ns_obj = mk_object_with_map_proto cx reason
        ~sealed:true ~frozen:true ns_obj_tmap proto
      in
      rec_flow cx trace (ns_obj, t)

    (********************************************)
    (* summary types forget literal information *)
    (********************************************)

    | (StrT (_, (Literal _ | Truthy | Falsy)), SummarizeT (reason, t)) ->
      rec_unify cx trace (StrT.why reason) t

    | (NumT (_, (Literal _ | Truthy | Falsy)), SummarizeT (reason, t)) ->
      rec_unify cx trace (NumT.why reason) t

    | (_, SummarizeT (reason, t)) ->
      rec_unify cx trace l t

    (*******************************)
    (* common implicit convertions *)
    (*******************************)

    | (_, NumT _) when numeric l -> ()

    | (_, AnyObjT _) when object_like l -> ()
    | (AnyObjT _, _) when object_like u -> ()
    | (AnyObjT _, _) when object_like_op u ->
      (* AnyObjT flows into any object operation, creating a potential
       * source of false negatives for TaintT, since the arguments are not
       * checked. *)
      parameters_of_taint_op u
      |> List.iter (fun t -> rec_flow cx trace (t, AnyT.t))

    | (_, AnyFunT _) when function_like l -> ()
    | (AnyFunT _, _) when function_like u || function_like_op u -> ()

    (***************)
    (* maybe types *)
    (***************)

    (** The type maybe(T) is the same as null | undefined | T *)

    | ((NullT _ | VoidT _), MaybeT _) -> ()

    | (MaybeT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (NullT.why reason, u);
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is the same as undefined | T *)

    | (VoidT _, OptionalT _) -> ()

    | (OptionalT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (*****************)
    (* logical types *)
    (*****************)

    | (BoolT (r, Some x), NotT(reason, tout))
    | (SingletonBoolT (r, x), NotT(reason, tout)) ->
      let reason = replace_reason (spf "boolean value `%b`" (not x)) reason in
      let t = BoolT (reason, Some (not x)) in
      rec_flow cx trace (t, tout)

    | (_, NotT(reason, tout)) ->
      rec_flow cx trace (BoolT.at (loc_of_reason reason), tout)

    | (left, AndT(reason, right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let truthy_left = filter_exists left in
      (match truthy_left with
      | EmptyT _ ->
        (* falsy *)
        rec_flow cx trace (left, PredicateT (NotP ExistsP, u))
      | _ ->
        (match filter_not_exists left with
        | EmptyT _ -> (* truthy *) rec_flow cx trace (right, u)
        | _ ->
          rec_flow cx trace (left, PredicateT (NotP ExistsP, u));
          (match truthy_left with
          | EmptyT _ -> ()
          | _ -> rec_flow cx trace (right, u))
        )
      )

    | (left, OrT(reason, right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let falsy_left = filter_not_exists left in
      (match falsy_left with
      | EmptyT _ ->
        (* truthy *)
        rec_flow cx trace (left, PredicateT (ExistsP, u))
      | _ ->
        (match filter_exists left with
        | EmptyT _ -> (* falsy *) rec_flow cx trace (right, u)
        | _ ->
          rec_flow cx trace (left, PredicateT (ExistsP, u));
          (match falsy_left with
          | EmptyT _ -> ()
          | _ -> rec_flow cx trace (right, u))
        )
      )

    | (_, ObjTestT(reason, default, u)) ->
      if object_like l
      then rec_flow cx trace (l, u)
      else rec_flow cx trace (default, u)

    (*****************************)
    (* upper and lower any types *)
    (*****************************)

    (** UpperBoundT and LowerBoundT are very useful types that concisely model
        subtyping constraints without introducing unwanted effects: they can
        appear on both sides of a type, but only have effects in one of those
        sides. In some sense, they are liked bounded AnyT: indeed, AnyT has the
        same behavior as UpperBoundT(EmptyT) and LowerBoundT(MixedT). Thus,
        these types can be used instead of AnyT when some precise typechecking
        is required without overconstraining the system. A completely static
        alternative would be achieved with bounded type variables, which Flow
        does not support yet. **)

    | (UpperBoundT t, _) ->
      rec_flow cx trace (t,u)

    | (_, UpperBoundT t) ->
      ()

    | (LowerBoundT t, _) ->
      ()

    | (_, LowerBoundT t) ->
      rec_flow cx trace (l,t)

    (*********************)
    (* type applications *)
    (*********************)

    (* Sometimes a polymorphic class may have a polymorphic method whose return
       type is a type application on the same polymorphic class, possibly
       expanded. See Array#map or Array#concat, e.g. It is not unusual for
       programmers to reuse variables, assigning the result of a method call on
       a variable to itself, in which case we could get into cycles of unbounded
       instantiation. We use caching to cut these cycles. Caching relies on
       reasons (see module Cache.I). This is OK since intuitively, there should
       be a unique instantiation of a polymorphic definition for any given use
       of it in the source code.

       In principle we could use caching more liberally, but we don't because
       not all use types arise from source code, and because reasons are not
       perfect. Indeed, if we tried caching for all use types, we'd lose
       precision and report spurious errors.

       Also worth noting is that we can never safely cache def types. This is
       because substitution of type parameters in def types does not affect
       their reasons, so we'd trivially lose precision. *)

    | (TypeAppT(c,ts), MethodT _) ->
        let reason_op = reason_of_t u in
        let t = mk_typeapp_instance cx reason_op ~cache:true c ts in
        rec_flow cx trace (t, u)

    | (TypeAppT(c,ts), _) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason = reason_of_t u in
          let t = mk_typeapp_instance cx reason c ts in
          rec_flow cx trace (t, u);
          TypeAppExpansion.pop ()
        )

    | (_, TypeAppT(c,ts)) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason = reason_of_t l in
          let t = mk_typeapp_instance cx reason c ts in
          rec_flow cx trace (l, t);
          TypeAppExpansion.pop ()
        )

    (***********************)
    (* Singletons and keys *)
    (***********************)

    (** Finite keysets over arbitrary objects can be represented by KeysT. While
        it is possible to also represent singleton string types using KeysT (by
        taking the keyset of an object with a single property whose key is that
        string and whose value is ignored), we can model them more directly
        using SingletonStrT. Specifically, SingletonStrT models a type
        annotation that looks like a string literal, which describes a singleton
        set containing that string literal. Going further, other uses of KeysT
        where the underlying object is created solely for the purpose of
        describing a keyset can be modeled using unions of singleton strings.

        One may also legitimately wonder why SingletonStrT(_, key) cannot be
        always replaced by StrT(_, Some key). The reason is that types of the
        latter form (string literal types) are inferred to be the type of string
        literals appearing as values, and we don't want to prematurely narrow
        down the type of the location where such values may appear, since that
        would preclude other strings to be stored in that location. Thus, by
        necessity we allow all string types to flow to StrT (whereas only
        exactly matching string literal types may flow to SingletonStrT).  **)

    (**********************)
    (* string singletons *)
    (**********************)

    | (StrT (_, Literal x), SingletonStrT (_, key)) ->
        if x <> key then
          let msg = spf "Expected string literal `%s`, got `%s` instead" key x in
          prerr_flow cx trace msg l u

    | (StrT (_, (Truthy | Falsy | AnyLiteral)), SingletonStrT (_, key)) ->
      prerr_flow cx trace (spf "Expected string literal `%s`" key) l u

    | (SingletonStrT (reason, key), _) ->
      rec_flow cx trace (StrT(reason, Literal key), u)

    (*********************)
    (* number singletons *)
    (*********************)

    (** Similar to SingletonStrT, SingletonNumT models a type annotation that
        looks like a single number literal. This contrasts with
        `NumT(_, Some num)`, which starts out representing `num` but allows any
        number, whereas `SingletonNumT` accepts only exactly that value. **)
    | (NumT (_, Literal (x, _)), SingletonNumT (_, (y, _))) ->
        (* this equality check is ok for now because we don't do arithmetic *)
        if x <> y then
          let msg = spf "Expected number literal `%.16g`, got `%.16g` instead" y x in
          prerr_flow cx trace msg l u

    | (NumT (_, (Truthy | Falsy | AnyLiteral)), SingletonNumT (_, (y, _))) ->
      prerr_flow cx trace (spf "Expected number literal `%.16g`" y) l u

    | (SingletonNumT (reason, lit), _) ->
      rec_flow cx trace (NumT(reason, Literal lit), u)

    (**********************)
    (* boolean singletons *)
    (**********************)

    (** Similar to SingletonStrT, SingletonBoolT models a type annotation that
        looks like a specific boolean literal (either true or false, but not
        both). This contrasts with `BoolT(_, Some b)`, which starts out
        representing `b` but allows any boolean, whereas `SingletonBoolT`
        accepts only exactly that value. **)
    | (BoolT (_, Some x), SingletonBoolT (_, y)) ->
        if x <> y then
          let msg = spf "Expected boolean literal `%b`, got `%b` instead" y x in
          prerr_flow cx trace msg l u

    | (BoolT (_, None), SingletonBoolT (_, y)) ->
      prerr_flow cx trace (spf "Expected boolean literal `%b`" y) l u

    | (SingletonBoolT (reason, b), _) ->
      rec_flow cx trace (BoolT(reason, Some b), u)

    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)

    | (StrT (reason_s, Literal x), KeysT (reason_op, o)) ->
      let reason_op = replace_reason (spf "string literal `%s`" x) reason_s in
      (* check that o has key x *)
      rec_flow cx trace (o, HasOwnPropT(reason_op,x))

    | (StrT (_, (Truthy | Falsy | AnyLiteral)), KeysT _) ->
      prerr_flow cx trace "Expected string literal" l u

    | (KeysT (reason1, o1), _) ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1, u))

    (* helpers *)

    | (ObjT (reason_o, { props_tmap = mapr; _ }), HasOwnPropT (reason_op, x)) ->
      if has_prop cx mapr x then ()
      else prmsg_flow_prop_not_found cx trace (reason_op, reason_o)

    | (InstanceT (reason_o, _, _, instance), HasOwnPropT(reason_op, x)) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some tx -> ()
      | None -> prmsg_flow_prop_not_found cx trace (reason_op, reason_o)
      )

    | (ObjT (reason, { flags; props_tmap = mapr; _ }), GetKeysT(_,key)) ->
      (match flags.sealed with
      | Sealed ->
        (* flow each key of l to key *)
        iter_props cx mapr (fun x tv ->
          let t = StrT (reason, Literal x) in
          rec_flow cx trace (t, key)
        );
      | _ ->
        let t = StrT (prefix_reason "key of " reason, AnyLiteral) in
        rec_flow cx trace (t, key))

    | (InstanceT (reason, _, _, instance), GetKeysT(_,key)) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      fields |> SMap.iter (fun x tv ->
        let t = StrT (reason, Literal x) in
        rec_flow cx trace (t, key)
      )

    (** In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

    (** NOTE: It is important that any def type that simplifies to a union or
        intersection of other def types be processed before we process unions
        and intersections: otherwise we may get spurious errors. **)

    (***************)
    (* union types *)
    (***************)

    | (UnionT(_,ts), _) ->
      ts |> List.iter (fun t -> rec_flow cx trace (t,u))

    (** To check that the concrete type l may flow to UnionT(_, ts), we try each
        type in ts in turn. The types in ts may be type variables, but by
        routing them through SpeculativeMatchFailureT, we ensure that they are
        tried only when their concrete uses are available. The different
        branches of unions are assumed to be disjoint at the top level, so that
        it is always sound to pick the first type in ts that matches the type
        l. *)

    | (SpeculativeMatchFailureT(r,t1,t), t2) ->
      (* Reached when trying to match t1 with t2|t. We speculatively match t1
         with t2, and on failure, (recursively) try to match t1 with t. *)
      if speculative_flow_error cx trace t1 t2
      then rec_flow cx trace (t1, t)

    | (_, UnionT(r,ts)) ->
      try_union cx trace l r ts

    | (t1, MaybeT(t2)) ->
      rec_flow cx trace (t1,t2)

    | (t1, OptionalT(t2)) ->
      rec_flow cx trace (t1, t2)

    (**********************)
    (* intersection types *)
    (**********************)

    | (_, IntersectionT(_,ts)) ->
      ts |> List.iter (fun t -> rec_flow cx trace (l,t))

    (** To check that IntersectionT(_, ts) may flow to the concrete type u, we
        try each type in ts in turn. The types in ts may be type variables, but
        by routing them through SpeculativeMatchFailureT, we ensure that they
        are tried only when their concrete definitionss are available. Note that
        unlike unions, the different branches of intersections are usually not
        distinct at the top level (e.g., they may all be function types, or
        object types): instead, they are assumed to be disjoint in their parts,
        so we must suffiently concretize parts of u to make the disjointness
        evident when the different branches are tried; see below. *)

    | (t1, SpeculativeMatchFailureT(r,t,t2)) ->
      (* Reached when trying to match t1&t with t2. We speculatively match t1
         with t2, and on failure, (recursively) try to match t with t2. *)
      if speculative_flow_error cx trace t1 t2
      then rec_flow cx trace (t, t2)

    | (IntersectionT (r,ts), ConcreteT u) ->
      try_intersection cx trace u r ts

    | (t, ConcretizeT (l, ts1, ts2, u)) ->
      concretize_parts cx trace l u (t::ts2) ts1

    (* special treatment for some operations on intersections *)

    (** lookup of properties **)

    | (IntersectionT (_,ts),
       LookupT (reason, strict, try_ts_on_failure, s, t)) ->
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace
        (List.hd ts,
         LookupT (reason, strict, (List.tl ts) @ try_ts_on_failure, s, t))

    (** extends **)
    | (IntersectionT (_,ts),
       ExtendsT (try_ts_on_failure, l, u)) ->
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace
        (List.hd ts,
         ExtendsT ((List.tl ts) @ try_ts_on_failure, l, u))

    (** consistent override of properties **)

    | (IntersectionT (_,ts), SuperT _) ->
      List.iter (fun t -> rec_flow cx trace (t, u)) ts

    (* all other constraints on intersections *)

    (* This duplicates the (_, ReposLowerT u) near the end of this pattern match
       but has to appear here to preempt the (IntersectionT, _) in between so
       that we reposition the entire intersection. *)
    | (IntersectionT _, ReposLowerT (reason_op, u)) ->
      rec_flow cx trace (reposition cx reason_op l, u)

    | (IntersectionT (r,ts), _) ->
      concretize_parts cx trace l u [] (parts_to_concretize cx u)

    (***************************************)
    (* generic function may be specialized *)
    (***************************************)

    (* Instantiate a polymorphic definition using the supplied type
       arguments. Use the instantiation cache if directed to do so by the
       operation. (SpecializeT operations are created when processing TypeAppT
       types, so the decision to cache or not originates there.) *)
    | (PolyT (ids,t), SpecializeT(reason,cache,ts,tvar)) ->
      let t_ = instantiate_poly_with_targs cx trace reason ~cache (ids,t) ts in
      rec_flow cx trace (t_, tvar)

    (* PolyT doesn't have a position since it takes on the position of the upper
       bound, so it doesn't need to be repositioned. This case is necessary to
       prevent the wildcard (PolyT, _) below from firing too early. *)
    | (PolyT _, ReposLowerT (_, u)) ->
      rec_flow cx trace (l, u)

    (* When do we consider a polymorphic type <X:U> T to be a subtype of another
       polymorphic type <X:U'> T'? This is the subject of a long line of
       research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
       that T is a subtype of T' for any X:U'. A more general rule that proves
       that U' is a subtype of U instead of forcing U = U' is known to cause
       undecidable subtyping (Pierce): the counterexamples are fairly
       pathological, but can be reliably constructed by exploiting the "switch"
       of bounds from U' to U (and back, with sufficient trickery), in ways that
       are difficult to detect statically.

       However, these results are somewhat tricky to interpret in Flow, since we
       are not proving stuff inductively: instead we are co-inductively assuming
       what we want to prove, and checking consistency.

       Separately, none of these rules capture the logical interpretation of the
       original subtyping question (interpreting subtyping as implication, and
       polymorphism as universal quantification). What we really want to show is
       that, for all X:U', there is some X:U such that T is a subtype of T'. But
       we already deal with statements of this form when checking polymorphic
       definitions! In particular, statements such as "there is some X:U...")
       correspond to "create a type variable with that constraint and ...", and
       statements such as "show that for all X:U" correspond to "show that for
       both X = bottom and X = U, ...".

       Thus, all we need to do when checking that any type flows to a
       polymorphic type is to follow the same principles used when checking that
       a polymorphic definition has a polymorphic type. This has the pleasant
       side effect that the type we're checking does not itself need to be a
       polymorphic type at all! For example, we can let a non-generic method be
       overridden with a generic method, as long as the non-generic signature
       can be derived as a specialization of the generic signature. *)
    | (_, PolyT (ids, t)) ->
        generate_tests cx (reason_of_t l) ids (fun map_ ->
          rec_flow cx trace (l, subst cx map_ t)
        )

    | (PolyT (ids,t), _) ->
      (* Some observed cases of u are listed below. These look legit, as common
         uses of polymorphic classes and functions. However, they may not cover
         all legit uses.

      (* class-like *)
      | TypeT _
      | ConstructorT _
      | GetPropT _
      | SetPropT _
      | MethodT _

      (* function-like *)
      | CallT _
      | FunT _

      (* general uses *)
      | PredicateT _

      *)

      let weak = match u with
        (* Implicitly instantiating polymorphic types in annotations leads to
           confusing errors. In particular, when multiple instantiations of a
           polymorphic type flow to such an annotation, the instantiations are
           unified via the implicit instantiation at the annotation. This
           situation is made worse by the fact that the annotation site is often
           not included as part of the ensuing errors due to such unification.

           Instead, we fall back to using `any` as type arguments when they are
           missing. This conforms to the general intuition that all
           instantiations of a polymorphic type Foo are subtypes of Foo. When
           falling back to `any` is inadequate, we force the programmer to be
           explicit about what the type arguments of a polymorphic type are in
           an annotation. Overall, this scheme reduces false positives as well
           as improves blaming when errors do arise.

           It is always possible to recover implicit instantiation by explicitly
           using `*`. *)
        | TypeT _ -> true
        | _ -> false in
      let reason = reason_of_t u in
      let t_ = instantiate_poly ~weak cx trace reason (ids,t) in
      rec_flow cx trace (t_, u)

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    | FunT (reason1, _, _,
        { this_t = o1; params_tlist = tins1; return_t = t1;
          closure_t = func_scope_id; _ }),
      FunT (reason2, _, _,
        { this_t = o2; params_tlist = tins2; return_t = t2;
          closure_t = call_scope_id; _ })
      ->
      rec_flow cx trace (o2, o1);
      multiflow cx trace reason2 (tins2, tins1);
      rec_flow cx trace (t1, t2);

    | FunT (reason_fundef, _, _,
        { this_t = o1; params_tlist = tins1; return_t = t1;
          closure_t = func_scope_id; changeset; _ }),
      CallT (reason_callsite,
        { this_t = o2; params_tlist = tins2; return_t = t2;
          closure_t = call_scope_id; _})
      ->
      Ops.push reason_callsite;
      rec_flow cx trace (o2, o1);
      multiflow cx trace reason_callsite (tins2, tins1);
      (* relocate the function's return type at the call site TODO remove? *)
      let t1 = reposition ~trace cx reason_callsite t1 in
      rec_flow cx trace (t1, t2);

      (if Context.is_verbose cx then
        prerr_endlinef "%d havoc_call_env fundef %s callsite %s"
          (Unix.getpid ())
          (string_of_reason reason_fundef)
          (string_of_reason reason_callsite));
      havoc_call_env cx func_scope_id call_scope_id changeset;

      Ops.pop ()

    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    | (ObjT (reason1, {props_tmap = flds1; proto_t; flags; dict_t = dict1; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; dict_t = dict2; _ }))
      ->
      (* if inflowing type is literal (thus guaranteed to be
         unaliased), propertywise subtyping is sound *)
      let desc1 = (desc_of_reason reason1) in
      let lit =
        (desc1 = "object literal")
        || (desc1 = "function")
        || (desc1 = "arrow function")
        || (desc1 = "frozen object literal")
        || (Str.string_match (Str.regexp ".*React") desc1 0)
      in

      (* If both are dictionaries, ensure the keys and values are compatible
         with each other. *)
      (match dict1, dict2 with
        | Some {key = k1; value = v1; _}, Some {key = k2; value = v2; _} ->
            dictionary cx trace k1 v1 dict2;
            dictionary cx trace k2 v2 dict1
        | _ -> ());

      (* Properties in u must either exist in l, or match l's indexer. *)
      iter_props_ cx flds2
        (fun s t2 ->
          if not (has_prop cx flds1 s)
          then
            (* property doesn't exist in inflowing type *)
            let reason2 = replace_reason (spf "property `%s`" s) reason2 in
            match t2 with
            | OptionalT t1 when flags.exact ->
              (* if property is marked optional or otherwise has a maybe type,
                 and if inflowing type is exact (i.e., it is not an
                 annotation), then we add it to the inflowing type as
                 an optional property, as well as ensuring compatibility
                 with dictionary constraints if present *)
              dictionary cx trace (string_key s reason2) t1 dict1;
              write_prop cx flds1 s t2;
            | _ ->
              (* otherwise, we ensure compatibility with dictionary constraints
                 if present, and look up the property in the prototype *)
              dictionary cx trace (string_key s reason2) t2 dict1;
              (* if l is NOT a dictionary, then do a strict lookup *)
              let strict = if dict1 = None then Some reason1 else None in
              rec_flow cx trace (proto_t, LookupT (reason2, strict, [], s, t2))
          (* TODO: instead, consider extending inflowing type with s:t2 when it
             is not sealed *)
          else
            let t1 = read_prop cx flds1 s in
            flow_to_mutable_child cx trace lit t1 t2);
      (* Any properties in l but not u must match indexer *)
      iter_props_ cx flds1
        (fun s t1 ->
          if (not(has_prop cx flds2 s))
          then dictionary cx trace (string_key s reason1) t1 dict2
        );
      rec_flow cx trace (l, u_)

    | (InstanceT (reason1, _, super, { fields_tmap; methods_tmap; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; _ }))
      ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let flds1 = SMap.union fields_tmap methods_tmap in
      iter_props_ cx flds2
        (fun s -> fun t2 ->
          if (not(SMap.mem s flds1))
          then
            let reason2 = replace_reason (spf "property `%s`" s) reason2 in
            match t2 with
            | OptionalT t1 ->
                rec_flow cx trace
                  (l, LookupT (reason2, None, [], s, t1))
            | _ ->
                rec_flow cx trace
                  (super, LookupT (reason2, Some reason1, [], s, t2))
          else
            let t1 = SMap.find_unsafe s flds1 in
            rec_unify cx trace t1 t2
        );
      rec_flow cx trace (l, u_)

    (* TODO Try and remove this. We're not sure why this case should exist but
     * it does seem to be triggered in www. *)
    | (ObjT _,
       InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = false;
         _;
       }))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | ((ObjT (reason, _) | InstanceT (reason, _, _, _)),
       (FunT (reason_op, _, _, _) | CallT (reason_op, _))) ->
      let tvar = mk_tvar cx (suffix_reason " used as a function" reason) in
      lookup_prop cx trace l reason_op (Some reason) "$call" tvar;
      rec_flow cx trace (tvar, u)

    (******************************)
    (* matching shapes of objects *)
    (******************************)

    (** When something of type ShapeT(o) is used, it behaves like it had type o.

        On the other hand, things that can be passed to something of type
        ShapeT(o) must be "subobjects" of o: they may have fewer properties, but
        those properties should be transferable to o.

        Because a property x with a type OptionalT(t) could be considered
        missing or having type t, we consider such a property to be transferable
        if t is a subtype of x's type in o. Otherwise, the property should be
        assignable to o.

        TODO: The type constructors ShapeT, DiffT, ObjAssignT, ObjRestT express
        related meta-operations on objects. Consolidate these meta-operations
        and ensure consistency of their semantics. **)

    | (ShapeT (o), _) ->
        rec_flow cx trace (o, u)

    | (ObjT (reason, { props_tmap = mapr; _ }), ShapeT (proto)) ->
        iter_props cx mapr (fun x t ->
          let reason = prefix_reason (spf "property `%s` of " x) reason in
          let t = filter_optional cx ~trace reason t in
          rec_flow cx trace (proto, SetPropT (reason, (reason, x), t));
        )

    | (_, ShapeT (o)) ->
        let reason = reason_of_t o in
        rec_flow cx trace (l, ObjAssignT(reason, o, AnyT.t, [], false))

    | (_, DiffT (o1, o2)) ->
        let reason = reason_of_t u in
        let t2 = mk_tvar cx reason in
        rec_flow cx trace (o2, ObjRestT (reason, [], t2));
        rec_flow cx trace (l, ObjAssignT(reason, t2, o1, [], false))

    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    | ArrT (r1, t1, ts1), ArrT (r2, t2, ts2) ->
      let lit = (desc_of_reason r1) = "array literal" in
      array_flow cx trace lit (ts1, t1, ts2, t2)

    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)

    | (InstanceT (_, _, _, instance),
       InstanceT (_, _, _, instance_super)) ->
      rec_flow cx trace (l, ExtendsT([],l,u))

    | (InstanceT (_,_,super,instance),
       ExtendsT(_, _, InstanceT (_,_,_,instance_super))) ->

      if instance.class_id = instance_super.class_id
      then
        flow_type_args cx trace instance instance_super
      else
        rec_flow cx trace (super, u)

    (***************************************************************)
    (* Enable structural subtyping for upperbounds like interfaces *)
    (***************************************************************)

    | (_,
       InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       }))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | (ClassT(it), TypeT(r,t)) ->
      (* a class value annotation becomes the instance type *)
      rec_flow cx trace (it, BecomeT (r, t))

    | (FunT(reason,_,prototype,_), TypeT(r,t)) ->
      (* a function value annotation becomes the prototype type *)
      rec_flow cx trace (prototype, BecomeT (r, t))

    | (TypeT(_,l), TypeT(_,u)) ->
      rec_unify cx trace l u

    (* non-class/function values used in annotations are errors *)
    | _, TypeT _ ->
      prerr_flow cx trace
        "Ineligible value used in/as type annotation (did you forget 'typeof'?)"
        l u

    | (ClassT(l), ClassT(u)) ->
      rec_unify cx trace l u

    | (FunT(_,static1,prototype,_), ClassT(InstanceT(_,static2,_, _) as u_)) ->
      rec_unify cx trace static1 static2;
      rec_unify cx trace prototype u_

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | ClassT (this),
      ConstructorT (reason_op, args, t) ->
      let reason_o = replace_reason "constructor return" (reason_of_t this) in
      Ops.push reason_op;
      (* call this.constructor(args) *)
      let ret = mk_tvar_where cx reason_o (fun t ->
        let funtype = mk_methodtype this args t in
        rec_flow cx trace (
          this,
          MethodT (reason_op, (reason_o, "constructor"), funtype)
        );
      ) in
      (* return this *)
      rec_flow cx trace (ret, ObjTestT(reason_o, this, t));
      Ops.pop ();

    (****************************************************************)
    (* function types derive objects through explicit instantiation *)
    (****************************************************************)

    | FunT (reason, _, proto, {
        this_t = this;
        params_tlist = params;
        return_t = ret;
        _ }),
      ConstructorT (reason_op, args, t) ->
      (* TODO: closure *)
      (** create new object **)
      let reason_c = replace_reason "new object" reason in
      let proto_reason = reason_of_t proto in
      let sealed = UnsealedInFile (Loc.source (loc_of_reason proto_reason)) in
      let flags = { default_flags with sealed } in
      let dict = None in
      let pmap = mk_propmap cx SMap.empty in
      let new_obj = ObjT (reason_c, mk_objecttype ~flags dict pmap proto) in
      (** call function with this = new_obj, params = args **)
      rec_flow cx trace (new_obj, this);
      multiflow cx trace reason_op (args, params);
      (** if ret is object-like, return ret; otherwise return new_obj **)
      let reason_o = replace_reason "constructor return" reason in
      rec_flow cx trace (ret, ObjTestT(reason_o, new_obj, t))

    (*************************)
    (* "statics" can be read *)
    (*************************)

    | InstanceT (_, static, _, _), GetPropT (_, (_, "statics"), t) ->
      rec_flow cx trace (static, t)

    | MixedT reason, GetPropT(_, (_, "statics"), u) ->
      (* MixedT not only serves as the instance type of the root class, but also
         as the statics of the root class. *)
      rec_flow cx trace (MixedT (prefix_reason "statics of " reason), u)

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | InstanceT(reason, _, super, instance),
      LookupT (reason_op, strict, _, x, t)
      ->
      let strict = if instance.mixins then None else strict in
      let pmap = match strict, t with
        (* t = UpperBoundT _ means that the lookup is trying to write t, rather
           than read t. Existing places that play a role here are set_prop and
           get_prop, which use UpperBoundT and LowerBoundT, respectively. The
           general pattern has been used previously, e.g. to distinguish element
           writes from reads.

           strict = Some _ means that we want to throw errors when x is not
           found. Some lookups are non-strict (e.g. when we want to enforce
           consistency between properties if they exist higher up in the
           inheritance chain), and for methods, we want the consistency to be
           one-way, so we use UpperBoundT _, and we don't want methods to be
           excluded from the lookup in that case, obviously. *)
        | Some _, UpperBoundT _ ->
          let fields_tmap = find_props cx instance.fields_tmap in
          fields_tmap
        | _ ->
          let fields_tmap = find_props cx instance.fields_tmap in
          let methods_tmap = find_props cx instance.methods_tmap in
          SMap.union fields_tmap methods_tmap
      in
      (match SMap.get x pmap with
      | None ->
        rec_flow cx trace (super, u)
      | Some (AbstractT tx) | Some tx ->
        rec_unify cx trace t tx
      );

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | InstanceT (reason_c, static, super, instance),
      SetPropT (reason_op, (reason_prop, x), tin) ->
      Ops.push reason_op;
      (* methods are immutable, so we hide them from property set operations *)
      let fields = instance.fields_tmap in
      set_prop cx trace reason_prop reason_c super x fields tin;
      Ops.pop ();

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | InstanceT (_, _, super, _), GetPropT (_, (_, "__proto__"), t) ->
      rec_flow cx trace (super, t)

    | InstanceT _ as instance, GetPropT (_, (_, "constructor"), t) ->
      rec_flow cx trace (ClassT instance, t)

    | InstanceT (reason_c, static, super, instance),
      GetPropT (reason_op, (reason_prop, x), tout) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_prop reason_op strict super x fields tout;

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | InstanceT (reason_c, static, super, instance),
      MethodT (reason_op, (reason_prop, x), funtype)
      -> (* TODO: closure *)
      Ops.push reason_op;
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let methods = SMap.union fields_tmap methods_tmap in
      let funt = mk_tvar cx reason_op in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_prop reason_op strict super x methods funt;
      let callt = CallT (reason_op, funtype) in
      rec_flow cx trace (funt, callt);
      Ops.pop ();

    (** In traditional type systems, object types are not extensible.  E.g., an
        object {x: 0, y: ""} has type {x: number; y: string}. While it is
        possible to narrow the object's type to hide some of its properties (aka
        width subtyping), extending its type to model new properties is
        impossible. This is not without reason: all object types would then be
        equatable via subtyping, thereby making them unsound.

        In JavaScript, on the other hand, objects can grow dynamically, and
        doing so is a common idiom during initialization (i.e., before they
        become available for general use). Objects that typically grow
        dynamically include not only object literals, but also prototypes,
        export objects, and so on. Thus, it is important to model this idiom.

        To balance utility and soundness, Flow's object types are extensible by
        default, but become sealed as soon as they are subject to width
        subtyping. However, implementing this simple idea needs a lot of care.

        To ensure that aliases have the same underlying type, object types are
        represented indirectly as pointers to records (rather than directly as
        records). And to ensure that typing is independent of the order in which
        fragments of code are analyzed, new property types can be added on gets
        as well as sets (and due to indirection, the new property types become
        immediately available to aliases).

        Looking up properties of an object, e.g. for the purposes of copying,
        when it is not fully initialized is prone to races, and requires careful
        manual reasoning about escape to avoid surprising results.

        Prototypes cause further complications. In JavaScript, objects inherit
        properties of their prototypes, and may override those properties. (This
        is similar to subclasses inheriting and overriding methods of
        superclasses.) At the same time, prototypes are extensible just as much
        as the objects they derive are. In other words, we want to maintain the
        invariant that an object's type is a subtype of its prototype's type,
        while letting them be extensible by default. This invariant is achieved
        by constraints that unify a property's type if and when that property
        exists both on the object and its prototype.

        Here's some example code with type calculations in comments. (We use the
        symbol >=> to denote a flow between a pair of types. The direction of
        flow roughly matches the pattern 'rvalue' >=> 'lvalue'.)

        var o = {}; // o:T, T |-> {}
        o.x = 4; // T |-> {x:X}, number >=> X
        var s:string = o.x; // ERROR: number >=> string

        function F() { } // F.prototype:P, P |-> {}
        var f = new F(); // f:O, O |-> {}&P

        F.prototype.m = function() { this.y = 4; } // P |-> {m:M}, ... >=> M
        f.m(); // O |-> {y:Y}&P, number >=> Y

    **)

    (**********************************************************************)
    (* objects can be assigned, i.e., their properties can be set in bulk *)
    (**********************************************************************)

    (** When some object-like type O1 flows to ObjAssignT(_,O2,X,_,false), the
        properties of O1 are copied to O2, and O2 is linked to X to signal that
        the copying is done; the intention is that when those properties are
        read through X, they should be found (whereas this cannot be guaranteed
        when those properties are read through O2). However, there is an
        additional twist: this scheme may not work when O2 is unresolved. In
        particular, when O2 is unresolved, the constraints that copy the
        properties from O1 may race with reads of those properties through X as
        soon as O2 is resolved. To avoid this race, we make O2 flow to
        ObjAssignT(_,O1,X,_,true); when O2 is resolved, we make the switch. **)

    | (ObjT (reason_, { props_tmap = mapr; _ }),
       ObjAssignT (reason, proto, t, props_to_skip, false)) ->
      Ops.push reason;
      iter_props cx mapr (fun x t ->
        if not (List.mem x props_to_skip) then (
          (* move the reason to the call site instead of the definition, so that
             it is in the same scope as the Object.assign, so that strictness
             rules apply. *)
          let r = prefix_reason (spf "property `%s` of " x) reason_ in
          let r = repos_reason (loc_of_reason reason) r in
          let t = filter_optional cx ~trace r t in
          rec_flow cx trace (proto, SetPropT (r, (r, x), t));
        );
      );
      Ops.pop ();
      rec_flow cx trace (proto, t)

    | (InstanceT (reason_, _, _, { fields_tmap; methods_tmap; _ }),
       ObjAssignT (reason, proto, t, props_to_skip, false)) ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let map = SMap.union fields_tmap methods_tmap in
      map |> SMap.iter (fun x t ->
        if not (List.mem x props_to_skip) then (
          rec_flow cx trace (proto, SetPropT (reason, (reason, x), t));
        );
      );
      rec_flow cx trace (proto, t)

    | (MixedT _, ObjAssignT (_, proto, t, _, false)) ->
      rec_flow cx trace (proto, t)

    | (_, ObjAssignT(reason, o, t, xs, true)) ->
      rec_flow cx trace (o, ObjAssignT(reason, l, t, xs, false))

    (* Object.assign semantics *)
    | ((NullT _ | VoidT _), ObjAssignT _) -> ()

    (*************************)
    (* objects can be copied *)
    (*************************)

    | (ObjT (_, { props_tmap = mapr; _ }), ObjRestT (reason, xs, t)) ->
      let map = find_props cx mapr in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let o = mk_object_with_map_proto cx reason map (MixedT reason) in
      rec_flow cx trace (o, t)

    | (InstanceT (_, _, super, insttype), ObjRestT (reason, xs, t)) ->
      (* Spread fields from super into an object *)
      let obj_super = mk_tvar_where cx reason (fun tvar ->
        rec_flow cx trace (super, ObjRestT (reason, xs, tvar))
      ) in

      (* Spread fields from the instance into another object *)
      let map = find_props cx insttype.fields_tmap in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let obj_inst = mk_object_with_map_proto cx reason map (MixedT reason) in

      (* ObjAssign the inst-generated obj into the super-generated obj *)
      let o = mk_tvar_where cx reason (fun tvar ->
        rec_flow cx trace (
          obj_inst,
          ObjAssignT(reason, obj_super, tvar, [], false)
        )
      ) in

      rec_flow cx trace (o, t)

    | (MixedT _, ObjRestT (reason, _, t)) ->
      let obj = mk_object_with_proto cx reason l in
      rec_flow cx trace (obj, t)

    | ((NullT _ | VoidT _), ObjRestT (reason, _, t)) ->
      (* mirroring Object.assign semantics, treat null/void as empty objects *)
      let o = mk_object_with_proto cx reason (MixedT reason) in
      rec_flow cx trace (o, t)

    (*************************************)
    (* objects can be copied-then-sealed *)
    (*************************************)
    | (ObjT (_, { props_tmap = mapr; _ }), ObjSealT (reason, t)) ->
      let src_props = find_props cx mapr in
      let new_obj =
        mk_object_with_map_proto cx reason ~sealed:true src_props l
      in
      rec_flow cx trace (new_obj, t)

    (*************************)
    (* objects can be frozen *)
    (*************************)

    | (ObjT (reason_o, objtype), ObjFreezeT (reason_op, t)) ->
      (* make the reason describe the result (e.g. a frozen object literal),
         but point at the entire Object.freeze call. *)
      let desc = desc_of_reason reason_o |> spf "frozen %s" in
      let reason = replace_reason desc reason_op in

      let flags = {frozen = true; sealed = Sealed; exact = true;} in
      let new_obj = ObjT (reason, {objtype with flags}) in
      rec_flow cx trace (new_obj, t)

    (*******************************************)
    (* objects may have their fields looked up *)
    (*******************************************)

    | (ObjT (reason_obj, { props_tmap = mapr; proto_t = proto; dict_t; _ }),
       LookupT(reason_op,strict,_,x,t_other))
      ->
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_obj reason_op trace in
      rec_flow cx trace (t, UnifyT(t, t_other))

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    | (ObjT (_, {flags; _}), SetPropT(_, (_, "constructor"), _)) ->
      if flags.frozen then prerr_flow cx trace "Mutation not allowed on" l u

    (** o.x = ... has the additional effect of o[_] = ... **)

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       SetPropT(reason_op, (reason_prop, x), tin))
      ->
      if flags.frozen then
        prerr_flow cx trace "Mutation not allowed on" l u
      else
        let strict_reason = mk_strict_lookup_reason
          flags.sealed (dict_t <> None) reason_o reason_op in
        let t = ensure_prop_for_write cx trace strict_reason mapr x proto
          reason_op reason_prop in
        dictionary cx trace (string_key x reason_op) t dict_t;
        rec_flow cx trace (tin,t)

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | ObjT (_, {proto_t = proto; _}), GetPropT (_, (_, "__proto__"), t) ->
      rec_flow cx trace (proto,t)

    | ObjT _, GetPropT(reason_op, (_, "constructor"), tout) ->
      rec_flow cx trace (AnyT.why reason_op, tout)

    | ObjT (reason_o, { flags; props_tmap = mapr; proto_t = proto; dict_t }),
      GetPropT (reason_op, (reason_prop, x), tout) ->
      let strict = mk_strict_lookup_reason
        flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_o reason_prop trace in
      (* move property type to read site *)
      rec_flow cx trace (t, ReposLowerT (reason_op, tout))

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (ObjT _, MethodT(_, (_, "constructor"), _)) -> ()

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       MethodT(reason_op, (reason_prop, x), funtype))
      ->
      let strict = mk_strict_lookup_reason
        flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_o reason_prop trace in
      let callt = CallT (reason_op, funtype) in
      rec_flow cx trace (t, callt)

    (******************************************)
    (* strings may have their characters read *)
    (******************************************)

    | (StrT (reason_s, _), GetElemT(reason_op,index,tout)) ->
      rec_flow cx trace (index, NumT.why reason_s);
      rec_flow cx trace (StrT.why reason_op, tout)

    (** Expressions may be used as keys to access objects and arrays. In
        general, we cannot evaluate such expressions at compile time. However,
        in some idiomatic special cases, we can; in such cases, we know exactly
        which strings/numbers the keys may be, and thus, we can use precise
        properties and indices to resolve the accesses. *)

    (**********************************************************************)
    (* objects/arrays may have their properties/elements written and read *)
    (**********************************************************************)

    | (ObjT _, SetElemT(reason_op,key,tin))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ObjT _, GetElemT(reason_op,key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, LowerBoundT tout))

    | (ArrT (_, _, []), SetElemT(reason_op, key,tin))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, UpperBoundT tin));
      rec_flow cx trace (key, num)

    | (ArrT _, SetElemT(reason_op, key,tin))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ArrT (_, _, []), GetElemT(reason_op, key,tout))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, LowerBoundT tout));
      rec_flow cx trace (key, num)

    | (ArrT _, GetElemT(reason_op, key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, LowerBoundT tout))

    | (StrT (reason_x, Literal x), ElemT(reason_op, (ObjT _ as o), t)) ->
      let reason_x = replace_reason (spf "property `%s`" x) reason_x in
      (match t with
      | UpperBoundT tin ->
          rec_flow cx trace (o, SetPropT(reason_op, (reason_x, x), tin))
      | LowerBoundT tout ->
          rec_flow cx trace (o, GetPropT(reason_op, (reason_x, x), tout))
      | _ -> assert false)

    (* if the object is a dictionary, verify it *)
    | (_, ElemT(_, ObjT(_, {dict_t = Some { key; value; _ }; _}), t))
      ->
      rec_flow cx trace (l, key);
      rec_flow cx trace (value,t);
      rec_flow cx trace (t,value)

    (* otherwise, string and number keys are allowed, but there's nothing else
       to flow without knowing their literal values. *)
    | (StrT _, ElemT(_, ObjT _, _))
    | (NumT _, ElemT(_, ObjT _, _)) ->
      ()

    | (NumT (reason_i, literal),
       ElemT(reason_op, ArrT(_, value, ts), t))
      ->
      let value = match literal with
      | Literal (float_value, _) ->
          begin try
            float_value
            |> int_of_float
            |> List.nth ts
          with _ ->
            value
          end
      | _ -> value
      in
      (match t with
      | UpperBoundT tin -> rec_flow cx trace (tin, value)
      | LowerBoundT tout -> rec_flow cx trace (value, tout)
      | _ -> assert false)

    | (ArrT _, GetPropT(reason_op, (_, "constructor"), tout)) ->
      rec_flow cx trace (AnyT.why reason_op, tout)

    | (ArrT _, SetPropT(_, (_, "constructor"), _))
    | (ArrT _, MethodT(_, (_, "constructor"), _)) ->
      ()

    (**************************************************)
    (* array pattern can consume the rest of an array *)
    (**************************************************)

    | (ArrT (_, t, ts), ArrRestT (reason, i, tout)) ->
      let a = ArrT (reason, t, Core_list.drop ts i) in
      rec_flow cx trace (a, tout)

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | (FunT (reason_f,_,t,_),
       SetPropT(reason_op, (_, "prototype"), tin))
      ->
      rec_flow cx trace (tin, ObjAssignT(reason_op, t, AnyT.t, [], false))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | (FunT (reason_f,_,t,_),
       GetPropT(reason_op, (_, "prototype"), tout))
      ->
      rec_flow cx trace (t,tout)

    | (ClassT (instance),
       GetPropT(reason_op, (_, "prototype"), tout))
      ->
      rec_flow cx trace (instance, tout)

    (***************************************************************)
    (* functions may be called by passing a receiver and arguments *)
    (***************************************************************)

    | (FunT _,
       MethodT(
         reason_op,
         (_, "call"),
         ({params_tlist=o2::tins2; _} as funtype)
       ))
      -> (* TODO: closure *)

      let funtype = { funtype with this_t = o2; params_tlist = tins2 } in
      rec_flow cx trace (l, CallT (prefix_reason "call " reason_op, funtype))

    (*******************************************)
    (* ... or a receiver and an argument array *)
    (*******************************************)

    | (FunT _,
       MethodT(r, (_, "apply"), ({params_tlist=[_;ts]; _} as funtype))) ->
      (* TODO: closure *)
      rec_flow cx trace (ts, ApplyT (r, l, funtype))

    | (ArrT (_, t, ts),
       ApplyT (r, l, ({params_tlist=this_t::_; _} as funtype))) ->
      let funtype = { funtype with this_t; params_tlist = ts @ [RestT t] } in
      rec_flow cx trace (l, CallT (r, funtype))

    (************************************************************************)
    (* functions may be bound by passing a receiver and (partial) arguments *)
    (************************************************************************)

    | (FunT (reason,_,_,
             {this_t = o1; params_tlist = tins1; return_t = tout1; _}),
       MethodT(
         reason_op, (_, "bind"), ({params_tlist=o2::tins2; _} as funtype)))
      -> (* TODO: closure *)

        rec_flow cx trace (o2,o1);

        let tins1 = multiflow_partial cx trace (tins2,tins1) in

        rec_flow cx trace (
          FunT(reason_op, dummy_static, dummy_prototype,
               mk_functiontype tins1 tout1),
          funtype.return_t)

    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | (FunT (reason, statics, proto, _) , ObjT _) ->
        let map = SMap.add "$call" l SMap.empty in
        let function_proto = get_builtin_type cx reason "Function" in
        let obj = mk_object_with_map_proto cx reason map function_proto in
        let t = mk_tvar_where cx reason (fun t ->
          rec_flow cx trace (statics, ObjAssignT (reason, obj, t, [], false))
        ) in
        rec_flow cx trace (t, u)

    (*********************************************************************)
    (* class A is a base class of class B iff                            *)
    (* properties in B that override properties in A or its base classes *)
    (* have the same signatures                                          *)
    (*********************************************************************)

    (** The purpose of SuperT is to establish consistency between overriding
        properties with overridden properties. As such, the lookups performed
        for the inherited properties are non-strict: they are not required to
        exist. **)

    | (InstanceT (_,_,_,instance_super),
       SuperT (reason,instance))
      ->
        iter_props cx instance_super.fields_tmap (fun x -> function
          | AbstractT t when not (has_prop cx instance.fields_tmap x) ->
            (* when abstract fields are not implemented, make them void *)
            let reason = reason_of_t t in
            let desc_void = spf "undefined. Did you forget to declare %s?"
              (desc_of_reason reason) in
            let reason_void = replace_reason desc_void reason in
            rec_unify cx trace (VoidT reason_void) t
          | _ -> ()
        );
        iter_props cx instance.fields_tmap (fun x t ->
          lookup_prop cx trace l reason None x t
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if inherited_method x then
            (* we're able to do supertype compatibility in super methods because
               they're immutable *)
            lookup_prop cx trace l reason None x (UpperBoundT t)
        )

    | (ObjT _,
       SuperT (reason,instance))
      ->
        iter_props cx instance.fields_tmap (fun x t ->
          rec_flow cx trace (l, LookupT(reason,None,[],x,t))
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if inherited_method x then
            rec_flow cx trace (l, LookupT(reason,None,[],x,t))
        )

    (***********************************************************)
    (* addition                                                *)
    (***********************************************************)

    | (l, AdderT (reason, r, u)) ->
      Ops.push reason;
      flow_addition cx trace reason l r u;
      Ops.pop ()

    (**************************)
    (* relational comparisons *)
    (**************************)

    | (l, ComparatorT(reason, r)) ->
      Ops.push reason;
      flow_comparator cx trace reason l r;
      Ops.pop ()

    | (l, EqT(reason, r)) ->
      Ops.push reason;
      flow_eq cx trace reason l r;
      Ops.pop ()

    (************************)
    (* unary minus operator *)
    (************************)

    | (NumT (_, lit), UnaryMinusT (reason_op, t_out)) ->
      let num = match lit with
      | Literal (value, raw) ->
        let raw_len = String.length raw in
        let raw = if raw_len > 0 && raw.[0] = '-'
          then String.sub raw 1 (raw_len - 1)
          else "-" ^ raw
        in
        NumT (reason_op, Literal (~-. value, raw))
      | AnyLiteral
      | Truthy
      | Falsy ->
        l
      in
      rec_flow cx trace (num, t_out)

    (**************************************)
    (* types may be refined by predicates *)
    (**************************************)

    | (_, PredicateT(p,t)) ->
      predicate cx trace t (l,p)

    (**********************)
    (* Array library call *)
    (**********************)

    | (ArrT (_, t, _), (GetPropT _ | SetPropT _ | MethodT _ | LookupT _)) ->
      let reason = reason_of_t u in
      let arrt = get_builtin_typeapp cx reason "Array" [t] in
      rec_flow cx trace (arrt, u)

    (***********************)
    (* String library call *)
    (***********************)

    | (StrT (reason, _), (GetPropT _ | MethodT _ | LookupT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "String",u)

    (***********************)
    (* Number library call *)
    (***********************)

    | (NumT (reason, _), (GetPropT _ | MethodT _ | LookupT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Number",u)

    (***********************)
    (* Boolean library call *)
    (***********************)

    | (BoolT (reason, _), (GetPropT _ | MethodT _ | LookupT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Boolean",u)

    (*************************)
    (* Function library call *)
    (*************************)

    | (FunProtoT reason, (GetPropT _ | MethodT _ | LookupT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Function",u)

    (*********************)
    (* functions statics *)
    (*********************)

    | (FunT (_,static,_,_), _) when object_like_op u ->
      rec_flow cx trace (static, u)

    (*****************)
    (* class statics *)
    (*****************)

    | (ClassT instance, _) when object_like_op u ->
      let reason = prefix_reason "statics of " (reason_of_t instance) in
      let tvar = mk_tvar cx reason in
      rec_flow cx trace (instance, GetPropT(reason, (reason, "statics"), tvar));
      rec_flow cx trace (tvar, ReposLowerT (reason, u))

    (***************************************************************************)
    (* classes can behave like functions, functions can be declared as classes *)
    (***************************************************************************)

    (* When a class value flows to a function annotation or call site, check for
       the presence of a $call property in the former (as a static) compatible
       with the latter. *)
    | (ClassT instance, (FunT (reason, _, _, _) | CallT (reason, _))) ->
      rec_flow cx trace (l, GetPropT(reason, (reason, "$call"),u))

    (* For a function type to be used as a class type, the following must hold:
       - the class's instance type must be a subtype of the function's prototype
       property type and 'this' type
       - the function's statics should be included in the class's statics
       (typically a function's statics are under-specified, so we don't
       enforce equality)
       - the class's static $call property type must be a subtype of the
       function type. *)
    | (FunT (reason, static, prototype, funtype), ClassT instance) ->
      rec_flow cx trace (instance, prototype);
      rec_flow cx trace (instance, funtype.this_t);
      rec_flow cx trace (instance, GetPropT(reason, (reason, "statics"), static));
      rec_flow cx trace (u, GetPropT(reason, (reason, "$call"), l))

    (************)
    (* indexing *)
    (************)

    | (InstanceT _, GetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetPropT(reason, (reason, "$key"), i));
      rec_flow cx trace (l, GetPropT(reason, (reason, "$value"), t))

    | (InstanceT _, SetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetPropT(reason, (reason, "$key"), i));
      rec_flow cx trace (l, SetPropT(reason, (reason, "$value"), t))

    (*************************)
    (* repositioning, part 2 *)
    (*************************)

    (* waits for a lower bound to become concrete, and then repositions it to
       the location stored in the ReposLowerT, which is usually the location
       where that lower bound was used; the lower bound's location (which is
       being overwritten) is where it was defined. *)
    | (_, ReposLowerT (reason_op, t)) ->
      rec_flow cx trace (reposition cx reason_op l, t)

    (***************)
    (* unsupported *)
    (***************)

    (** Lookups can be strict or non-strict, as denoted by the presence or
        absence of strict_reason in the following two pattern matches.
        Strictness derives from whether the object is sealed and was
        created in the same scope in which the lookup occurs - see
        mk_strict_lookup_reason below. The failure of a strict lookup
        to find the desired property causes an error; a non-strict one
        does not.
     *)

    | (MixedT _,
       LookupT (reason, strict, next::try_ts_on_failure, s, t)) ->
      (* When s is not found, we always try to look it up in the next element in
         the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, LookupT (reason, strict, try_ts_on_failure, s, t))

    | (MixedT reason, LookupT (reason_op, Some strict_reason, [], x, _)) ->

      if is_object_prototype_method x
      then
        (** TODO: These properties should go in Object.prototype. Currently we
            model Object.prototype as a MixedT, as an optimization against a
            possible deluge of shadow properties on Object.prototype, since it
            is shared by every object. **)
        rec_flow cx trace (get_builtin_type cx reason "Object", u)

      (* if we're looking something up on the global/builtin object, then tweak
         the error to say that `x` doesn't exist. We can tell this is the global
         object because that should be the only object created with
         `builtin_reason` instead of an actual location (see `Init_js.init`). *)
      else if is_builtin_reason reason
      then
        let msg =
          if is_internal_module_name x
          then "Required module not found"
          else "Could not resolve name"
        in
        let message_list = [
          reason_op, msg
        ] in
        add_warning cx message_list ~trace
      else
        let msg =
          if x = "$call"
          then "Callable signature not found in"
          else if x = "$key" || x = "$value"
          then "Indexable signature not found in"
          else "Property not found in"
        in
        prmsg_flow cx
          Errors_js.InferError
          trace
          msg
          (reason_op, strict_reason)

    (* LookupT is a non-strict lookup, never fired *)
    | (MixedT _, LookupT _) -> ()

    (* SuperT only involves non-strict lookups *)
    | (MixedT _, SuperT _) -> ()

    (** ExtendsT searches for a nominal superclass. The search terminates with
        either failure at the root or a structural subtype check. **)

    | (MixedT _, ExtendsT (next::try_ts_on_failure, l, u)) ->
      (* When seaching for a nominal superclass fails, we always try to look it
         up in the next element in the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, ExtendsT (try_ts_on_failure, l, u))

    | (MixedT _, ExtendsT ([], l, InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       })))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    | (MixedT _, ExtendsT ([], t, tc)) ->
      let msg = "This type is incompatible with" in
      prmsg_flow cx
        Errors_js.InferError
        trace
        msg
        (reason_of_t t, reason_of_t tc)

    (* when unexpected types flow into a GetPropT/SetPropT (e.g. void or other
       non-object-ish things), then use `reason_prop`, which represents the
       reason for the prop itself, not the lookup action. *)
    | (_, GetPropT (_, (reason_prop, _), _))
    | (_, SetPropT (_, (reason_prop, _), _)) ->
      prmsg_flow cx
        Errors_js.InferError
        trace
        (err_msg l u)
        (reason_prop, reason_of_t l)

    | _ ->
      prerr_flow cx trace (err_msg l u) l u
  )

(* some types need to be resolved before proceeding further *)
and needs_resolution = function
  | OpenT _ | UnionT _ | OptionalT _ | MaybeT _ | AnnotT _ -> true
  | _ -> false

and parameters_of_taint_op = function
  | MethodT (_, _, funtype) -> funtype.params_tlist
  | SetPropT (_, _, t) -> [t]
  (* TODO(rcastano): This is a source of false negatives for the Tainted type.
   * We should add more rules in the future.*)
  | _ -> []

(**
 * Addition
 *
 * Given l + r:
 *  - if l or r is a string, or a Date, or an object whose
 *    valueOf() returns an object, returns a string.
 *  - otherwise, returns a number
 *
 * Since we don't consider valueOf() right now, Date is no different than
 * any other object. The only things that are neither objects nor strings
 * are numbers, booleans, null, undefined and symbols. Since we can more
 * easily enumerate those things, this implementation inverts the check:
 * anything that is a number, boolean, null or undefined is treated as a
 * number; everything else is a string.
 *
 * We are less permissive than the spec when it comes to string coersion:
 * only numbers can be coerced, to allow things like `num + '%'`.
 *
 * TODO: handle symbols (which raise a TypeError, so should be banned)
 *
 **)
and flow_addition cx trace reason l r u =
  if needs_resolution r then rec_flow cx trace (r, AdderT (reason, l, u))
  else match (l, r) with
  | (StrT _, StrT _)
  | (StrT _, NumT _)
  | (NumT _, StrT _) ->
    rec_flow cx trace (StrT.why reason, u)

  | (MixedT _, _)
  | (_, MixedT _) ->
    rec_flow cx trace (MixedT.why reason, u)

  | ((NumT _ | SingletonNumT _ | BoolT _ | SingletonBoolT _ | NullT _ | VoidT _),
     (NumT _ | SingletonNumT _ | BoolT _ | SingletonBoolT _ | NullT _ | VoidT _)) ->
    rec_flow cx trace (NumT.why reason, u)

  | (_, _) ->
    let fake_str = StrT.why reason in
    rec_flow cx trace (l, fake_str);
    rec_flow cx trace (r, fake_str);
    rec_flow cx trace (fake_str, u);

(**
 * relational comparisons like <, >, <=, >=
 *
 * typecheck iff either of the following hold:
 *   number <> number = number
 *   string <> string = string
 **)
and flow_comparator cx trace reason l r =
  if needs_resolution r then rec_flow cx trace (r, ComparatorT (reason, l))
  else match (l, r) with
  | (StrT _, StrT _) -> ()
  | (_, _) when numeric l && numeric r -> ()
  | (_, _) -> prerr_flow cx trace "Cannot be compared to" l r

(**
 * == equality
 *
 * typecheck iff they intersect (otherwise, unsafe coercions may happen).
 *
 * note: any types may be compared with === (in)equality.
 **)
and flow_eq cx trace reason l r =
  if needs_resolution r then rec_flow cx trace (r, EqT(reason, l))
  else match (l, r) with
  | (_, _) when equatable cx trace (l, r) -> ()
  | (_, _) -> prerr_flow cx trace "Cannot be compared to" l r

and is_object_prototype_method = function
  | "hasOwnProperty"
  | "propertyIsEnumerable"
  | "toLocaleString"
  | "toString"
  | "valueOf" -> true
  | _ -> false

(* neither object prototype methods nor callable signatures should be
 * implied by an object indexer type *)
and is_dictionary_exempt = function
  | x when is_object_prototype_method x -> true
  | "$call" -> true
  | _ -> false

(* only on use-types - guard calls with is_use t *)
and err_operation = function
  | GetPropT _ -> "Property cannot be accessed on"
  | SetPropT _ -> "Property cannot be assigned on"
  | MethodT _ -> "Method cannot be called on"
  | CallT _ -> "Function cannot be called on"
  | ApplyT _ -> "Expected array of arguments instead of"
  | ConstructorT _ -> "Constructor cannot be called on"
  | GetElemT _ -> "Computed property/element cannot be accessed on"
  | SetElemT _ -> "Computed property/element cannot be assigned on"
  | ElemT (_, ObjT _, LowerBoundT _) -> "Computed property cannot be accessed with"
  | ElemT (_, ArrT _, LowerBoundT _) -> "Element cannot be accessed with"
  | ElemT (_, ObjT _, UpperBoundT _) -> "Computed property cannot be assigned with"
  | ElemT (_, ArrT _, UpperBoundT _) -> "Element cannot be assigned with"
  | ObjAssignT _ -> "Expected object instead of"
  | ObjRestT _ -> "Expected object instead of"
  | ObjSealT _ -> "Expected object instead of"
  | ArrRestT _ -> "Expected array instead of"
  | SuperT _ -> "Cannot inherit"
  | SpecializeT _ -> "Expected polymorphic type instead of"
  | LookupT _ -> "Property not found in"
  | GetKeysT _ -> "Expected object instead of"
  | HasOwnPropT _ -> "Property not found in"
  | UnaryMinusT _ -> "Expected number instead of"
  (* unreachable or unclassified use-types. until we have a mechanical way
     to verify that all legit use types are listed above, we can't afford
     to throw on a use type, so mark the error instead *)
  | t when is_use t ->
    (spf "Type is incompatible with (unclassified use type: %s)"
      (string_of_ctor t))
  (* def-types *)
  | t ->
    failwith (spf "err_operation called on def type %s" (string_of_ctor t))

and err_value = function
  | NullT _ -> " possibly null value"
  | VoidT _ -> " possibly undefined value"
  | MaybeT t -> spf " possibly null or undefined value"
  | t -> ""

and err_msg l u =
  if is_use u
  then spf "%s%s" (err_operation u) (err_value l)
  else "This type is incompatible with"

and ground_subtype = function
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _) | (_, OpenT _) -> false
  (* Prevents Tainted<any> -> any *)
  | (UnionT _, _) | (TaintT _, _) -> false

  | (NumT _,NumT _)
  | (StrT _,StrT _)
  | (BoolT _, BoolT _)
  | (NullT _, NullT _)
  | (VoidT _, VoidT _)
  | (EmptyT _,_)
  | (_,MixedT _)
  | (_,FunProtoT _) (* MixedT is used for object protos, this is for funcs *)
  | (AnyT _,_)
  | (_,AnyT _)
    -> true

  | _ ->
    false

and numeric = function
  | NumT _ -> true
  | SingletonNumT _ -> true

  | InstanceT (reason, _, _, _) ->
    desc_of_reason reason = "Date"

  | _ -> false

and object_like = function
  | ObjT _ | InstanceT _ -> true
  | t -> function_like t

and object_like_op = function
  | SetPropT _ | GetPropT _ | MethodT _ | LookupT _
  | SuperT _
  | GetKeysT _ | HasOwnPropT _
  | ObjAssignT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | AnyObjT _ -> true
  | _ -> false

and function_like = function
  | ClassT _
  | FunT _ -> true
  | _ -> false

and function_like_op = function
  | CallT _ | TypeT _
  | ConstructorT _
  | AnyFunT _ -> true
  | t -> object_like_op t

and equatable cx trace = function

  | (NumT _,NumT _)

  | (StrT _,StrT _)

  | (BoolT _, BoolT _)

  | (EmptyT _,_) | (_, EmptyT _)

  | (_,MixedT _) | (MixedT _,_)

  | (AnyT _,_) | (_,AnyT _)

  | (VoidT _,_) | (_, VoidT _)

  | (NullT _,_) | (_, NullT _)
    -> true

  | ((NumT _ | StrT _ | BoolT _), _)
  | (_, (NumT _ | StrT _ | BoolT _))
    -> false

  | _ -> true

and taint_op = function
  | AdderT _ | GetPropT _ | GetElemT _ | ComparatorT _ -> true
  | _ -> false

and result_of_taint_op = function
  | AdderT (_, _, u) | GetPropT (_, _, u) | GetElemT (_, _, u) -> Some u
  | ComparatorT _ -> None
  | _ -> assert false

(* generics *)

(* Generate for every type parameter a pair of tests, instantiating that type
   parameter with its bound and Bottom. Run a closure that takes these
   instantiations, each one in turn, and does something with it. We use a
   different value for test_id (see Reason_js.TestID) for every instantiation so
   that re-analyzing the same AST with different instantiations causes different
   reasons to be generated. *)

and generate_tests cx reason typeparams each =
  (* generate 2^|typeparams| maps *)
  let maps = List.fold_left (fun list {name; bound; _ } ->
    let xreason = replace_reason name reason in
    let bot = EmptyT (
      prefix_reason "some incompatible instantiation of " xreason
    ) in
    List.rev_append
      (List.map (fun map -> SMap.add name (subst cx map bound) map) list)
      (List.map (SMap.add name bot) list)
  ) [SMap.empty] typeparams in
  match maps with
  | [map] -> each map (* no typeparams, so reuse current test_id *)
  | _ -> List.iter (TestID.run each) maps

(*********************)
(* inheritance utils *)
(*********************)

and mk_nominal cx =
  let nominal = mk_id () in
  (if Context.is_verbose cx then prerr_endlinef
      "NOM %d %s" nominal (string_of_filename (Context.file cx)));
  nominal

and unify_map cx trace tmap1 tmap2 =
  tmap1 |> SMap.iter (fun x t1 ->
    let t2 = SMap.find_unsafe x tmap2 in
    rec_unify cx trace t1 t2
  )

and flow_type_args cx trace instance instance_super =
  (* with this out of the way, we can assume polaritiy maps are the same *)
  (if instance.class_id != instance_super.class_id then
    assert_false "unexpected difference in class_ids in flow_type_args");
  let { type_args = tmap1; arg_polarities = pmap; _ } = instance in
  let { type_args = tmap2; _ } = instance_super in
  tmap1 |> SMap.iter (fun x t1 ->
    let t2 = SMap.find_unsafe x tmap2 in
    (* type_args contains a mixture of args to type params declared on the
       instance's class, and args to outer-scope type params.
       OTOH arg_polarities only holds polarities of declared params.
       it'll take some upstream refactoring to handle variance to in-scope
       type params - meanwhile, we fall back to neutral (invariant) *)
    match SMap.get x pmap with
    | Some Negative -> rec_flow cx trace (t2, t1)
    | Some Positive -> rec_flow cx trace (t1, t2)
    | Some Neutral
    | None -> rec_unify cx trace t1 t2
  )

and inherited_method x = x <> "constructor" && x <> "$call"

(* Indicate whether property checking should be strict for a given object and an
   operation on it. Strictness is enforced when the object is not a dictionary,
   and it is sealed (e.g., it is a type annotation) or it and the operation
   originate in different scopes. The enforcement is done via the returned
   "blame token" that is used when looking up properties of objects in the
   prototype chain as part of that operation. *)
and mk_strict_lookup_reason sealed is_dict reason_o reason_op =
  let sealed = not is_dict && match sealed with
  | Sealed -> true
  | UnsealedInFile source -> source <> (Loc.source (loc_of_reason reason_op))
  in
  if sealed then
    Some reason_o
  else
    None

(* dispatch checks to verify that lower satisfies the structural
   requirements given in the tuple. *)
and structural_subtype cx trace lower reason_struct
  (super, fields_tmap, methods_tmap) =
  let lower_reason = reason_of_t lower in
  let fields_tmap = find_props cx fields_tmap in
  let methods_tmap = find_props cx methods_tmap in
  fields_tmap |> SMap.iter (fun s t2 ->
    match t2 with
    | OptionalT t2 ->
      let lookup_reason =
        prefix_reason (spf "optional property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, None, [], s, t2))
    | _ ->
      let lookup_reason =
        prefix_reason (spf "property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, Some lower_reason, [], s, t2))
  );
  methods_tmap |> SMap.iter (fun s t2 ->
    if inherited_method s then
      let lookup_reason =
        prefix_reason (spf "property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, Some lower_reason, [], s, LowerBoundT (t2)))
  );
  rec_flow cx trace (lower, super)


(*****************)
(* substitutions *)
(*****************)

(* need to consider only "def" types *)

(** Substitute bound type variables with associated types in a type. Do not
    force substitution under polymorphic types. This ensures that existential
    type variables under a polymorphic type remain unevaluated until the
    polymorphic type is applied. **)
and subst cx ?(force=true) (map: Type.t SMap.t) t =
  if SMap.is_empty map then t
  else match t with
  | BoundT typeparam ->
    (match SMap.get typeparam.name map with
    | None -> t
    | Some t -> t)

  | ExistsT reason ->
    if force then mk_tvar cx reason
    else t

  | OpenT _
  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | NullT _
  | VoidT _
  | MixedT _
  | TaintT _
  | AnyT _
  | FunProtoT _
    ->
    t

  | FunT (reason, static, proto, {
    this_t = this;
    params_tlist = params;
    params_names = names;
    return_t = ret;
    closure_t = j;
    changeset
  }) ->
    FunT (reason, subst cx ~force map static, subst cx ~force map proto, {
      this_t = subst cx ~force map this;
      params_tlist = List.map (subst cx ~force map) params;
      params_names = names;
      return_t = subst cx ~force map ret;
      closure_t = j;
      changeset
    })

  | PolyT (xs,t) ->
    let xs, map = List.fold_left (fun (xs, map) typeparam ->
      { typeparam with bound = subst cx ~force map typeparam.bound }::xs,
      SMap.remove typeparam.name map
    ) ([], map) xs in
    PolyT (List.rev xs, subst cx ~force:false map t)

  | ObjT (reason, {
    flags;
    dict_t;
    props_tmap = id;
    proto_t = proto
  }) ->
    ObjT (reason, {
      flags;
      dict_t = (match dict_t with
      | None -> None
      | Some dict ->
          Some { dict with
            key = subst cx ~force map dict.key;
            value = subst cx ~force map dict.value;
          });
      props_tmap = subst_propmap cx force map id;
      proto_t = subst cx ~force map proto
    })

  | ArrT (reason, t, ts) ->
    ArrT (reason,
          subst cx ~force map t,
          ts |> List.map (subst cx ~force map))

  | ClassT t -> ClassT (subst cx ~force map t)

  | TypeT (reason, t) ->
    TypeT (reason, subst cx ~force map t)

  | AnnotT (sink_t, source_t) ->
    AnnotT (subst cx ~force map sink_t, subst cx ~force map source_t)

  | InstanceT (reason, static, super, instance) ->
    InstanceT (
      reason,
      subst cx ~force map static,
      subst cx ~force map super,
      { instance with
        type_args = instance.type_args |> SMap.map (subst cx ~force map);
        fields_tmap = subst_propmap cx force map instance.fields_tmap;
        methods_tmap = subst_propmap cx force map instance.methods_tmap;
      }
    )

  | OptionalT (t) -> OptionalT (subst cx ~force map t)

  | RestT (t) -> RestT (subst cx ~force map t)

  | AbstractT (t) -> AbstractT (subst cx ~force map t)

  | TypeAppT(c, ts) ->
      let c = subst cx ~force map c in
      let ts = List.map (subst cx ~force map) ts in
      TypeAppT(c, ts)

  | MaybeT(t) ->
    MaybeT(subst cx ~force map t)

  | IntersectionT(reason, ts) ->
    IntersectionT(reason, List.map (subst cx ~force map) ts)

  | UnionT(reason, ts) ->
    UnionT(reason, List.map (subst cx ~force map) ts)

  | UpperBoundT(t) ->
    UpperBoundT(subst cx ~force map t)

  | LowerBoundT(t) ->
    LowerBoundT(subst cx ~force map t)

  | AnyObjT _ -> t
  | AnyFunT _ -> t

  | ShapeT(t) ->
    ShapeT(subst cx ~force map t)

  | DiffT(t1, t2) ->
    DiffT(subst cx ~force map t1, subst cx ~force map t2)

  | KeysT(reason, t) ->
    KeysT(reason, subst cx ~force map t)

  | SingletonNumT _
  | SingletonBoolT _
  | SingletonStrT _ -> t

  | ObjAssignT(reason, t1, t2, xs, resolve) ->
    ObjAssignT(reason, subst cx ~force map t1, subst cx ~force map t2, xs, resolve)

  | _ ->
      failwith (spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)

and subst_propmap cx force map id =
  let pmap = find_props cx id in
  let pmap_ = SMap.map (subst cx ~force map) pmap in
  if pmap_ = pmap then id
  else mk_propmap cx pmap_

and typeapp_arity_mismatch cx expected_num reason =
  let msg = spf "wrong number of type arguments (expected %d)" expected_num in
  add_error cx [reason, msg];

(* Instantiate a polymorphic definition given type arguments. *)
and instantiate_poly_with_targs cx trace reason_op ?(cache=false) (xs,t) ts =
  let len_xs = List.length xs in
  if len_xs <> List.length ts
  then (
    typeapp_arity_mismatch cx len_xs reason_op;
    AnyT reason_op
  )
  else
    let map =
      List.fold_left2
        (fun map typeparam t ->
          let t_ = cache_instantiate cx trace cache (typeparam, reason_op) t in
          rec_flow cx trace (t_, subst cx map typeparam.bound);
          SMap.add typeparam.name t_ map
        )
        SMap.empty xs ts
    in
    subst cx map t

(* Given a type parameter, a supplied type argument for specializing it, and a
   reason for specialization, either return the type argument or, when directed,
   look up the instantiation cache for an existing type argument for the same
   purpose and unify it with the supplied type argument. *)
and cache_instantiate cx trace cache (typeparam, reason_op) t =
  if cache then
    let t_ = Cache.PolyInstantiation.find cx (typeparam, reason_op) in
    rec_unify cx trace t t_;
    t_
  else t

(* Instantiate a polymorphic definition by creating fresh type arguments. *)
and instantiate_poly ?(weak=false) cx trace reason_op (xs,t) =
  let ts = xs |> List.map (fun typeparam ->
    if weak then AnyT.why reason_op
    else ImplicitTypeArgument.mk_targ cx (typeparam, reason_op)
  )
  in
  instantiate_poly_with_targs cx trace reason_op (xs,t) ts

and mk_object_with_proto cx reason proto =
  mk_object_with_map_proto cx reason SMap.empty proto

and mk_object_with_map_proto cx reason ?(sealed=false) ?frozen ?dict map proto =
  let sealed =
    if sealed then Sealed
    else UnsealedInFile (Loc.source (loc_of_reason reason))
  in
  let flags = { default_flags with sealed } in
  let flags = match frozen with
  | Some frozen -> { flags with frozen }
  | None -> flags
  in
  let pmap = mk_propmap cx map in
  ObjT (reason, mk_objecttype ~flags dict pmap proto)

(* Speculatively match types, returning Some(error messages) when the match
   fails, and None otherwise. *)
and speculative_flow_error cx trace l u =
  (* save the ops stack, since throws from within __flow will screw it up *)
  let ops = Ops.get () in
  let typeapp_stack = TypeAppExpansion.get () in
  throw_on_error := true;
  let result =
    try rec_flow cx trace (l, u); false
    with
    | FlowError msgs -> true
    | exn ->
        throw_on_error := false;
        raise exn
  in
  throw_on_error := false;
  TypeAppExpansion.set typeapp_stack;
  (* restore ops stack *)
  Ops.set ops;
  result

(* try each branch of a union in turn *)
and try_union cx trace l reason = function
  | [] -> (* fail: bottom is the unit of unions *)
    rec_flow cx trace (l, EmptyT reason)
  | t::ts ->
    rec_flow cx trace
      (SpeculativeMatchFailureT(reason, l, UnionT(reason, ts)),
       t)

(* try each branch of an intersection in turn *)
and try_intersection cx trace u reason = function
  | [] -> (* fail: top is the unit of intersections *)
    rec_flow cx trace (MixedT reason, u)
  | t::ts ->
    rec_flow cx trace
      (t,
       SpeculativeMatchFailureT(reason, IntersectionT(reason, ts), u))

(* Some types need their parts to be concretized (i.e., type variables may need
   to be replaced by concrete types) so that speculation has a chance to fail
   early (and other branches are tried): otherwise, those failures may remain
   latent, and cause spurious errors to be reported. *)

(** TODO: Concretization is a general pattern that could be useful elsewhere as
    well. For example, we often have constraints that model "binary" operations,
    where both arguments need to be concretized for an operation to proceed
    (e.g., see rules involving AdderT, ComparatorT, ObjAssignT); in those cases,
    we effectively concretize the first argument, then the second argument by
    pushing the concretized first argument back into the constraint, and
    signaling that we are done afterwards. A general solution would have to do
    the following: specify parts of a constraint to be concretized, concretize
    those parts, replace the constraint with the concrete parts, and signal that
    we are done. The specification and replacement can be carried out generically
    using type substitution (see rules involving SpecializeT). The signaling
    could be done either by wrapping with ConcreteT, or by generically checking
    that the specified parts to be concretized have indeed been replaced with
    concrete parts. **)

(* The set of type patterns that currently need to be concretized appear
   below. This set needs to be kept in sync across parts_to_concretize and
   replace_with_concrete parts. *)

and parts_to_concretize cx = function
  (* call of overloaded function *)
  | CallT (reason, callt) -> callt.params_tlist
  (* selection of overloaded function *)
  | FunT (reason, _, _, callt) -> callt.params_tlist
  | u -> []

and replace_with_concrete_parts ts = function
  | CallT (reason, callt) ->
      CallT (reason, { callt with params_tlist = ts })
  | FunT (reason, static, prototype, callt) ->
      FunT (reason, static, prototype, { callt with params_tlist = ts })
  | u -> u

(* Take a todo_list of types and recursively concretize them, moving the
   concretized types to a done_list. Types in the todo_list, e.g. t, may be type
   variables, and here we simply set up the requirement that they be
   concretized; when they hit the rule for ConcretizeT, they are concrete, and
   therefore ready to be moved to the done_list. *)
and concretize_parts cx trace l u done_list = function
  | [] ->
      (* items were moved from todo_list to done_list in LIFO order *)
      let done_list = List.rev done_list in
      rec_flow cx trace
        (l, ConcreteT(replace_with_concrete_parts done_list u))
  | t::todo_list ->
      rec_flow cx trace
        (t, ConcretizeT(l, todo_list, done_list, u))

(* property lookup functions in objects and instances *)

(**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
and is_munged_prop_name cx name =
  (Context.should_munge_underscores cx)
  && (String.length name >= 2)
  && name.[0] = '_'
  && name.[1] <> '_'

and ensure_prop_for_read cx strict mapr x proto dict_t reason_obj reason_op trace =
  let ops = Ops.clear () in
  let t = match (read_prop_opt cx mapr x, dict_t) with
  | Some t, _ -> Some t
  | None, Some { key; value; _ } ->
    if is_dictionary_exempt x
    then None
    else (
      rec_flow cx trace (string_key x reason_op, key);
      Some value
    )
  | None, None -> None
  in
  let tout = match t with
  (* map contains property x at type t *)
  | Some t -> t
  (* otherwise, check for/maybe add shadow property *)
  | None ->
    let t =
      match read_prop_opt cx mapr (internal_name x) with
      | Some t -> t
      | None -> intro_prop cx reason_obj x mapr
    in
    t |> recurse_proto cx strict proto reason_op x trace
  in
  Ops.set ops;
  tout

and ensure_prop_for_write cx trace strict mapr x proto reason_op reason_prop =
  match read_prop_opt cx mapr x with
  (* map contains property x at type t *)
  | Some t -> t
  (* otherwise, error if strict, else unshadow/add prop *)
  | None -> (
    match strict with
    | Some reason_o ->
      prmsg_flow_prop_not_found cx trace (reason_prop, reason_o);
      AnyT.t
    | None ->
      let t =
        if has_prop cx mapr (internal_name x)
        then read_and_delete_prop cx mapr (internal_name x) |> (fun t ->
          write_prop cx mapr x t; t
        )
        else intro_prop_ cx reason_op x mapr
      in
      t |> recurse_proto cx None proto reason_prop x trace
    )

and lookup_prop cx trace l reason strict x t =
  let l =
    (* munge names beginning with single _ *)
    if is_munged_prop_name cx x
    then MixedT (reason_of_t l)
    else l
  in
  rec_flow cx trace (l, LookupT (reason, strict, [], x, t))

and get_prop cx trace reason_prop reason_op strict super x map tout =
  let ops = Ops.clear () in
  let tout = ReposLowerT (reason_op, tout) in
  begin if SMap.mem x map
  then
    rec_flow cx trace (SMap.find_unsafe x map, tout)
  else
    lookup_prop cx trace super reason_prop strict x (LowerBoundT tout)
  end;
  Ops.set ops

and set_prop cx trace reason_op reason_c super x map tin =
  let map = find_props cx map in
  if SMap.mem x map
  then
    rec_flow cx trace (tin, SMap.find_unsafe x map)
  else
    lookup_prop cx trace super reason_op (Some reason_c) x (UpperBoundT tin)

and intro_prop cx reason_obj x mapr =
  let reason_prop = prefix_reason (spf ".%s of " x) reason_obj in
  mk_tvar_where cx reason_prop (fun tvar ->
    write_prop cx mapr (internal_name x) tvar
  )

and intro_prop_ cx reason_op x mapr =
  mk_tvar_where cx reason_op (fun tvar ->
    write_prop cx mapr x tvar
  )

and recurse_proto cx strict proto reason_op x trace t =
  rec_flow cx trace (proto, LookupT(reason_op,strict,[],x,t));
  t

(* other utils *)

and filter cx trace t l pred =
  if (pred l) then rec_flow cx trace (l,t)

and is_string = function StrT _ -> true | _ -> false
and is_number = function NumT _ -> true | _ -> false
and is_function = function AnyFunT _ | FunT _ -> true | _ -> false
and is_object = function (AnyObjT _ | ObjT _ | ArrT _ | NullT _) -> true | _ -> false
and is_array = function ArrT _ -> true | _ -> false
and is_bool = function BoolT _ -> true | _ -> false

and not_ pred x = not(pred x)

and recurse_into_union filter_fn (r, ts) =
  let new_ts = ts |> List.filter (fun t ->
    match filter_fn t with
    | EmptyT _ -> false
    | _ -> true
  ) in
  match new_ts with
  | [] -> EmptyT r
  | [t] -> t
  | _ -> UnionT (r, new_ts)

and filter_exists = function
  (* falsy things get removed *)
  | NullT r
  | VoidT r
  | BoolT (r, Some false)
  | StrT (r, (Literal "" | Falsy))
  | NumT (r, (Literal (0., _) | Falsy)) -> EmptyT r

  (* unknown things become truthy *)
  | MaybeT t -> t
  | OptionalT t -> filter_exists t
  | BoolT (r, None) -> BoolT (r, Some true)
  | StrT (r, AnyLiteral) -> StrT (r, Truthy)
  | NumT (r, AnyLiteral) -> NumT (r, Truthy)

  (* truthy things pass through *)
  | t -> t

and filter_not_exists t = match t with
  (* falsy things pass through *)
  | NullT _
  | VoidT _
  | BoolT (_, Some false)
  | StrT (_, (Literal "" | Falsy))
  | NumT (_, (Literal (0., _) | Falsy)) -> t

  (* truthy things get removed *)
  | BoolT (r, Some _)
  | StrT (r, (Literal _ | Truthy))
  | ArrT (r, _, _)
  | ObjT (r, _)
  | InstanceT (r, _, _, _)
  | AnyObjT r
  | FunT (r, _, _, _)
  | AnyFunT r
  | NumT (r, (Literal _ | Truthy)) -> EmptyT r

  | ClassT t -> EmptyT (reason_of_t t)

  (* unknown boolies become falsy *)
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; VoidT.why reason])
  | BoolT (r, None) -> BoolT (r, Some false)
  | StrT (r, AnyLiteral) -> StrT (r, Falsy)
  | NumT (r, AnyLiteral) -> NumT (r, Falsy)

  (* things that don't track truthiness pass through *)
  | t -> t

and filter_maybe = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; VoidT.why reason])
  | MixedT r -> UnionT (r, [NullT.why r; VoidT.why r])
  | NullT r -> NullT r
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_maybe = function
  | MaybeT t -> t
  | OptionalT t -> filter_not_maybe t
  | NullT r | VoidT r -> EmptyT r
  | t -> t

and filter_null = function
  | OptionalT (MaybeT t)
  | MaybeT t -> NullT.why (reason_of_t t)
  | NullT r -> NullT r
  | MixedT r -> NullT.why r
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_null = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [VoidT.why reason; t])
  | OptionalT t -> OptionalT (filter_not_null t)
  | UnionT (r, ts) -> recurse_into_union filter_not_null (r, ts)
  | NullT r -> EmptyT r
  | t -> t

and filter_undefined = function
  | MaybeT t -> VoidT.why (reason_of_t t)
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | MixedT r -> VoidT.why r
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_undefined = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; t])
  | OptionalT t -> filter_not_undefined t
  | UnionT (r, ts) -> recurse_into_union filter_not_undefined (r, ts)
  | VoidT r -> EmptyT r
  | t -> t

and filter_true = function
  | BoolT (r, Some true)
  | BoolT (r, None) -> BoolT (r, Some true)
  | MixedT r -> BoolT (replace_reason "boolean" r, Some true)
  | t -> EmptyT (reason_of_t t)

and filter_not_true = function
  | BoolT (r, Some true) -> EmptyT r
  | BoolT (r, None) -> BoolT (r, Some false)
  | t -> t

and filter_false = function
  | BoolT (r, Some false)
  | BoolT (r, None) -> BoolT (r, Some false)
  | MixedT r -> BoolT (replace_reason "boolean" r, Some false)
  | t -> EmptyT (reason_of_t t)

and filter_not_false = function
  | BoolT (r, Some false) -> EmptyT r
  | BoolT (r, None) -> BoolT (r, Some true)
  | t -> t

(* filter out undefined from a type *)
and filter_optional cx ?trace reason opt_t =
  mk_tvar_where cx reason (fun t ->
    flow_opt cx ?trace (opt_t, OptionalT(t))
  )

and predicate cx trace t (l,p) = match (l,p) with

  (************************)
  (* deconstruction of && *)
  (************************)

  | (_, AndP(p1,p2)) ->
    let reason = mk_reason "and" (loc_of_predicate p1) in
    let tvar = mk_tvar cx reason in
    rec_flow cx trace (l,PredicateT(p1,tvar));
    rec_flow cx trace (tvar,PredicateT(p2,t))

  (************************)
  (* deconstruction of || *)
  (************************)

  | (_, OrP(p1,p2)) ->
    rec_flow cx trace (l,PredicateT(p1,t));
    rec_flow cx trace (l,PredicateT(p2,t))

  (*********************************)
  (* deconstruction of binary test *)
  (*********************************)

  (* when left is evaluated, store it and evaluate right *)
  | (_, LeftP(b, r)) ->
    rec_flow cx trace (r, PredicateT(RightP(b, l), t))
  | (_, NotP(LeftP(b, r))) ->
    rec_flow cx trace (r, PredicateT(NotP(RightP(b, l)), t))

  (* when right is evaluated, call appropriate handler *)
  | (r, RightP(b, l)) ->
    binary_predicate cx trace true b l r t
  | (r, NotP(RightP(b, l))) ->
    binary_predicate cx trace false b l r t

  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)

  | (MixedT r, BoolP) ->
    rec_flow cx trace (BoolT.why r, t)

  | (_, BoolP) ->
    filter cx trace t l is_bool

  | (_, NotP(BoolP)) ->
    filter cx trace t l (not_ is_bool)

  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)

  | (MixedT r, StrP) ->
    rec_flow cx trace (StrT.why r, t)

  | (_, StrP) ->
    filter cx trace t l is_string

  | (_, NotP(StrP)) ->
    filter cx trace t l (not_ is_string)

  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)

  | (MixedT r, NumP) ->
    rec_flow cx trace (NumT.why r, t)

  | (_, NumP) ->
    filter cx trace t l is_number

  | (_, NotP(NumP)) ->
    filter cx trace t l (not_ is_number)

  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)

  | (MixedT r, FunP) ->
    rec_flow cx trace (AnyFunT (replace_reason "function" r), t)

  | (_, FunP) ->
    filter cx trace t l is_function

  | (_, NotP(FunP)) ->
    filter cx trace t l (not_ is_function)

  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)

  | (MixedT r, ObjP) ->
    let obj = AnyObjT (replace_reason "object" r) in
    let union = create_union [NullT.why r; obj] in
    rec_flow cx trace (union, t)

  | (_, ObjP) ->
    filter cx trace t l is_object

  | (_, NotP(ObjP)) ->
    filter cx trace t l (not_ is_object)

  (*******************)
  (* Array.isArray _ *)
  (*******************)

  | (MixedT r, ArrP) ->
    let filtered_l = ArrT (replace_reason "array" r, AnyT.why r, []) in
    rec_flow cx trace (filtered_l, t)

  | (_, ArrP) ->
    filter cx trace t l is_array

  | (_, NotP(ArrP)) ->
    filter cx trace t l (not_ is_array)

  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)

  | (_, VoidP) ->
    rec_flow cx trace (filter_undefined l, t)

  | (_, NotP(VoidP)) ->
    rec_flow cx trace (filter_not_undefined l, t)

  (********)
  (* null *)
  (********)

  | (_, NullP) ->
    rec_flow cx trace (filter_null l, t)

  | (_, NotP(NullP)) ->
    rec_flow cx trace (filter_not_null l, t)

  (*********)
  (* maybe *)
  (*********)

  | (_, MaybeP) ->
    rec_flow cx trace (filter_maybe l, t)

  | (_, NotP(MaybeP)) ->
    rec_flow cx trace (filter_not_maybe l, t)

  (********)
  (* true *)
  (********)

  | (_, TrueP) ->
    rec_flow cx trace (filter_true l, t)

  | (_, NotP(TrueP)) ->
    rec_flow cx trace (filter_not_true l, t)

  (*********)
  (* false *)
  (*********)

  | (_, FalseP) ->
    rec_flow cx trace (filter_false l, t)

  | (_, NotP(FalseP)) ->
    rec_flow cx trace (filter_not_false l, t)

  (************************)
  (* truthyness *)
  (************************)

  | (_, ExistsP) ->
    rec_flow cx trace (filter_exists l, t)

  | (_, NotP(ExistsP)) ->
    rec_flow cx trace (filter_not_exists l, t)

  (* unreachable *)
  | (_, NotP (NotP _))
  | (_, NotP (AndP _))
  | (_, NotP (OrP _)) ->
    assert_false (spf "Unexpected predicate %s" (string_of_predicate p))

and binary_predicate cx trace sense test left right result =
  let handler =
    match test with
    | Instanceof -> instanceof_test
    | SentinelProp key -> sentinel_prop_test key
  in
  handler cx trace result (sense, left, right)

and instanceof_test cx trace result = function
  (** instanceof on an ArrT is a special case since we treat ArrT as its own
      type, rather than an InstanceT of the Array builtin class. So, we resolve
      the ArrT to an InstanceT of Array, and redo the instanceof check. We do
      it at this stage instead of simply converting (ArrT, InstanceofP c)
      to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
      first. *)
  | (true, (ArrT (reason, elemt, _) as arr), ClassT(InstanceT _ as a)) ->

    let right = ClassT(ExtendsT([], arr, a)) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    rec_flow cx trace (arrt, PredicateT(LeftP(Instanceof, right), result))

  | (false, (ArrT (reason, elemt, _) as arr), ClassT(InstanceT _ as a)) ->

    let right = ClassT(ExtendsT([], arr, a)) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    rec_flow cx trace (arrt, PredicateT(NotP(LeftP(Instanceof, right)), result))

  (** An object is considered `instanceof` a function F when it is constructed
      by F. Note that this is incomplete with respect to the runtime semantics,
      where instanceof is transitive: if F.prototype `instanceof` G, then the
      object is `instanceof` G. There is nothing fundamentally difficult in
      modeling the complete semantics, but we haven't found a need to do it. **)
  | (true, (ObjT (_,{proto_t = proto2; _}) as obj), FunT (_,_,proto1,_))
      when proto1 = proto2 ->

    rec_flow cx trace (obj, result)

  (** Suppose that we have an instance x of class C, and we check whether x is
      `instanceof` class A. To decide what the appropriate refinement for x
      should be, we need to decide whether C extends A, choosing either C or A
      based on the result. Thus, we generate a constraint to decide whether C
      extends A (while remembering C), which may recursively generate further
      constraints to decide super(C) extends A, and so on, until we hit the root
      class. (As a technical tool, we use Extends(_, _) to perform this
      recursion; it is also used elsewhere for running similar recursive
      subclass decisions.) **)
  | (true, (InstanceT _ as c), ClassT(InstanceT _ as a)) ->

    predicate cx trace result
      (ClassT(ExtendsT([], c, a)), RightP(Instanceof, c))

  (** If C is a subclass of A, then don't refine the type of x. Otherwise,
      refine the type of x to A. (In general, the type of x should be refined to
      C & A, but that's hard to compute.) **)
  | (true, InstanceT (_,_,super_c,instance_c),
     (ClassT(ExtendsT(_, c, InstanceT (_,_,_,instance_a))) as right))
    -> (* TODO: intersection *)

    if instance_a.class_id = instance_c.class_id
    then rec_flow cx trace (c, result)
    else
      (** Recursively check whether super(C) extends A, with enough context. **)
      rec_flow cx trace (super_c, PredicateT(LeftP(Instanceof, right), result))

  | (true, MixedT _, ClassT(ExtendsT (_, _, a)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow cx trace (a, result)

  (** Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
  | (true, _, _) ->
    ()

  | (false, ObjT (_,{proto_t = proto2; _}), FunT (_,_,proto1,_))
      when proto1 = proto2 ->
    ()

  (** Like above, now suppose that we have an instance x of class C, and we
      check whether x is _not_ `instanceof` class A. To decide what the
      appropriate refinement for x should be, we need to decide whether C
      extends A, choosing either nothing or C based on the result. **)
  | (false, (InstanceT _ as c), ClassT(InstanceT _ as a)) ->

    predicate cx trace result
      (ClassT(ExtendsT([], c, a)), NotP(RightP(Instanceof, c)))

  (** If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
  | (false, InstanceT (_,_,super_c,instance_c),
     (ClassT(ExtendsT(_, c, InstanceT (_,_,_,instance_a))) as right))
    ->

    if instance_a.class_id = instance_c.class_id
    then ()
    else rec_flow cx trace
      (super_c, PredicateT(NotP(LeftP(Instanceof, right)), result))

  | (false, MixedT _, ClassT(ExtendsT(_, c, _)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow cx trace (c, result)

  (** Don't refine the type when any other `instanceof` check fails. **)
  | (false, left, _) ->
    rec_flow cx trace (left, result)

and sentinel_prop_test key cx trace result = function
  (** Evaluate a refinement predicate of the form

      obj.key eq value

      where eq is === or !==.

      * key is key
      * (sense, obj, value) are the sense of the test, obj and value as above,
      respectively.

      As with other predicate filters, the goal is to statically determine when
      the predicate is definitely satisfied and when it is definitely
      unsatisfied, and narrow the possible types of obj under those conditions,
      while not narrowing in all other cases.

      In this case, the predicate is definitely satisfied (respectively,
      definitely unsatisfied) when the type of the key property in the type obj
      can be statically verified as having (respectively, not having) value as
      its only inhabitant.

      When satisfied, type obj flows to the recipient type result (in other
      words, we allow all such types in the refined type for obj).

      Otherwise, nothing flows to type result (in other words, we don't allow
      any such type in the refined type for obj).

      Overall the filtering process is somewhat tricky to understand. Refer to
      the predicate function and its callers to understand how the context is
      set up so that filtering ultimately only depends on what flows to
      result. **)
  (* obj.key ===/!== string value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), StrT (_, Literal value)) ->
      (match read_prop_opt cx props_tmap key with
        | Some (SingletonStrT (_, v))
        | Some (StrT (_, Literal v)) when (value = v) != sense ->
            (* provably unreachable, so prune *)
            ()
        | _ ->
            (* not enough info to refine: either the property exists but is
               something else that we cannot use as a refinement, or the
               property doesn't exist at all *)
            rec_flow cx trace (obj, result)
      )

  (* obj.key ===/!== number value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), NumT (_, Literal (value, _))) ->
      (match read_prop_opt cx props_tmap key with
        | Some (SingletonNumT (_, (v, _)))
        | Some (NumT (_, Literal (v, _))) when (value = v) != sense ->
            (* provably unreachable, so prune *)
            ()
        | _ ->
            (* not enough info to refine: either the property exists but is
               something else that we cannot use as a refinement, or the
               property doesn't exist at all *)
            rec_flow cx trace (obj, result)
      )

  (* TODO: refine based on boolean values (currently those go to bool_test) *)

  | (_, obj, _) -> (* not enough info to refine *)
    rec_flow cx trace (obj, result)

(***********************)
(* bounds manipulation *)
(***********************)

(** The following general considerations apply when manipulating bounds.

    1. All type variables start out as roots, but some of them eventually become
    goto nodes. As such, bounds of roots may contain goto nodes. However, we
    never perform operations directly on goto nodes; instead, we perform those
    operations on their roots. It is tempting to replace goto nodes proactively
    with their roots to avoid this issue, but doing so may be expensive, whereas
    the union-find data structure amortizes the cost of looking up roots.

    2. Another issue is that while the bounds of a type variable start out
    empty, and in particular do not contain the type variable itself, eventually
    other type variables in the bounds may be unified with the type variable. We
    do not remove these type variables proactively, but instead filter them out
    when considering the bounds. In the future we might consider amortizing the
    cost of this filtering.

    3. When roots are resolved, they act like the corresponding concrete
    types. We maintain the invariant that whenever lower bounds or upper bounds
    contain resolved roots, they also contain the corresponding concrete types.

    4. When roots are unresolved (they have lower bounds and upper bounds,
    possibly consisting of concrete types as well as type variables), we
    maintain the invarant that every lower bound has already been propagated to
    every upper bound. We also maintain the invariant that the bounds are
    transitively closed modulo equivalence: for every type variable in the
    bounds, all the bounds of its root are also included.

**)

(* for each l in ls: l => u *)
and flows_to_t cx trace ls u =
  ls |> TypeMap.iter (fun l trace_l ->
    join_flow cx [trace_l;trace] (l,u)
  )

(* for each u in us: l => u *)
and flows_from_t cx trace l us =
  us |> TypeMap.iter (fun u trace_u ->
    join_flow cx [trace;trace_u] (l,u)
  )

(* for each l in ls, u in us: l => u *)
and flows_across cx trace ls us =
  ls |> TypeMap.iter (fun l trace_l ->
    us |> TypeMap.iter (fun u trace_u ->
      join_flow cx [trace_l;trace;trace_u] (l,u)
    )
  )

(* bounds.upper += u *)
and add_upper u trace bounds =
  bounds.upper <- TypeMap.add u trace bounds.upper

(* bounds.lower += l *)
and add_lower l trace bounds =
  bounds.lower <- TypeMap.add l trace bounds.lower

(* Helper for functions that follow. *)
(* Given a map of bindings from tvars to traces, a tvar to skip, and an `each`
   function taking a tvar and its associated trace, apply `each` to all
   unresolved root constraints reached from the bound tvars, except those of
   skip_tvar. (Typically skip_tvar is a tvar that will be processed separately,
   so we don't want to redo that work. We also don't want to consider any tvar
   that has already been resolved, because the resolved type will be processed
   separately, too, as part of the bounds of skip_tvar. **)
and iter_with_filter cx bindings skip_id each =
  bindings |> IMap.iter (fun id trace ->
    match find_constraints cx id with
    | root_id, Unresolved bounds when root_id <> skip_id ->
        each (root_id, bounds) trace
    | _ ->
        ()
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
*)
(** When going through bounds1.lowertvars, filter out id1. **)
(** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_to_t cx trace ?(opt=false) (id1, bounds1) t2 =
  if not opt then add_upper t2 trace bounds1;
  iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) trace_l ->
    add_upper t2 (Trace.concat_trace[trace_l;trace]) bounds
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
*)
(** When going through bounds2.uppertvars, filter out id2. **)
(** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_from_t cx trace ?(opt=false) t1 (id2, bounds2) =
  if not opt then add_lower t1 trace bounds2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) trace_u ->
    add_lower t1 (Trace.concat_trace[trace;trace_u]) bounds
  )

(* for each id' in id + bounds.lowertvars:
   id'.bounds.upper += us
*)
and edges_to_ts cx trace ?(opt=false) (id, bounds) us =
  us |> TypeMap.iter (fun u trace_u ->
    edges_to_t cx (Trace.concat_trace[trace;trace_u]) ~opt (id, bounds) u
  )

(* for each id' in id + bounds.uppertvars:
   id'.bounds.lower += ls
*)
and edges_from_ts cx trace ?(opt=false) ls (id, bounds) =
  ls |> TypeMap.iter (fun l trace_l ->
    edges_from_t cx (Trace.concat_trace[trace_l;trace]) ~opt l (id, bounds)
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
   for each l in bounds1.lower: l => t2
*)
(** As an invariant, bounds1.lower should already contain id.bounds.lower for
    each id in bounds1.lowertvars. **)
and edges_and_flows_to_t cx trace ?(opt=false) (id1, bounds1) t2 =
  if not (TypeMap.mem t2 bounds1.upper) then (
    edges_to_t cx trace ~opt (id1, bounds1) t2;
    flows_to_t cx trace bounds1.lower t2
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
   for each u in bounds2.upper: t1 => u
*)
(** As an invariant, bounds2.upper should already contain id.bounds.upper for
    each id in bounds2.uppertvars. **)
and edges_and_flows_from_t cx trace ?(opt=false) t1 (id2, bounds2) =
  if not (TypeMap.mem t1 bounds2.lower) then (
    edges_from_t cx trace ~opt t1 (id2, bounds2);
    flows_from_t cx trace t1 bounds2.upper
  )

(* bounds.uppertvars += id *)
and add_uppertvar id trace bounds =
  bounds.uppertvars <- IMap.add id trace bounds.uppertvars

(* bounds.lowertvars += id *)
and add_lowertvar id trace bounds =
  bounds.lowertvars <- IMap.add id trace bounds.lowertvars

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.uppertvars += id2
*)
(** When going through bounds1.lowertvars, filter out id1. **)
(** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_to_tvar cx trace ?(opt=false) (id1, bounds1) id2 =
  if not opt then add_uppertvar id2 trace bounds1;
  iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) trace_l ->
    add_uppertvar id2 (Trace.concat_trace[trace_l;trace]) bounds
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lowertvars += id1
*)
(** When going through bounds2.uppertvars, filter out id2. **)
(** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_from_tvar cx trace ?(opt=false) id1 (id2, bounds2) =
  if not opt then add_lowertvar id1 trace bounds2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) trace_u ->
    add_lowertvar id1 (Trace.concat_trace[trace;trace_u]) bounds
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += bounds2.upper
   id.bounds.uppertvars += id2
   id.bounds.uppertvars += bounds2.uppertvars
*)
and add_upper_edges cx trace ?(opt=false) (id1, bounds1) (id2, bounds2) =
  edges_to_ts cx trace ~opt (id1, bounds1) bounds2.upper;
  edges_to_tvar cx trace ~opt (id1, bounds1) id2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (tvar, _) trace_u ->
    let trace = Trace.concat_trace [trace;trace_u] in
    edges_to_tvar cx trace ~opt (id1, bounds1) tvar
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += bounds1.lower
   id.bounds.lowertvars += id1
   id.bounds.lowertvars += bounds1.lowertvars
*)
and add_lower_edges cx trace ?(opt=false) (id1, bounds1) (id2, bounds2) =
  edges_from_ts cx trace ~opt bounds1.lower (id2, bounds2);
  edges_from_tvar cx trace ~opt id1 (id2, bounds2);
  iter_with_filter cx bounds1.lowertvars id1 (fun (tvar, _) trace_l ->
    let trace = Trace.concat_trace [trace_l;trace] in
    edges_from_tvar cx trace ~opt tvar (id2, bounds2)
  )

(***************)
(* unification *)
(***************)

(* Chain a root to another root. If both roots are unresolved, this amounts to
   copying over the bounds of one root to another, and adding all the
   connections necessary when two non-unifiers flow to each other. If one or
   both of the roots are resolved, they effectively act like the corresponding
   concrete types. *)
and goto cx trace ?(flag=false) (id1, root1) (id2, root2) =
  replace_node cx id1 (Goto id2);
  (match root1.constraints, root2.constraints with

  | Unresolved bounds1, Unresolved bounds2 ->
    let cond1 = not_linked (id1, bounds1) (id2, bounds2) in
    let cond2 = not_linked (id2, bounds2) (id1, bounds1) in
    if cond1 then
      flows_across cx trace bounds1.lower bounds2.upper;
    if cond2 then
      flows_across cx trace bounds2.lower bounds1.upper;
    if cond1 then (
      add_upper_edges cx trace ~opt:true (id1, bounds1) (id2, bounds2);
      add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
    );
    if cond2 then (
      add_upper_edges cx trace (id2, bounds2) (id1, bounds1);
      add_lower_edges cx trace ~opt:true (id2, bounds2) (id1, bounds1);
    );

  | Unresolved bounds1, Resolved t2 ->
    edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) t2;
    edges_and_flows_from_t cx trace ~opt:true t2 (id1, bounds1);

  | Resolved t1, Unresolved bounds2 ->
    replace_node cx id2 (Root { root2 with constraints = Resolved t1 });
    edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) t1;
    edges_and_flows_from_t cx trace ~opt:true t1 (id2, bounds2);

  | Resolved t1, Resolved t2 ->
    rec_unify cx trace t1 t2;
  )

(* Unify two type variables. This involves finding their roots, and making one
   point to the other. Ranks are used to keep chains short. *)
and merge_ids cx trace id1 id2 =
  let (id1, root1), (id2, root2) = find_root cx id1, find_root cx id2 in
  if id1 = id2 then ()
  else if root1.rank < root2.rank then goto cx trace (id1, root1) (id2, root2)
  else if root2.rank < root1.rank then goto cx trace (id2, root2) (id1, root1)
  else (
    replace_node cx id2 (Root { root2 with rank = root1.rank+1; });
    goto cx trace (id1, root1) (id2, root2);
  )

(* Resolve a type variable to a type. This involves finding its root, and
   resolving to that type. *)
and resolve_id cx trace id t =
  let id, root = find_root cx id in
  match root.constraints with
  | Unresolved bounds ->
    replace_node cx id (Root { root with constraints = Resolved t });
    edges_and_flows_to_t cx trace ~opt:true (id, bounds) t;
    edges_and_flows_from_t cx trace ~opt:true t (id, bounds);

  | Resolved t_ ->
    rec_unify cx trace t_ t

(******************)

(* Unification of two types *)

(* It is potentially dangerous to unify a type variable to a type that "forgets"
   constraints during propagation. These types are "any-like": the canonical
   example of such a type is any. Overall, we want unification to be a sound
   "optimization," in the sense that replacing bidirectional flows with
   unification should not miss errors. But consider a scenario where we have a
   type variable with two incoming flows, string and any, and two outgoing
   flows, number and any. If we replace the flows from/to any with an
   unification with any, we will miss the string/number incompatibility error.

   However, unifying with any-like types is sometimes desirable /
   intentional. Thus, we limit the set of types on which unification is banned
   to just LowerBoundT and UpperBoundT, which are internal types.
*)
and ok_unify = function
  | LowerBoundT _ | UpperBoundT _ -> false
  | _ -> true

and rec_unify cx trace t1 t2 =
  if t1 = t2 then () else (
  (* Expect that t1 and t2 are def types. *)
  expect_def t1; expect_def t2;
  not_expect_bound t1; not_expect_bound t2;

  match (t1,t2) with
  | (OpenT (_,id1), OpenT (_,id2)) ->
    merge_ids cx trace id1 id2

  | (OpenT (_, id), t) | (t, OpenT (_, id)) when ok_unify t ->
    resolve_id cx trace id t

  | (ArrT (_, t1, ts1), ArrT (_, t2, ts2)) ->
    array_unify cx trace (ts1,t1, ts2,t2)

  | (ObjT (reason1, {props_tmap = flds1; dict_t = dict1; _}),
     ObjT (reason2, {props_tmap = flds2; dict_t = dict2; _})) ->

    (* ensure the keys and values are compatible with each other. *)
    begin match dict1, dict2 with
    | Some {key = k1; value = v1; _}, Some {key = k2; value = v2; _} ->
        dictionary cx trace k1 v1 dict2;
        dictionary cx trace k2 v2 dict1
    | Some _, None ->
        let reason1 = replace_reason "some property" reason1 in
        add_warning cx [reason1, "Property not found in"; reason2, ""] ~trace
    | None, Some _ ->
        let reason2 = replace_reason "some property" reason2 in
        add_warning cx [reason2, "Property not found in"; reason1, ""] ~trace
    | None, None -> ()
    end;

    let pmap1, pmap2 = find_props cx flds1, find_props cx flds2 in
    SMap.merge (fun x t1 t2 ->
      if not (is_internal_name x)
      then (match t1, t2 with
      | Some t1, Some t2 -> rec_unify cx trace t1 t2
      | Some t1, None ->
          (* x exists in obj1 but not obj2; if obj2 is a dictionary make sure
             t1 is allowed, otherwise error *)
          flow_prop_to_dict cx trace x t1 dict2 reason1 reason2
      | None, Some t2 ->
          (* x exists in obj2 but not obj1; if obj1 is a dictionary make sure
             t2 is allowed, otherwise error *)
          flow_prop_to_dict cx trace x t2 dict1 reason2 reason1
      | None, None -> ());
      None
    ) pmap1 pmap2 |> ignore

  | _ ->
    naive_unify cx trace t1 t2
  )

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)

and naive_unify cx trace t1 t2 =
  rec_flow cx trace (t1,t2); rec_flow cx trace (t2,t1)

and flow_prop_to_dict cx trace k v dict prop_reason dict_reason =
  match dict with
  | Some {key; value; _} ->
    dictionary cx trace (string_key k prop_reason) v dict
  | None ->
    let prop_reason = replace_reason (spf "property `%s`" k) prop_reason in
    add_warning cx [
      prop_reason, "Property not found in";
      dict_reason, ""
    ] ~trace

(* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
and flow_to_mutable_child cx trace fresh t1 t2 =
  if fresh
  then rec_flow cx trace (t1, t2)
  else rec_unify cx trace t1 t2

and array_flow cx trace lit = function
  | ([],e1, _,e2) -> (* general element1 = general element2 *)
    flow_to_mutable_child cx trace lit e1 e2

  | (_,e1, [],e2) -> (* specific element1 < general element2 *)
    rec_flow cx trace (e1, e2)

  | ([t1],_, t2::_,_) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2;
    array_flow cx trace lit (ts1,e1, ts2,e2)

(* array helper *)
and array_unify cx trace = function
  | ([],e1, [],e2) -> (* general element1 = general element2 *)
    rec_unify cx trace e1 e2

  | (ts1,_, [],e2)
  | ([],e2, ts1,_) -> (* specific element1 < general element2 *)
    List.iter (fun t1 ->
      rec_unify cx trace t1 e2;
    ) ts1

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    rec_unify cx trace t1 t2;
    array_unify cx trace (ts1,e1, ts2,e2)


(*******************************************************************)
(* subtyping a sequence of arguments with a sequence of parameters *)
(*******************************************************************)

and multiflow cx trace reason_op (arglist, parlist) =
  multiflow_partial cx trace ~strict:reason_op (arglist, parlist) |> ignore

(* Match arguments to parameters, taking an optional parameter 'strict':
   - when strict=None, missing arguments (and unmatched parameters) are
   expected. This is used, e.g., when processing Function.prototype.bind.
   - when strict=Some reason_op, missing arguments are treated as undefined,
   whose types are given a reason derived from reason_op when passing to
   unmatched parameters.
*)
and multiflow_partial cx trace ?strict = function
  (* Do not complain on too many arguments.
     This pattern is ubiqutous and causes a lot of noise when complained about.
     Note: optional/rest parameters do not provide a workaround in this case.
  *)
  | (_,[]) -> []

  | ([RestT tin],[RestT tout]) ->
    rec_flow cx trace (tin,tout);
    []

  | ([RestT tin],tout::touts) ->
    rec_flow cx trace (tin,tout);
    multiflow_partial cx trace ?strict ([RestT tin], touts)

  | (tin::tins,[RestT tout]) ->
    rec_flow cx trace (tin,tout);
    multiflow_partial cx trace ?strict (tins, [RestT tout])

  | ([],[RestT tout]) -> [RestT tout]

  | ([],tout::touts) ->
    (match strict with
    | Some reason_op ->
        let reason = replace_reason
          "undefined (too few arguments, expected default/rest parameters)"
          reason_op
        in
        rec_flow cx trace (VoidT reason, tout)
    | None -> ()
    );
    multiflow_partial cx trace ?strict ([], touts)

  | (tin::tins,tout::touts) ->
    (* flow `tin` (param) to `tout` (argument). normally, `tin` is passed
       through a `ReposLowerT` to make sure that the concrete type points at
       the param's location. however, if `tin` is an implicit type argument
       (e.g. the `x` in `function foo<T>(x: T)`), then don't reposition it
       because implicit type args have no explicit location to point at.
       instead, let it flow through transparently, so that we point at the
       place that constrained the type arg. this is pretty hacky. *)
    let tout =
      if ImplicitTypeArgument.has_typeparam_prefix (desc_of_t tin) then tout
      else ReposLowerT (reason_of_t tin, tout)
    in
    rec_flow cx trace (tin, tout);
    multiflow_partial cx trace ?strict (tins,touts)

and dictionary cx trace keyt valuet = function
  | None -> ()
  | Some { key; value; _ } ->
      rec_flow cx trace (keyt, key);
      begin match keyt with
      | StrT (_, Literal str) ->
        if not (is_dictionary_exempt str)
        then rec_flow cx trace (valuet, value)
      | _ ->
        rec_flow cx trace (valuet, value)
      end

and string_key s reason =
  let key_reason =
    replace_reason (spf "property `%s` is a string" s) reason in
  StrT (key_reason, Literal s)

(* builtins, contd. *)

and get_builtin cx x reason =
  mk_tvar_where cx reason (fun builtin ->
    flow_opt cx (builtins cx, GetPropT(reason, (reason, x), builtin))
  )

and lookup_builtin cx x reason strict builtin =
  flow_opt cx (builtins cx, LookupT(reason,strict,[],x,builtin))

and get_builtin_typeapp cx reason x ts =
  TypeAppT(get_builtin cx x reason, ts)

(* Specialize a polymorphic class, make an instance of the specialized class. *)
and mk_typeapp_instance cx reason ?(cache=false) c ts =
  let t = mk_tvar cx reason in
  flow_opt cx (c, SpecializeT(reason,cache,ts,t));
  mk_instance cx (reason_of_t c) t

(* NOTE: the for_type flag is true when expecting a type (e.g., when processing
   an annotation), and false when expecting a runtime value (e.g., when
   processing an extends). *)
and mk_instance cx instance_reason ?(for_type=true) c =
  if for_type then
    (* Make an annotation. Part of this operation is similar to making
       a runtime value type (see below), except that for annotations,
       we must ensure that values flowing into the annotated site do
       not interact with downstream uses of it. *)
    let source_t = mk_tvar_where cx instance_reason (fun t ->
      (* this part is similar to making a runtime value *)
      flow_opt cx (c, TypeT(instance_reason,t))
    ) in
    let sink_t = mk_tvar_where cx instance_reason (fun t ->
      (* when source_t is concrete, unify with sink_t *)
      flow_opt cx (source_t, UnifyT(source_t, t))
    ) in
    AnnotT (sink_t, source_t)
  else
    mk_tvar_derivable_where cx instance_reason (fun t ->
      flow_opt cx (c, ClassT(t))
    )

(* We want to match against some t, which may either be a concrete
   type, or a tvar that will be concretized by a single incoming
   lower bound (for instance, a tvar representing an externally
   defined type - the concrete definition will appear as a lower
   bound to that tvar at merge time).

   Given such a t, `become` will return
   * t itself, if it is concrete already, or
   * a new tvar which will behave exactly like the first concrete
    lower bound that flows into t, whenever that happens.
 *)
and become cx ?trace r t = match t with
  | OpenT _ ->
    (* if t is not concrete, create a new tvar within a BecomeT
       operation and add it to t's uppertvars. When a concrete
       lower bound flows to t, it will also flow to this BecomeT,
       and the rule for BecomeT in __flow will unify the new tvar
       with it. *)
    mk_tvar_derivable_where cx r (fun tvar ->
      flow_opt cx ?trace (t, BecomeT (r, tvar)))
  | _ ->
    (* optimization: if t is already concrete, become t immediately :) *)
    t

and reposition cx ?trace reason t =
  match t with
  | OpenT (r, id) ->
    let constraints = find_graph cx id in
    begin match constraints with
    | Resolved t -> mod_reason_of_t (repos_reason (loc_of_reason reason)) t
    | _ ->
      let mk_tvar_where = if is_derivable_reason r
        then mk_tvar_derivable_where
        else mk_tvar_where
      in
      mk_tvar_where cx reason (fun tvar ->
        flow_opt cx ?trace (t, ReposLowerT (reason, tvar))
      )
    end
  | _ -> mod_reason_of_t (repos_reason (loc_of_reason reason)) t

(* given the type of a value v, return the type term
   representing the `typeof v` annotation expression *)
and mk_typeof_annotation cx ?trace valtype =
  let r = prefix_reason "typeof " (reason_of_t valtype) in
  become cx ?trace r valtype

and get_builtin_type cx reason x =
  let t = get_builtin cx x reason in
  mk_instance cx reason t

and instantiate_poly_t cx t types =
  match t with
  | PolyT (type_params, t_) -> (
    try
      let subst_map = List.fold_left2 (fun acc {name; _} type_ ->
        SMap.add name type_ acc
      ) SMap.empty type_params types in
      subst cx subst_map t_
    with _ ->
      prerr_endline "Instantiating poly type failed";
      t
  )
  | _ ->
    assert false

and instantiate_type t =
  match t with
  | ClassT t -> t
  | _ -> AnyT.why (reason_of_t t) (* ideally, assert false *)

and static_method_call cx name reason reason_prop m argts =
  let cls = get_builtin cx name reason in
  mk_tvar_where cx reason (fun tvar ->
    let funtype = mk_methodtype cls argts tvar in
    flow_opt cx (cls, MethodT(reason, (reason_prop, m), funtype))
  )

(** TODO: this should rather be moved close to ground_type_impl/resolve_type
    etc. but Ocaml name resolution rules make that require a lot more moving
    code around. **)
and resolve_builtin_class cx = function
  | BoolT (reason, _) ->
    let bool_t = get_builtin_type cx reason "Boolean" in
    resolve_type cx bool_t
  | NumT (reason, _) ->
    let num_t = get_builtin_type cx reason "Number" in
    resolve_type cx num_t
  | StrT (reason, _) ->
    let string_t = get_builtin_type cx reason "String" in
    resolve_type cx string_t
  | ArrT (reason, t, _) ->
    let array_t = get_builtin cx "Array" reason in
    let array_t = resolve_type cx array_t in
    let array_t = instantiate_poly_t cx array_t [t] in
    instantiate_type array_t
  | t ->
    t

and set_builtin cx x t =
  let reason = builtin_reason x in
  flow_opt cx (builtins cx, SetPropT(reason, (reason, x), t))

(* Wrapper functions around __flow that manage traces. Use these functions for
   all recursive calls in the implementation of __flow. *)

(* Call __flow while concatenating traces. Typically this is used in code that
   propagates bounds across type variables, where nothing interesting is going
   on other than concatenating subtraces to make longer traces to describe
   transitive data flows *)
and join_flow cx ts (t1, t2) =
  __flow cx (t1, t2) (Trace.concat_trace ts)

(* Call __flow while embedding traces. Typically this is used in code that
   simplifies a constraint to generate subconstraints: the current trace is
   "pushed" when recursing into the subconstraints, so that when we finally hit
   an error and walk back, we can know why the particular constraints that
   caused the immediate error were generated. *)
and rec_flow cx trace (t1, t2) =
  __flow cx (t1, t2) (Trace.rec_trace t1 t2 trace)

(* Ideally this function would not be required: either we call `flow` from
   outside without a trace (see below), or we call one of the functions above
   with a trace. However, there are some functions that need to call __flow,
   which are themselves called both from outside and inside (with or without
   traces), so they call this function instead. *)
and flow_opt cx ?trace (t1, t2) =
  let trace = match trace with
    | None -> Trace.unit_trace t1 t2
    | Some trace -> Trace.rec_trace t1 t2 trace in
  __flow cx (t1, t2) trace

(* Externally visible function for subtyping. *)
(* Calls internal entry point and traps runaway recursion. *)
and flow cx (lower, upper) =
  try
    flow_opt cx (lower, upper)
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let msg = "*** Recursion limit exceeded ***" in
    prerr_flow_full_trace cx trace msg lower upper
  | ex ->
    (* rethrow *)
    raise ex

(* Externally visible function for unification. *)
let unify cx t1 t2 =
  rec_unify cx (Trace.unit_trace t1 t2) t1 t2

(********* Garbage collection *********)

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
      -> ()

  | FunT(_, static, prototype, funtype) ->
      gc cx state funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx state);
      gc cx state funtype.return_t;
      gc cx state prototype;
      gc cx state static

  | ObjT(_, objtype) ->
      let id = objtype.props_tmap in
      iter_props cx id (fun _ -> gc cx state);
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
      iter_props cx instance.fields_tmap (fun _ -> gc cx state);
      iter_props cx instance.methods_tmap (fun _ -> gc cx state);
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
      iter_props cx exports_tmap (fun _ -> gc cx state);
      (match cjs_export with
        | Some t -> gc cx state t
        | None -> ()
      )

  | ReposUpperT (_, t) ->
      gc cx state t

  (** use types **)

  | SummarizeT (_, t) ->
      gc cx state t

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
      iter_props cx instance.fields_tmap (fun _ -> gc cx state);
      iter_props cx instance.methods_tmap (fun _ -> gc cx state)

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

  | ImportModuleNsT (_, t) ->
      gc cx state t

  | ImportTypeT (_, t) ->
      gc cx state t

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

and gc_id cx state id =
  let root_id, constraints = find_constraints cx id in (
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
  let constraints = find_graph cx id in
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
    List.iter (gc cx state) ((builtins cx)::(List.map (lookup_module cx) ms));
    cleanup cx state;
  )

let intersect_members cx members =
  match members with
  | [] -> SMap.empty
  | _ ->
      let map = SMap.map (fun x -> [x]) (List.hd members) in
      let map = List.fold_left (fun acc x ->
          SMap.merge (fun name tl t ->
              match (tl, t) with
              | (None, None)      -> None
              | (None, Some _)    -> None
              | (Some _, None)    -> None
              | (Some tl, Some t) -> Some (t :: tl)
            ) acc x
        ) map (List.tl members) in
      SMap.map (List.fold_left (fun acc x ->
          merge_type cx (acc, x)
        ) EmptyT.t) map

(* It's kind of lame that Autocomplete is in this module, but it uses a bunch
 * of internal APIs so for now it's easier to keep it here than to expose those
 * APIs *)
module Autocomplete : sig
  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val map_of_member_result: member_result -> Type.t SMap.t

  val extract_members: Context.t -> Type.t -> member_result

end = struct

  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  let map_of_member_result = function
    | Success map -> map
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType _ ->
        SMap.empty

  let find_props cx fields =
    SMap.filter (fun key _ ->
      (* Filter out keys that start with "$" *)
      not (Str.string_match (Str.regexp "\\$") key 0)
    ) (find_props cx fields)

  (* TODO: Think of a better place to put this *)
  let rec extract_members cx this_t =
    match this_t with
    | MaybeT _ | NullT _ | VoidT _ ->
        FailureMaybeType
    | AnyT _ ->
        FailureAnyType
    | AnyObjT reason ->
        extract_members cx (get_builtin_type cx reason "Object")
    | AnyFunT reason ->
        extract_members cx (get_builtin_type cx reason "Function")
    | AnnotT (source, _) ->
        let source_t = resolve_type cx source in
        extract_members cx source_t
    | InstanceT (reason, _, super,
                {fields_tmap = fields;
                methods_tmap = methods;
                _}) ->
        let fields = find_props cx fields in
        let methods = find_props cx methods in
        let super_t = resolve_type cx super in
        let members = SMap.union fields methods in
        let super_flds = extract_members_as_map cx super_t in
        Success (SMap.union super_flds members)
    | ObjT (_, {props_tmap = flds; proto_t = proto; _}) ->
        let proto_reason = reason_of_t proto in
        let proto_t = resolve_type cx (IntersectionT (proto_reason, [
          proto;
          get_builtin_type cx proto_reason "Object";
        ])) in
        let prot_members = extract_members_as_map cx proto_t in
        let members = find_props cx flds in
        Success (SMap.union prot_members members)
    | TypeAppT (c, ts) ->
        let c = resolve_type cx c in
        let inst_t = instantiate_poly_t cx c ts in
        let inst_t = instantiate_type inst_t in
        extract_members cx inst_t
    | PolyT (type_params, sub_type) ->
        (* TODO: replace type parameters with stable/proper names? *)
        extract_members cx sub_type
    | ClassT (InstanceT (_, static, _, _)) ->
        let static_t = resolve_type cx static in
        extract_members cx static_t
    | FunT (_, static, proto, _) ->
        let static_t = resolve_type cx static in
        let proto_reason = reason_of_t proto in
        let proto_t = resolve_type cx (IntersectionT (proto_reason, [
          proto;
          get_builtin_type cx proto_reason "Function";
        ])) in
        let members = extract_members_as_map cx static_t in
        let prot_members = extract_members_as_map cx proto_t in
        Success (SMap.union prot_members members)
    | IntersectionT (r, ts) ->
        (* Intersection type should autocomplete for every property of every type
        * in the intersection *)
        let ts = List.map (resolve_type cx) ts in
        let members = List.map (extract_members_as_map cx) ts in
        Success (List.fold_left SMap.union SMap.empty members)
    | UnionT (r, ts) ->
        (* Union type should autocomplete for only the properties that are in
        * every type in the intersection *)
        let ts = List.map (resolve_type cx) ts in
        let members = ts
          (* Although we'll ignore the any-ish members of the union *)
          |> List.filter (function AnyT _ | AnyObjT _ | AnyFunT _ -> false | _ -> true)
          |> List.map (extract_members_as_map cx)
          |> intersect_members cx in
        Success members
    | SingletonStrT (reason, _)
    | StrT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "String")
    | SingletonNumT (reason, _)
    | NumT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "Number")
    | SingletonBoolT (reason, _)
    | BoolT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "Boolean")
    | _ ->
        (* TODO: What types could come up here which we need to handle? *)
        FailureUnhandledType this_t

  and extract_members_as_map cx this_t =
    let member_result = extract_members cx this_t in
    map_of_member_result member_result

end

(* Given a type, report missing annotation errors if

   - the given type is a tvar whose id isn't explicitly specified in the given
   skip set, or isn't explicitly marked as derivable, or if

   - the infer flag is true, and such tvars are reachable from the given tvar

   Type variables that are in the skip set are marked in assume_ground as
   depending on `require`d modules. Thus, e.g., when the superclass of an
   exported class is `require`d, we should not insist on an annotation for the
   superclass.
*)
(* need to consider only "def" types *)
let rec assert_ground ?(infer=false) cx skip ids = function
  | BoundT _ -> ()

  (* Type variables that are not forced to be annotated include those that
     are dependent on requires, or whose reasons indicate that they are
     derivable. The latter category includes annotations and builtins. *)
  | OpenT (reason_open, id)
      when (ISet.mem id skip || is_derivable_reason reason_open)
        -> ()

  (* when the infer flag is set, traverse the types reachable from this tvar,
     rather than stopping here and reporting a missing annotation. Note that
     when this function is called recursively on those types, infer will be
     false. *)
  | OpenT (_, id) when infer ->
      assert_ground_id cx skip ids id

  | OpenT (reason_open, id) ->
      unify cx (OpenT (reason_open, id)) AnyT.t;
      let message_list = Errors_js.([
        message_of_reason reason_open;
        message_of_string "Missing annotation"
      ]) in
      add_output cx Errors_js.InferWarning message_list

  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | TaintT _
    ->
      ()

  | FunT (reason, static, prototype, { this_t; params_tlist; return_t; _ }) ->
      let f = assert_ground cx skip ids in
      unify cx static AnyT.t;
      unify cx prototype AnyT.t;
      unify cx this_t AnyT.t;
      List.iter f params_tlist;
      f return_t

  | PolyT (xs,t) ->
      assert_ground cx skip ids t

  | ObjT (reason, { props_tmap = id; proto_t; _ }) ->
      unify cx proto_t AnyT.t;
      iter_props cx id (fun _ -> assert_ground ~infer:true cx skip ids)

  | ArrT (reason, t, ts) ->
      assert_ground cx skip ids t;
      ts |> List.iter (assert_ground cx skip ids)

  | ClassT t -> assert_ground cx skip ids t

  | TypeT (reason, t) -> assert_ground cx skip ids t

  | InstanceT (reason, static, super, instance) ->
      let process_element name t =
        let infer = is_munged_prop_name cx name in
        assert_ground cx skip ids ~infer t
      in
      iter_props cx instance.fields_tmap process_element;
      iter_props cx instance.methods_tmap process_element;
      unify cx static AnyT.t;
      assert_ground cx skip ids super

  | RestT (t) -> assert_ground cx skip ids t

  | OptionalT (t) -> assert_ground cx skip ids t

  | TypeAppT(c,ts) ->
      assert_ground ~infer:true cx skip ids c;
      List.iter (assert_ground cx skip ids) ts

  | MaybeT(t) -> assert_ground cx skip ids t

  | IntersectionT(reason,ts) ->
      List.iter (assert_ground cx skip ids) ts

  | UnionT(reason,ts) ->
      List.iter (assert_ground cx skip ids) ts

  | UpperBoundT(t) ->
      assert_ground cx skip ids t

  | LowerBoundT(t) ->
      assert_ground cx skip ids t

  | AnyObjT _ -> ()
  | AnyFunT _ -> ()

  | ShapeT(t) ->
      assert_ground cx skip ids t
  | DiffT(t1, t2) ->
      assert_ground cx skip ids t1;
      assert_ground cx skip ids t2

  | KeysT(reason,t) ->
      assert_ground cx skip ids t

  | SingletonStrT _ -> ()
  | SingletonNumT _ -> ()
  | SingletonBoolT _ -> ()

  | ModuleT(reason, {exports_tmap; cjs_export}) ->
      iter_props cx exports_tmap
        (fun _ -> assert_ground ~infer:true cx skip ids);
      (match cjs_export with
       | Some(t) -> assert_ground ~infer:true cx skip ids t
       | None -> ()
      )

  | AnnotT (sink_t, source_t) ->
      (* don't ask for an annotation if one is already provided :) *)
      (** TODO: one of the uses of derivable_reason was to mark type variables
          that represented annotations so that they could be ignored. Since we
          can now ignore annotations directly, consider renaming or getting rid
          of derivable entirely. **)
      ()

  | t -> failwith (string_of_reason (reason_of_t t)) (** TODO **)

and assert_ground_id cx skip ids id =
  if not (ISet.mem id !ids)
  then (
    ids := !ids |> ISet.add id;
    match find_graph cx id with
    | Unresolved { lower; lowertvars; _ } ->
        TypeMap.keys lower |> List.iter (assert_ground cx skip ids);
        IMap.keys lowertvars |> List.iter (assert_ground_id cx skip ids);
    | Resolved t ->
        assert_ground cx skip ids t
  )

let enforce_strict cx id =
  (* First, compute a set of ids to be skipped by calling `assume_ground`. After
     the call, skip_ids contains precisely those ids that correspond to
     requires/imports. *)
  let skip_ids = ref ISet.empty in
  SSet.iter (fun r ->
    let tvar = lookup_module cx r in
    assume_ground cx skip_ids tvar
  ) (Context.required cx);

  (* With the computed skip_ids, call `assert_ground` to force annotations while
     walking the graph starting from id. Typically, id corresponds to
     exports. *)
  assert_ground_id cx !skip_ids (ref ISet.empty) id

(****************** signature contexts *********************)

(* Once a context is merged with other contexts it depends on, it is ready to be
   optimized for use by contexts that in turn depend on it. The only interesting
   part of the context for such use is its exports. Thus, we call a context
   optimized for such use a "signature context."

   A signature context should contain only descriptions of its exports. Anything
   that does not contribute to those descriptions are redundant and should be
   discarded: otherwise, we would end up paying significant hidden costs, in
   terms of memory (storing in the heap) and time (serializing / deserializing
   from the heap, garbage collection pauses).

   As it turns out, it is not always possible or even desirable to describe the
   exports in "closed form" as types: instead, the descriptions take the form of
   a (mutually) recursive system of "equations" relating type variables to
   "solved" types, and the types themselves may contain these type variables
   inside. As indicated above, such descriptions are both necessary (due to
   recursive types) and desirable (due to sharing, and for cyclically depending
   contexts, due to the need to deal with the exports of all such contexts at
   once). This form is quite adequate for type checking (indeed, it can be
   thought of as merely a condensed form of usual contexts, which already
   exhibit this "interlinked" behavior).

   Unsurprisingly, since types also make references to some other maps in a
   context (property maps, envs), these references need to be carried around
   as well for the descriptions to be complete.

   We can collect compact and complete descriptions of exports by using a custom
   type visitor that marks relevant things that need to be carried over as it
   walks the context from its exports. Once the walk is done, we can replace the
   corresponding parts of the context (graph, property maps, envs) by their
   reduced forms.
*)
module ContextOptimizer = struct
  type quotient = {
    reduced_graph : node IMap.t;
    reduced_property_maps : Type.t SMap.t IMap.t;
    reduced_envs : Context.env IMap.t
  }

  let empty = {
    reduced_graph = IMap.empty;
    reduced_property_maps = IMap.empty;
    reduced_envs = IMap.empty;
  }

  class context_optimizer = object
    inherit [quotient] Type_visitor.t as super

    method! id_ cx quotient id =
      let { reduced_graph; _ } = quotient in
      if (IMap.mem id reduced_graph) then quotient
      else
        let types = possible_types cx id in
        let t = match types with
          | [t] -> t
          | t::_ -> UnionT (reason_of_t t, types)
          | [] -> AnyT.t in
        let node = Root { rank = 0; constraints = Resolved t } in
        let reduced_graph = IMap.add id node reduced_graph in
        super#type_ cx { quotient with reduced_graph } t

    method! props cx quotient id =
      let { reduced_property_maps; _ } = quotient in
      if (IMap.mem id reduced_property_maps) then quotient
      else
        let pmap = IMap.find_unsafe id (Context.property_maps cx) in
        let reduced_property_maps = IMap.add id pmap reduced_property_maps in
        super#props cx { quotient with reduced_property_maps } id

    method! fun_type cx quotient funtype =
      let id = funtype.closure_t in
      if id = 0 then super#fun_type cx quotient funtype
      else
        let { reduced_envs; _ } = quotient in
        let closure = IMap.find_unsafe id (Context.envs cx) in
        let reduced_envs = IMap.add id closure reduced_envs in
        super#fun_type cx { quotient with reduced_envs } funtype

  end

  (* walk a context from a list of exports *)
  let reduce_context cx exports =
    let reducer = new context_optimizer in
    List.fold_left (reducer#type_ cx) empty exports

  let export cx =
    let m = Modulename.to_string (Context.module_name cx) in
    lookup_module cx m

  (* reduce a context to a "signature context" *)
  let sig_context component_cxs =
    let cx, other_cxs = List.hd component_cxs, List.tl component_cxs in
    let exports = List.map export component_cxs in
    let quotient = reduce_context cx exports in
    Context.set_graph cx quotient.reduced_graph;
    Context.set_property_maps cx quotient.reduced_property_maps;
    Context.set_envs cx quotient.reduced_envs;
    other_cxs |> List.iter (fun other_cx ->
      let m = Modulename.to_string (Context.module_name other_cx) in
      Context.add_module cx m (export other_cx)
    )

end
