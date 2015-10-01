(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module defines the ML data types that represent types in Flow. *)

open Utils
open Utils_js
open Modes_js
open Reason_js

module Ast = Spider_monkey_ast

type ident = int

(******************************************************************************)
(* Traces                                                                     *)
(******************************************************************************)

(*
  Terminology:

   * A step records a single test of lower bound against
   upper bound, analogous to an invocation of the flow function.

   * A step may have a tvar as its lower or upper bound (or both).
   tvars act as conduits for concrete types, so steps which
   begin or end in tvars may be joined with other steps
   representing tests which adjoin the same tvar.

   The resulting sequence of steps, corresponding to an invocation
   of the flow function followed by the extension of the original
   lower/upper pair through any adjacent type variables, forms the
   basis of a trace. (In trace dumps this is called a "path".)

   * When a step has been induced recursively from a prior invocation
   of the flow function, it's said to have the trace associated with
   that invocation as a parent.

   (Note that each step in a path may have its own parent: consider
   an incoming, recursively induced step joining with a dormant step
   attached to some tvar in an arbitrarily removed invocation of the
   flow function.)

   * A trace is just a sequence of steps along with a (possibly empty)
   parent trace for each step. Since steps may share parents,
   a trace forms a graph, though it is naturally built up as a tree
   when recorded during evaluation of the flow function.
   (The formatting we do in reasons_of_trace recovers the graph
   structure for readability.)
 *)
module Trace = struct
  type step = Type.t * Type.t * parent * int
  and t = step list
  and parent = Parent of t

  let compare = Pervasives.compare

  (* trace depth is 1 + the length of the longest ancestor chain
     in the trace. We keep this precomputed because a) actual ancestors
     may be thrown away due to externally imposed limits on trace depth;
     b) the recursion limiter in the flow function checks this on every
     call. *)
  let trace_depth trace =
    List.fold_left (fun acc (_, _, _, d) -> max acc d) 0 trace

  (* Single-step trace with no parent. This corresponds to a
     top-level invocation of the flow function, e.g. due to
     a constraint generated in Type_inference_js *)
  let unit_trace lower upper =
    [lower, upper, Parent [], 1]

  (* Single-step trace with a parent. This corresponds to a
     recursive invocation of the flow function.
     Optimization: only embed when modes.trace > 0,
     because otherwise we're not going to see any traces anyway.
  *)
  let rec_trace lower upper parent =
    let parent_depth = trace_depth parent in
    let parent = if modes.traces > 0 then parent else [] in
    [lower, upper, Parent parent, parent_depth + 1]

  (* join two traces (see comment header *)
  let join_trace = (@)

  (* join a list of traces *)
  let concat_trace = List.concat

end

(* for export *)
type trace = Trace.t

let trace_depth = Trace.trace_depth
let unit_trace = Trace.unit_trace
let rec_trace = Trace.rec_trace
let join_trace = Trace.join_trace
let concat_trace = Trace.concat_trace

(* used to index trace nodes *)
module TraceMap : MapSig with type key = Trace.t
  = MyMap(Trace)

(* index the nodes in a trace down to a given level.
   returns two maps, trace -> index and index -> trace
 *)
let index_trace = Trace.(
  let rec f (level, tmap, imap) trace =
    if level <= 0 || TraceMap.mem trace tmap
    then level, tmap, imap
    else (
      let tmap, imap =
        let i = TraceMap.cardinal tmap in
        TraceMap.(add trace i tmap), IMap.(add i trace imap)
      in
      List.fold_left (fun acc (_, _, Parent parent, _) ->
        match parent with [] -> acc | _ -> f acc parent
      ) (level - 1, tmap, imap) trace
    )
  in
  fun level trace ->
    let _, tmap, imap = f (level, TraceMap.empty, IMap.empty) trace in
    tmap, imap
)

(*****************************************************************)

open Type

(** Type variables are unknowns, and we are ultimately interested in constraints
    on their solutions for type inference.

    Type variables form nodes in a "union-find" forest: each tree denotes a set
    of type variables that are considered by the type system to be equivalent.

    There are two kinds of nodes: Goto nodes and Root nodes.

    - All Goto nodes of a tree point, directly or indirectly, to the Root node
    of the tree.
    - A Root node holds the actual non-trivial state of a tvar, represented by a
    root structure (see below).
**)
type node =
| Goto of ident
| Root of root

(** A root structure carries the actual non-trivial state of a tvar, and
    consists of:

    - rank, which is a quantity roughly corresponding to the longest chain of
    gotos pointing to the tvar. It's an implementation detail of the unification
    algorithm that simply has to do with efficiently finding the root of a tree.
    We merge a tree with another tree by converting the root with the lower rank
    to a goto node, and making it point to the root with the higher rank. See
    http://en.wikipedia.org/wiki/Disjoint-set_data_structure for more details on
    this data structure and supported operations.

    - constraints, which carry type information that narrows down the possible
    solutions of the tvar (see below).  **)

and root = {
  rank: int;
  constraints: constraints;
}

(** Constraints carry type information that narrows down the possible solutions
    of tvar, and are of two kinds:

    - A Resolved constraint contains a concrete type that is considered by the
    type system to be the solution of the tvar carrying the constraint. In other
    words, the tvar is equivalent to this concrete type in all respects.

    - Unresolved constraints contain bounds that carry both concrete types and
    other tvars as upper and lower bounds (see below).
**)

and constraints =
| Resolved of Type.t
| Unresolved of bounds

(** The bounds structure carries the evolving constraints on the solution of an
    unresolved tvar.

    - upper and lower hold concrete upper and lower bounds, respectively. At any
    point in analysis the aggregate lower bound of a tvar is (conceptually) the
    union of the concrete types in lower, and the aggregate upper bound is
    (conceptually) the intersection of the concrete types in upper. (Upper and
    lower are maps, with the types as keys, and trace information as values.)

    - lowertvars and uppertvars hold tvars which are also (latent) lower and
    upper bounds, respectively. See the __flow function for how these structures
    are populated and operated on.  Here the map keys are tvar ids, with trace
    info as values.
**)
and bounds = {
  mutable lower: trace TypeMap.t;
  mutable upper: trace TypeMap.t;
  mutable lowertvars: trace IMap.t;
  mutable uppertvars: trace IMap.t;
}

(* Extract bounds from a node. *)
(** WARNING: This function is unsafe, since not all nodes are roots, and not all
    roots are unresolved. Use this function only when you are absolutely sure
    that a node is an unresolved root: this is guaranteed to be the case when
    the type variable it denotes is never involved in unification. **)
let bounds_of_unresolved_root node =
  match node with
  | Root { constraints = Unresolved bounds; _ } -> bounds
  | _ -> failwith "expected unresolved root"

let new_bounds () = {
  lower = TypeMap.empty;
  upper = TypeMap.empty;
  lowertvars = IMap.empty;
  uppertvars = IMap.empty;
}

let new_unresolved_root () =
  Root { rank = 0; constraints = Unresolved (new_bounds ()) }

let copy_bounds = function
  | { lower; upper; lowertvars; uppertvars; } ->
    { lower; upper; lowertvars; uppertvars; }

let copy_node node = match node with
  | Root { rank; constraints = Unresolved bounds } ->
    Root { rank; constraints = Unresolved (copy_bounds bounds) }
  | _ -> node

(***************************************)
(* type context *)

type stack = int list

type context = {
  file: filename;
  _module: string;
  checked: bool;
  weak: bool;

  (* required modules, and map to their locations *)
  mutable required: SSet.t;
  mutable require_loc: Loc.t SMap.t;
  mutable module_exports_type: module_exports_type;

  (* map from tvar ids to nodes (type info structures) *)
  mutable graph: node IMap.t;

  (* obj types point to mutable property maps *)
  mutable property_maps: Type.properties IMap.t;

  (* map from closure ids to env snapshots *)
  mutable closures: (stack * Scope.t list) IMap.t;

  (* map from module names to their types *)
  mutable modulemap: Type.t SMap.t;

  mutable errors: Errors_js.ErrorSet.t;
  mutable globals: SSet.t;

  mutable error_suppressions: Errors_js.ErrorSuppressions.t;

  type_table: (Loc.t, Type.t) Hashtbl.t;
  annot_table: (Loc.t, Type.t) Hashtbl.t;
}

and module_exports_type =
  | CommonJSModule of Loc.t option
  | ESModule

(* create a new context structure.
   Flow_js.fresh_context prepares for actual use.
 *)
let new_context ?(checked=false) ?(weak=false) ~file ~_module = {
  file;
  _module;
  checked;
  weak;

  required = SSet.empty;
  require_loc = SMap.empty;
  module_exports_type = CommonJSModule(None);

  graph = IMap.empty;
  closures = IMap.empty;
  property_maps = IMap.empty;
  modulemap = SMap.empty;

  errors = Errors_js.ErrorSet.empty;
  globals = SSet.empty;

  error_suppressions = Errors_js.ErrorSuppressions.empty;

  type_table = Hashtbl.create 0;
  annot_table = Hashtbl.create 0;
}

(********************************************************************)

let name_prefix_of_t = function
  | RestT _ -> "..."
  | _ -> ""

let name_suffix_of_t = function
  | OptionalT _ -> "?"
  | _ -> ""

let parameter_name cx n t =
  (name_prefix_of_t t) ^ n ^ (name_suffix_of_t t)

type enclosure_t =
    EnclosureNone
  | EnclosureUnion
  | EnclosureIntersect
  | EnclosureParam
  | EnclosureMaybe
  | EnclosureAppT
  | EnclosureRet

let parenthesize t_str enclosure triggers =
  if List.mem enclosure triggers
  then "(" ^ t_str ^ ")"
  else t_str

(* general-purpose type printer. not the cleanest visitor in the world,
   but reasonably general. override gets a chance to print the incoming
   type first. if it passes, the bulk of printable types are formatted
   in a reasonable way. fallback is sent the rest. enclosure drives
   delimiter choice. see e.g. dump_t and string_of_t for callers.
 *)
let rec type_printer override fallback enclosure cx t =
  let pp = type_printer override fallback in
  match override cx t with
  | Some s -> s
  | None ->
    match t with
    | BoundT typeparam -> typeparam.name

    | SingletonStrT (_, s) -> spf "'%s'" s
    | SingletonNumT (_, (_, raw)) -> raw
    | SingletonBoolT (_, b) -> string_of_bool b

    (* reasons for VoidT use "undefined" for more understandable error output.
       For parsable types we need to use "void" though, thus overwrite it. *)
    | VoidT _ -> "void"

    | FunT (_,_,_,{params_tlist = ts; params_names = pns; return_t = t; _}) ->
        let pns =
          match pns with
          | Some pns -> pns
          | None -> List.map (fun _ -> "_") ts in
        let type_s = spf "(%s) => %s"
          (List.map2 (fun n t ->
              (parameter_name cx n t) ^
              ": "
              ^ (pp EnclosureParam cx t)
            ) pns ts
           |> String.concat ", "
          )
          (pp EnclosureNone cx t) in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureIntersect]

    | ObjT (_, {props_tmap = flds; dict_t; _}) ->
        let props =
          IMap.find_unsafe flds cx.property_maps
           |> SMap.elements
           |> List.filter (fun (x,_) -> not (Reason_js.is_internal_name x))
           |> List.rev
           |> List.map (fun (x,t) -> x ^ ": " ^ (pp EnclosureNone cx t) ^ ";")
           |> String.concat " "
        in
        let indexer =
          (match dict_t with
          | Some { dict_name; key; value } ->
              let indexer_prefix =
                if props <> ""
                then " "
                else ""
              in
              let dict_name = match dict_name with
                | None -> "_"
                | Some name -> name
              in
              (spf "%s[%s: %s]: %s;"
                indexer_prefix
                dict_name
                (pp EnclosureNone cx key)
                (pp EnclosureNone cx value)
              )
          | None -> "")
        in
        spf "{%s%s}" props indexer

    | ArrT (_, t, ts) ->
        (*(match ts with
        | [] -> *)spf "Array<%s>" (pp EnclosureNone cx t)
        (*| _ -> spf "[%s]"
                  (ts
                    |> List.map (pp cx EnclosureNone)
                    |> String.concat ", "))*)

    | InstanceT (reason,static,super,instance) ->
        desc_of_reason reason (* nominal type *)

    | TypeAppT (c,ts) ->
        let type_s =
          spf "%s <%s>"
            (pp EnclosureAppT cx c)
            (ts
              |> List.map (pp EnclosureNone cx)
              |> String.concat ", "
            )
        in
        parenthesize type_s enclosure [EnclosureMaybe]

    | MaybeT t ->
        spf "?%s" (pp EnclosureMaybe cx t)

    | PolyT (xs,t) ->
        let type_s =
          spf "<%s> %s"
            (xs
              |> List.map (fun param -> param.name)
              |> String.concat ", "
            )
            (pp EnclosureNone cx t)
        in
        parenthesize type_s enclosure [EnclosureAppT; EnclosureMaybe]

    | IntersectionT (_, ts) ->
        let type_s =
          (ts
            |> List.map (pp EnclosureIntersect cx)
            |> String.concat " & "
          ) in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureMaybe]

    | UnionT (_, ts) ->
        let type_s =
          (ts
            |> List.map (pp EnclosureUnion cx)
            |> String.concat " | "
          ) in
        parenthesize type_s enclosure [EnclosureIntersect; EnclosureMaybe]

    (* The following types are not syntax-supported in all cases *)
    | RestT t ->
        let type_s =
          spf "Array<%s>" (pp EnclosureNone cx t) in
        if enclosure == EnclosureParam
        then type_s
        else "..." ^ type_s

    | OptionalT t ->
        let type_s = pp EnclosureNone cx t in
        if enclosure == EnclosureParam
        then type_s
        else "=" ^ type_s

    | AnnotT (_, t) -> pp EnclosureNone cx t
    | KeysT (_, t) -> spf "$Keys<%s>" (pp EnclosureNone cx t)
    | ShapeT t -> spf "$Shape<%s>" (pp EnclosureNone cx t)

    (* The following types are not syntax-supported *)
    | ClassT t ->
        spf "[class: %s]" (pp EnclosureNone cx t)

    | TypeT (_, t) ->
        spf "[type: %s]" (pp EnclosureNone cx t)

    | BecomeT (_, t) ->
        spf "[become: %s]" (pp EnclosureNone cx t)

    | LowerBoundT t ->
        spf "$Subtype<%s>" (pp EnclosureNone cx t)

    | UpperBoundT t ->
        spf "$Supertype<%s>" (pp EnclosureNone cx t)

    | AnyObjT _ ->
        "Object"

    | AnyFunT _ ->
        "Function"

    | t ->
        fallback t

(* pretty printer *)
let string_of_t_ =
  let override cx t = match t with
    | OpenT (r, id) -> Some (spf "TYPE_%d" id)
    | NumT _
    | StrT _
    | BoolT _
    | UndefT _
    | MixedT _
    | AnyT _
    | NullT _ -> Some (desc_of_reason (reason_of_t t))
    | _ -> None
  in
  let fallback t =
    assert_false (spf "Missing printer for %s" (string_of_ctor t))
  in
  fun enclosure cx t ->
    type_printer override fallback enclosure cx t

let string_of_t =
  string_of_t_ EnclosureNone

let string_of_param_t =
  string_of_t_ EnclosureParam

(****************** json ******************)

module Json = Hh_json

let string_of_pred_ctor = function
  | AndP _ -> "AndP"
  | OrP _ -> "OrP"
  | NotP _ -> "NotP"
  | LeftP _ -> "LeftP"
  | RightP _ -> "RightP"
  | ExistsP -> "ExistsP"
  | IsP _ -> "IsP"

let string_of_binary_test_ctor = function
  | Instanceof -> "Instanceof"
  | SentinelProp _ -> "SentinelProp"

type json_cx = {
  stack: ISet.t;
  depth: int;
  cx: context;
}

let check_depth continuation json_cx =
  let depth = json_cx.depth - 1 in
  if depth < 0
  then fun _ -> Json.JNull
  else continuation { json_cx with depth; }

let rec _json_of_t json_cx = check_depth _json_of_t_impl json_cx
and _json_of_t_impl json_cx t = Json.(
  JAssoc ([
    "reason", json_of_reason (reason_of_t t);
    "kind", JString (string_of_ctor t)
  ] @
  match t with
  | OpenT (_, id) -> [
      "id", JInt id
    ] @
    if ISet.mem id json_cx.stack then []
    else [
      "node", json_of_node json_cx id
    ]

  | NumT (_, lit) ->
    begin match lit with
    | Literal (_, raw) -> ["literal", JString raw]
    | Truthy
    | Falsy
    | AnyLiteral -> []
    end

  | StrT (_, lit) ->
    begin match lit with
    | Literal s -> ["literal", JString s]
    | Truthy
    | Falsy
    | AnyLiteral -> []
    end

  | BoolT (_, b) ->
    (match b with
      | Some b -> ["literal", JBool b]
      | None -> [])

  | UndefT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _ ->
    []

  | FunT (_, static, proto, funtype) -> [
      "static", _json_of_t json_cx static;
      "prototype", _json_of_t json_cx proto;
      "funType", json_of_funtype json_cx funtype
    ]

  | ObjT (_, objtype) -> [
      "type", json_of_objtype json_cx objtype
    ]

  | ArrT (_, elemt, tuplet) -> [
      "elemType", _json_of_t json_cx elemt;
      "tupleType", JList (List.map (_json_of_t json_cx) tuplet)
    ]

  | ClassT t -> [
      "type", _json_of_t json_cx t
    ]

  | InstanceT (_, static, super, instance) -> [
      "static", _json_of_t json_cx static;
      "super", _json_of_t json_cx super;
      "instance", json_of_insttype json_cx instance
    ]

  | OptionalT t
  | RestT t -> [
      "type", _json_of_t json_cx t
    ]

  | PolyT (tparams, t) -> [
      "typeParams", JList (List.map (json_of_typeparam json_cx) tparams);
      "type", _json_of_t json_cx t
    ]

  | TypeAppT (t, targs) -> [
      "typeArgs", JList (List.map (_json_of_t json_cx) targs);
      "type", _json_of_t json_cx t
    ]

  | BoundT tparam -> [
      "typeParam", json_of_typeparam json_cx tparam
    ]

  | ExistsT tparam ->
    []

  | MaybeT t -> [
      "type", _json_of_t json_cx t
    ]

  | IntersectionT (_, ts)
  | UnionT (_, ts) -> [
      "types", JList (List.map (_json_of_t json_cx) ts)
    ]

  | UpperBoundT t
  | LowerBoundT t -> [
      "type", _json_of_t json_cx t
    ]

  | AnyObjT _
  | AnyFunT _ ->
    []

  | ShapeT t -> [
      "type", _json_of_t json_cx t
    ]

  | DiffT (t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | KeysT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | SingletonStrT (_, s) -> [
      "literal", JString s
    ]

  | SingletonNumT (_, (_, raw)) -> [
      "literal", JString raw
    ]

  | SingletonBoolT (_, b) -> [
      "literal", JBool b
    ]

  | TypeT (_, t) -> [
      "result", _json_of_t json_cx t
    ]

  | AnnotT (t1, t2) -> [
      "assert", _json_of_t json_cx t1;
      "assume", _json_of_t json_cx t2
    ]

  | BecomeT (_, t) -> [
      "result", _json_of_t json_cx t
    ]

  | SpeculativeMatchFailureT (_, attempt, target) -> [
      "attemptType", _json_of_t json_cx attempt;
      "targetType", _json_of_t json_cx target
    ]

  | ModuleT (_, {exports_tmap; cjs_export;}) -> [
      "namedExports",
      (let tmap = IMap.find_unsafe exports_tmap json_cx.cx.property_maps in
       json_of_tmap json_cx tmap);

      "cjsExport",
      match cjs_export with Some(t) -> _json_of_t json_cx t | None -> JNull;
    ]

  | SummarizeT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | CallT (_, funtype) -> [
      "funType", json_of_funtype json_cx funtype
    ]

  | MethodT (_, name, funtype) -> [
      "name", json_of_propname json_cx name;
      "funType", json_of_funtype json_cx funtype
    ]

  | ReposLowerT (_, t)
  | ReposUpperT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | SetPropT (_, name, t)
  | GetPropT (_, name, t) -> [
      "propName", json_of_propname json_cx name;
      "propType", _json_of_t json_cx t
    ]

  | SetElemT (_, indext, elemt)
  | GetElemT (_, indext, elemt) -> [
      "indexType", _json_of_t json_cx indext;
      "elemType", _json_of_t json_cx elemt
    ]

  | ConstructorT (_, tparams, t) -> [
      "typeParams", JList (List.map (_json_of_t json_cx) tparams);
      "type", _json_of_t json_cx t
    ]

  | SuperT (_, instance) -> [
      "instance", json_of_insttype json_cx instance
    ]

  | ExtendsT (_, t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | AdderT (_, l, r) -> [
      "leftType", _json_of_t json_cx l;
      "rightType", _json_of_t json_cx r
    ]

  | ComparatorT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | UnaryMinusT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | PredicateT (p, t) -> [
      "pred", json_of_pred json_cx p;
      "type", _json_of_t json_cx t
    ]

  | EqT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | AndT (_, right, res)
  | OrT (_, right, res) -> [
      "rightType", _json_of_t json_cx right;
      "resultType", _json_of_t json_cx res
    ]

  | NotT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | SpecializeT (_, cache, targs, tvar) -> [
      "cache", JBool cache;
      "types", JList (List.map (_json_of_t json_cx) targs);
      "tvar", _json_of_t json_cx tvar
    ]

  | LookupT (_, rstrict, _, name, t) ->
    (match rstrict with
      | None -> []
      | Some r -> ["strictReason", json_of_reason r]
    ) @ [
      "name", JString name;
      "type", _json_of_t json_cx t
    ]

  | ObjAssignT (_, assignee, tvar, prop_names, flag) -> [
      "assigneeType", _json_of_t json_cx assignee;
      "resultType", _json_of_t json_cx tvar;
      "propNames", JList (List.map (fun s -> JString s) prop_names);
      "flag", JBool flag
    ]

  | ObjFreezeT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ObjRestT (_, excludes, tvar) -> [
      "excludedProps", JList (List.map (fun s -> JString s) excludes);
      "resultType", _json_of_t json_cx tvar;
    ]

  | ObjSealT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ObjTestT (_, default, res) -> [
      "defaultType", _json_of_t json_cx default;
      "resultType", _json_of_t json_cx res
    ]

  | UnifyT (t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | ConcretizeT (l, todo_list, done_list, u) -> [
      "inType", _json_of_t json_cx l;
      "todoTypes", JList (List.map (_json_of_t json_cx) todo_list);
      "doneTypes", JList (List.map (_json_of_t json_cx) done_list);
      "absType", _json_of_t json_cx u
    ]

  | ConcreteT t
  | GetKeysT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | HasKeyT (_, key) -> [
      "key", JString key
    ]

  | ElemT (_, base, elem) -> [
      "baseType", _json_of_t json_cx base;
      "elemType", _json_of_t json_cx elem
    ]

  | CJSRequireT (_, export) -> [
      "export",
      _json_of_t json_cx export
    ]
  | ImportModuleNsT (_, t)
  | ImportTypeT (_, t)
  | ImportTypeofT (_, t)
    -> ["type", _json_of_t json_cx t]

  | CJSExtractNamedExportsT (_, module_t, t_out) -> [
      "module", _json_of_t json_cx module_t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | SetNamedExportsT (_, tmap, t_out) -> [
      "tmap", json_of_tmap json_cx tmap;
      "t_out", _json_of_t json_cx t_out;
    ]
  | SetCJSExportT (_, t, t_out) -> [
      "cjsExportType", _json_of_t json_cx t;
      "t_out", _json_of_t json_cx t_out;
    ]
))

and json_of_polarity json_cx = check_depth json_of_polarity_impl json_cx
and json_of_polarity_impl json_cx polarity =
  Json.JString (match polarity with
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"
)

and json_of_typeparam json_cx = check_depth json_of_typeparam_impl json_cx
and json_of_typeparam_impl json_cx tparam = Json.(
  JAssoc [
    "reason", json_of_reason tparam.reason;
    "name", JString tparam.name;
    "bound", _json_of_t json_cx tparam.bound;
    "polarity", json_of_polarity json_cx tparam.polarity;
  ]
)

and json_of_objtype json_cx = check_depth json_of_objtype_impl json_cx
and json_of_objtype_impl json_cx objtype = Json.(
  JAssoc ([
    "flags", json_of_flags json_cx objtype.flags;
  ] @ (match objtype.dict_t with
    | None -> []
    | Some d -> ["dictType", json_of_dicttype json_cx d]
  ) @ [
    "propTypes",
      (let tmap = IMap.find_unsafe objtype.props_tmap json_cx.cx.property_maps in
      json_of_tmap json_cx tmap);
    "prototype", _json_of_t json_cx objtype.proto_t
  ])
)

and json_of_dicttype json_cx = check_depth json_of_dicttype_impl json_cx
and json_of_dicttype_impl json_cx dicttype = Json.(
  JAssoc (
    (match dicttype.dict_name with
    | None -> []
    | Some name -> ["name", JString name]
  ) @ [
    "keyType", _json_of_t json_cx dicttype.key;
    "valueType", _json_of_t json_cx dicttype.value
  ])
)

and json_of_flags json_cx = check_depth json_of_flags_impl json_cx
and json_of_flags_impl json_cx flags = Json.(
  JAssoc [
    "frozen", JBool flags.frozen;
    "sealed", JBool (match flags.sealed with
      | Sealed -> true
      | UnsealedInFile _ -> false);
    "exact", JBool flags.exact;
  ]
)

and json_of_funtype json_cx = check_depth json_of_funtype_impl json_cx
and json_of_funtype_impl json_cx funtype = Json.(
  JAssoc ([
    "thisType", _json_of_t json_cx funtype.this_t;
    "paramTypes", JList (List.map (_json_of_t json_cx) funtype.params_tlist)
  ] @ (match funtype.params_names with
    | None -> []
    | Some names -> ["paramNames", JList (List.map (fun s -> JString s) names)]
  ) @ [
    "returnType", _json_of_t json_cx funtype.return_t;
    "closureTypeIndex", JInt funtype.closure_t
  ])
)

and json_of_insttype json_cx = check_depth json_of_insttype_impl json_cx
and json_of_insttype_impl json_cx insttype = Json.(
  JAssoc [
    "classId", JInt insttype.class_id;
    "typeArgs", json_of_tmap json_cx insttype.type_args;
    "argPolarities", json_of_polarity_map json_cx insttype.arg_polarities;
    "fieldTypes",
      (let tmap = IMap.find_unsafe insttype.fields_tmap json_cx.cx.property_maps in
       json_of_tmap json_cx tmap);
    "methodTypes",
      (let tmap = IMap.find_unsafe insttype.methods_tmap json_cx.cx.property_maps in
       json_of_tmap json_cx tmap);
    "mixins", JBool insttype.mixins;
    "structural", JBool insttype.structural;
  ]
)

and json_of_polarity_map json_cx = check_depth json_of_polarity_map_impl json_cx
and json_of_polarity_map_impl json_cx pmap = Json.(
  let lst = SMap.fold (fun name pol acc ->
    JAssoc ["name", JString name; "polarity", json_of_polarity json_cx pol] :: acc
  ) pmap [] in
  JList (List.rev lst)
)

and json_of_propname json_cx = check_depth json_of_propname_impl json_cx
and json_of_propname_impl json_cx (reason, literal) = Json.(
  JAssoc [
    "reason", json_of_reason reason;
    "literal", JString literal;
  ]
)

and json_of_tmap json_cx = check_depth json_of_tmap_impl json_cx
and json_of_tmap_impl json_cx bindings = Json.(
  let lst = SMap.fold (fun name t acc ->
    json_of_type_binding json_cx (name, t) :: acc
  ) bindings [] in
  JList (List.rev lst)
)

and json_of_type_binding json_cx = check_depth json_of_type_binding_impl json_cx
and json_of_type_binding_impl json_cx (name, t) = Json.(
  JAssoc ["name", JString name; "type", _json_of_t json_cx t]
)

and json_of_pred json_cx = check_depth json_of_pred_impl json_cx
and json_of_pred_impl json_cx p = Json.(
  JAssoc ([
    "kind", JString (string_of_pred_ctor p)
  ] @
  match p with
  | AndP (l, r)
  | OrP (l, r) -> [
      "left", json_of_pred json_cx l;
      "right", json_of_pred json_cx r
    ]
  | NotP p -> ["pred", json_of_pred json_cx p]
  | LeftP (b, t)
  | RightP (b, t) -> [
      "binaryTest", json_of_binary_test json_cx b;
      "type", _json_of_t json_cx t
    ]
  | ExistsP -> []
  | IsP s -> ["typeName", JString s]
))

and json_of_binary_test json_cx = check_depth json_of_binary_test_impl json_cx
and json_of_binary_test_impl json_cx b = Json.(
  JAssoc ([
    "kind", JString (string_of_binary_test_ctor b)
  ] @
  match b with
  | Instanceof -> []
  | SentinelProp s -> ["key", JString s]
))

and json_of_node json_cx = check_depth json_of_node_impl json_cx
and json_of_node_impl json_cx id = Json.(
  JAssoc (
    let json_cx = { json_cx with stack = ISet.add id json_cx.stack } in
    match IMap.find_unsafe id json_cx.cx.graph with
    | Goto id ->
      ["kind", JString "Goto"]
      @ ["id", JInt id]
    | Root root ->
      ["kind", JString "Root"]
      @ ["root", json_of_root json_cx root]
  )
)

and json_of_root json_cx = check_depth json_of_root_impl json_cx
and json_of_root_impl json_cx root = Json.(
  JAssoc ([
    "rank", JInt root.rank;
    "constraints", json_of_constraints json_cx root.constraints
  ])
)

and json_of_constraints json_cx = check_depth json_of_constraints_impl json_cx
and json_of_constraints_impl json_cx constraints = Json.(
  JAssoc (
    match constraints with
    | Resolved t ->
      ["kind", JString "Resolved"]
      @ ["type", _json_of_t json_cx t]
    | Unresolved bounds ->
      ["kind", JString "Unresolved"]
      @ ["bounds", json_of_bounds json_cx bounds]
  )
)

and json_of_bounds json_cx = check_depth json_of_bounds_impl json_cx
and json_of_bounds_impl json_cx bounds = Json.(
  match bounds with
  | { lower; upper; lowertvars; uppertvars; } -> JAssoc ([
      "lower", json_of_tkeys json_cx lower;
      "upper", json_of_tkeys json_cx upper;
      "lowertvars", json_of_tvarkeys json_cx lowertvars;
      "uppertvars", json_of_tvarkeys json_cx uppertvars;
    ])
)

and json_of_tkeys json_cx = check_depth json_of_tkeys_impl json_cx
and json_of_tkeys_impl json_cx tmap = Json.(
  JList (TypeMap.fold (fun t _ acc -> _json_of_t json_cx t :: acc) tmap [])
)

and json_of_tvarkeys json_cx = check_depth json_of_tvarkeys_impl json_cx
and json_of_tvarkeys_impl json_cx imap = Json.(
  JList (IMap.fold (fun i _ acc -> JInt i :: acc) imap [])
)

let json_of_t ?(depth=1000) cx t =
  let json_cx = { cx; depth; stack = ISet.empty; } in
  _json_of_t json_cx t

let jstr_of_t ?(depth=1000) cx t =
  Json.json_to_multiline (json_of_t ~depth cx t)

let json_of_graph ?(depth=1000) cx = Json.(
  let entries = IMap.fold (fun id _ entries ->
    let json_cx = { cx; depth; stack = ISet.empty; } in
    (spf "%d" id, json_of_node json_cx id) :: entries
  ) cx.graph [] in
  JAssoc (List.rev entries)
)

let jstr_of_graph ?(depth=1000) cx =
  Json.json_to_multiline (json_of_graph ~depth cx)

(****************** end json ******************)

(* debug printer *)
let rec dump_t cx t =
  dump_t_ ISet.empty cx t

and dump_t_ =
  (* we'll want to add more here *)
  let override stack cx t = match t with
    | OpenT (r, id) -> Some (dump_tvar stack cx r id)
    | NumT (r, lit) -> Some (match lit with
        | Literal (_, raw) -> spf "NumT(%s)" raw
        | Truthy -> spf "NumT(truthy)"
        | Falsy -> spf "NumT(0)"
        | AnyLiteral -> "NumT")
    | StrT (r, c) -> Some (match c with
        | Literal s -> spf "StrT(%S)" s
        | Truthy -> spf "StrT(truthy)"
        | Falsy -> spf "StrT(falsy)"
        | AnyLiteral -> "StrT")
    | BoolT (r, c) -> Some (match c with
        | Some b -> spf "BoolT(%B)" b
        | None -> "BoolT")
    | UndefT _
    | MixedT _
    | AnyT _
    | NullT _ -> Some (string_of_ctor t)
    | SetPropT (_, (_, n), t) ->
        Some (spf "SetPropT(%s: %s)" n (dump_t_ stack cx t))
    | GetPropT (_, (_, n), t) ->
        Some (spf "GetPropT(%s: %s)" n (dump_t_ stack cx t))
    | LookupT (_, _, ts, n, t) ->
        Some (spf "LookupT(%s: %s)" n (dump_t_ stack cx t))
    | PredicateT (p, t) -> Some (spf "PredicateT(%s | %s)"
        (string_of_predicate p) (dump_t_ stack cx t))
    | _ -> None
  in
  fun stack cx t ->
    type_printer (override stack) string_of_ctor EnclosureNone cx t

(* type variable dumper. abbreviates a few simple cases for readability.
   note: if we turn the tvar record into a datatype, these will give a
   sense of some of the obvious data constructors *)
and dump_tvar stack cx r id =
  let sbounds = if ISet.mem id stack then "(...)" else (
    let stack = ISet.add id stack in
    match IMap.find_unsafe id cx.graph with
    | Goto id -> spf "Goto TYPE_%d" id
    | Root { rank; constraints = Resolved t } ->
        spf "Root (rank = %d, resolved = %s)"
          rank (dump_t_ stack cx t)
    | Root { rank; constraints = Unresolved bounds } ->
        spf "Root (rank = %d, unresolved = %s)"
          rank (dump_bounds stack cx id bounds)
  ) in
  (spf "TYPE_%d: " id) ^ sbounds

and dump_bounds stack cx id bounds = match bounds with
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = TypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* no inflows or outflows *)
      "(free)"
  | { lower; upper; lowertvars; uppertvars; }
      when upper = TypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete inflows *)
      spf "L %s" (dump_tkeys stack cx lower)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = TypeMap.empty
      && IMap.cardinal uppertvars = 1 ->
      (* only tvar inflows *)
      spf "LV %s" (dump_tvarkeys cx id lowertvars)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete outflows *)
      spf "U %s" (dump_tkeys stack cx upper)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = TypeMap.empty
      && IMap.cardinal lowertvars = 1 ->
      (* only tvar outflows *)
      spf "UV %s" (dump_tvarkeys cx id uppertvars)
  | { lower; upper; lowertvars; uppertvars; }
      when IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete inflows/outflows *)
      let l = dump_tkeys stack cx lower in
      let u = dump_tkeys stack cx upper in
      if l = u then "= " ^ l
      else "L " ^ l ^ " U " ^ u
  | { lower; upper; lowertvars; uppertvars; } ->
    let slower = if lower = TypeMap.empty then "" else
      spf " lower = %s;" (dump_tkeys stack cx lower) in
    let supper = if upper = TypeMap.empty then "" else
      spf " upper = %s;" (dump_tkeys stack cx upper) in
    let sltvars = if IMap.cardinal lowertvars <= 1 then "" else
      spf " lowertvars = %s;" (dump_tvarkeys cx id lowertvars) in
    let sutvars = if IMap.cardinal uppertvars <= 1 then "" else
      spf " uppertvars = %s;" (dump_tvarkeys cx id uppertvars) in
    "{" ^ slower ^ supper ^ sltvars ^ sutvars ^ " }"

(* dump the keys of a type map as a list *)
and dump_tkeys stack cx tmap =
  "[" ^ (
    String.concat "," (
      List.rev (
        TypeMap.fold (
          fun t _ acc -> dump_t_ stack cx t :: acc
        ) tmap []
      )
    )
  ) ^ "]"

(* dump the keys of a tvar map as a list *)
and dump_tvarkeys cx self imap =
  "[" ^ (
    String.concat "," (
      List.rev (
        IMap.fold (
          fun id _ acc ->
            if id = self then acc else spf "TYPE_%d" id :: acc
        ) imap []
      )
    )
  ) ^ "]"

let rec is_printed_type_parsable_impl weak cx enclosure = function
  (* Base cases *)
  | BoundT _
  | NumT _
  | StrT _
  | BoolT _
  | AnyT _
    ->
      true

  | VoidT _
    when (enclosure == EnclosureRet)
    ->
      true

  | AnnotT (_, t) ->
      is_printed_type_parsable_impl weak cx enclosure t

  (* Composed types *)
  | MaybeT t
    ->
      is_printed_type_parsable_impl weak cx EnclosureMaybe t

  | ArrT (_, t, ts)
    ->
      (*(match ts with
      | [] -> *)is_printed_type_parsable_impl weak cx EnclosureNone t
      (*| _ ->
          is_printed_type_list_parsable weak cx EnclosureNone t*)

  | RestT t
  | OptionalT t
    when (enclosure == EnclosureParam)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | FunT (_, _, _, { params_tlist; return_t; _ })
    ->
      (is_printed_type_parsable_impl weak cx EnclosureRet return_t) &&
      (is_printed_type_list_parsable weak cx EnclosureParam params_tlist)

  | ObjT (_, { props_tmap; dict_t; _ })
    ->
      let is_printable =
        match dict_t with
        | Some { key; value; _ } ->
            (is_printed_type_parsable_impl weak cx EnclosureNone key) &&
            (is_printed_type_parsable_impl weak cx EnclosureNone value)
        | None -> true
      in
      let prop_map = IMap.find_unsafe props_tmap cx.property_maps in
      SMap.fold (fun name t acc ->
          acc && (
            (* We don't print internal properties, thus we do not care whether
               their type is printable or not *)
            (Reason_js.is_internal_name name) ||
            (is_printed_type_parsable_impl weak cx EnclosureNone t)
          )
        ) prop_map is_printable

  | InstanceT _
    ->
      true

  | IntersectionT (_, ts)
    ->
      is_printed_type_list_parsable weak cx EnclosureIntersect ts

  | UnionT (_, ts)
    ->
      is_printed_type_list_parsable weak cx EnclosureUnion ts

  | PolyT (_, t)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | AnyObjT _ -> true
  | AnyFunT _ -> true

  (* weak mode *)

  (* these are types which are not really parsable, but they make sense to a
     human user in cases of autocompletion *)
  | OptionalT t
  | RestT t
  | TypeT (_, t)
  | LowerBoundT t
  | UpperBoundT t
  | ClassT t
    when weak
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | VoidT _
    when weak
    ->
      true

  (* This gives really ugly output, but would need to figure out a better way
     to print these types otherwise, maybe substitute on printing? *)
  | TypeAppT (t, ts)
    when weak
    ->
      (is_printed_type_parsable_impl weak cx EnclosureAppT t) &&
      (is_printed_type_list_parsable weak cx EnclosureNone ts)

  | _
    ->
      false

and is_printed_type_list_parsable weak cx enclosure ts =
  List.fold_left (fun acc t ->
      acc && (is_printed_type_parsable_impl weak cx enclosure t)
    ) true ts

let is_printed_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureNone t

let is_printed_param_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureParam t

(*****************************************************************)

(* scopes and types *)

let string_of_loc_opt = function
| Some loc -> string_of_loc loc
| None -> "(none)"

let string_of_entry = Scope.(

  let string_of_value cx {
    Entry.kind; value_state; value_loc; specific; general
  } =
    Utils.spf "{ kind: %s; value_state: %s; value_loc: %s; \
      specific: %s; general: %s }"
      (Entry.string_of_value_kind kind)
      (Entry.string_of_state value_state)
      (string_of_loc_opt value_loc)
      (dump_t cx specific)
      (dump_t cx general)
  in

  let string_of_type cx { Entry.type_state; type_loc; _type } =
    Utils.spf "{ type_state: %s; type_loc: %s; _type: %s }"
      (Entry.string_of_state type_state)
      (string_of_loc_opt type_loc)
      (dump_t cx _type)
  in

  fun cx -> Entry.(function
  | Value r -> spf "Value %s" (string_of_value cx r)
  | Type r -> spf "Type %s" (string_of_type cx r)
  )
)

let string_of_scope = Scope.(

  let string_of_entries cx entries =
    SMap.fold (fun name entry acc ->
      (Utils.spf "%s: %s" name (string_of_entry cx entry))
        :: acc
    ) entries []
    |> String.concat ";\n  "
  in

  let string_of_refi cx { refi_loc; refined; original } =
    Utils.spf "{ refi_loc: %s; refined: %s; original: %s }"
      (string_of_loc_opt refi_loc)
      (dump_t cx refined)
      (dump_t cx original)
  in

  let string_of_refis cx refis =
    KeyMap.fold (fun key refi acc ->
      (Utils.spf "%s: %s" (Key.string_of_key key) (string_of_refi cx refi))
        :: acc
    ) refis []
    |> String.concat ";\n  "
  in

  let string_of_function_kind = function
  | Ordinary -> "Ordinary"
  | Async -> "Async"
  | Generator -> "Generator"
  in

  let string_of_scope_kind = function
  | VarScope kind -> spf "VarScope %s" (string_of_function_kind kind)
  | LexScope -> "LexScope"
  in

  fun cx scope ->
    Utils.spf "{ kind: %s;\nentries:\n%s\nrefis:\n%s\n}"
      (string_of_scope_kind scope.kind)
      (string_of_entries cx scope.entries)
      (string_of_refis cx scope.refis)
)

(*****************************************************************)

(* traces and types *)

let level_spaces level indent = indent * level

let spaces n = String.make n ' '

let fill n tab =
  let rec loop n =
    if n <= 0 then ""
    else if n < tab then "." ^ (spaces (n - 1))
    else "." ^ (spaces (tab - 1)) ^ (loop (n - tab))
  in loop n

let prep_path r =
  if not (Modes_js.modes.strip_root) then r
  else
    let path = FlowConfig.((get_unsafe ()).root) in
    Reason_js.strip_root path r

(* string length of printed position, as it would
   appear in an error *)
let pos_len r =
  let r = prep_path r in
  let loc = loc_of_reason r in
  let fmt = Errors_js.(format_reason_color (BlameM (loc, ""))) in
  let str = String.concat "" (List.map snd fmt) in
  String.length str

(* scan a trace tree, return maximum position length
   of reasons at or above the given depth limit, and
   min of that limit and actual max depth *)
let max_pos_len_and_depth limit trace =
  let rec f (len, depth) (lower, upper, parent, _) =
    let len = max len (pos_len (reason_of_t lower)) in
    let len = max len (pos_len (reason_of_t upper)) in
    if depth > limit then len, depth
    else Trace.(
      match parent with
      | Parent [] -> len, depth
      | Parent trace -> List.fold_left f (len, depth + 1) trace
    )
  in List.fold_left f (0, 0) trace

(* reformat a reason's description with
   - the given left margin
   - the given prefix and suffix: if either is nonempty,
     "desc" becomes "prefix[desc]suffix"
  *)
let pretty_r margin r prefix suffix =
  let len = pos_len r in
  let ind = if margin > len then spaces (margin - len) else "" in
  if prefix = "" && suffix = ""
  then prefix_reason ind r
  else wrap_reason (ind ^ (spf "%s[" prefix)) (spf "]%s" suffix) r

(* helper: we want the tvar id as well *)
(* NOTE: uncalled for now, because ids are nondetermistic
   due to parallelism, which messes up test diffs. Should
   add a config, but for now must uncomment impl to use *)
let reason_of_t_add_id = reason_of_t
(* function
| OpenT (r, id) -> prefix_reason (spf "%d: " id) r
| t -> reason_of_t t *)

(* prettyprint a trace. what we print:

   - a list of paths, numbered 1..n, root first.

   - for each path, its list of steps.
     usually a step is 2 main lines, one each for lower and upper.
     but we elide the former if its a tvar that was also the  prior
     step's upper.
     if the step was derived from another path, we append a note
     to that effect.
 *)
let reasons_of_trace ?(level=0) trace =
  let max_pos_len, max_depth = max_pos_len_and_depth level trace in
  let level = min level max_depth in

  let tmap, imap = index_trace level trace in

  let print_step steps i (lower, upper, Trace.Parent parent, _) =
    (* omit lower if it's a pipelined tvar *)
    (if i > 0 &&
      lower = (match List.nth steps (i - 1) with (_, upper, _, _) -> upper)
    then []
    else [pretty_r max_pos_len (reason_of_t_add_id lower)
      (spf "%s " (string_of_ctor lower)) ""]
    )
    @
    [pretty_r max_pos_len (reason_of_t_add_id upper)
      (spf "~> %s " (string_of_ctor upper))
      (if parent = []
        then ""
        else match TraceMap.get parent tmap with
        | Some i -> spf " (from path %d)" (i + 1)
        | None -> " (from [not shown])"
      )
    ]
  in

  let print_path i steps =
    (reason_of_string (spf "* path %d:" (i + 1))) ::
    List.concat (List.mapi (print_step steps) steps)
  in

  List.concat (List.rev (IMap.fold (
    fun i flow acc -> (print_path i flow) :: acc
  ) imap []))

(********* type visitor *********)

(* We walk types in a lot of places for all kinds of things, but often most of
   the code is boilerplate. The following visitor class for types aims to
   reduce that boilerplate. It is designed as a fold on the structure of types,
   parameterized by an accumulator.

   WARNING: This is only a partial implementation, sufficient for current
   purposes but intended to be completed in a later diff.
*)
class ['a] type_visitor = object(self)
  method type_ cx (acc: 'a) = function
  | OpenT (_, id) -> self#id_ cx acc id

  | NumT _
  | StrT _
  | BoolT _
  | UndefT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _ -> acc

  | FunT (_, static, prototype, funtype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc prototype in
    let acc = self#fun_type cx acc funtype in
    acc

  | ObjT (_, { dict_t; props_tmap; proto_t; _ }) ->
    let acc = self#opt (self#dict_ cx) acc dict_t in
    let acc = self#props cx acc props_tmap in
    let acc = self#type_ cx acc proto_t in
    acc

  | ArrT (_, t, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | ClassT t -> self#type_ cx acc t

  | InstanceT (_, static, super, insttype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc super in
    let acc = self#inst_type cx acc insttype in
    acc

  | OptionalT t -> self#type_ cx acc t

  | RestT t -> self#type_ cx acc t

  | PolyT (typeparams, t) ->
    let acc = self#list (self#type_param cx) acc typeparams in
    let acc = self#type_ cx acc t in
    acc

  | TypeAppT (t, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | BoundT typeparam -> self#type_param cx acc typeparam

  | ExistsT _ -> acc

  | MaybeT t -> self#type_ cx acc t

  | IntersectionT (_, ts)
  | UnionT (_, ts) -> self#list (self#type_ cx) acc ts

  | UpperBoundT t
  | LowerBoundT t -> self#type_ cx acc t

  | AnyObjT _
  | AnyFunT _ -> acc

  | ShapeT t -> self#type_ cx acc t

  | DiffT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | KeysT (_, t) -> self#type_ cx acc t

  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _ -> acc

  | TypeT (_, t) -> self#type_ cx acc t

  | AnnotT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | BecomeT (_, t) -> self#type_ cx acc t

  | SpeculativeMatchFailureT (_, t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | ModuleT (_, exporttypes) ->
    self#export_types cx acc exporttypes

  (* Currently not walking use types. This will change in an upcoming diff. *)
  | SummarizeT (_, _)
  | CallT (_, _)
  | MethodT (_, _, _)
  | ReposLowerT (_, _)
  | ReposUpperT (_, _)
  | SetPropT (_, _, _)
  | GetPropT (_, _, _)
  | SetElemT (_, _, _)
  | GetElemT (_, _, _)
  | ConstructorT (_, _, _)
  | SuperT (_, _)
  | ExtendsT (_, _, _)
  | AdderT (_, _, _)
  | ComparatorT (_, _)
  | PredicateT (_, _)
  | EqT (_, _)
  | AndT (_, _, _)
  | OrT (_, _, _)
  | NotT (_, _)
  | SpecializeT (_, _, _, _)
  | LookupT (_, _, _, _, _)
  | ObjAssignT (_, _, _, _, _)
  | ObjFreezeT (_, _)
  | ObjRestT (_, _, _)
  | ObjSealT (_, _)
  | ObjTestT (_, _, _)
  | UnaryMinusT (_, _)
  | UnifyT (_, _)
  | ConcretizeT (_, _, _, _)
  | ConcreteT _
  | GetKeysT (_, _)
  | HasKeyT (_, _)
  | ElemT (_, _, _)
  | CJSRequireT (_, _)
  | ImportModuleNsT (_, _)
  | ImportTypeT (_, _)
  | ImportTypeofT (_, _)
  | CJSExtractNamedExportsT (_, _, _)
  | SetCJSExportT (_, _, _)
  | SetNamedExportsT (_, _, _)
    -> self#__TODO__ cx acc

  (* The default behavior here could be fleshed out a bit, to look up the graph,
     handle Resolved and Unresolved cases, etc. *)
  method id_ cx acc id = acc

  method private dict_ cx acc { key; value; _ } =
    let acc = self#type_ cx acc key in
    let acc = self#type_ cx acc value in
    acc

  method props cx acc id =
    self#smap (self#type_ cx) acc (IMap.find_unsafe id cx.property_maps)

  method private type_param cx acc { bound; _ } =
    self#type_ cx acc bound

  method fun_type cx acc { this_t; params_tlist; return_t; _ } =
    let acc = self#type_ cx acc this_t in
    let acc = self#list (self#type_ cx) acc params_tlist in
    let acc = self#type_ cx acc return_t in
    acc

  method private inst_type cx acc { type_args; fields_tmap; methods_tmap; _ } =
    let acc = self#smap (self#type_ cx) acc type_args in
    let acc = self#props cx acc fields_tmap in
    let acc = self#props cx acc methods_tmap in
    acc

  method private export_types cx acc { exports_tmap; cjs_export } =
    let acc = self#props cx acc exports_tmap in
    let acc = self#opt (self#type_ cx) acc cjs_export in
    acc

  method private __TODO__ cx acc = acc

  method private list: 't. ('a -> 't -> 'a) -> 'a -> 't list -> 'a =
    List.fold_left

  method private opt: 't. ('a -> 't -> 'a) -> 'a -> 't option -> 'a =
    fun f acc -> function
    | None -> acc
    | Some x -> f acc x

  method private smap: 't. ('a -> 't -> 'a) -> 'a -> 't SMap.t -> 'a =
    fun f acc map ->
      SMap.fold (fun _ t acc -> f acc t) map acc
end
