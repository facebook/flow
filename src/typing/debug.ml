(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Json = Hh_json

open Context
open Reason_js
open Type
open Utils

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
  cx: Context.t;
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
    | Constraint_js.Goto id ->
      ["kind", JString "Goto"]
      @ ["id", JInt id]
    | Constraint_js.Root root ->
      ["kind", JString "Root"]
      @ ["root", json_of_root json_cx root]
  )
)

and json_of_root json_cx = check_depth json_of_root_impl json_cx
and json_of_root_impl json_cx root = Json.(Constraint_js.(
  JAssoc ([
    "rank", JInt root.rank;
    "constraints", json_of_constraints json_cx root.constraints
  ])
))

and json_of_constraints json_cx = check_depth json_of_constraints_impl json_cx
and json_of_constraints_impl json_cx constraints = Json.(
  JAssoc (
    match constraints with
    | Constraint_js.Resolved t ->
      ["kind", JString "Resolved"]
      @ ["type", _json_of_t json_cx t]
    | Constraint_js.Unresolved bounds ->
      ["kind", JString "Unresolved"]
      @ ["bounds", json_of_bounds json_cx bounds]
  )
)

and json_of_bounds json_cx = check_depth json_of_bounds_impl json_cx
and json_of_bounds_impl json_cx bounds = Json.(
  match bounds with
  | { Constraint_js.lower; upper; lowertvars; uppertvars; } -> JAssoc ([
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


(* scopes *)

let json_of_scope = Scope.(
  let open Hh_json in

  let json_of_value_impl json_cx {
    Entry.kind; value_state; value_loc; specific; general
  } =
    JAssoc [
      "entry_type", JString "Value";
      "kind", JString (Entry.string_of_value_kind kind);
      "value_state", JString (Entry.string_of_state value_state);
      "value_loc", json_of_loc value_loc;
      "specific", _json_of_t json_cx specific;
      "general", _json_of_t json_cx general;
    ]
  in
  let json_of_value json_cx = check_depth json_of_value_impl json_cx in

  let json_of_type_impl json_cx { Entry.type_state; type_loc; _type } =
    JAssoc [
      "entry_type", JString "Type";
      "type_state", JString (Entry.string_of_state type_state);
      "type_loc", json_of_loc type_loc;
      "_type", _json_of_t json_cx _type;
    ]
  in
  let json_of_type json_cx = check_depth json_of_type_impl json_cx in

  let json_of_entry_impl json_cx = Entry.(function
    | Value r -> json_of_value json_cx r
    | Type r -> json_of_type json_cx r
  ) in
  let json_of_entry json_cx = check_depth json_of_entry_impl json_cx in

  let json_of_entries_impl json_cx entries =
    let props = SMap.fold (fun name entry acc ->
      (name, json_of_entry json_cx entry) :: acc
    ) entries []
    |> List.rev
    in
    JAssoc props
  in
  let json_of_entries json_cx = check_depth json_of_entries_impl json_cx in

  let json_of_refi_impl json_cx { refi_loc; refined; original } =
    JAssoc [
      "refi_loc", json_of_loc refi_loc;
      "refined", _json_of_t json_cx refined;
      "original", _json_of_t json_cx original;
    ]
  in
  let json_of_refi json_cx = check_depth json_of_refi_impl json_cx in

  let json_of_refis_impl json_cx refis =
    let props = KeyMap.fold (fun key refi acc ->
      (Key.string_of_key key, json_of_refi json_cx refi) :: acc
    ) refis []
    |> List.rev
    in
    JAssoc props
  in
  let json_of_refis json_cx = check_depth json_of_refis_impl json_cx in

  let string_of_function_kind = function
  | Ordinary -> "Ordinary"
  | Async -> "Async"
  | Generator -> "Generator"
  in

  let string_of_scope_kind = function
  | VarScope kind -> spf "VarScope %s" (string_of_function_kind kind)
  | LexScope -> "LexScope"
  in

  fun ?(depth=1000) cx scope ->
    let json_cx = { cx; depth; stack = ISet.empty; } in
    JAssoc [
      "kind", JString (string_of_scope_kind scope.kind);
      "entries", json_of_entries json_cx scope.entries;
      "refis", json_of_refis json_cx scope.refis;
    ]
)

let json_of_env ?(depth=1000) cx env =
  Json.JList (List.map (json_of_scope ~depth cx) env)
