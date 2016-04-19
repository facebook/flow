(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js
open Type
open Utils_js

let string_of_pred_ctor = function
  | AndP _ -> "AndP"
  | OrP _ -> "OrP"
  | NotP _ -> "NotP"
  | LeftP _ -> "LeftP"
  | RightP _ -> "RightP"
  | ExistsP -> "ExistsP"
  | VoidP -> "VoidP"
  | NullP -> "NullP"
  | MaybeP -> "MaybeP"
  | BoolP -> "BoolP"
  | StrP -> "StrP"
  | NumP -> "NumP"
  | FunP -> "FunP"
  | ObjP -> "ObjP"
  | ArrP -> "ArrP"
  | SingletonBoolP _ -> "SingletonBoolP"
  | SingletonStrP _ -> "SingletonStrP"
  | SingletonNumP _ -> "SingletonNumP"
  | PropExistsP _ -> "PropExistsP"

let string_of_binary_test_ctor = function
  | InstanceofTest -> "InstanceofTest"
  | SentinelProp _ -> "SentinelProp"

type json_cx = {
  stack: ISet.t;
  depth: int;
  cx: Context.t;
}

let check_depth continuation json_cx =
  let depth = json_cx.depth - 1 in
  if depth < 0
  then fun _ -> Hh_json.JSON_Null
  else continuation { json_cx with depth; }

let rec _json_of_t json_cx = check_depth _json_of_t_impl json_cx
and _json_of_t_impl json_cx t = Hh_json.(
  JSON_Object ([
    "reason", json_of_reason (reason_of_t t);
    "kind", JSON_String (string_of_ctor t)
  ] @
  match t with
  | OpenT (_, id) -> [
      "id", int_ id
    ] @
    if ISet.mem id json_cx.stack then []
    else [
      "node", json_of_node json_cx id
    ]

  | NumT (_, lit) ->
    begin match lit with
    | Literal (_, raw) -> ["literal", JSON_String raw]
    | Truthy -> ["refinement", JSON_String "Truthy"]
    | AnyLiteral -> []
    end

  | StrT (_, lit) -> _json_of_string_literal lit

  | BoolT (_, b) ->
    (match b with
      | Some b -> ["literal", JSON_Bool b]
      | None -> [])

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
    -> []

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
      "tupleType", JSON_Array (List.map (_json_of_t json_cx) tuplet)
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
  | RestT t
  | AbstractT t -> [
      "type", _json_of_t json_cx t
    ]

  | EvalT (t, defer_use_t, id) -> [
      "type", _json_of_t json_cx t;
      "defer_use_type", json_of_defer_use_t json_cx defer_use_t
    ] @
      let evaluated = Context.evaluated json_cx.cx in
      begin match IMap.get id evaluated with
      | None -> []
      | Some t -> [ "result", _json_of_t json_cx t ]
      end

  | PolyT (tparams, t) -> [
      "typeParams", JSON_Array (List.map (json_of_typeparam json_cx) tparams);
      "type", _json_of_t json_cx t
    ]

  | TypeAppT (t, targs) -> [
      "typeArgs", JSON_Array (List.map (_json_of_t json_cx) targs);
      "type", _json_of_t json_cx t
    ]

  | ThisClassT t -> [
      "type", _json_of_t json_cx t
    ]

  | ThisTypeAppT (t, this, targs) -> [
      "typeArgs", JSON_Array (List.map (_json_of_t json_cx) targs);
      "thisArg", _json_of_t json_cx this;
      "type", _json_of_t json_cx t
    ]

  | BoundT tparam -> [
      "typeParam", json_of_typeparam json_cx tparam
    ]

  | ExistsT _ ->
    []

  | MaybeT t -> [
      "type", _json_of_t json_cx t
    ]

  | IntersectionT (_, rep) -> [
      let ts = InterRep.members rep in
      "types", JSON_Array (List.map (_json_of_t json_cx) ts)
    ]

  | UnionT (_, rep) -> [
      let ts = UnionRep.members rep in
      "types", JSON_Array (List.map (_json_of_t json_cx) ts)
    ]

  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t -> [
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
      "literal", JSON_String s
    ]

  | SingletonNumT (_, (_, raw)) -> [
      "literal", JSON_String raw
    ]

  | SingletonBoolT (_, b) -> [
      "literal", JSON_Bool b
    ]

  | TypeT (_, t) -> [
      "result", _json_of_t json_cx t
    ]

  | AnnotT (t1, t2) -> [
      "assert", _json_of_t json_cx t1;
      "assume", _json_of_t json_cx t2
    ]

  | SpeculativeMatchT (_, attempt, target) -> [
      "attemptType", _json_of_t json_cx attempt;
      "targetType", _json_of_use_t json_cx target
    ]

  | ModuleT (_, {exports_tmap; cjs_export;}) ->
    let property_maps = Context.property_maps json_cx.cx in
    let tmap = IMap.find_unsafe exports_tmap property_maps in
    let cjs_export = match cjs_export with
    | Some(t) -> _json_of_t json_cx t
    | None -> JSON_Null
    in
    [
      "namedExports", json_of_tmap json_cx tmap;
      "cjsExport", cjs_export;
    ]

  | ReposUpperT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ExtendsT (_, t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | CustomFunT (_, kind) -> [
      "kind", JSON_String (match kind with
      | ObjectAssign -> "Object.assign"
      | ObjectGetPrototypeOf -> "Object.getPrototypeOf"
      | PromiseAll -> "Promise.all"
      | Merge -> "merge"
      | MergeDeepInto -> "mergeDeepInto"
      | MergeInto -> "mergeInto"
      | Mixin -> "mixin"
      );
    ]
  )
)

and _json_of_import_kind = Hh_json.(function
  | ImportType -> JSON_String "ImportType"
  | ImportTypeof -> JSON_String "ImportTypeof"
  | ImportValue -> JSON_String "ImportValue"
)

and _json_of_string_literal = Hh_json.(function
  | Literal s -> ["literal", JSON_String s]
  | Truthy -> ["refinement", JSON_String "Truthy"]
  | AnyLiteral -> []
)

and _json_of_use_t json_cx = check_depth _json_of_use_t_impl json_cx
and _json_of_use_t_impl json_cx t = Hh_json.(
  JSON_Object ([
    "reason", json_of_reason (reason_of_use_t t);
    "kind", JSON_String (string_of_use_ctor t)
  ] @
  match t with
  | UseT t -> [
      "type", _json_of_t json_cx t
    ]

  | BecomeT (_, t) -> [
      "result", _json_of_t json_cx t
    ]

  | SummarizeT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | BindT (_, funtype)
  | CallT (_, funtype) -> [
      "funType", json_of_funtype json_cx funtype
    ]

  | ApplyT (_, l, funtype) -> [
      "lower", _json_of_t json_cx l;
      "funType", json_of_funtype json_cx funtype
    ]

  | MethodT (_, name, funtype) -> [
      "name", json_of_propname json_cx name;
      "funType", json_of_funtype json_cx funtype
    ]

  | ReposLowerT (_, use_t) -> [
      "type", _json_of_use_t json_cx use_t
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
      "typeParams", JSON_Array (List.map (_json_of_t json_cx) tparams);
      "type", _json_of_t json_cx t
    ]

  | SuperT (_, instance) -> [
      "instance", json_of_insttype json_cx instance
    ]

  | MixinT (_, t) -> [
      "type", _json_of_t json_cx t
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

  | AndT (_, _, right, res)
  | OrT (_, _, right, res) -> [
      "rightType", _json_of_t json_cx right;
      "resultType", _json_of_t json_cx res
    ]

  | NotT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ReifyTypeT (_, t_out) -> [
      "t_out", _json_of_t json_cx t_out
    ]

  | SpecializeT (_, cache, targs, tvar) -> [
      "cache", JSON_Bool cache;
      "types", JSON_Array (List.map (_json_of_t json_cx) targs);
      "tvar", _json_of_t json_cx tvar
    ]

  | ThisSpecializeT (_, this, tvar) -> [
      "type", _json_of_t json_cx this;
      "tvar", _json_of_t json_cx tvar
    ]

  | VarianceCheckT (_, targs, polarity) -> [
      "types", JSON_Array (List.map (_json_of_t json_cx) targs);
      "polarity", json_of_polarity json_cx polarity
    ]

  | LookupT (_, rstrict, _, name, t) ->
    (match rstrict with
      | None -> []
      | Some r -> ["strictReason", json_of_reason r]
    ) @ [
      "name", JSON_String name;
      "type", _json_of_t json_cx t
    ]

  | ObjAssignT (_, assignee, tvar, prop_names, flag) -> [
      "assigneeType", _json_of_t json_cx assignee;
      "resultType", _json_of_t json_cx tvar;
      "propNames", JSON_Array (List.map (fun s -> JSON_String s) prop_names);
      "flag", JSON_Bool flag
    ]

  | ObjFreezeT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ObjRestT (_, excludes, tvar) -> [
      "excludedProps", JSON_Array (List.map (fun s -> JSON_String s) excludes);
      "resultType", _json_of_t json_cx tvar;
    ]

  | ObjSealT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ObjTestT (_, default, res) -> [
      "defaultType", _json_of_t json_cx default;
      "resultType", _json_of_t json_cx res
    ]

  | ArrRestT (_, i, t) -> [
      "index", JSON_Number (string_of_int i);
      "resultType", _json_of_t json_cx t
    ]

  | UnifyT (t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | ConcretizeLowerT (l, todo_list, done_list, u)
  | ConcretizeUpperT (l, todo_list, done_list, u) -> [
      "lowerType", _json_of_t json_cx l;
      "todoTypes", JSON_Array (List.map (_json_of_t json_cx) todo_list);
      "doneTypes", JSON_Array (List.map (_json_of_t json_cx) done_list);
      "upperType", _json_of_use_t json_cx u
    ]

  | GetKeysT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | HasOwnPropT (_, key) -> [
      "key", JSON_Object (_json_of_string_literal key)
    ]

  | HasPropT (_, strict, key) ->
    (match strict with
      | None -> []
      | Some r -> ["strictReason", json_of_reason r]
    ) @ [
      "key", JSON_Object (_json_of_string_literal key)
    ]

  | ElemT (_, base, elem) -> [
      "baseType", _json_of_t json_cx base;
      "elemType", _json_of_t json_cx elem
    ]

  | CJSRequireT (_, export) -> [
      "export",
      _json_of_t json_cx export
    ]
  | ImportModuleNsT (_, t) -> [
      "t_out", _json_of_t json_cx t
    ]
  | ImportDefaultT (_, import_kind, (local_name, module_name), t) -> [
      "import_kind", _json_of_import_kind import_kind;
      "local_name", JSON_String local_name;
      "module_name", JSON_String module_name;
      "t_out", _json_of_t json_cx t;
    ]
  | ImportNamedT (_, import_kind, export_name, t) -> [
      "import_kind", _json_of_import_kind import_kind;
      "export_name", JSON_String export_name;
      "t_out", _json_of_t json_cx t;
    ]
  | ImportTypeT (_, export_name, t)
  | ImportTypeofT (_, export_name, t) -> [
      "export_name", JSON_String export_name;
      "t_out", _json_of_t json_cx t;
    ]
  | AssertImportIsValueT (_, name) -> [
      "name", JSON_String name;
    ]

  | CJSExtractNamedExportsT (_, module_t, t_out) -> [
      "module", _json_of_t json_cx module_t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | ExportNamedT (_, tmap, t_out) -> [
      "tmap", json_of_tmap json_cx tmap;
      "t_out", _json_of_t json_cx t_out;
    ]
  | ExportStarFromT (_, target_module_t, t_out) -> [
      "target_module_t", _json_of_t json_cx target_module_t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | DebugPrintT _reason -> []
  | TupleMapT (_, t, t_out) -> [
      "t", _json_of_t json_cx t;
      "t_out", _json_of_t json_cx t_out;
    ]
  )
)

and json_of_polarity json_cx = check_depth json_of_polarity_impl json_cx
and json_of_polarity_impl _json_cx polarity =
  Hh_json.JSON_String (match polarity with
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"
)

and json_of_typeparam json_cx = check_depth json_of_typeparam_impl json_cx
and json_of_typeparam_impl json_cx tparam = Hh_json.(
  JSON_Object [
    "reason", json_of_reason tparam.reason;
    "name", JSON_String tparam.name;
    "bound", _json_of_t json_cx tparam.bound;
    "polarity", json_of_polarity json_cx tparam.polarity;
  ]
)

and json_of_objtype json_cx = check_depth json_of_objtype_impl json_cx
and json_of_objtype_impl json_cx objtype = Hh_json.(
  let property_maps = Context.property_maps json_cx.cx in
  let tmap = IMap.find_unsafe objtype.props_tmap property_maps in
  JSON_Object ([
    "flags", json_of_flags json_cx objtype.flags;
  ] @ (match objtype.dict_t with
    | None -> []
    | Some d -> ["dictType", json_of_dicttype json_cx d]
  ) @ [
    "propTypes", json_of_tmap json_cx tmap;
    "prototype", _json_of_t json_cx objtype.proto_t
  ])
)

and json_of_dicttype json_cx = check_depth json_of_dicttype_impl json_cx
and json_of_dicttype_impl json_cx dicttype = Hh_json.(
  JSON_Object (
    (match dicttype.dict_name with
    | None -> []
    | Some name -> ["name", JSON_String name]
  ) @ [
    "keyType", _json_of_t json_cx dicttype.key;
    "valueType", _json_of_t json_cx dicttype.value
  ])
)

and json_of_flags json_cx = check_depth json_of_flags_impl json_cx
and json_of_flags_impl _json_cx flags = Hh_json.(
  JSON_Object [
    "frozen", JSON_Bool flags.frozen;
    "sealed", JSON_Bool (match flags.sealed with
      | Sealed -> true
      | UnsealedInFile _ -> false);
    "exact", JSON_Bool flags.exact;
  ]
)

and json_of_changeset json_cx = check_depth json_of_changeset_impl json_cx
and json_of_changeset_impl _json_cx = Hh_json.(

  let json_of_entry_ref (scope_id, name, op) =
    JSON_Object [
      "scope_id", int_ scope_id;
      "name", JSON_String name;
      "op", JSON_String (Changeset.string_of_op op)
    ]
  in

  let json_of_changed_vars changed_vars =
    JSON_Array (List.rev (Changeset.EntryRefSet.fold
      (fun entry_ref acc -> json_of_entry_ref entry_ref :: acc)
      changed_vars []
    ))
  in

  let json_of_refi_ref (scope_id, key, op) =
    JSON_Object [
      "scope_id", int_ scope_id;
      "key", JSON_String (Key.string_of_key key);
      "op", JSON_String (Changeset.string_of_op op)
    ]
  in

  let json_of_changed_refis changed_refis =
    JSON_Array (List.rev (Changeset.RefiRefSet.fold
      (fun refi_ref acc -> json_of_refi_ref refi_ref :: acc)
      changed_refis []
    ))
  in

  fun (changed_vars, changed_refis) ->
    JSON_Object [
      "vars", json_of_changed_vars changed_vars;
      "refis", json_of_changed_refis changed_refis
    ]
)

and json_of_funtype json_cx = check_depth json_of_funtype_impl json_cx
and json_of_funtype_impl json_cx {
  this_t;
  params_tlist;
  params_names;
  return_t;
  closure_t;
  changeset
} = Hh_json.(
  JSON_Object ([
    "thisType", _json_of_t json_cx this_t;
    "paramTypes", JSON_Array (List.map (_json_of_t json_cx) params_tlist)
  ] @ (match params_names with
    | None -> []
    | Some names -> ["paramNames", JSON_Array (List.map (fun s -> JSON_String s) names)]
  ) @ [
    "returnType", _json_of_t json_cx return_t;
    "closureIndex", int_ closure_t;
    "changeset", json_of_changeset json_cx changeset
  ])
)

and json_of_insttype json_cx = check_depth json_of_insttype_impl json_cx
and json_of_insttype_impl json_cx insttype = Hh_json.(
  let property_maps = Context.property_maps json_cx.cx in
  let field_tmap = IMap.find_unsafe insttype.fields_tmap property_maps in
  let method_tmap = IMap.find_unsafe insttype.methods_tmap property_maps in
  JSON_Object [
    "classId", int_ insttype.class_id;
    "typeArgs", json_of_tmap json_cx insttype.type_args;
    "argPolarities", json_of_polarity_map json_cx insttype.arg_polarities;
    "fieldTypes", json_of_tmap json_cx field_tmap;
    "methodTypes", json_of_tmap json_cx method_tmap;
    "mixins", JSON_Bool insttype.mixins;
    "structural", JSON_Bool insttype.structural;
  ]
)

and json_of_selector json_cx = check_depth json_of_selector_impl json_cx
and json_of_selector_impl json_cx = Hh_json.(function
  | Prop x -> JSON_Object [
      "propName", JSON_String x;
    ]
  | Elem key -> JSON_Object [
      "keyType", _json_of_t json_cx key;
    ]
  | ObjRest excludes -> JSON_Object [
      "excludedProps", JSON_Array (List.map (fun s -> JSON_String s) excludes);
    ]
  | ArrRest i -> JSON_Object [
      "index", JSON_Number (string_of_int i);
    ]
  | Default -> JSON_Object [
      "default", JSON_Bool true
  ]
)

and json_of_polarity_map json_cx = check_depth json_of_polarity_map_impl json_cx
and json_of_polarity_map_impl json_cx pmap = Hh_json.(
  let lst = SMap.fold (fun name pol acc ->
    JSON_Object ["name", JSON_String name; "polarity", json_of_polarity json_cx pol] :: acc
  ) pmap [] in
  JSON_Array (List.rev lst)
)

and json_of_propname json_cx = check_depth json_of_propname_impl json_cx
and json_of_propname_impl _json_cx (reason, literal) = Hh_json.(
  JSON_Object [
    "reason", json_of_reason reason;
    "literal", JSON_String literal;
  ]
)

and json_of_tmap json_cx = check_depth json_of_tmap_impl json_cx
and json_of_tmap_impl json_cx bindings = Hh_json.(
  let lst = SMap.fold (fun name t acc ->
    json_of_type_binding json_cx (name, t) :: acc
  ) bindings [] in
  JSON_Array (List.rev lst)
)

and json_of_defer_use_t json_cx = check_depth json_of_defer_use_t_impl json_cx
and json_of_defer_use_t_impl json_cx = Hh_json.(function
  | DestructuringT (_, s) -> JSON_Object [
      "selector", json_of_selector json_cx s
    ]
)

and json_of_type_binding json_cx = check_depth json_of_type_binding_impl json_cx
and json_of_type_binding_impl json_cx (name, t) = Hh_json.(
  JSON_Object ["name", JSON_String name; "type", _json_of_t json_cx t]
)

and json_of_pred json_cx = check_depth json_of_pred_impl json_cx
and json_of_pred_impl json_cx p = Hh_json.(
  JSON_Object ([
    "kind", JSON_String (string_of_pred_ctor p)
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

  | SingletonBoolP value -> ["value", JSON_Bool value]
  | SingletonStrP str -> ["value", JSON_String str]
  | SingletonNumP (_,raw) -> ["value", JSON_String raw]

  | PropExistsP key -> ["propName", JSON_String key]

  | ExistsP
  | VoidP
  | NullP
  | MaybeP
  | BoolP
  | StrP
  | NumP
  | FunP
  | ObjP
  | ArrP
      -> []
))

and json_of_binary_test json_cx = check_depth json_of_binary_test_impl json_cx
and json_of_binary_test_impl _json_cx b = Hh_json.(
  JSON_Object ([
    "kind", JSON_String (string_of_binary_test_ctor b)
  ] @
  match b with
  | InstanceofTest -> []
  | SentinelProp s -> ["key", JSON_String s]
))

and json_of_node json_cx = check_depth json_of_node_impl json_cx
and json_of_node_impl json_cx id = Hh_json.(
  JSON_Object (
    let json_cx = { json_cx with stack = ISet.add id json_cx.stack } in
    match IMap.find_unsafe id (Context.graph json_cx.cx) with
    | Constraint_js.Goto id ->
      ["kind", JSON_String "Goto"]
      @ ["id", int_ id]
    | Constraint_js.Root root ->
      ["kind", JSON_String "Root"]
      @ ["root", json_of_root json_cx root]
  )
)

and json_of_root json_cx = check_depth json_of_root_impl json_cx
and json_of_root_impl json_cx root = Hh_json.(Constraint_js.(
  JSON_Object ([
    "rank", int_ root.rank;
    "constraints", json_of_constraints json_cx root.constraints
  ])
))

and json_of_constraints json_cx = check_depth json_of_constraints_impl json_cx
and json_of_constraints_impl json_cx constraints = Hh_json.(
  JSON_Object (
    match constraints with
    | Constraint_js.Resolved t ->
      ["kind", JSON_String "Resolved"]
      @ ["type", _json_of_t json_cx t]
    | Constraint_js.Unresolved bounds ->
      ["kind", JSON_String "Unresolved"]
      @ ["bounds", json_of_bounds json_cx bounds]
  )
)

and json_of_bounds json_cx = check_depth json_of_bounds_impl json_cx
and json_of_bounds_impl json_cx bounds = Hh_json.(
  match bounds with
  | { Constraint_js.lower; upper; lowertvars; uppertvars; } -> JSON_Object ([
      "lower", json_of_tkeys json_cx lower;
      "upper", json_of_use_tkeys json_cx upper;
      "lowertvars", json_of_tvarkeys json_cx lowertvars;
      "uppertvars", json_of_tvarkeys json_cx uppertvars;
    ])
)

and json_of_tkeys json_cx = check_depth json_of_tkeys_impl json_cx
and json_of_tkeys_impl json_cx tmap = Hh_json.(
  JSON_Array (TypeMap.fold (fun t _ acc -> _json_of_t json_cx t :: acc) tmap [])
)

and json_of_use_tkeys json_cx = check_depth json_of_use_tkeys_impl json_cx
and json_of_use_tkeys_impl json_cx tmap = Hh_json.(
  JSON_Array (UseTypeMap.fold (fun t _ acc -> _json_of_use_t json_cx t :: acc) tmap [])
)

and json_of_tvarkeys json_cx = check_depth json_of_tvarkeys_impl json_cx
and json_of_tvarkeys_impl _json_cx imap = Hh_json.(
  JSON_Array (IMap.fold (fun i _ acc -> ((int_ i) :: acc)) imap [])
)

let json_of_t ?(depth=1000) cx t =
  let json_cx = { cx; depth; stack = ISet.empty; } in
  _json_of_t json_cx t

let json_of_use_t ?(depth=1000) cx use_t =
  let json_cx = { cx; depth; stack = ISet.empty; } in
  _json_of_use_t json_cx use_t

let jstr_of_t ?(depth=1000) cx t =
  Hh_json.json_to_multiline (json_of_t ~depth cx t)

let jstr_of_use_t ?(depth=1000) cx use_t =
  Hh_json.json_to_multiline (json_of_use_t ~depth cx use_t)

let json_of_graph ?(depth=1000) cx = Hh_json.(
  let entries = IMap.fold (fun id _ entries ->
    let json_cx = { cx; depth; stack = ISet.empty; } in
    (spf "%d" id, json_of_node json_cx id) :: entries
  ) (Context.graph cx) [] in
  JSON_Object (List.rev entries)
)

let jstr_of_graph ?(depth=1000) cx =
  Hh_json.json_to_multiline (json_of_graph ~depth cx)


(* scopes *)

let json_of_scope = Scope.(
  let open Hh_json in

  let json_of_value_impl json_cx {
    Entry.kind; value_state; value_declare_loc; value_assign_loc; specific; general
  } =
    JSON_Object [
      "entry_type", JSON_String "Value";
      "kind", JSON_String (Entry.string_of_value_kind kind);
      "value_state", JSON_String (State.to_string value_state);
      "value_declare_loc", json_of_loc value_declare_loc;
      "value_assign_loc", json_of_loc value_assign_loc;
      "specific", _json_of_t json_cx specific;
      "general", _json_of_t json_cx general;
    ]
  in
  let json_of_value json_cx = check_depth json_of_value_impl json_cx in

  let json_of_type_impl json_cx { Entry.type_state; type_loc; _type } =
    JSON_Object [
      "entry_type", JSON_String "Type";
      "type_state", JSON_String (State.to_string type_state);
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
    JSON_Object props
  in
  let json_of_entries json_cx = check_depth json_of_entries_impl json_cx in

  let json_of_refi_impl json_cx { refi_loc; refined; original } =
    JSON_Object [
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
    JSON_Object props
  in
  let json_of_refis json_cx = check_depth json_of_refis_impl json_cx in

  fun ?(depth=1000) cx scope ->
    let json_cx = { cx; depth; stack = ISet.empty; } in
    JSON_Object [
      "kind", JSON_String (string_of_kind scope.kind);
      "entries", json_of_entries json_cx scope.entries;
      "refis", json_of_refis json_cx scope.refis;
    ]
)

let json_of_env ?(depth=1000) cx env =
  Hh_json.JSON_Array (List.map (json_of_scope ~depth cx) env)

(*****************************************************************)

(* old debug type printer - ad-hoc middle ground between the public
   printer (no internal types, no literal refinements, etc.) and
   json *)
let rec dump_t cx t =
  dump_t_ ISet.empty cx t

and dump_t_ =
  (* we'll want to add more here *)
  let override stack cx t = match t with
    | OpenT (r, id) -> Some (dump_tvar stack cx r id)
    | NumT (_, lit) -> Some (match lit with
        | Literal (_, raw) -> spf "NumT(%s)" raw
        | Truthy -> spf "NumT(truthy)"
        | AnyLiteral -> "NumT")
    | StrT (_, c) -> Some (match c with
        | Literal s -> spf "StrT(%S)" s
        | Truthy -> spf "StrT(truthy)"
        | AnyLiteral -> "StrT")
    | BoolT (_, c) -> Some (match c with
        | Some b -> spf "BoolT(%B)" b
        | None -> "BoolT")
    | EmptyT _
    | MixedT _
    | AnyT _
    | NullT _ -> Some (string_of_ctor t)
    | _ -> None
  in
  fun stack cx t -> Type_printer.(
    type_printer (override stack) string_of_ctor EnclosureNone cx t
  )

and dump_use_t cx t =
  dump_use_t_ ISet.empty cx t

and dump_use_t_ stack cx t = match t with
  | UseT t -> dump_t_ stack cx t

  | SetPropT (_, (_, n), t) ->
      spf "SetPropT(%s: %s)" n (dump_t_ stack cx t)
  | GetPropT (_, (_, n), t) ->
      spf "GetPropT(%s: %s)" n (dump_t_ stack cx t)
  | LookupT (_, _, _, n, t) ->
      spf "LookupT(%s: %s)" n (dump_t_ stack cx t)
  | PredicateT (p, t) -> spf "PredicateT(%s | %s)"
      (string_of_predicate p) (dump_t_ stack cx t)
  | _ -> "" (* TODO *)


(* type variable dumper. abbreviates a few simple cases for readability.
   note: if we turn the tvar record into a datatype, these will give a
   sense of some of the obvious data constructors *)
and dump_tvar stack cx _r id = Constraint_js.(
  let sbounds = if ISet.mem id stack then "(...)" else (
    let stack = ISet.add id stack in
    match IMap.find_unsafe id (Context.graph cx) with
    | Goto id -> spf "Goto TYPE_%d" id
    | Root { rank; constraints = Resolved t } ->
        spf "Root (rank = %d, resolved = %s)"
          rank (dump_t_ stack cx t)
    | Root { rank; constraints = Unresolved bounds } ->
        spf "Root (rank = %d, unresolved = %s)"
          rank (dump_bounds stack cx id bounds)
  ) in
  (spf "TYPE_%d: " id) ^ sbounds
)

and dump_bounds stack cx id bounds = Constraint_js.(match bounds with
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = UseTypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* no inflows or outflows *)
      "(free)"
  | { lower; upper; lowertvars; uppertvars; }
      when upper = UseTypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete inflows *)
      spf "L %s" (dump_tkeys stack cx lower)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = UseTypeMap.empty
      && IMap.cardinal uppertvars = 1 ->
      (* only tvar inflows *)
      spf "LV %s" (dump_tvarkeys cx id lowertvars)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty
      && IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete outflows *)
      spf "U %s" (dump_use_tkeys stack cx upper)
  | { lower; upper; lowertvars; uppertvars; }
      when lower = TypeMap.empty && upper = UseTypeMap.empty
      && IMap.cardinal lowertvars = 1 ->
      (* only tvar outflows *)
      spf "UV %s" (dump_tvarkeys cx id uppertvars)
  | { lower; upper; lowertvars; uppertvars; }
      when IMap.cardinal lowertvars = 1 && IMap.cardinal uppertvars = 1 ->
      (* only concrete inflows/outflows *)
      let l = dump_tkeys stack cx lower in
      let u = dump_use_tkeys stack cx upper in
      if l = u then "= " ^ l
      else "L " ^ l ^ " U " ^ u
  | { lower; upper; lowertvars; uppertvars; } ->
    let slower = if lower = TypeMap.empty then "" else
      spf " lower = %s;" (dump_tkeys stack cx lower) in
    let supper = if upper = UseTypeMap.empty then "" else
      spf " upper = %s;" (dump_use_tkeys stack cx upper) in
    let sltvars = if IMap.cardinal lowertvars <= 1 then "" else
      spf " lowertvars = %s;" (dump_tvarkeys cx id lowertvars) in
    let sutvars = if IMap.cardinal uppertvars <= 1 then "" else
      spf " uppertvars = %s;" (dump_tvarkeys cx id uppertvars) in
    "{" ^ slower ^ supper ^ sltvars ^ sutvars ^ " }"
)

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

and dump_use_tkeys stack cx tmap =
  "[" ^ (
    String.concat "," (
      List.rev (
        UseTypeMap.fold (
          fun t _ acc -> dump_use_t_ stack cx t :: acc
        ) tmap []
      )
    )
  ) ^ "]"

(* dump the keys of a tvar map as a list *)
and dump_tvarkeys _cx self imap =
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


(* scopes and types *)

let string_of_scope_entry = Scope.(

  let string_of_value_binding cx {
    Entry.kind; value_state; value_declare_loc; value_assign_loc; specific; general
  } =
    spf "{ kind: %s; value_state: %s; value_declare_loc: %S; \
      value_assign_loc: %s; specific: %s; general: %s }"
      (Entry.string_of_value_kind kind)
      (State.to_string value_state)
      (string_of_loc value_declare_loc)
      (string_of_loc value_assign_loc)
      (dump_t cx specific)
      (dump_t cx general)
  in

  let string_of_type_binding cx { Entry.type_state; type_loc; _type } =
    spf "{ type_state: %s; type_loc: %S; _type: %s }"
      (State.to_string type_state)
      (string_of_loc type_loc)
      (dump_t cx _type)
  in

  fun cx -> Entry.(function
  | Value r -> spf "Value %s" (string_of_value_binding cx r)
  | Type r -> spf "Type %s" (string_of_type_binding cx r)
  )
)

let string_of_scope_entries cx entries =
  let strings = SMap.fold (fun name entry acc ->
      (spf "%s: %s" name (string_of_scope_entry cx entry))
      :: acc
    ) entries []
    |> String.concat "; \n"
  in spf "[ %s ]" strings

let string_of_scope_refi cx { Scope.refi_loc; refined; original } =
  spf "{ refi_loc: %S; refined: %s; original: %s }"
    (string_of_loc refi_loc)
    (dump_t cx refined)
    (dump_t cx original)


let string_of_scope_refis cx refis = Scope.(
  let strings = KeyMap.fold (fun key refi acc ->
      (spf "%s: %s"
        (Key.string_of_key key)
        (string_of_scope_refi cx refi))
      :: acc
    ) refis []
    |> String.concat ";\n"
  in spf "[ %s ]" strings
)

let string_of_scope cx scope = Scope.(
  spf "{ kind: %s;\nentries:\n%s\nrefis:\n%s\n}"
    (string_of_kind scope.kind)
    (string_of_scope_entries cx scope.entries)
    (string_of_scope_refis cx scope.refis)
)

let string_of_selector = function
  | Elem _ -> "Elem _" (* TODO print info about the key *)
  | Prop x -> spf "Prop %s" x
  | ArrRest i -> spf "ArrRest %i" i
  | ObjRest xs -> spf "ObjRest [%s]" (String.concat "; " xs)
  | Default -> "Default"

let string_of_default = Default.fold
  ~expr:(fun (loc, _) ->
    spf "Expr %s" (string_of_loc loc))
  ~selector:(fun _ str sel ->
    spf "Selector (%s) (%s)" str (string_of_selector sel))
  ~cons:(fun str default ->
    spf "Cons (%s) (%s)" str default)

let debug_flow (l,u) =
  spf "%s ~> %s" (string_of_ctor l) (string_of_use_ctor u)

let debug_count =
  let count = ref 0 in
  fun f ->
    incr count;
    prerr_endlinef "[%d] %s" !count (f())
