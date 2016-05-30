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

  | ExtendsT (_, t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | CustomFunT (_, kind) -> [
      "kind", JSON_String (match kind with
      | ObjectAssign -> "Object.assign"
      | ObjectGetPrototypeOf -> "Object.getPrototypeOf"
      | PromiseAll -> "Promise.all"
      | ReactCreateElement -> "React.createElement"
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
  | UseT (op, t) -> [
      "use", JSON_String (string_of_use_op op);
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

  | ReposUseT (_, op, t) -> [
      "use", JSON_String (string_of_use_op op);
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

  | SpecializeT (_, _, cache, targs, tvar) -> [
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

  | ElemT (_, base, elem, rw) -> [
      "baseType", _json_of_t json_cx base;
      "elemType", _json_of_t json_cx elem;
      "rw", JSON_String (string_of_rw rw);
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

  | ReactCreateElementT (_, t, t_out) -> [
      "config", _json_of_t json_cx t;
      "returnType", _json_of_t json_cx t_out;
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
  JSON_Object ([
    "reason", json_of_reason tparam.reason;
    "name", JSON_String tparam.name;
    "bound", _json_of_t json_cx tparam.bound;
    "polarity", json_of_polarity json_cx tparam.polarity;
  ] @ match tparam.default with
    | None -> []
    | Some t -> ["default", _json_of_t json_cx t])
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

(* debug printer *)

let rec dump_t ?(depth=3) cx t =
  dump_t_ (depth, ISet.empty) cx t

and dump_t_ (depth, tvars) cx t =

  let p ?(reason=true) ?(extra="") t =
    spf "%s (%s%s%s)"
      (string_of_ctor t)
      (if reason then spf "%S" (desc_of_reason (reason_of_t t)) else "")
      (if reason && extra <> "" then ", " else "")
      extra
  in

  let kid = dump_t_ (depth-1, tvars) cx in
  let use_kid = dump_use_t_ (depth-1, tvars) cx in

  let tvar id =
    if ISet.mem id tvars then spf "%d, ^" id else
    let stack = ISet.add id tvars in
    let open Constraint_js in
    match IMap.find_unsafe id (Context.graph cx) with
    | Goto g -> spf "%d, Goto %d" id g
    | Root { constraints = Resolved t; _ } ->
      spf "%d, Resolved %s" id (dump_t_ (depth-1, stack) cx t)
    | Root { constraints = Unresolved { lower; upper; _ }; _ } ->
      if lower = TypeMap.empty && upper = UseTypeMap.empty
      then spf "%d" id
      else spf "%d, [%s], [%s]" id
        (String.concat "; " (List.rev (TypeMap.fold
          (fun t _ acc ->
            dump_t_ (depth-1, stack) cx t :: acc
          ) lower [])))
        (String.concat "; " (List.rev (UseTypeMap.fold
          (fun use_t _ acc ->
            dump_use_t_ (depth-1, stack) cx use_t :: acc
          ) upper [])))
  in

  if depth = 0 then string_of_ctor t
  else match t with
  | OpenT (_, id) -> p ~extra:(tvar id) t
  | NumT (_, lit) -> p ~extra:(match lit with
    | Literal (_, raw) -> raw
    | Truthy -> "truthy"
    | AnyLiteral -> "") t
  | StrT (_, c) -> p ~extra:(match c with
    | Literal s -> spf "%S" s
    | Truthy -> "truthy"
    | AnyLiteral -> "") t
  | BoolT (_, c) -> p ~extra:(match c with
    | Some b -> spf "%B" b
    | None -> "") t
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | FunT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | PolyT _ -> p t
  | ThisClassT _ -> p t
  | BoundT param -> p ~extra:param.name t
  | ExistsT _ -> p t
  | ObjT (_, { props_tmap; _ }) -> p ~extra:(spf "%d" props_tmap) t
  | ArrT (_, elem, tup) -> p ~extra:(spf "%s, %s" (kid elem)
      (spf "[%s]" (String.concat "; " (List.map kid tup)))) t
  | ClassT inst -> p ~reason:false ~extra:(kid inst) t
  | InstanceT (_, _, _, { class_id; _ }) -> p ~extra:(spf "#%d" class_id) t
  | TypeT (_, arg) -> p ~extra:(kid arg) t
  | AnnotT (sink, source) -> p ~reason:false
      ~extra:(spf "%s, %s" (kid sink) (kid source)) t
  | OptionalT arg
  | RestT arg
  | AbstractT arg -> p ~reason:false ~extra:(kid arg) t
  | EvalT (_, expr, id) -> p
      ~extra:(spf "%s, %d" (string_of_defer_use_ctor expr) id) t
  | TypeAppT (base, args) -> p ~reason:false ~extra:(spf "%s, [%s]"
      (kid base) (String.concat "; " (List.map kid args))) t
  | ThisTypeAppT (base, this, args) -> p ~reason:false
      ~extra:(spf "%s, %s, [%s]" (kid base) (kid this)
        (String.concat "; " (List.map kid args))) t
  | MaybeT arg -> p ~reason:false ~extra:(kid arg) t
  | TaintT _ -> p t
  | IntersectionT (_, rep) -> p ~extra:(spf "[%s]"
      (String.concat "; " (List.map kid (InterRep.members rep)))) t
  | UnionT (_, rep) -> p ~extra:(spf "[%s]"
      (String.concat "; " (List.map kid (UnionRep.members rep)))) t
  | SpeculativeMatchT (_, l, next) -> p
      ~extra:(spf "%s, %s" (kid l) (use_kid next)) t
  | AnyWithLowerBoundT arg
  | AnyWithUpperBoundT arg -> p ~reason:false ~extra:(kid arg) t
  | AnyObjT _
  | AnyFunT _ -> p t
  | ShapeT arg -> p ~reason:false ~extra:(kid arg) t
  | DiffT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | KeysT (_, arg) -> p ~extra:(kid arg) t
  | SingletonStrT (_, s) -> p ~extra:(spf "%S" s) t
  | SingletonNumT (_, (_, s)) -> p ~extra:s t
  | SingletonBoolT (_, b) -> p ~extra:(spf "%B" b) t
  | ModuleT _ -> p t
  | ExtendsT (nexts, l, u) -> p ~reason:false ~extra:(spf "[%s], %s, %s"
    (String.concat "; " (List.map kid nexts)) (kid l) (kid u)) t
  | CustomFunT _ -> p t

and dump_use_t ?(depth=3) cx t =
  dump_use_t_ (depth, ISet.empty) cx t

and dump_use_t_ (depth, tvars) cx t =

  let p ?(reason=true) ?(extra="") use_t =
    spf "%s (%s%s%s)"
      (string_of_use_ctor use_t)
      (if reason then spf "%S" (desc_of_reason (reason_of_use_t use_t)) else "")
      (if reason && extra <> "" then ", " else "")
      extra
  in

  let kid t = dump_t_ (depth-1, tvars) cx t in
  let use_kid use_t = dump_use_t_ (depth-1, tvars) cx use_t in

  if depth = 0 then string_of_use_ctor t
  else match t with
  | UseT (use_op, t) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
  | SummarizeT (_, arg) -> p ~extra:(kid arg) t
  | SuperT _ -> p t
  | MixinT (_, arg) -> p ~extra:(kid arg) t
  | ApplyT (_, f, _) -> p ~extra:(kid f) t
  | BindT _ -> p t
  | CallT _ -> p t
  | MethodT (_, (r, name), _) -> p
      ~extra:(spf "(%S, %S)" (desc_of_reason r) name) t
  | SetPropT (_, (r, name), ptype) -> p
      ~extra:(spf "(%S, %S), %s" (desc_of_reason r) name (kid ptype)) t
  | GetPropT (_, (r, name), ptype) -> p
      ~extra:(spf "(%S, %S), %s" (desc_of_reason r) name (kid ptype)) t
  | SetElemT (_, ix, etype) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
  | GetElemT (_, ix, etype) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
  | ConstructorT _ -> p t
  | AdderT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | ComparatorT (_, arg) -> p ~extra:(kid arg) t
  | ReposLowerT (_, arg) -> p ~extra:(use_kid arg) t
  | ReposUseT (_, _, arg) -> p ~extra:(kid arg) t
  | BecomeT (_, arg) -> p ~extra:(kid arg) t
  | PredicateT (pred, arg) -> p ~reason:false
      ~extra:(spf "%s, %s" (string_of_predicate pred) (kid arg)) t
  | EqT (_, arg) -> p ~extra:(kid arg) t
  | AndT (_, _, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | OrT (_, _, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | NotT (_, arg) -> p ~extra:(kid arg) t
  | ReifyTypeT (_, arg) -> p ~extra:(kid arg) t
  | SpecializeT (_, _, b, args, ret) -> p ~extra:(spf "%b, [%s], %s"
      b (String.concat "; " (List.map kid args)) (kid ret)) t
  | ThisSpecializeT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | VarianceCheckT (_, args, pol) -> p ~extra:(spf "[%s], %s"
      (String.concat "; " (List.map kid args)) (Polarity.string pol)) t
  | LookupT (_, _, _, name, ret) -> p ~extra:(spf "%S, %s" name (kid ret)) t
  | UnifyT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | ObjAssignT _
  | ObjFreezeT _
  | ObjRestT _
  | ObjSealT _
  | ObjTestT _
  | ArrRestT _
  | UnaryMinusT _
  | GetKeysT _
  | HasOwnPropT _
  | HasPropT _
  | ElemT _
  | ConcretizeLowerT _
  | ConcretizeUpperT _
  | ImportModuleNsT _
  | ImportDefaultT _
  | ImportNamedT _
  | ImportTypeT _
  | ImportTypeofT _
  | AssertImportIsValueT _
  | CJSRequireT _
  | CJSExtractNamedExportsT _
  | ExportNamedT _
  | ExportStarFromT _
  | DebugPrintT _
  | TupleMapT _
  | ReactCreateElementT _ ->
    p t

let dump_reason cx reason = if Context.should_strip_root cx
  then dump_reason (strip_root (Context.root cx) reason)
  else dump_reason reason

(*****************************************************)

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

let string_of_reason cx reason = if Context.should_strip_root cx
  then string_of_reason (strip_root (Context.root cx) reason)
  else string_of_reason reason

let string_of_file cx =
  let filename = Loc.string_of_filename (Context.file cx) in
  match Context.is_verbose cx with
  | false -> filename
  | true ->
    let root_str = Path.to_string (Context.root cx) ^ Filename.dir_sep in
    if Utils.str_starts_with filename root_str
      then Files_js.relative_path root_str filename
      else filename

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
