(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason
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
  | LatentP _ -> "LatentP"

let string_of_binary_test_ctor = function
  | InstanceofTest -> "InstanceofTest"
  | SentinelProp _ -> "SentinelProp"

let string_of_type_map = function
  | TupleMap -> "TupleMap"
  | ObjectMap -> "ObjectMap"
  | ObjectMapi -> "ObjectMapi"

let string_of_polarity = function
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"

let string_of_rw = function
  | Read -> "Read"
  | Write -> "Write"

type json_cx = {
  stack: ISet.t;
  size: int ref;
  depth: int;
  cx: Context.t;
  strip_root: Path.t option;
}

let check_depth continuation json_cx =
  let depth = json_cx.depth - 1 in
  if depth < 0
  then fun _ -> Hh_json.JSON_Null
  else continuation { json_cx with depth; }

let rec _json_of_t json_cx t =
  count_calls ~counter:json_cx.size ~default:Hh_json.JSON_Null (fun () ->
    check_depth _json_of_t_impl json_cx t
  )

and _json_of_t_impl json_cx t = Hh_json.(
  JSON_Object ([
    "reason", json_of_reason ~strip_root:json_cx.strip_root (reason_of_t t);
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

  | DefT (_, NumT lit) ->
    begin match lit with
    | Literal (_, (_, raw)) -> ["literal", JSON_String raw]
    | Truthy -> ["refinement", JSON_String "Truthy"]
    | AnyLiteral -> []
    end

  | DefT (_, StrT lit) -> _json_of_string_literal lit

  | DefT (_, BoolT b) ->
    (match b with
      | Some b -> ["literal", JSON_Bool b]
      | None -> [])

  | DefT (_, EmptyT)
  | DefT (_, MixedT _)
  | DefT (_, AnyT)
  | DefT (_, NullT)
  | DefT (_, VoidT)
    -> []

  | TaintT _
  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
    -> []

  | DefT (_, FunT (static, proto, funtype)) -> [
      "static", _json_of_t json_cx static;
      "prototype", _json_of_t json_cx proto;
      "funType", json_of_funtype json_cx funtype
    ]

  | DefT (_, ObjT objtype) -> [
      "type", json_of_objtype json_cx objtype
    ]

  | DefT (_, ArrT (ArrayAT (elemt, tuple_types))) -> [
      "kind", JSON_String "Array";
      "elemType", _json_of_t json_cx elemt;
      "tupleType", match tuple_types with
      | Some tuplet -> JSON_Array (List.map (_json_of_t json_cx) tuplet)
      | None -> JSON_Null
    ]

  | DefT (_, ArrT (TupleAT (elemt, tuple_types))) -> [
      "kind", JSON_String "Tuple";
      "elemType", _json_of_t json_cx elemt;
      "tupleType", JSON_Array (List.map (_json_of_t json_cx) tuple_types);
    ]

  | DefT (_, ArrT (ROArrayAT (elemt))) -> [
      "kind", JSON_String "ReadOnlyArray";
      "elemType", _json_of_t json_cx elemt;
    ]

  | DefT (_, ArrT EmptyAT) -> [
      "kind", JSON_String "EmptyArray";
    ]

  | DefT (_, ClassT t) -> [
      "type", _json_of_t json_cx t
    ]

  | DefT (_, InstanceT (static, super, implements, instance)) -> [
      "static", _json_of_t json_cx static;
      "super", _json_of_t json_cx super;
      "implements", JSON_Array (List.map (_json_of_t json_cx) implements);
      "instance", json_of_insttype json_cx instance
    ]

  | DefT (_, OptionalT t)
  | AbstractT (_, t) -> [
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

  | DefT (_, PolyT (tparams, t)) -> [
      "typeParams", JSON_Array (List.map (json_of_typeparam json_cx) tparams);
      "type", _json_of_t json_cx t
    ]

  | DefT (_, TypeAppT (t, targs)) -> [
      "typeArgs", JSON_Array (List.map (_json_of_t json_cx) targs);
      "type", _json_of_t json_cx t
    ]

  | ThisClassT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ThisTypeAppT (_, t, this, targs) -> [
      "typeArgs", JSON_Array (List.map (_json_of_t json_cx) targs);
      "thisArg", _json_of_t json_cx this;
      "type", _json_of_t json_cx t
    ]

  | BoundT tparam -> [
      "typeParam", json_of_typeparam json_cx tparam
    ]

  | ExistsT _ ->
    []

  | ExactT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | DefT (_, MaybeT t) -> [
      "type", _json_of_t json_cx t
    ]

  | DefT (_, IntersectionT rep) -> [
      let ts = InterRep.members rep in
      "types", JSON_Array (List.map (_json_of_t json_cx) ts)
    ]

  | DefT (_, UnionT rep) -> [
      let ts = UnionRep.members rep in
      "types", JSON_Array (List.map (_json_of_t json_cx) ts)
    ]

  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t -> [
      "type", _json_of_t json_cx t
    ]

  | DefT (_, AnyObjT)
  | DefT (_, AnyFunT) ->
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

  | DefT (_, SingletonStrT s) -> [
      "literal", JSON_String s
    ]

  | DefT (_, SingletonNumT (_, raw)) -> [
      "literal", JSON_String raw
    ]

  | DefT (_, SingletonBoolT b) -> [
      "literal", JSON_Bool b
    ]

  | DefT (_, TypeT t) -> [
      "result", _json_of_t json_cx t
    ]

  | AnnotT t -> [
      "assume", _json_of_t json_cx t
    ]

  | TypeMapT (_, kind, t1, t2) -> [
      "kind", JSON_String (string_of_type_map kind);
      "type", _json_of_t json_cx t1;
      "funType", _json_of_t json_cx t2;
    ]

  | ModuleT (_, {exports_tmap; cjs_export; has_every_named_export;}) ->
    let tmap = Context.find_exports json_cx.cx exports_tmap in
    let cjs_export = match cjs_export with
    | Some(t) -> _json_of_t json_cx t
    | None -> JSON_Null
    in
    [
      "namedExports", json_of_tmap json_cx tmap;
      "cjsExport", cjs_export;
      "hasEveryNamedExport", JSON_Bool has_every_named_export;
    ]

  | ExtendsT (_, _, t1, t2) -> [
      "type1", _json_of_t json_cx t1;
      "type2", _json_of_t json_cx t2
    ]

  | ChoiceKitT (_, tool) -> [
      "tool", JSON_String (match tool with
      | Trigger -> "trigger"
      );
    ]

  | CustomFunT (_, kind) -> [
      "kind", JSON_String (match kind with
      | ObjectAssign -> "Object.assign"
      | ObjectGetPrototypeOf -> "Object.getPrototypeOf"
      | ReactPropType _ -> "ReactPropsCheckType"
      | ReactCreateClass -> "React.createClass"
      | ReactCreateElement -> "React.createElement"
      | Merge -> "merge"
      | MergeDeepInto -> "mergeDeepInto"
      | MergeInto -> "mergeInto"
      | Mixin -> "mixin"
      | Idx -> "idx"
      | DebugPrint -> "$Flow$DebugPrint"
      );
    ]

  | IdxWrapper (_, t) -> [
      "wrappedObj", _json_of_t json_cx t
    ]

  | OpenPredT (_,t, pos_preds, neg_preds) -> [
      let json_key_map f map = JSON_Object (
        Key_map.elements map |>
        List.map (Utils_js.map_pair Key.string_of_key f)
      ) in
      let json_pred_key_map = json_key_map (json_of_pred json_cx) in
      "OpenPred", JSON_Object [
        ("base_type", _json_of_t_impl json_cx t);
        ("pos_preds", json_pred_key_map pos_preds);
        ("neg_preds", json_pred_key_map neg_preds)
      ]
    ]

  | ReposT (_, t)
  | ReposUpperT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  )
)

and _json_of_import_kind = Hh_json.(function
  | ImportType -> JSON_String "ImportType"
  | ImportTypeof -> JSON_String "ImportTypeof"
  | ImportValue -> JSON_String "ImportValue"
)

and _json_of_string_literal = Hh_json.(function
  | Literal (_, s) -> ["literal", JSON_String s]
  | Truthy -> ["refinement", JSON_String "Truthy"]
  | AnyLiteral -> []
)

and _json_of_cont json_cx = Hh_json.(function
  | Upper u -> [
      "cont", JSON_String "upper";
      "type", _json_of_use_t json_cx u
    ]
  | Lower l -> [
      "cont", JSON_String "lower";
      "type", _json_of_t json_cx l
    ]
)

and _json_of_use_t json_cx = check_depth _json_of_use_t_impl json_cx
and _json_of_use_t_impl json_cx t = Hh_json.(
  JSON_Object ([
    "reason", json_of_reason ~strip_root:json_cx.strip_root (reason_of_use_t t);
    "kind", JSON_String (string_of_use_ctor t)
  ] @
  match t with
  | UseT (op, t) -> [
      "use", JSON_String (string_of_use_op op);
      "type", _json_of_t json_cx t
    ]

  | AssertArithmeticOperandT _ -> []
  | AssertBinaryInLHST _ -> []
  | AssertBinaryInRHST _ -> []
  | AssertForInRHST _ -> []

  | BecomeT (_, t) -> [
      "result", _json_of_t json_cx t
    ]

  | BindT (_, funtype, pass) -> [
      "funType", json_of_funcalltype json_cx funtype;
      "passThrough", JSON_Bool pass
    ]

  | CallT (_, funtype) -> [
      "funType", json_of_funcalltype json_cx funtype
    ]

  | MethodT (_, _, propref, funtype) -> [
      "propRef", json_of_propref json_cx propref;
      "funType", json_of_funcalltype json_cx funtype
    ]

  | ReposLowerT (_, use_t) -> [
      "type", _json_of_use_t json_cx use_t
    ]

  | ReposUseT (_, op, t) -> [
      "use", JSON_String (string_of_use_op op);
      "type", _json_of_t json_cx t
    ]

  | SetPropT (_, name, t)
  | GetPropT (_, name, t)
  | TestPropT (_, name, t) -> [
      "propRef", json_of_propref json_cx name;
      "propType", _json_of_t json_cx t
    ]

  | SetElemT (_, indext, elemt)
  | GetElemT (_, indext, elemt) -> [
      "indexType", _json_of_t json_cx indext;
      "elemType", _json_of_t json_cx elemt
    ]

  | CallElemT (_, _, indext, funtype) -> [
      "indexType", _json_of_t json_cx indext;
      "funType", json_of_funcalltype json_cx funtype
    ]

  | GetStaticsT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | ConstructorT (_, args, t) -> [
      "argTypes", JSON_Array (List.map (json_of_funcallarg json_cx) args);
      "type", _json_of_t json_cx t
    ]

  | SuperT (_, instance) -> [
      "instance", json_of_insttype json_cx instance
    ]

  | ImplementsT t -> [
      "instance", _json_of_t json_cx t;
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

  | GuardT (p, r, t) -> [
      "pred", json_of_pred json_cx p;
      "result", _json_of_t json_cx r;
      "sink", _json_of_t json_cx t
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

  | SpecializeT (_, _, cache, targs, tvar) -> [
      "cache", json_of_specialize_cache json_cx cache;
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

  | TypeAppVarianceCheckT (_, _, targs) -> [
      "typeArgs", JSON_Array (List.map (fun (t1, t2) ->
        JSON_Object [
          "t1", _json_of_t json_cx t1;
          "t2", _json_of_t json_cx t2;
        ]
      ) targs)
    ]

  | LookupT (_, rstrict, _, propref, action) ->
    (match rstrict with
      | NonstrictReturning None -> []
      | NonstrictReturning (Some (default, result)) -> [
          "defaultType", _json_of_t json_cx default;
          "resultType", _json_of_t json_cx result;
        ]
      | Strict r -> [
          "strictReason", json_of_reason ~strip_root:json_cx.strip_root r
        ]
      | ShadowRead (_, ids) -> [
          "shadowRead", JSON_Array (Nel.to_list ids |> List.map (fun id ->
            JSON_Number (Properties.string_of_id id)
        ))]
      | ShadowWrite ids -> [
          "shadowWrite", JSON_Array (Nel.to_list ids |> List.map (fun id ->
            JSON_Number (Properties.string_of_id id)
        ))]
    ) @ [
      "propref", json_of_propref json_cx propref;
      "action", json_of_lookup_action json_cx action
    ]

  | ObjAssignFromT (_, proto, tvar, prop_names, kind) -> [
      "target", _json_of_t json_cx proto;
      "resultType", _json_of_t json_cx tvar;
      "propNames", JSON_Array (List.map (fun s -> JSON_String s) prop_names);
      "kind", json_of_obj_assign_kind json_cx kind;
  ]

  | ObjAssignToT (_, from, tvar, prop_names, kind) -> [
      "source", _json_of_t json_cx from;
      "resultType", _json_of_t json_cx tvar;
      "propNames", JSON_Array (List.map (fun s -> JSON_String s) prop_names);
      "kind", json_of_obj_assign_kind json_cx kind;
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

  | GetKeysT (_, t) -> [
      "type", _json_of_t json_cx t
    ]

  | HasOwnPropT (_, key) -> [
      "key", JSON_Object (_json_of_string_literal key)
    ]

  | ElemT (_, base, action) -> [
      "baseType", _json_of_t json_cx base;
      match action with
      | ReadElem t -> "readElem", _json_of_t json_cx t
      | WriteElem t -> "writeElem", _json_of_t json_cx t
      | CallElem (_, funtype) -> "callElem", json_of_funcalltype json_cx funtype
    ]

  | MakeExactT (_, cont) -> _json_of_cont json_cx cont

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

  | AssertRestParamT _ -> []

  | CJSExtractNamedExportsT (_, (module_t_reason, exporttypes), t_out) -> [
      "module", _json_of_t json_cx (ModuleT (module_t_reason, exporttypes));
      "t_out", _json_of_t json_cx t_out;
    ]
  | CopyNamedExportsT (_, target_module_t, t_out) -> [
      "target_module_t", _json_of_t json_cx target_module_t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | CopyTypeExportsT (_, target_module_t, t_out) -> [
      "target_module_t", _json_of_t json_cx target_module_t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | ExportNamedT (_, skip_dupes, tmap, t_out) -> [
      "skip_duplicates", JSON_Bool skip_dupes;
      "tmap", json_of_tmap json_cx tmap;
      "t_out", _json_of_t json_cx t_out;
    ]
  | ExportTypeT (_, skip_dupes, name, t, t_out) -> [
      "skip_duplicates", JSON_Bool skip_dupes;
      "name", JSON_String name;
      "tmap", _json_of_t json_cx t;
      "t_out", _json_of_t json_cx t_out;
    ]
  | DebugPrintT _reason -> []

  | MapTypeT (_, kind, t, cont) -> [
      "kind", JSON_String (string_of_type_map kind);
      "t", _json_of_t json_cx t;
      "cont", JSON_Object (_json_of_cont json_cx cont);
    ]

  | ObjSpreadT (_, _, _, tout) -> [
      "t_out", _json_of_t json_cx tout;
    ]

  | ReactKitT (_, React.CreateElement (t, t_out)) -> [
      "config", _json_of_t json_cx t;
      "returnType", _json_of_t json_cx t_out;
    ]

  | ReactKitT _ -> [] (* TODO *)

  | ChoiceKitUseT (_, tool) -> [
      "tool", JSON_String (match tool with
      | FullyResolveType _ -> "fullyResolveType"
      | TryFlow _ -> "tryFlow"
      );
    ]

  | IntersectionPreprocessKitT (_, tool) -> [
      "tool", JSON_String (match tool with
      | ConcretizeTypes _ -> "concretizeTypes"
      | SentinelPropTest _ -> "sentinelPropTest"
      | PropExistsTest _ -> "propExistsTest"
      );
    ]

  | SentinelPropTestT (l, sense, sentinel, result) -> [
      "l", _json_of_t json_cx l;
      "sense", JSON_Bool sense;
      "sentinel", (match sentinel with
      | SentinelStr s -> JSON_String s
      | SentinelNum (_, raw) -> JSON_String raw
      | SentinelBool b -> JSON_Bool b
      | SentinelNull -> JSON_Null
      | SentinelVoid -> JSON_Null (* hmm, undefined doesn't exist in JSON *)
      );
      "result", _json_of_t json_cx result;
    ]
  | IdxUnwrap (_, t_out) -> [
      "t_out", _json_of_t json_cx t_out
    ]
  | IdxUnMaybeifyT (_, t_out) -> [
      "t_out", _json_of_t json_cx t_out
    ]

  | CallLatentPredT (_, sense, offset, l, t) -> [
      "sense", JSON_Bool sense;
      "offset", JSON_Number (spf "%d" offset);
      "t_in", _json_of_t json_cx l;
      "t_out", _json_of_t json_cx t
    ]

  | CallOpenPredT (_, sense, key, l, t) -> [
      "sense", JSON_Bool sense;
      "key", JSON_String (Key.string_of_key key);
      "t_in", _json_of_t json_cx l;
      "t_out", _json_of_t json_cx t
    ]

  | SubstOnPredT (_, subst, t) -> [
      "PredWithSubst", JSON_Object [
        ("subst", JSON_Array (subst |> SMap.elements |>
          List.map (fun (x,k) ->
            JSON_Array [JSON_String x; JSON_String (Key.string_of_key k)])));
        ("pred_t", _json_of_t_impl json_cx t)
      ]
    ]

  | RefineT (_, p, t) -> [
      "Refined", JSON_Object [
        ("pred_t", json_of_pred json_cx p);
        ("refined_t", _json_of_t_impl json_cx t)
      ]
    ]
  | ResolveSpreadT (_, {
    rrt_resolved;
    rrt_unresolved;
    rrt_resolve_to;
  }) -> [
      "resolved", JSON_Array (List.map (fun param ->
        let kind, t = match param with
        | ResolvedArg t -> "ResolvedArg", t
        | ResolvedSpreadArg (r, at) ->
          "ResolvedSpreadArg", DefT (r, ArrT at)
        | ResolvedAnySpreadArg r ->
          "ResolvedAnySpreadArg", DefT (r, AnyT)
        in
        JSON_Object [
          "kind", JSON_String kind;
          "type", _json_of_t_impl json_cx t;
        ]
      ) rrt_resolved);
      "unresolved", JSON_Array (List.map (fun param ->
        let kind, t = match param with
        | UnresolvedArg t -> "UnresolvedArg", t
        | UnresolvedSpreadArg t -> "UnresolvedSpreadArg", t in
        JSON_Object [
          "kind", JSON_String kind;
          "type", _json_of_t_impl json_cx t;
        ]
      ) rrt_unresolved);
      "resolve_to", json_of_resolve_to json_cx rrt_resolve_to;
    ]
  )
)

and json_of_resolve_to json_cx = check_depth json_of_resolve_to_impl json_cx
and json_of_resolve_to_impl json_cx resolve_to = Hh_json.(JSON_Object (
  match resolve_to with
  | ResolveSpreadsToTuple (id, tout) -> [
    "id", JSON_Number (string_of_int id);
    "t_out", _json_of_t json_cx tout;
  ]
  | ResolveSpreadsToArrayLiteral (id, tout) -> [
    "id", JSON_Number (string_of_int id);
    "t_out", _json_of_t json_cx tout;
  ]
  | ResolveSpreadsToArray (tout) -> [
    "t_out", _json_of_t json_cx tout;
  ]
  | ResolveSpreadsToMultiflowCallFull (id, ft)
  | ResolveSpreadsToMultiflowSubtypeFull (id, ft) -> [
    "id", JSON_Number (string_of_int id);
    "funtype", json_of_funtype json_cx ft;
  ]
  | ResolveSpreadsToMultiflowPartial (id, ft, call_reason, tout) -> [
    "id", JSON_Number (string_of_int id);
    "funtype", json_of_funtype json_cx ft;
    "callReason", json_of_reason ~strip_root:json_cx.strip_root call_reason;
    "t_out", _json_of_t json_cx tout;
  ]
  | ResolveSpreadsToCallT (fct, tin) -> [
    "funcalltype", json_of_funcalltype json_cx fct;
    "t_in", _json_of_t json_cx tin;
  ]
))

and json_of_polarity json_cx = check_depth json_of_polarity_impl json_cx
and json_of_polarity_impl _json_cx polarity =
  Hh_json.JSON_String (string_of_polarity polarity)

and json_of_typeparam json_cx = check_depth json_of_typeparam_impl json_cx
and json_of_typeparam_impl json_cx tparam = Hh_json.(
  JSON_Object ([
    "reason", json_of_reason ~strip_root:json_cx.strip_root tparam.reason;
    "name", JSON_String tparam.name;
    "bound", _json_of_t json_cx tparam.bound;
    "polarity", json_of_polarity json_cx tparam.polarity;
  ] @ match tparam.default with
    | None -> []
    | Some t -> ["default", _json_of_t json_cx t])
)

and json_of_objtype json_cx = check_depth json_of_objtype_impl json_cx
and json_of_objtype_impl json_cx objtype = Hh_json.(
  let pmap = Context.find_props json_cx.cx objtype.props_tmap in
  JSON_Object ([
    "flags", json_of_flags json_cx objtype.flags;
  ] @ (match objtype.dict_t with
    | None -> []
    | Some d -> ["dictType", json_of_dicttype json_cx d]
  ) @ [
    "propTypes", json_of_pmap json_cx pmap;
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
  rest_param;
  return_t;
  is_predicate;
  closure_t;
  changeset;
  def_reason;
} = Hh_json.(
  JSON_Object ([
    "thisType", _json_of_t json_cx this_t;
    "paramTypes", JSON_Array (List.map (_json_of_t json_cx) params_tlist)
  ] @ (match params_names with
    | None -> []
    | Some names -> [
        "paramNames",
        JSON_Array (List.map (fun s -> JSON_String s) names)
      ]
  ) @ [
    "restParam", (match rest_param with
    | None -> JSON_Null
    | Some (name, _, t) -> JSON_Object (
      [
        "restParamType", _json_of_t json_cx t;
      ] @ (match name with
        | None -> []
        | Some name -> ["restParamName", JSON_String name])));
    "returnType", _json_of_t json_cx return_t;
    "isPredicate", JSON_Bool is_predicate;
    "closureIndex", int_ closure_t;
    "changeset", json_of_changeset json_cx changeset;
    "defLoc", json_of_reason ~strip_root:json_cx.strip_root def_reason;
  ])
)

and json_of_funcalltype json_cx = check_depth json_of_funcalltype_impl json_cx
and json_of_funcalltype_impl json_cx {
  call_this_t;
  call_args_tlist;
  call_tout;
  call_closure_t;
  call_strict_arity;
} = Hh_json.(
  let arg_types = List.map (json_of_funcallarg json_cx) call_args_tlist in
  JSON_Object ([
    "thisType", _json_of_t json_cx call_this_t;
    "argTypes", JSON_Array arg_types;
    "tout", _json_of_t json_cx call_tout;
    "closureIndex", int_ call_closure_t;
    "strictArity", JSON_Bool call_strict_arity;
  ])
)

and json_of_funcallarg json_cx = check_depth json_of_funcallarg_impl json_cx
and json_of_funcallarg_impl json_cx arg =
  let kind, t = match arg with
  | Arg t -> "argument", t
  | SpreadArg t -> "spread", t
  in

  Hh_json.(JSON_Object ([
    "argKind", JSON_String kind;
    "argType", _json_of_t json_cx t;
  ]))

and json_of_insttype json_cx = check_depth json_of_insttype_impl json_cx
and json_of_insttype_impl json_cx insttype = Hh_json.(
  let field_pmap = Context.find_props json_cx.cx insttype.fields_tmap in
  let method_pmap = Context.find_props json_cx.cx insttype.methods_tmap in
  JSON_Object [
    "classId", int_ insttype.class_id;
    "typeArgs", json_of_tmap json_cx insttype.type_args;
    "argPolarities", json_of_polarity_map json_cx insttype.arg_polarities;
    "fieldTypes", json_of_pmap json_cx field_pmap;
    "methodTypes", json_of_pmap json_cx method_pmap;
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
      "default", JSON_Bool true;
    ]
  | Become -> JSON_Object [
      "become", JSON_Bool true;
    ]
  | Refine p -> JSON_Object [
      "predicate", json_of_pred json_cx p
    ]
)

and json_of_destructor json_cx = check_depth json_of_destructor_impl json_cx
and json_of_destructor_impl json_cx = Hh_json.(function
  | NonMaybeType -> JSON_Object [
      "non null/void", JSON_Bool true;
    ]
  | PropertyType x -> JSON_Object [
      "propName", JSON_String x;
    ]
  | Bind t -> JSON_Object [
      "thisType", _json_of_t json_cx t
    ]
  | SpreadType (exact, ts) -> JSON_Object [
      "spread", JSON_Array (List.map (_json_of_t json_cx) ts);
      "exact", JSON_Bool exact;
    ]
)

and json_of_polarity_map json_cx = check_depth json_of_polarity_map_impl json_cx
and json_of_polarity_map_impl json_cx pmap = Hh_json.(
  let lst = SMap.fold (fun name pol acc ->
    JSON_Object [
      "name", JSON_String name;
      "polarity", json_of_polarity json_cx pol
    ] :: acc
  ) pmap [] in
  JSON_Array (List.rev lst)
)

and json_of_propref json_cx = check_depth json_of_propref_impl json_cx
and json_of_propref_impl json_cx = Hh_json.(function
  | Named (r, x) -> JSON_Object [
      "reason", json_of_reason ~strip_root:json_cx.strip_root r;
      "name", JSON_String x;
    ]
  | Computed t -> JSON_Object [
      "elem", _json_of_t json_cx t
    ]
)

and json_of_tmap json_cx = check_depth json_of_tmap_impl json_cx
and json_of_tmap_impl json_cx bindings = Hh_json.(
  let lst = SMap.fold (fun name t acc ->
    json_of_type_binding json_cx (name, t) :: acc
  ) bindings [] in
  JSON_Array (List.rev lst)
)

and json_of_pmap json_cx = check_depth json_of_pmap_impl json_cx
and json_of_pmap_impl json_cx bindings = Hh_json.(
  let lst = SMap.fold (fun name p acc ->
    json_of_prop_binding json_cx (name, p) :: acc
  ) bindings [] in
  JSON_Array (List.rev lst)
)

and json_of_defer_use_t json_cx = check_depth json_of_defer_use_t_impl json_cx
and json_of_defer_use_t_impl json_cx = Hh_json.(function
  | DestructuringT (_, s) -> JSON_Object [
      "selector", json_of_selector json_cx s
    ]
  | TypeDestructorT (_, s) -> JSON_Object [
      "destructor", json_of_destructor json_cx s
    ]
)

and json_of_prop_binding json_cx = check_depth json_of_prop_binding_impl json_cx
and json_of_prop_binding_impl json_cx (name, p) = Hh_json.(
  JSON_Object [
    "name", JSON_String name;
    "prop", json_of_prop json_cx p;
  ]
)

and json_of_prop json_cx = check_depth json_of_prop_impl json_cx
and json_of_prop_impl json_cx p = Hh_json.(
  JSON_Object (match p with
  | Field (t, polarity) -> [
      "field", _json_of_t json_cx t;
      "polarity", json_of_polarity json_cx polarity
    ]
  | Get t -> [
      "getter", _json_of_t json_cx t;
    ]
  | Set t -> [
      "setter", _json_of_t json_cx t;
    ]
  | GetSet (t1, t2) -> [
      "getter", _json_of_t json_cx t1;
      "setter", _json_of_t json_cx t2;
    ]
  | Method t -> [
      "method", _json_of_t json_cx t;
    ]
))

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
  | SingletonStrP (_, _, str) -> ["value", JSON_String str]
  | SingletonNumP (_, _, (_,raw)) -> ["value", JSON_String raw]

  | PropExistsP (_, key) -> ["propName", JSON_String key]

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

  | LatentP (t,i) -> [
      "latent", JSON_Object [
        ("type", _json_of_t_impl json_cx t);
        ("position", JSON_Number (spf "%d" i))
      ]
    ]
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
    | Constraint.Goto id ->
      ["kind", JSON_String "Goto"]
      @ ["id", int_ id]
    | Constraint.Root root ->
      ["kind", JSON_String "Root"]
      @ ["root", json_of_root json_cx root]
  )
)

and json_of_root json_cx = check_depth json_of_root_impl json_cx
and json_of_root_impl json_cx root = Hh_json.(Constraint.(
  JSON_Object ([
    "rank", int_ root.rank;
    "constraints", json_of_constraints json_cx root.constraints
  ])
))

and json_of_constraints json_cx = check_depth json_of_constraints_impl json_cx
and json_of_constraints_impl json_cx constraints = Hh_json.(
  JSON_Object (
    match constraints with
    | Constraint.Resolved t ->
      ["kind", JSON_String "Resolved"]
      @ ["type", _json_of_t json_cx t]
    | Constraint.Unresolved bounds ->
      ["kind", JSON_String "Unresolved"]
      @ ["bounds", json_of_bounds json_cx bounds]
  )
)

and json_of_bounds json_cx = check_depth json_of_bounds_impl json_cx
and json_of_bounds_impl json_cx bounds = Hh_json.(
  match bounds with
  | { Constraint.lower; upper; lowertvars; uppertvars; } -> JSON_Object ([
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
  let f = fun t _ acc -> _json_of_use_t json_cx t :: acc in
  JSON_Array (UseTypeMap.fold f tmap [])
)

and json_of_tvarkeys json_cx = check_depth json_of_tvarkeys_impl json_cx
and json_of_tvarkeys_impl _json_cx imap = Hh_json.(
  JSON_Array (IMap.fold (fun i _ acc -> ((int_ i) :: acc)) imap [])
)

and json_of_lookup_action json_cx =
  check_depth json_of_lookup_action_impl json_cx
and json_of_lookup_action_impl json_cx action = Hh_json.(
  JSON_Object (
    match action with
    | RWProp (t, rw) -> [
        "kind", JSON_String "RWProp";
        "rw", JSON_String (string_of_rw rw);
        "t", _json_of_t json_cx t
      ]
    | LookupProp (op, p) -> [
        "kind", JSON_String "LookupProp";
        "use", JSON_String (string_of_use_op op);
        "prop", json_of_prop json_cx p;
      ]
    | SuperProp p -> [
        "kind", JSON_String "SuperProp";
        "prop", json_of_prop json_cx p;
      ]
  )
)

and json_of_specialize_cache json_cx =
  check_depth json_of_specialize_cache_impl json_cx
and json_of_specialize_cache_impl json_cx cache = Hh_json.(
  JSON_Object (
    match cache with
    | None -> []
    | Some rs -> [
        "reasons", JSON_Array
          (List.map (json_of_reason ~strip_root:json_cx.strip_root) rs);
      ]
  )
)

and json_of_obj_assign_kind json_cx =
  check_depth json_of_obj_assign_kind_impl json_cx

and json_of_obj_assign_kind_impl _json_cx kind = Hh_json.JSON_String (
  match kind with
  | ObjAssign -> "normal"
  | ObjSpreadAssign -> "spread"
)

let json_of_t ?(size=5000) ?(depth=1000) ?(strip_root=None) cx t =
  let json_cx = {
    cx;
    size = ref size;
    depth;
    stack = ISet.empty;
    strip_root;
  } in
  _json_of_t json_cx t

let json_of_use_t ?(size=5000) ?(depth=1000) ?(strip_root=None) cx use_t =
  let json_cx = {
    cx;
    size = ref size;
    depth;
    stack = ISet.empty;
    strip_root;
  } in
  _json_of_use_t json_cx use_t

let jstr_of_t ?(size=5000) ?(depth=1000) ?(strip_root=None) cx t =
  Hh_json.json_to_multiline (json_of_t ~size ~depth ~strip_root cx t)

let jstr_of_use_t ?(size=5000) ?(depth=1000) ?(strip_root=None) cx use_t =
  Hh_json.json_to_multiline (json_of_use_t ~size ~depth ~strip_root cx use_t)

let json_of_graph ?(size=5000) ?(depth=1000) ?(strip_root=None) cx = Hh_json.(
  let entries = IMap.fold (fun id _ entries ->
    let json_cx = {
      cx;
      size = ref size;
      depth;
      stack = ISet.empty;
      strip_root;
    } in
    (spf "%d" id, json_of_node json_cx id) :: entries
  ) (Context.graph cx) [] in
  JSON_Object (List.rev entries)
)

let jstr_of_graph ?(size=5000) ?(depth=1000) ?(strip_root=None) cx =
  Hh_json.json_to_multiline (json_of_graph ~size ~depth ~strip_root cx)


(* scopes *)

let json_of_scope = Scope.(
  let open Hh_json in

  let json_of_value_impl json_cx { Entry.
    kind; value_state; value_declare_loc; value_assign_loc; specific; general
  } =
    JSON_Object [
      "entry_type", JSON_String "Value";
      "kind", JSON_String (Entry.string_of_value_kind kind);
      "value_state", JSON_String (State.to_string value_state);
      "value_declare_loc",
        json_of_loc ~strip_root:json_cx.strip_root value_declare_loc;
      "value_assign_loc",
        json_of_loc ~strip_root:json_cx.strip_root value_assign_loc;
      "specific", _json_of_t json_cx specific;
      "general", _json_of_t json_cx general;
    ]
  in
  let json_of_value json_cx = check_depth json_of_value_impl json_cx in

  let json_of_type_impl json_cx { Entry.type_state; type_loc; _type;
                                  type_binding_kind = _ } =
    JSON_Object [
      "entry_type", JSON_String "Type";
      "type_state", JSON_String (State.to_string type_state);
      "type_loc", json_of_loc ~strip_root:json_cx.strip_root type_loc;
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
      "refi_loc", json_of_loc ~strip_root:json_cx.strip_root refi_loc;
      "refined", _json_of_t json_cx refined;
      "original", _json_of_t json_cx original;
    ]
  in
  let json_of_refi json_cx = check_depth json_of_refi_impl json_cx in

  let json_of_refis_impl json_cx refis =
    let props = Key_map.fold (fun key refi acc ->
      (Key.string_of_key key, json_of_refi json_cx refi) :: acc
    ) refis []
    |> List.rev
    in
    JSON_Object props
  in
  let json_of_refis json_cx = check_depth json_of_refis_impl json_cx in

  fun ?(size=5000) ?(depth=1000) ?(strip_root=None) cx scope ->
    let json_cx = {
      cx;
      size = ref size;
      depth;
      stack = ISet.empty;
      strip_root;
    } in
    JSON_Object [
      "kind", JSON_String (string_of_kind scope.kind);
      "entries", json_of_entries json_cx scope.entries;
      "refis", json_of_refis json_cx scope.refis;
    ]
)

let json_of_env ?(size=5000) ?(depth=1000) cx env =
  Hh_json.JSON_Array (List.map (json_of_scope ~size ~depth cx) env)

(*****************************************************************)

(* debug printer *)

let dump_reason cx reason =
  let strip_root = if Context.should_strip_root cx
    then Some (Context.root cx)
    else None in
  Reason.dump_reason ~strip_root reason

let rec dump_t ?(depth=3) cx t =
  dump_t_ (depth, ISet.empty) cx t

and dump_t_ (depth, tvars) cx t =

  let p ?(reason=true) ?(extra="") t =
    spf "%s (%s%s%s)"
      (string_of_ctor t)
      (if reason then spf "%S" (dump_reason cx (reason_of_t t)) else "")
      (if reason && extra <> "" then ", " else "")
      extra
  in

  let kid = dump_t_ (depth-1, tvars) cx in
  let tvar id = dump_tvar_ (depth-1, tvars) cx id in

  let defer_use =
    let string_of_selector = function
    | Prop name -> spf "prop `%s`" name
    | Elem _ -> "elem"
    | ObjRest _ -> "obj rest"
    | ArrRest i -> spf "arr rest at index %d" i
    | Default -> "default"
    | Become -> "become"
    | Refine _ -> "refine"
    in
    let string_of_destructor = function
    | NonMaybeType -> "non-maybe type"
    | PropertyType x -> spf "property type `%s`" x
    | Bind _ -> "bind"
    | SpreadType _ -> "spread"
    in
    fun expr t -> match expr with
    | DestructuringT (_, selector) ->
      spf "Destructure %s on %s" (string_of_selector selector) t
    | TypeDestructorT (_, destructor) ->
      spf "TypeDestruct %s on %s" (string_of_destructor destructor) t
  in

  let string_of_mixed_flavor = function
    | Mixed_everything -> "Mixed_everything"
    | Mixed_truthy -> "Mixed_truthy"
    | Mixed_non_maybe -> "Mixed_non_maybe"
    | Mixed_non_null -> "Mixed_non_null"
    | Mixed_non_void -> "Mixed_non_void"
    | Empty_intersection -> "Empty_intersection"
  in

  let custom_fun =
    let react_prop_type =
      let open React.PropType in
      let complex = function
        | ArrayOf -> "ArrayOf"
        | InstanceOf -> "InstanceOf"
        | ObjectOf -> "ObjectOf"
        | OneOf -> "OneOf"
        | OneOfType -> "OneOfType"
        | Shape -> "Shape"
      in
      function
      | Primitive (is_required, t) ->
        spf "Primitive (%b, %s)" is_required (kid t)
      | Complex kind -> complex kind
    in
    function
    | ObjectAssign -> "ObjectAssign"
    | ObjectGetPrototypeOf -> "ObjectGetPrototypeOf"
    | ReactPropType p -> spf "ReactPropType (%s)" (react_prop_type p)
    | ReactCreateElement -> "ReactCreateElement"
    | ReactCreateClass -> "ReactCreateClass"
    | Merge -> "Merge"
    | MergeDeepInto -> "MergeDeepInto"
    | MergeInto -> "MergeInto"
    | Mixin -> "Mixin"
    | Idx -> "Idx"
    | DebugPrint -> "DebugPrint"
  in

  if depth = 0 then string_of_ctor t
  else match t with
  | OpenT (_, id) -> p ~extra:(tvar id) t
  | DefT (_, NumT lit) -> p ~extra:(match lit with
    | Literal (_, (_, raw)) -> raw
    | Truthy -> "truthy"
    | AnyLiteral -> "") t
  | DefT (_, StrT c) -> p ~extra:(match c with
    | Literal (_, s) -> spf "%S" s
    | Truthy -> "truthy"
    | AnyLiteral -> "") t
  | DefT (_, BoolT c) -> p ~extra:(match c with
    | Some b -> spf "%B" b
    | None -> "") t
  | DefT (_, FunT (_, _, {params_tlist; return_t; this_t; _})) -> p
      ~extra:(spf "<this: %s>(%s) => %s"
        (kid this_t)
        (String.concat "; " (List.map kid params_tlist))
        (kid return_t)) t
  | DefT (_, MixedT flavor) -> p ~extra:(string_of_mixed_flavor flavor) t
  | DefT (_, EmptyT)
  | DefT (_, AnyT)
  | DefT (_, NullT)
  | DefT (_, VoidT)
      -> p t
  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _ -> p t
  | DefT (_, PolyT (tps,c)) -> p ~extra:(spf "%s [%s]"
      (kid c)
      (String.concat "; " (List.map (fun tp -> tp.name) tps))) t
  | ThisClassT (_, inst) -> p ~extra:(kid inst) t
  | BoundT param -> p ~extra:param.name t
  | ExistsT _ -> p t
  | DefT (_, ObjT { props_tmap; _ }) -> p t
      ~extra:(Properties.string_of_id props_tmap)
  | DefT (_, ArrT (ArrayAT (elemt, None))) -> p ~extra:(spf "Array %s" (kid elemt)) t
  | DefT (_, ArrT (ArrayAT (elemt, Some tup))) -> p
      ~extra:(spf "Array %s, %s" (kid elemt)
        (spf "[%s]" (String.concat "; " (List.map kid tup)))) t
  | DefT (_, ArrT (TupleAT (_, tup))) -> p
      ~extra:(spf "Tuple [%s]" (String.concat ", " (List.map kid tup))) t
  | DefT (_, ArrT (ROArrayAT (elemt))) -> p
      ~extra:(spf "ReadOnlyArray %s" (kid elemt)) t
  | DefT (_, ArrT EmptyAT) -> p ~extra:("EmptyArray") t
  | DefT (_, ClassT inst) -> p ~extra:(kid inst) t
  | DefT (_, InstanceT (_, _, _, { class_id; _ })) -> p ~extra:(spf "#%d" class_id) t
  | DefT (_, TypeT arg) -> p ~extra:(kid arg) t
  | AnnotT source -> p ~reason:false
      ~extra:(spf "%s" (kid source)) t
  | DefT (_, OptionalT arg)
  | AbstractT (_, arg) -> p ~extra:(kid arg) t
  | EvalT (arg, expr, id) -> p
      ~extra:(spf "%s, %d" (defer_use expr (kid arg)) id) t
  | DefT (_, TypeAppT (base, args)) -> p ~extra:(spf "%s, [%s]"
      (kid base) (String.concat "; " (List.map kid args))) t
  | ThisTypeAppT (_, base, this, args) -> p ~reason:false
      ~extra:(spf "%s, %s, [%s]" (kid base) (kid this)
        (String.concat "; " (List.map kid args))) t
  | ExactT (_, arg) -> p ~extra:(kid arg) t
  | DefT (_, MaybeT arg) -> p ~extra:(kid arg) t
  | TaintT _ -> p t
  | DefT (_, IntersectionT rep) -> p ~extra:(spf "[%s]"
      (String.concat "; " (List.map kid (InterRep.members rep)))) t
  | DefT (_, UnionT rep) -> p ~extra:(spf "[%s]"
      (String.concat "; " (List.map kid (UnionRep.members rep)))) t
  | AnyWithLowerBoundT arg
  | AnyWithUpperBoundT arg -> p ~reason:false ~extra:(kid arg) t
  | DefT (_, AnyObjT)
  | DefT (_, AnyFunT) -> p t
  | ShapeT arg -> p ~reason:false ~extra:(kid arg) t
  | DiffT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | KeysT (_, arg) -> p ~extra:(kid arg) t
  | DefT (_, SingletonStrT s) -> p ~extra:(spf "%S" s) t
  | DefT (_, SingletonNumT (_, s)) -> p ~extra:s t
  | DefT (_, SingletonBoolT b) -> p ~extra:(spf "%B" b) t
  | ModuleT _ -> p t
  | ExtendsT (_, nexts, l, u) -> p ~extra:(spf "[%s], %s, %s"
    (String.concat "; " (List.map kid nexts)) (kid l) (kid u)) t
  | CustomFunT (_, kind) -> p ~extra:(custom_fun kind) t
  | ChoiceKitT _ -> p t
  | IdxWrapper (_, inner_obj) -> p ~extra:(kid inner_obj) t
  | OpenPredT (_, inner_type, _, _) -> p ~extra:(kid inner_type) t
  | TypeMapT (_, kind, t1, t2) -> p ~extra:(spf "%s, %s, %s"
      (string_of_type_map kind)
      (kid t1)
      (kid t2)) t
  | ReposT (_, arg)
  | ReposUpperT (_, arg) -> p ~extra:(kid arg) t

and dump_use_t ?(depth=3) cx t =
  dump_use_t_ (depth, ISet.empty) cx t

and dump_use_t_ (depth, tvars) cx t =

  let p ?(reason=true) ?(extra="") use_t =
    spf "%s (%s%s%s)"
      (string_of_use_ctor use_t)
      (if reason then spf "%S" (dump_reason cx (reason_of_use_t use_t)) else "")
      (if reason && extra <> "" then ", " else "")
      extra
  in

  let kid t = dump_t_ (depth-1, tvars) cx t in
  let use_kid use_t = dump_use_t_ (depth-1, tvars) cx use_t in
  let tvar id = dump_tvar_ (depth-1, tvars) cx id in
  let prop p = dump_prop_ (depth-1, tvars) cx p in

  let call_arg_kid = function
  | Arg t -> kid t
  | SpreadArg t -> spf "...%s" (kid t)
  in

  let tlist ts = spf "[%s]" (String.concat "; " (List.map kid ts)) in
  let props map = spf "{%s}" (String.concat "; " (
    SMap.fold (fun k p acc ->
      spf "%s = %s" k (prop p) :: acc
    ) map []
  )) in

  let propref = function
    | Named (r, x) -> spf "%S %s" (dump_reason cx r) x
    | Computed t -> kid t
  in

  let lookup_kind = function
  | NonstrictReturning None -> "Nonstrict"
  | NonstrictReturning (Some (t, _)) -> spf "Nonstrict returning %s" (kid t)
  | Strict r -> spf "Strict %S" (dump_reason cx r)
  | ShadowRead (_, ids) -> spf "ShadowRead [%s]"
      (String.concat "; " (Nel.to_list ids |> List.map Properties.string_of_id))
  | ShadowWrite ids -> spf "ShadowWrite [%s]"
      (String.concat "; " (Nel.to_list ids |> List.map Properties.string_of_id))
  in

  let lookup_action = function
  | RWProp (t, Read) -> spf "Read %s" (kid t)
  | RWProp (t, Write) -> spf "Write %s" (kid t)
  | LookupProp (op, p) -> spf "Lookup (%s, %s)" (string_of_use_op op) (prop p)
  | SuperProp p -> spf "Super %s" (prop p)
  in

  let specialize_cache = function
    | None -> "None"
    | Some rs -> spf "Some [%s]"
        (String.concat "; " @@ List.map (dump_reason cx) rs)
  in

  let try_flow = function
    | UnionCases (t, ts) ->
        spf "(%s, [%s])" (kid t) (String.concat "; " (List.map kid ts))
    | IntersectionCases (ts, use_t) ->
        spf "([%s], %s)" (String.concat "; " (List.map kid ts)) (use_kid use_t)
  in

  let react_kit =
    let open React in
    let resolved_object (_, pmap, _, _) = props pmap in
    let resolve_array = function
      | ResolveArray -> "ResolveArray"
      | ResolveElem (todo, done_rev) ->
        spf "ResolveElem (%s, %s)" (tlist todo) (tlist done_rev)
    in
    let resolve_object = function
      | ResolveObject -> "ResolveObject"
      | ResolveDict (_, todo, acc) ->
        spf "ResolveDict (%s, %s)" (props todo) (resolved_object acc)
      | ResolveProp (k, todo, acc) ->
        spf "ResolveProp (%s, %s, %s)" k (props todo) (resolved_object acc)
    in
    let simplify_prop_type = SimplifyPropType.(function
      | ArrayOf -> "ArrayOf"
      | InstanceOf -> "InstanceOf"
      | ObjectOf -> "ObjectOf"
      | OneOf tool -> spf "OneOf (%s)" (resolve_array tool)
      | OneOfType tool -> spf "OneOfType (%s)" (resolve_array tool)
      | Shape tool -> spf "Shape (%s)" (resolve_object tool)
    ) in
    let create_class = CreateClass.(
      let tool = function
        | Spec _ -> "Spec"
        | Mixins _ -> "Mixins"
        | Statics _ -> "Statics"
        | PropTypes (_, tool) ->
          spf "PropTypes (%s)" (resolve_object tool)
        | DefaultProps _ -> "DefaultProps"
        | InitialState _ -> "InitialState"
      in
      let knot {this; static; state_t; default_t} =
        spf "{this = %s; static = %s; state = %s; default = %s}"
          (kid this)
          (kid static)
          (kid state_t)
          (kid default_t)
      in
      fun t k -> spf "%s, %s" (tool t) (knot k)
    ) in
    function
    | CreateElement (config, tout) ->
      spf "CreateElement (%s, %s)" (kid config) (kid tout)
    | SimplifyPropType (tool, tout) ->
      spf "SimplifyPropType (%s, %s)" (simplify_prop_type tool) (kid tout)
    | CreateClass (tool, knot, tout) ->
      spf "CreateClass (%s, %s)" (create_class tool knot) (kid tout)
  in

  let slice (_, props, dict, {exact; _}) =
    let xs = match dict with
    | Some {dict_polarity=p; _} -> [(Polarity.sigil p)^"[]"]
    | None -> []
    in
    let xs = SMap.fold (fun k (t,_) xs ->
      let opt = match t with DefT (_, OptionalT _) -> "?" | _ -> "" in
      (k^opt)::xs
    ) props xs in
    let xs = String.concat "; " xs in
    if exact
    then spf "{|%s|}" xs
    else spf "{%s}" xs
  in

  let object_spread =
    let open ObjectSpread in
    let join = function And -> "And" | Or -> "Or" in
    let resolved xs =
      spf "[%s]" (String.concat "; " (List.map slice (Nel.to_list xs)))
    in
    let resolve = function
      | Next -> "Next"
      | List0 (todo, j) ->
        spf "List0 ([%s], %s)"
          (String.concat "; " (List.map kid (Nel.to_list todo)))
          (join j)
      | List (todo, done_rev, j) ->
        spf "List ([%s], [%s], %s)"
          (String.concat "; " (List.map kid todo))
          (String.concat "; " (List.map resolved (Nel.to_list done_rev)))
          (join j)
    in
    let tool = function
      | Resolve tool -> spf "Resolve %s" (resolve tool)
      | Super (s, tool) -> spf "Super (%s, %s)" (slice s) (resolve tool)
    in
    let state {todo_rev; acc; make_exact} =
      spf "{todo_rev=[%s]; acc=[%s]; make_exact=%b}"
        (String.concat "; " (List.map kid todo_rev))
        (String.concat "; " (List.map resolved acc))
        make_exact
    in
    fun t s ->
      spf "(%s, %s)" (tool t) (state s)
  in

  if depth = 0 then string_of_use_ctor t
  else match t with
  | UseT (use_op, t) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
  | AdderT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | AndT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | ArrRestT _ -> p t
  | AssertArithmeticOperandT _ -> p t
  | AssertBinaryInLHST _ -> p t
  | AssertBinaryInRHST _ -> p t
  | AssertForInRHST _ -> p t
  | AssertImportIsValueT _ -> p t
  | AssertRestParamT _ -> p t
  | BecomeT (_, arg) -> p ~extra:(kid arg) t
  | BindT _ -> p t
  | CallElemT (_, _, ix, _) -> p ~extra:(kid ix) t
  | CallT (_,{call_args_tlist;call_tout;call_this_t;_}) -> p
      ~extra:(spf "<this: %s>(%s) => %s"
        (kid call_this_t)
        (String.concat "; " (List.map call_arg_kid call_args_tlist))
        (kid call_tout)) t
  | CallLatentPredT _ -> p t
  | CallOpenPredT _ -> p t
  | ChoiceKitUseT (_, TryFlow (_, spec)) ->
      p ~extra:(try_flow spec) t
  | ChoiceKitUseT (_, FullyResolveType id) -> p ~extra:(tvar id) t
  | CJSExtractNamedExportsT _ -> p t
  | CJSRequireT _ -> p t
  | ComparatorT (_, arg) -> p ~extra:(kid arg) t
  | ConstructorT _ -> p t
  | CopyNamedExportsT _ -> p t
  | CopyTypeExportsT _ -> p t
  | DebugPrintT _ -> p t
  | ElemT _ -> p t
  | EqT (_, arg) -> p ~extra:(kid arg) t
  | ExportNamedT _ -> p t
  | ExportTypeT _ -> p t
  | GetElemT (_, ix, etype) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
  | GetKeysT _ -> p t
  | GetPropT (_, prop, ptype) -> p ~extra:(spf "(%s), %s"
      (propref prop)
      (kid ptype)) t
  | GetStaticsT (_, arg) -> p ~extra:(kid arg) t
  | GuardT (pred, result, sink) -> p ~reason:false
      ~extra:(spf "%s, %s, %s"
        (string_of_predicate pred) (kid result) (kid sink))
      t
  | HasOwnPropT _ -> p t
  | IdxUnMaybeifyT _ -> p t
  | IdxUnwrap _ -> p t
  | ImportDefaultT _ -> p t
  | ImportModuleNsT _ -> p t
  | ImportNamedT _ -> p t
  | ImportTypeofT _ -> p t
  | ImportTypeT _ -> p t
  | IntersectionPreprocessKitT _ -> p t
  | LookupT (_, kind, _, prop, action) -> p ~extra:(spf "%S, %s, %s"
      (propref prop)
      (lookup_kind kind)
      (lookup_action action)) t
  | MakeExactT _ -> p t
  | MapTypeT _ -> p t
  | MethodT (_, _, prop, _) -> p ~extra:(spf "(%s)" (propref prop)) t
  | MixinT (_, arg) -> p ~extra:(kid arg) t
  | NotT (_, arg) -> p ~extra:(kid arg) t
  | ObjAssignToT _ -> p t
  | ObjAssignFromT _ -> p t
  | ObjFreezeT _ -> p t
  | ObjRestT _ -> p t
  | ObjSealT _ -> p t
  | ObjTestT _ -> p t
  | OrT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | PredicateT (pred, arg) -> p ~reason:false
      ~extra:(spf "%s, %s" (string_of_predicate pred) (kid arg)) t
  | ReactKitT (_, tool) -> p ~extra:(react_kit tool) t
  | RefineT _ -> p t
  | ReposLowerT (_, arg) -> p ~extra:(use_kid arg) t
  | ReposUseT (_, _, arg) -> p ~extra:(kid arg) t
  | ResolveSpreadT (_, {rrt_resolve_to; _;}) ->
      (match rrt_resolve_to with
      | ResolveSpreadsToTuple (_, tout)
      | ResolveSpreadsToArrayLiteral (_, tout)
      | ResolveSpreadsToArray (tout)
      | ResolveSpreadsToMultiflowPartial (_, _, _, tout) ->
        p ~extra:(kid tout) t
      | ResolveSpreadsToCallT (_, tin) ->
        p ~extra:(kid tin) t
      | ResolveSpreadsToMultiflowCallFull _
      | ResolveSpreadsToMultiflowSubtypeFull _ ->
        p t)
  | SentinelPropTestT (l, sense, sentinel, result) -> p ~reason:false
      ~extra:(spf "%s, %b, %s, %s"
        (kid l)
        sense
        (match sentinel with
        | SentinelStr x -> spf "string %s" x
        | SentinelNum (_,x) -> spf "number %s" x
        | SentinelBool x -> spf "boolean %b" x
        | SentinelNull -> "null"
        | SentinelVoid -> "void")
        (kid result))
      t
  | SubstOnPredT _ -> p t
  | SuperT _ -> p t
  | ImplementsT arg -> p ~reason:false ~extra:(kid arg) t
  | SetElemT (_, ix, etype) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
  | SetPropT (_, prop, ptype) -> p ~extra:(spf "(%s), %s"
      (propref prop)
      (kid ptype)) t
  | SpecializeT (_, _, cache, args, ret) -> p ~extra:(spf "%s, [%s], %s"
      (specialize_cache cache) (String.concat "; " (List.map kid args)) (kid ret)) t
  | ObjSpreadT (_, tool, state, arg) -> p ~extra:(spf "%s, %s"
      (object_spread tool state)
      (kid arg)) t
  | TestPropT (_, prop, ptype) -> p ~extra:(spf "(%s), %s"
      (propref prop)
      (kid ptype)) t
  | ThisSpecializeT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | UnaryMinusT _ -> p t
  | UnifyT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
  | VarianceCheckT (_, args, pol) -> p ~extra:(spf "[%s], %s"
      (String.concat "; " (List.map kid args)) (Polarity.string pol)) t
  | TypeAppVarianceCheckT _ -> p t

and dump_tvar ?(depth=3) cx id =
  dump_tvar_ (depth, ISet.empty) cx id

and dump_tvar_ (depth, tvars) cx id =
  if ISet.mem id tvars then spf "%d, ^" id else
  let stack = ISet.add id tvars in
  let open Constraint in
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

and dump_prop ?(depth=3) cx p =
  dump_prop_ (depth, ISet.empty) cx p

and dump_prop_ (depth, tvars) cx p =
  let kid t = dump_t_ (depth-1, tvars) cx t in
  match p with
  | Field (t, polarity) ->
    spf "Field (%s) %s" (string_of_polarity polarity) (kid t)
  | Get t ->
    spf "Get %s" (kid t)
  | Set t ->
    spf "Set %s" (kid t)
  | GetSet (t1, t2) ->
    spf "Get %s Set %s" (kid t1) (kid t2)
  | Method t ->
    spf "Method %s" (kid t)

(*****************************************************)

(* scopes and types *)

let string_of_scope_entry = Scope.(

  let string_of_value_binding cx { Entry.
    kind; value_state; value_declare_loc; value_assign_loc; specific; general
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

  let string_of_type_binding cx { Entry.type_state; type_loc; _type;
                                  type_binding_kind = _ } =
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


let string_of_scope_refis cx refis =
  let strings = Key_map.fold (fun key refi acc ->
      (spf "%s: %s"
        (Key.string_of_key key)
        (string_of_scope_refi cx refi))
      :: acc
    ) refis []
    |> String.concat ";\n"
  in spf "[ %s ]" strings

let string_of_scope cx scope = Scope.(
  spf "{ kind: %s;\nentries:\n%s\nrefis:\n%s\n}"
    (string_of_kind scope.kind)
    (string_of_scope_entries cx scope.entries)
    (string_of_scope_refis cx scope.refis)
)

let string_of_reason cx reason =
  let strip_root = if Context.should_strip_root cx
    then Some (Context.root cx)
    else None in
  Reason.string_of_reason ~strip_root reason

let string_of_file cx =
  let filename = Loc.string_of_filename (Context.file cx) in
  match Context.is_verbose cx with
  | false -> filename
  | true ->
    let root_str = Path.to_string (Context.root cx) ^ Filename.dir_sep in
    if String_utils.string_starts_with filename root_str
      then Files.relative_path root_str filename
      else filename

let string_of_selector = function
  | Elem _ -> "Elem _" (* TODO print info about the key *)
  | Prop x -> spf "Prop %s" x
  | ArrRest i -> spf "ArrRest %i" i
  | ObjRest xs -> spf "ObjRest [%s]" (String.concat "; " xs)
  | Default -> "Default"
  | Become -> "Become"
  | Refine p -> spf "Refine with %s" (string_of_predicate p)

let string_of_destructor = function
  | NonMaybeType -> "NonMaybeType"
  | PropertyType x -> spf "PropertyType %s" x
  | Bind _ -> "Bind"
  | SpreadType _ -> "Spread"

let string_of_default = Default.fold
  ~expr:(fun (loc, _) ->
    spf "Expr %s" (string_of_loc loc))
  ~selector:(fun _ str sel ->
    spf "Selector (%s) (%s)" str (string_of_selector sel))
  ~cons:(fun str default ->
    spf "Cons (%s) (%s)" str default)

let string_of_errors errors =
  Hh_json.json_to_string ~pretty:true
    (Errors.Json_output.json_of_errors ~strip_root:None errors)

let dump_flow_error =
  let open Flow_error in
  let dump_internal_error = function
  | PackageHeapNotFound _ -> "PackageHeapNotFound"
  | AbnormalControlFlow -> "AbnormalControlFlow"
  | UncaughtException _ -> "UncaughtException"
  | MethodNotAFunction -> "MethodNotAFunction"
  | OptionalMethod -> "OptionalMethod"
  | OpenPredWithoutSubst -> "OpenPredWithoutSubst"
  | PredFunWithoutParamNames -> "PredFunWithoutParamNames"
  | UnsupportedGuardPredicate _ -> "UnsupportedGuardPredicate"
  | BreakEnvMissingForCase -> "BreakEnvMissingForCase"
  | PropertyDescriptorPropertyCannotBeRead ->
      "PropertyDescriptorPropertyCannotBeRead"
  | ForInLHS -> "ForInLHS"
  | ForOfLHS -> "ForOfLHS"
  | InstanceLookupComputed -> "InstanceLookupComputed"
  | PropRefComputedOpen -> "PropRefComputedOpen"
  | PropRefComputedLiteral -> "PropRefComputedLiteral"
  | ShadowReadComputed -> "ShadowReadComputed"
  | ShadowWriteComputed -> "ShadowWriteComputed"
  | RestParameterNotIdentifierPattern -> "RestParameterNotIdentifierPattern"
  | InterfaceTypeSpread -> "InterfaceTypeSpread"
  in
  fun ?(depth=3) cx err ->
    match err with
    | EIncompatible (l, u) ->
        spf "EIncompatible (%s, %s)"
          (dump_t ~depth cx l)
          (dump_use_t ~depth cx u)
    | EIncompatibleProp (l, u, reason_prop) ->
        spf "EIncompatibleProp (%s, %s, %s)"
          (dump_t ~depth cx l)
          (dump_use_t ~depth cx u)
          (dump_reason cx reason_prop)
    | EDebugPrint (reason, _) ->
        spf "EDebugPrint (%s, _)" (dump_reason cx reason)
    | EImportValueAsType (reason, str) ->
        spf "EImportValueAsType (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsTypeof (reason, str) ->
        spf "EImportTypeAsTypeof (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsValue (reason, str) ->
        spf "EImportTypeAsValue (%s, %s)" (dump_reason cx reason) str
    | EImportTypeofNamespace (reason, local_name, module_name) ->
        spf "EImportTypeofNamespace (%s, %s, %s)"
          (dump_reason cx reason) local_name module_name
    | ENoDefaultExport (reason, module_name, _) ->
        spf "ENoDefaultExport (%s, %s)" (dump_reason cx reason) module_name
    | EOnlyDefaultExport (reason, export_name) ->
        spf "EOnlyDefaultExport (%s, %s)" (dump_reason cx reason) export_name
    | ENoNamedExport (reason, export_name, _) ->
        spf "ENoNamedExport (%s, %s)" (dump_reason cx reason) export_name
    | EMissingTypeArgs (reason, _) ->
        spf "EMissingTypeArgs (%s, _)" (dump_reason cx reason)
    | EValueUsedAsType (reason1, reason2) ->
        spf "EValueUsedAsType (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EMutationNotAllowed (reason1, reason2) ->
        spf "EMutationNotAllowed (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EExpectedStringLit ((reason1, reason2), expected, literal) ->
        let literal = match literal with
        | Literal (_, str) -> spf "%S" str
        | Truthy -> "truthy"
        | AnyLiteral -> "any"
        in
        spf "EExpectedStringLit ((%s, %s), %S, %S)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          expected
          literal
    | EExpectedNumberLit ((reason1, reason2), (_, expected), literal) ->
        let literal = match literal with
        | Literal (_, (_, raw)) -> spf "%S" raw
        | Truthy -> "truthy"
        | AnyLiteral -> "any"
        in
        spf "EExpectedNumberLit ((%s, %s), %s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          expected
          literal
    | EExpectedBooleanLit ((reason1, reason2), expected, literal) ->
        let literal = match literal with
        | Some b -> spf "%b" b
        | None -> "any"
        in
        spf "EExpectedBooleanLit ((%s, %s), %b, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          expected
          literal
    | EPropNotFound ((prop_reason, obj_reason), _use_op) ->
        spf "EPropNotFound (%s, %s)"
          (dump_reason cx prop_reason)
          (dump_reason cx obj_reason)
    | EPropAccess ((reason1, reason2), x, _, _) ->
        spf "EPropAccess ((%s, %s), %s, _, _)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          (match x with Some x -> spf "%S" x | None -> "(computed)")
    | EPropPolarityMismatch ((reason1, reason2), x, _) ->
        spf "EPropPolarityMismatch ((%s, %s), %s, _)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          (match x with Some x -> spf "%S" x | None -> "(computed)")
    | EPolarityMismatch (tp, p) ->
        spf "EPolarityMismatch (%s %S, %s)"
          (Polarity.string tp.polarity)
          tp.name
          (Polarity.string p)
    | EStrictLookupFailed ((reason1, reason2), reason, x) ->
        spf "EStrictLookupFailed ((%s, %s), %s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          (dump_reason cx reason)
          (match x with Some x -> spf "Some %S" x | None -> "None")
    | EFunCallParam (reason1, reason2) ->
        spf "EFunCallParam (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EFunCallThis (reason1, reason2, reason_call) ->
        spf "EFunCallThis (%s, %s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          (dump_reason cx reason_call)
    | EFunReturn (reason1, reason2) ->
        spf "EFunReturn (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EFunImplicitReturn (reason1, reason2) ->
        spf "EFunImplicitReturn (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EAddition (reason1, reason2) ->
        spf "EAddition (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EAdditionMixed reason ->
        spf "EAdditionMixed (%s)" (dump_reason cx reason)
    | ECoercion (reason1, reason2) ->
        spf "ECoercion (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EComparison (reason1, reason2) ->
        spf "EComparison (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | ETupleArityMismatch ((reason1, reason2), arity1, arity2) ->
        spf "ETupleArityMismatch (%s, %s, %d, %d)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          arity1 arity2
    | ENonLitArrayToTuple (reason1, reason2) ->
        spf "ENonLitArrayToTuple (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | ETupleOutOfBounds ((reason1, reason2), arity1, arity2) ->
        spf "ETupleOutOfBounds (%s, %s, %d, %d)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          arity1 arity2
    | ETupleUnsafeWrite (reason1, reason2) ->
        spf "ETupleUnsafeWrite (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | ESpeculationFailed (l, u, _) ->
        spf "ESpeculationFailed (%s, %s)"
          (dump_t ~depth cx l)
          (dump_use_t ~depth cx u)
    | ESpeculationAmbiguous ((reason1, reason2), _, _, _) ->
        spf "ESpeculationAmbiguous ((%s, %s), _, _, _)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EIncompatibleWithExact (reason1, reason2) ->
        spf "EIncompatibleWithExact (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EUnsupportedExact (reason1, reason2) ->
        spf "EUnsupportedExact (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EIdxArity reason ->
        spf "EIdxArity (%s)" (dump_reason cx reason)
    | EIdxUse1 reason ->
        spf "EIdxUse1 (%s)" (dump_reason cx reason)
    | EIdxUse2 reason ->
        spf "EIdxUse2 (%s)" (dump_reason cx reason)
    | EUnexpectedThisType loc ->
        spf "EUnexpectedThisType (%s)" (string_of_loc loc)
    | EInvalidRestParam reason ->
        spf "EInvalidRestParam (%s)" (dump_reason cx reason)
    | ETypeParamArity (loc, expected) ->
        spf "ETypeParamArity (%s, %d)" (string_of_loc loc) expected
    | ETypeParamMinArity (loc, expected) ->
        spf "ETypeParamMinArity (%s, %d)" (string_of_loc loc) expected
    | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
        spf "ETooManyTypeArgs (%s, %s, %d)"
          (dump_reason cx reason_tapp)
          (dump_reason cx reason_arity)
          maximum_arity
    | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
        spf "ETooFewTypeArgs (%s, %s, %d)"
          (dump_reason cx reason_tapp)
          (dump_reason cx reason_arity)
          minimum_arity
    | EPropertyTypeAnnot loc ->
        spf "EPropertyTypeAnnot (%s)" (string_of_loc loc)
    | EExportsAnnot loc ->
        spf "EExportsAnnot (%s)" (string_of_loc loc)
    | EUnsupportedKeyInObjectType loc ->
        spf "EUnsupportedKeyInObjectType (%s)" (string_of_loc loc)
    | EPredAnnot loc ->
        spf "EPredAnnot (%s)" (string_of_loc loc)
    | ERefineAnnot loc ->
        spf "ERefineAnnot (%s)" (string_of_loc loc)
    | EUnexpectedTypeof loc ->
        spf "EUnexpectedTypeof (%s)" (string_of_loc loc)
    | ECustom ((reason1, reason2), msg) ->
        spf "ECustom (%s, %s, %S)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          msg
    | EInternal (loc, err) ->
        spf "EInternal (%s, %s)" (string_of_loc loc) (dump_internal_error err)
    | EUnsupportedSyntax (loc, _) ->
        spf "EUnsupportedSyntax (%s, _)" (string_of_loc loc)
    | EIllegalName loc ->
        spf "EIllegalName (%s)" (string_of_loc loc)
    | EUseArrayLiteral loc ->
        spf "EUseArrayLiteral (%s)" (string_of_loc loc)
    | EMissingAnnotation reason ->
        spf "EMissingAnnotation (%s)" (dump_reason cx reason)
    | EBindingError (_binding_error, loc, x, entry) ->
        spf "EBindingError (_, %s, %s, %s)"
          (string_of_loc loc)
          x
          (Scope.Entry.string_of_kind entry)
    | ERecursionLimit (reason1, reason2) ->
        spf "ERecursionLimit (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EModuleOutsideRoot (loc, name) ->
        spf "EModuleOutsideRoot (%s, %S)" (string_of_loc loc) name
    | EExperimentalDecorators loc ->
        spf "EExperimentalDecorators (%s)" (string_of_loc loc)
    | EExperimentalClassProperties (loc, static) ->
        spf "EExperimentalClassProperties (%s, %b)" (string_of_loc loc) static
    | EUnsafeGetSet loc ->
        spf "EUnsafeGetSet (%s)" (string_of_loc loc)
    | EExperimentalExportStarAs loc ->
        spf "EExperimentalExportStarAs (%s)" (string_of_loc loc)
    | EIndeterminateModuleType loc ->
        spf "EIndeterminateModuleType (%s)" (string_of_loc loc)
    | EUnreachable loc ->
        spf "EUnreachable (%s)" (string_of_loc loc)
    | EInvalidTypeof (loc, name) ->
        spf "EInvalidTypeof (%s, %S)" (string_of_loc loc) name
    | EBinaryInLHS reason ->
        spf "EBinaryInLHS (%s)" (dump_reason cx reason)
    | EBinaryInRHS reason ->
        spf "EBinaryInRHS (%s)" (dump_reason cx reason)
    | EArithmeticOperand reason ->
        spf "EArithmeticOperand (%s)" (dump_reason cx reason)
    | EForInRHS reason ->
        spf "EForInRHS (%s)" (dump_reason cx reason)
    | EObjectComputedPropertyAccess (reason1, reason2) ->
        spf "EObjectComputedPropertyAccess (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EObjectComputedPropertyAssign (reason1, reason2) ->
        spf "EObjectComputedPropertyAssign (%s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EInvalidLHSInAssignment loc ->
        spf "EInvalidLHSInAssignment (%s)" (string_of_loc loc)
    | EIncompatibleWithUseOp (reason1, reason2, use_op) ->
        spf "EIncompatibleWithUseOp (%s, %s, %s)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
          (string_of_use_op use_op)
    | EUnsupportedImplements reason ->
        spf "EUnsupportedImplements (%s)" (dump_reason cx reason)
    | EReactKit ((reason1, reason2), _) ->
        spf "EReactKit (%s, %s, _)"
          (dump_reason cx reason1)
          (dump_reason cx reason2)
    | EFunctionCallExtraArg (unused_reason, def_reason, param_count) ->
        spf "EFunctionCallExtraArg (%s, %s, %d)"
          (dump_reason cx  unused_reason)
          (dump_reason cx def_reason)
          param_count
