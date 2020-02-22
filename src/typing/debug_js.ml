(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  | ExistsP _ -> "ExistsP"
  | VoidP -> "VoidP"
  | NullP -> "NullP"
  | MaybeP -> "MaybeP"
  | BoolP _ -> "BoolP"
  | StrP _ -> "StrP"
  | NumP _ -> "NumP"
  | FunP -> "FunP"
  | ObjP -> "ObjP"
  | ArrP -> "ArrP"
  | SymbolP _ -> "SymbolP"
  | SingletonBoolP _ -> "SingletonBoolP"
  | SingletonStrP _ -> "SingletonStrP"
  | SingletonNumP _ -> "SingletonNumP"
  | PropExistsP _ -> "PropExistsP"
  | PropNonMaybeP _ -> "PropNonMaybeP"
  | LatentP _ -> "LatentP"

let string_of_binary_test_ctor = function
  | InstanceofTest -> "InstanceofTest"
  | SentinelProp _ -> "SentinelProp"

let string_of_type_map = function
  | TupleMap _ -> "TupleMap"
  | ObjectMap _ -> "ObjectMap"
  | ObjectMapi _ -> "ObjectMapi"

let string_of_polarity = function
  | Polarity.Negative -> "Negative"
  | Polarity.Neutral -> "Neutral"
  | Polarity.Positive -> "Positive"

let string_of_union_enum = function
  | UnionEnum.Str x -> spf "string %s" x
  | UnionEnum.Num (_, x) -> spf "number %s" x
  | UnionEnum.Bool x -> spf "boolean %b" x
  | UnionEnum.Null -> "null"
  | UnionEnum.Void -> "void"

let string_of_sentinel = function
  | UnionEnum.One enum -> string_of_union_enum enum
  | UnionEnum.Many enums ->
    ListUtils.to_string " | " string_of_union_enum @@ UnionEnumSet.elements enums

let string_of_selector = function
  | Elem _ -> "Elem _" (* TODO print info about the key *)
  | Prop (x, _) -> spf "Prop %s" x
  | ArrRest i -> spf "ArrRest %i" i
  | ObjRest xs -> spf "ObjRest [%s]" (String.concat "; " xs)
  | Default -> "Default"

let string_of_destructor = function
  | NonMaybeType -> "NonMaybeType"
  | PropertyType x -> spf "PropertyType %s" x
  | ElementType _ -> "ElementType"
  | Bind _ -> "Bind"
  | ReadOnlyType -> "ReadOnly"
  | SpreadType _ -> "Spread"
  | RestType _ -> "Rest"
  | ValuesType -> "Values"
  | CallType _ -> "CallType"
  | TypeMap (TupleMap _) -> "TupleMap"
  | TypeMap (ObjectMap _) -> "ObjectMap"
  | TypeMap (ObjectMapi _) -> "ObjectMapi"
  | ReactElementPropsType -> "ReactElementProps"
  | ReactElementConfigType -> "ReactElementConfig"
  | ReactElementRefType -> "ReactElementRef"
  | ReactConfigType _ -> "ReactConfig"

let string_of_destruct_kind = function
  | DestructAnnot -> "Annot"
  | DestructInfer -> "Infer"

type json_cx = {
  stack: ISet.t;
  size: int ref;
  depth: int;
  cx: Context.t;
  strip_root: Path.t option;
}

let json_of_aloc ?strip_root ?catch_offset_errors ~offset_table aloc =
  (* Okay because this is only for debugging output *)
  if ALoc.ALocRepresentationDoNotUse.is_abstract aloc then
    Hh_json.(
      let key = ALoc.ALocRepresentationDoNotUse.get_key_exn aloc in
      let source = ALoc.source aloc in
      JSON_Object
        [
          ("source", json_of_source ?strip_root source);
          ("type", json_source_type_of_source source);
          ("key", JSON_Number (ALoc.ALocRepresentationDoNotUse.string_of_key key));
        ])
  else
    json_of_loc ?strip_root ?catch_offset_errors ~offset_table (ALoc.to_loc_exn aloc)

let json_of_reason ?(strip_root = None) ~offset_table r =
  Hh_json.(
    JSON_Object
      [
        ("pos", json_of_aloc ~strip_root ~offset_table (aloc_of_reason r));
        ("desc", JSON_String (string_of_desc (desc_of_reason ~unwrap:false r)));
      ])

let check_depth continuation json_cx =
  let depth = json_cx.depth - 1 in
  if depth < 0 then
    fun _ ->
  Hh_json.JSON_Null
  else
    continuation { json_cx with depth }

let rec _json_of_t json_cx t =
  count_calls ~counter:json_cx.size ~default:Hh_json.JSON_Null (fun () ->
      check_depth _json_of_t_impl json_cx t)

and _json_of_tvar json_cx id =
  Hh_json.(
    [("id", int_ id)]
    @
    if ISet.mem id json_cx.stack then
      []
    else
      [("node", json_of_node json_cx id)])

and _json_of_targ json_cx t =
  Hh_json.(
    JSON_Object
      (match t with
      | ImplicitArg _ -> [("kind", JSON_String "implicit")]
      | ExplicitArg t -> [("kind", JSON_String "explicit"); ("type", _json_of_t json_cx t)]))

and _json_of_t_impl json_cx t =
  Hh_json.(
    JSON_Object
      ( [
          ( "reason",
            json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None (reason_of_t t) );
          ("kind", JSON_String (string_of_ctor t));
        ]
      @
      match t with
      | OpenT (_, id) -> _json_of_tvar json_cx id
      | DefT (_, _, NumT lit) ->
        begin
          match lit with
          | Literal (_, (_, raw)) -> [("literal", JSON_String raw)]
          | Truthy -> [("refinement", JSON_String "Truthy")]
          | AnyLiteral -> []
        end
      | DefT (_, _, StrT lit) -> _json_of_string_literal lit
      | DefT (_, _, BoolT b) ->
        (match b with
        | Some b -> [("literal", JSON_Bool b)]
        | None -> [])
      | DefT (_, _, EmptyT _)
      | DefT (_, _, MixedT _)
      | AnyT _
      | DefT (_, _, SymbolT)
      | DefT (_, _, NullT)
      | DefT (_, _, VoidT) ->
        []
      | NullProtoT _
      | ObjProtoT _
      | FunProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _ ->
        []
      | DefT (_, _, FunT (static, proto, funtype)) ->
        [
          ("static", _json_of_t json_cx static);
          ("prototype", _json_of_t json_cx proto);
          ("funType", json_of_funtype json_cx funtype);
        ]
      | DefT (_, _, ObjT objtype) -> [("type", json_of_objtype json_cx objtype)]
      | DefT (_, _, ArrT (ArrayAT (elemt, tuple_types))) ->
        [
          ("kind", JSON_String "Array");
          ("elemType", _json_of_t json_cx elemt);
          ( "tupleType",
            match tuple_types with
            | Some tuplet -> JSON_Array (Base.List.map ~f:(_json_of_t json_cx) tuplet)
            | None -> JSON_Null );
        ]
      | DefT (_, _, ArrT (TupleAT (elemt, tuple_types))) ->
        [
          ("kind", JSON_String "Tuple");
          ("elemType", _json_of_t json_cx elemt);
          ("tupleType", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) tuple_types));
        ]
      | DefT (_, _, ArrT (ROArrayAT elemt)) ->
        [("kind", JSON_String "ReadOnlyArray"); ("elemType", _json_of_t json_cx elemt)]
      | DefT (_, _, CharSetT chars) ->
        [("chars", JSON_String (String_utils.CharSet.to_string chars))]
      | DefT (_, _, ClassT t) -> [("type", _json_of_t json_cx t)]
      | DefT (_, _, InstanceT (static, super, implements, instance)) ->
        [
          ("static", _json_of_t json_cx static);
          ("super", _json_of_t json_cx super);
          ("implements", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) implements));
          ("instance", json_of_insttype json_cx instance);
        ]
      | DefT (_, _, EnumT enum)
      | DefT (_, _, EnumObjectT enum) ->
        let { enum_id; enum_name; members; representation_t } = enum in
        [
          ("enum_id", JSON_String (ALoc.debug_to_string (enum_id :> ALoc.t)));
          ("enum_name", JSON_String enum_name);
          ("members", JSON_Array (Base.List.map ~f:(fun s -> JSON_String s) (SSet.elements members)));
          ("representation_t", _json_of_t json_cx representation_t);
        ]
      | OptionalT { reason = _; type_ = t; use_desc = _ } -> [("type", _json_of_t json_cx t)]
      | EvalT (t, defer_use_t, id) ->
        [
          ("type", _json_of_t json_cx t); ("defer_use_type", json_of_defer_use_t json_cx defer_use_t);
        ]
        @
        let evaluated = Context.evaluated json_cx.cx in
        begin
          match Eval.Map.find_opt id evaluated with
          | None -> []
          | Some t -> [("result", _json_of_t json_cx t)]
        end
      | DefT (_, _, PolyT { tparams; t_out = t; id; _ }) ->
        [
          ("id", JSON_Number (Poly.string_of_id id));
          ( "typeParams",
            JSON_Array (Base.List.map ~f:(json_of_typeparam json_cx) (Nel.to_list tparams)) );
          ("type", _json_of_t json_cx t);
        ]
      | TypeAppT (_, _, t, targs) ->
        [
          ("typeArgs", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) targs));
          ("type", _json_of_t json_cx t);
        ]
      | ThisClassT (_, t) -> [("type", _json_of_t json_cx t)]
      | ThisTypeAppT (_, t, this, targs_opt) ->
        (match targs_opt with
        | Some targs -> [("typeArgs", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) targs))]
        | None -> [])
        @ [("thisArg", _json_of_t json_cx this); ("type", _json_of_t json_cx t)]
      | BoundT (_, name) -> [("name", JSON_String name)]
      | ExistsT _ -> []
      | ExactT (_, t) -> [("type", _json_of_t json_cx t)]
      | MaybeT (_, t) -> [("type", _json_of_t json_cx t)]
      | IntersectionT (_, rep) ->
        [
          (let ts = InterRep.members rep in
           ("types", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) ts)));
        ]
      | UnionT (_, rep) ->
        [
          (let ts = UnionRep.members rep in
           ("types", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) ts)));
        ]
      | MergedT (_, uses) -> [("uses", JSON_Array (Base.List.map ~f:(_json_of_use_t json_cx) uses))]
      | DefT (_, _, IdxWrapper t) -> [("wrappedObj", _json_of_t json_cx t)]
      | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
        [("config", _json_of_t json_cx config); ("instance", _json_of_t json_cx instance)]
      | ShapeT t -> [("type", _json_of_t json_cx t)]
      | MatchingPropT (_, x, t) -> [("name", JSON_String x); ("type", _json_of_t json_cx t)]
      | KeysT (_, t) -> [("type", _json_of_t json_cx t)]
      | DefT (_, _, SingletonStrT s) -> [("literal", JSON_String s)]
      | DefT (_, _, SingletonNumT (_, raw)) -> [("literal", JSON_String raw)]
      | DefT (_, _, SingletonBoolT b) -> [("literal", JSON_Bool b)]
      | DefT (_, _, TypeT (_, t)) -> [("result", _json_of_t json_cx t)]
      | AnnotT (_, t, use_desc) -> [("type", _json_of_t json_cx t); ("useDesc", JSON_Bool use_desc)]
      | OpaqueT (_, opaquetype) ->
        let t =
          match opaquetype.underlying_t with
          | Some t -> _json_of_t json_cx t
          | None -> JSON_Null
        in
        let st =
          match opaquetype.super_t with
          | Some st -> _json_of_t json_cx st
          | None -> JSON_Null
        in
        [
          ("type", t);
          ("id", JSON_String (ALoc.debug_to_string (opaquetype.opaque_id :> ALoc.t)));
          ("supertype", st);
        ]
      | ModuleT (_, { exports_tmap; cjs_export; has_every_named_export }, is_strict) ->
        let tmap = Context.find_exports json_cx.cx exports_tmap in
        let cjs_export =
          match cjs_export with
          | Some t -> _json_of_t json_cx t
          | None -> JSON_Null
        in
        [
          ("namedExports", json_of_loc_tmap json_cx tmap);
          ("cjsExport", cjs_export);
          ("hasEveryNamedExport", JSON_Bool has_every_named_export);
          ("isStrict", JSON_Bool is_strict);
        ]
      | InternalT (ExtendsT (_, t1, t2)) ->
        [("type1", _json_of_t json_cx t1); ("type2", _json_of_t json_cx t2)]
      | InternalT (ChoiceKitT (_, tool)) ->
        [
          ( "tool",
            JSON_String
              (match tool with
              | Trigger -> "trigger") );
        ]
      | TypeDestructorTriggerT (_, _, _, s, t) ->
        [("destructor", json_of_destructor json_cx s); ("type", _json_of_t json_cx t)]
      | CustomFunT (_, kind) ->
        [("kind", _json_of_custom_fun_kind kind)]
        @
        (match kind with
        | ReactElementFactory t -> [("componentType", _json_of_t json_cx t)]
        | _ -> [])
      | OpenPredT { base_t = t; m_pos = pos_preds; m_neg = neg_preds; reason = _ } ->
        [
          (let json_key_map f map =
             JSON_Object
               (Key_map.elements map |> Base.List.map ~f:(Utils_js.map_pair Key.string_of_key f))
           in
           let json_pred_key_map = json_key_map (json_of_pred json_cx) in
           ( "OpenPred",
             JSON_Object
               [
                 ("base_type", _json_of_t_impl json_cx t);
                 ("pos_preds", json_pred_key_map pos_preds);
                 ("neg_preds", json_pred_key_map neg_preds);
               ] ));
        ]
      | ReposT (_, t)
      | InternalT (ReposUpperT (_, t)) ->
        [("type", _json_of_t json_cx t)] ))

and _json_of_import_kind =
  Hh_json.(
    function
    | ImportType -> JSON_String "ImportType"
    | ImportTypeof -> JSON_String "ImportTypeof"
    | ImportValue -> JSON_String "ImportValue")

and _json_of_string_literal =
  Hh_json.(
    function
    | Literal (_, s) -> [("literal", JSON_String s)]
    | Truthy -> [("refinement", JSON_String "Truthy")]
    | AnyLiteral -> [])

and _json_of_cont json_cx =
  Hh_json.(
    function
    | Upper u -> [("cont", JSON_String "upper"); ("type", _json_of_use_t json_cx u)]
    | Lower (op, l) ->
      [
        ("cont", JSON_String "lower");
        ("use", JSON_String (string_of_use_op op));
        ("type", _json_of_t json_cx l);
      ])

and _json_of_custom_fun_kind kind =
  Hh_json.JSON_String
    (match kind with
    | ObjectAssign -> "Object.assign"
    | ObjectGetPrototypeOf -> "Object.getPrototypeOf"
    | ObjectSetPrototypeOf -> "Object.setPrototypeOf"
    | Compose false -> "Compose"
    | Compose true -> "ComposeReverse"
    | ReactPropType _ -> "ReactPropsCheckType"
    | ReactCreateClass -> "React.createClass"
    | ReactCreateElement -> "React.createElement"
    | ReactCloneElement -> "React.cloneElement"
    | ReactElementFactory _ -> "React.createFactory"
    | Idx -> "idx"
    | TypeAssertIs -> "TypeAssert.is"
    | TypeAssertThrows -> "TypeAssert.throws"
    | TypeAssertWraps -> "TypeAssert.wraps"
    | DebugPrint -> "$Flow$DebugPrint"
    | DebugThrow -> "$Flow$DebugThrow"
    | DebugSleep -> "$Flow$DebugSleep")

and _json_of_use_t json_cx = check_depth _json_of_use_t_impl json_cx

and _json_of_use_t_impl json_cx t =
  Hh_json.(
    JSON_Object
      ( [
          ( "reason",
            json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None (reason_of_use_t t) );
          ("kind", JSON_String (string_of_use_ctor t));
        ]
      @
      match t with
      | UseT (op, t) -> [("use", JSON_String (string_of_use_op op)); ("type", _json_of_t json_cx t)]
      | AssertArithmeticOperandT _ -> []
      | AssertBinaryInLHST _ -> []
      | AssertBinaryInRHST _ -> []
      | AssertForInRHST _ -> []
      | BecomeT (_, t) -> [("result", _json_of_t json_cx t)]
      | BindT (_, _, funtype, pass) ->
        [("funType", json_of_funcalltype json_cx funtype); ("passThrough", JSON_Bool pass)]
      | CallT (_, _, funtype) -> [("funType", json_of_funcalltype json_cx funtype)]
      | MethodT (_, _, _, propref, funtype, _) ->
        [
          ("propRef", json_of_propref json_cx propref);
          ("funType", json_of_methodaction json_cx funtype);
        ]
      | ReposLowerT (_, use_desc, use_t) ->
        [("type", _json_of_use_t json_cx use_t); ("useDesc", JSON_Bool use_desc)]
      | ReposUseT (_, use_desc, op, t) ->
        [
          ("use", JSON_String (string_of_use_op op));
          ("type", _json_of_t json_cx t);
          ("useDesc", JSON_Bool use_desc);
        ]
      | SetPropT (_, _, name, _, _, t, _)
      | GetPropT (_, _, name, t)
      | MatchPropT (_, _, name, t)
      | TestPropT (_, _, name, t) ->
        [("propRef", json_of_propref json_cx name); ("propType", _json_of_t json_cx t)]
      | SetPrivatePropT (_, _, name, _, _, _, t, _)
      | GetPrivatePropT (_, _, name, _, _, t) ->
        [("propRef", JSON_String name); ("propType", _json_of_t json_cx t)]
      | SetElemT (_, _, indext, _, elemt, _)
      | GetElemT (_, _, indext, elemt) ->
        [("indexType", _json_of_t json_cx indext); ("elemType", _json_of_t json_cx elemt)]
      | CallElemT (_, _, indext, action) ->
        [("indexType", _json_of_t json_cx indext); ("funType", json_of_methodaction json_cx action)]
      | GetStaticsT (_, t) -> [("type", _json_of_t json_cx t)]
      | GetProtoT (_, t)
      | SetProtoT (_, t) ->
        [("type", _json_of_t json_cx t)]
      | ConstructorT (_, _, targs, args, t) ->
        [
          ( "typeArgs",
            match targs with
            | None -> JSON_Null
            | Some ts -> JSON_Array (Base.List.map ~f:(_json_of_targ json_cx) ts) );
          ("argTypes", JSON_Array (Base.List.map ~f:(json_of_funcallarg json_cx) args));
          ("type", _json_of_t json_cx t);
        ]
      | SuperT (_, _, Derived { own; proto; static }) ->
        [
          ("own", json_of_pmap json_cx own);
          ("proto", json_of_pmap json_cx proto);
          ("static", json_of_pmap json_cx static);
        ]
      | ImplementsT (op, t) ->
        [("use", JSON_String (string_of_use_op op)); ("instance", _json_of_t json_cx t)]
      | MixinT (_, t) -> [("type", _json_of_t json_cx t)]
      | ToStringT (_, t) -> [("type", _json_of_use_t json_cx t)]
      | AdderT (_, _, _, l, r) ->
        [("leftType", _json_of_t json_cx l); ("rightType", _json_of_t json_cx r)]
      | ComparatorT (_, _, t) -> [("type", _json_of_t json_cx t)]
      | UnaryMinusT (_, t) -> [("type", _json_of_t json_cx t)]
      | PredicateT (p, t) -> [("pred", json_of_pred json_cx p); ("type", _json_of_t json_cx t)]
      | GuardT (p, r, t) ->
        [
          ("pred", json_of_pred json_cx p);
          ("result", _json_of_t json_cx r);
          ("sink", _json_of_t json_cx t);
        ]
      | StrictEqT { arg; _ } -> [("type", _json_of_t json_cx arg)]
      | EqT (_, _, t) -> [("type", _json_of_t json_cx t)]
      | AndT (_, right, res)
      | OrT (_, right, res)
      | NullishCoalesceT (_, right, res) ->
        [("rightType", _json_of_t json_cx right); ("resultType", _json_of_t json_cx res)]
      | NotT (_, t) -> [("type", _json_of_t json_cx t)]
      | SpecializeT (_, _, _, cache, targs_opt, tvar) ->
        [("cache", json_of_specialize_cache json_cx cache)]
        @ (match targs_opt with
          | Some targs -> [("types", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) targs))]
          | None -> [])
        @ [("tvar", _json_of_t json_cx tvar)]
      | ThisSpecializeT (_, this, k) -> ("this", _json_of_t json_cx this) :: _json_of_cont json_cx k
      | VarianceCheckT (_, _, targs, polarity) ->
        [
          ("types", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) targs));
          ("polarity", json_of_polarity json_cx polarity);
        ]
      | TypeAppVarianceCheckT (_, _, _, targs) ->
        [
          ( "typeArgs",
            JSON_Array
              (Base.List.map
                 ~f:(fun (t1, t2) ->
                   JSON_Object [("t1", _json_of_t json_cx t1); ("t2", _json_of_t json_cx t2)])
                 targs) );
        ]
      | TypeCastT (op, t) ->
        [("use", JSON_String (string_of_use_op op)); ("arg", _json_of_t json_cx t)]
      | FilterOptionalT (op, t) ->
        [("use", JSON_String (string_of_use_op op)); ("arg", _json_of_t json_cx t)]
      | FilterMaybeT (op, t) ->
        [("use", JSON_String (string_of_use_op op)); ("arg", _json_of_t json_cx t)]
      | EnumCastT { use_op; enum = (reason, trust, enum) } ->
        [
          ("use", JSON_String (string_of_use_op use_op));
          ("enum", _json_of_t json_cx (DefT (reason, trust, EnumT enum)));
        ]
      | ConcretizeTypeAppsT (_, (ts1, _, _), (t2, ts2, _, _), will_flip) ->
        [
          ("willFlip", JSON_Bool will_flip);
          ("currentTypeArgs", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) ts1));
          ("currentUpper", _json_of_t json_cx t2);
          ("currentUpperTypeArgs", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) ts2));
        ]
      | LookupT { lookup_kind = rstrict; propref; lookup_action = action; _ } ->
        (match rstrict with
        | NonstrictReturning (default_opt, test_opt) ->
          let ret =
            match default_opt with
            | Some (default, result) ->
              [
                ("defaultType", _json_of_t json_cx default);
                ("resultType", _json_of_t json_cx result);
              ]
            | None -> []
          in
          Base.Option.value_map test_opt ~default:ret ~f:(fun (id, _) -> ("testID", int_ id) :: ret)
        | Strict r ->
          [("strictReason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None r)]
        | ShadowRead (_, ids) ->
          [
            ( "shadowRead",
              JSON_Array
                ( Nel.to_list ids
                |> Base.List.map ~f:(fun id -> JSON_Number (Properties.string_of_id id)) ) );
          ]
        | ShadowWrite ids ->
          [
            ( "shadowWrite",
              JSON_Array
                ( Nel.to_list ids
                |> Base.List.map ~f:(fun id -> JSON_Number (Properties.string_of_id id)) ) );
          ])
        @ [
            ("propref", json_of_propref json_cx propref);
            ("action", json_of_lookup_action json_cx action);
          ]
      | ObjAssignFromT (_, _, proto, tvar, kind) ->
        [
          ("target", _json_of_t json_cx proto);
          ("resultType", _json_of_t json_cx tvar);
          ("kind", json_of_obj_assign_kind json_cx kind);
        ]
      | ObjAssignToT (_, _, from, tvar, kind) ->
        [
          ("source", _json_of_t json_cx from);
          ("resultType", _json_of_t json_cx tvar);
          ("kind", json_of_obj_assign_kind json_cx kind);
        ]
      | ObjFreezeT (_, t) -> [("type", _json_of_t json_cx t)]
      | ObjRestT (_, excludes, tvar) ->
        [
          ("excludedProps", JSON_Array (Base.List.map ~f:(fun s -> JSON_String s) excludes));
          ("resultType", _json_of_t json_cx tvar);
        ]
      | ObjSealT (_, t) -> [("type", _json_of_t json_cx t)]
      | ObjTestProtoT (_, res) -> [("returnType", _json_of_t json_cx res)]
      | ObjTestT (_, default, res) ->
        [("defaultType", _json_of_t json_cx default); ("resultType", _json_of_t json_cx res)]
      | ArrRestT (_, _, i, t) ->
        [("index", JSON_Number (string_of_int i)); ("resultType", _json_of_t json_cx t)]
      | UnifyT (t1, t2) -> [("type1", _json_of_t json_cx t1); ("type2", _json_of_t json_cx t2)]
      | GetKeysT (_, t) -> [("type", _json_of_use_t json_cx t)]
      | HasOwnPropT (_, _, key) -> [("key", JSON_Object (_json_of_string_literal key))]
      | GetValuesT (_, t) -> [("type", _json_of_t json_cx t)]
      | ElemT (_, _, base, action) ->
        [
          ("baseType", _json_of_t json_cx base);
          (match action with
          | ReadElem t -> ("readElem", _json_of_t json_cx t)
          | WriteElem (t, _, _) -> ("writeElem", _json_of_t json_cx t)
          | CallElem (_, action) -> ("callElem", json_of_methodaction json_cx action));
        ]
      | MakeExactT (_, cont) -> _json_of_cont json_cx cont
      | CJSRequireT (_, export, _) -> [("export", _json_of_t json_cx export)]
      | ImportModuleNsT (_, t, _) -> [("t_out", _json_of_t json_cx t)]
      | ImportDefaultT (_, import_kind, (local_name, module_name), t, _) ->
        [
          ("import_kind", _json_of_import_kind import_kind);
          ("local_name", JSON_String local_name);
          ("module_name", JSON_String module_name);
          ("t_out", _json_of_t json_cx t);
        ]
      | ImportNamedT (_, import_kind, export_name, module_name, t, _) ->
        [
          ("import_kind", _json_of_import_kind import_kind);
          ("export_name", JSON_String export_name);
          ("module_name", JSON_String module_name);
          ("t_out", _json_of_t json_cx t);
        ]
      | ImportTypeT (_, export_name, t)
      | ImportTypeofT (_, export_name, t) ->
        [("export_name", JSON_String export_name); ("t_out", _json_of_t json_cx t)]
      | AssertImportIsValueT (_, name) -> [("name", JSON_String name)]
      | CJSExtractNamedExportsT (_, (module_t_reason, exporttypes, is_strict), t_out) ->
        [
          ("module", _json_of_t json_cx (ModuleT (module_t_reason, exporttypes, is_strict)));
          ("t_out", _json_of_t json_cx t_out);
        ]
      | CopyNamedExportsT (_, target_module_t, t_out) ->
        [
          ("target_module_t", _json_of_t json_cx target_module_t);
          ("t_out", _json_of_t json_cx t_out);
        ]
      | CopyTypeExportsT (_, target_module_t, t_out) ->
        [
          ("target_module_t", _json_of_t json_cx target_module_t);
          ("t_out", _json_of_t json_cx t_out);
        ]
      | ExportNamedT (_, tmap, _export_kind, t_out) ->
        [("tmap", json_of_loc_tmap json_cx tmap); ("t_out", _json_of_t json_cx t_out)]
      | ExportTypeT (_, name, t, t_out) ->
        [
          ("name", JSON_String name);
          ("tmap", _json_of_t json_cx t);
          ("t_out", _json_of_t json_cx t_out);
        ]
      | AssertExportIsTypeT (_, name, t_out) ->
        [("name", JSON_String name); ("t_out", _json_of_t json_cx t_out)]
      | DebugPrintT _ -> []
      | DebugSleepT _ -> []
      | MapTypeT (_, _, kind, t) ->
        [("kind", JSON_String (string_of_type_map kind)); ("t", _json_of_t json_cx t)]
      | ObjKitT (_, _, _, _, tout) -> [("t_out", _json_of_t json_cx tout)]
      | ReactKitT (_, _, React.CreateElement0 (shape, config, (children, children_spread), t_out))
        ->
        [
          ("shape", JSON_Bool shape);
          ("config", _json_of_t json_cx config);
          ("children", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) children));
          ( "childrenSpread",
            match children_spread with
            | Some children_spread -> _json_of_t json_cx children_spread
            | None -> JSON_Null );
          ("returnType", _json_of_t json_cx t_out);
        ]
      | ReactKitT
          (_, _, React.CreateElement (shape, component, config, (children, children_spread), t_out))
        ->
        [
          ("shape", JSON_Bool shape);
          ("component", _json_of_t json_cx component);
          ("config", _json_of_t json_cx config);
          ("children", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) children));
          ( "childrenSpread",
            match children_spread with
            | Some children_spread -> _json_of_t json_cx children_spread
            | None -> JSON_Null );
          ("returnType", _json_of_t json_cx t_out);
        ]
      | ReactKitT _ -> [] (* TODO *)
      | ChoiceKitUseT (_, tool) ->
        [
          ( "tool",
            JSON_String
              (match tool with
              | FullyResolveType _ -> "fullyResolveType"
              | TryFlow _ -> "tryFlow") );
        ]
      | IntersectionPreprocessKitT (_, tool) ->
        [
          ( "tool",
            JSON_String
              (match tool with
              | ConcretizeTypes _ -> "concretizeTypes"
              | SentinelPropTest _ -> "sentinelPropTest"
              | PropExistsTest _ -> "propExistsTest") );
        ]
      | SentinelPropTestT (_, l, key, sense, sentinel, result) ->
        [
          ("l", _json_of_t json_cx l);
          ("key", JSON_String key);
          ("sense", JSON_Bool sense);
          ("sentinel", json_of_sentinel json_cx sentinel);
          ("result", _json_of_t json_cx result);
        ]
      | IdxUnwrap (_, t_out) -> [("t_out", _json_of_t json_cx t_out)]
      | IdxUnMaybeifyT (_, t_out) -> [("t_out", _json_of_t json_cx t_out)]
      | OptionalChainT (_, _, this, out, void_out) ->
        [
          ("this", _json_of_t json_cx this);
          ("t_out", _json_of_use_t json_cx out);
          ("voidt_out", _json_of_t json_cx void_out);
        ]
      | InvariantT _ -> []
      | CallLatentPredT (_, sense, offset, l, t) ->
        [
          ("sense", JSON_Bool sense);
          ("offset", JSON_Number (spf "%d" offset));
          ("t_in", _json_of_t json_cx l);
          ("t_out", _json_of_t json_cx t);
        ]
      | CallOpenPredT (_, sense, key, l, t) ->
        [
          ("sense", JSON_Bool sense);
          ("key", JSON_String (Key.string_of_key key));
          ("t_in", _json_of_t json_cx l);
          ("t_out", _json_of_t json_cx t);
        ]
      | SubstOnPredT (_, subst, t) ->
        [
          ( "PredWithSubst",
            JSON_Object
              [
                ( "subst",
                  JSON_Array
                    ( subst
                    |> SMap.elements
                    |> Base.List.map ~f:(fun (x, k) ->
                           JSON_Array [JSON_String x; JSON_String (Key.string_of_key k)]) ) );
                ("pred_t", _json_of_t_impl json_cx t);
              ] );
        ]
      | ReactPropsToOut (_, props) -> [("props", _json_of_t json_cx props)]
      | ReactInToProps (_, props) -> [("props", _json_of_t json_cx props)]
      | RefineT (_, p, t) ->
        [
          ( "Refined",
            JSON_Object
              [("pred_t", json_of_pred json_cx p); ("refined_t", _json_of_t_impl json_cx t)] );
        ]
      | ResolveSpreadT (_, _, { rrt_resolved; rrt_unresolved; rrt_resolve_to }) ->
        [
          ( "resolved",
            JSON_Array
              (Base.List.map
                 ~f:(fun param ->
                   let (kind, t) =
                     match param with
                     | ResolvedArg t -> ("ResolvedArg", t)
                     | ResolvedSpreadArg (r, at) ->
                       ("ResolvedSpreadArg", DefT (r, bogus_trust (), ArrT at))
                     | ResolvedAnySpreadArg r -> ("ResolvedAnySpreadArg", AnyT.make Untyped r)
                   in
                   JSON_Object [("kind", JSON_String kind); ("type", _json_of_t_impl json_cx t)])
                 rrt_resolved) );
          ( "unresolved",
            JSON_Array
              (Base.List.map
                 ~f:(fun param ->
                   let (kind, t) =
                     match param with
                     | UnresolvedArg t -> ("UnresolvedArg", t)
                     | UnresolvedSpreadArg t -> ("UnresolvedSpreadArg", t)
                   in
                   JSON_Object [("kind", JSON_String kind); ("type", _json_of_t_impl json_cx t)])
                 rrt_unresolved) );
          ("resolve_to", json_of_resolve_to json_cx rrt_resolve_to);
        ]
      | CondT (_, consequent, alternate, t_out) ->
        [
          ( "consequent",
            match consequent with
            | Some t -> _json_of_t json_cx t
            | None -> JSON_Null );
          ("alternate", _json_of_t json_cx alternate);
          ("t_out", _json_of_t json_cx t_out);
        ]
      | ExtendsUseT (_, _, _, t1, t2) ->
        [("type1", _json_of_t json_cx t1); ("type2", _json_of_t json_cx t2)]
      | DestructuringT (_, k, s, t_out) ->
        [
          ( "kind",
            JSON_String
              (match k with
              | DestructAnnot -> "annot"
              | DestructInfer -> "infer") );
          ("selector", json_of_selector json_cx s);
          ("t_out", _json_of_t json_cx t_out);
        ]
      | CreateObjWithComputedPropT { reason; value; tout_tvar } ->
        [
          ("reason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None reason);
          ("value", _json_of_t json_cx value);
          ("tout", _json_of_t json_cx (OpenT tout_tvar));
        ]
      | ResolveUnionT { reason; resolved; unresolved; upper; id } ->
        [
          ("reason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None reason);
          ("unresolved", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) unresolved));
          ("resolved", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) resolved));
          ("upper", _json_of_use_t json_cx upper);
          ("id", JSON_Number (string_of_int id));
        ]
      | ModuleExportsAssignT (_, assign, t_out) ->
        [("assign", _json_of_t json_cx assign); ("t_out", _json_of_t json_cx t_out)] ))

and json_of_resolve_to json_cx = check_depth json_of_resolve_to_impl json_cx

and json_of_resolve_to_impl json_cx resolve_to =
  Hh_json.(
    JSON_Object
      (match resolve_to with
      | ResolveSpreadsToArrayLiteral (id, elem_t, tout) ->
        [
          ("id", JSON_Number (string_of_int id));
          ("elem_t", _json_of_t json_cx elem_t);
          ("t_out", _json_of_t json_cx tout);
        ]
      | ResolveSpreadsToArray (elem_t, tout) ->
        [("elem_t", _json_of_t json_cx elem_t); ("t_out", _json_of_t json_cx tout)]
      | ResolveSpreadsToMultiflowCallFull (id, ft)
      | ResolveSpreadsToMultiflowSubtypeFull (id, ft) ->
        [("id", JSON_Number (string_of_int id)); ("funtype", json_of_funtype json_cx ft)]
      | ResolveSpreadsToCustomFunCall (id, kind, tout) ->
        [
          ("id", JSON_Number (string_of_int id));
          ("kind", _json_of_custom_fun_kind kind);
          ("t_out", _json_of_t json_cx tout);
        ]
      | ResolveSpreadsToMultiflowPartial (id, ft, call_reason, tout) ->
        [
          ("id", JSON_Number (string_of_int id));
          ("funtype", json_of_funtype json_cx ft);
          ( "callReason",
            json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None call_reason );
          ("t_out", _json_of_t json_cx tout);
        ]
      | ResolveSpreadsToCallT (fct, tin) ->
        [("funcalltype", json_of_funcalltype json_cx fct); ("t_in", _json_of_t json_cx tin)]))

and _json_of_enum _json_cx = function
  | UnionEnum.Str s -> Hh_json.JSON_String s
  | UnionEnum.Num (_, raw) -> Hh_json.JSON_String raw
  | UnionEnum.Bool b -> Hh_json.JSON_Bool b
  | UnionEnum.Null -> Hh_json.JSON_Null
  | UnionEnum.Void -> Hh_json.JSON_Null

(* hmm, undefined doesn't exist in JSON *)
and json_of_sentinel json_cx = check_depth json_of_sentinel_impl json_cx

and json_of_sentinel_impl json_cx = function
  | UnionEnum.One enum -> _json_of_enum json_cx enum
  | UnionEnum.Many enums ->
    Hh_json.JSON_Array (Base.List.map ~f:(_json_of_enum json_cx) @@ UnionEnumSet.elements enums)

and json_of_polarity json_cx = check_depth json_of_polarity_impl json_cx

and json_of_polarity_impl _json_cx polarity = Hh_json.JSON_String (string_of_polarity polarity)

and json_of_typeparam json_cx = check_depth json_of_typeparam_impl json_cx

and json_of_typeparam_impl json_cx tparam =
  Hh_json.(
    JSON_Object
      ( [
          ("reason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None tparam.reason);
          ("name", JSON_String tparam.name);
          ("bound", _json_of_t json_cx tparam.bound);
          ("polarity", json_of_polarity json_cx tparam.polarity);
        ]
      @
      match tparam.default with
      | None -> []
      | Some t -> [("default", _json_of_t json_cx t)] ))

and json_of_objtype json_cx = check_depth json_of_objtype_impl json_cx

and json_of_objtype_impl json_cx objtype =
  Hh_json.(
    let pmap = Context.find_props json_cx.cx objtype.props_tmap in
    JSON_Object
      ( [("flags", json_of_flags json_cx objtype.flags)]
      @ (match objtype.dict_t with
        | None -> []
        | Some d -> [("dictType", json_of_dicttype json_cx d)])
      @ [
          ("propTypes", json_of_pmap json_cx pmap); ("prototype", _json_of_t json_cx objtype.proto_t);
        ] ))

and json_of_dicttype json_cx = check_depth json_of_dicttype_impl json_cx

and json_of_dicttype_impl json_cx dicttype =
  Hh_json.(
    JSON_Object
      ( (match dicttype.dict_name with
        | None -> []
        | Some name -> [("name", JSON_String name)])
      @ [
          ("keyType", _json_of_t json_cx dicttype.key);
          ("valueType", _json_of_t json_cx dicttype.value);
        ] ))

and json_of_flags json_cx = check_depth json_of_flags_impl json_cx

and bool_of_sealtype = function
  | Sealed -> true
  | _ -> false

and json_of_sealtype _json_cx sealtype = Hh_json.(JSON_Bool (bool_of_sealtype sealtype))

and json_of_flags_impl json_cx flags =
  Hh_json.(
    JSON_Object
      [
        ("frozen", JSON_Bool flags.frozen);
        ("sealed", json_of_sealtype json_cx flags.sealed);
        ("exact", JSON_Bool flags.exact);
      ])

and json_of_changeset json_cx = check_depth json_of_changeset_impl json_cx

and json_of_changeset_impl _json_cx =
  Hh_json.(
    let json_of_entry_ref (scope_id, name, op) =
      JSON_Object
        [
          ("scope_id", int_ scope_id);
          ("name", JSON_String name);
          ("op", JSON_String (Changeset.string_of_op op));
        ]
    in
    let json_of_changed_vars changed_vars =
      JSON_Array
        (List.rev
           (Changeset.EntryRefSet.fold
              (fun entry_ref acc -> json_of_entry_ref entry_ref :: acc)
              changed_vars
              []))
    in
    let json_of_refi_ref (scope_id, key, op) =
      JSON_Object
        [
          ("scope_id", int_ scope_id);
          ("key", JSON_String (Key.string_of_key key));
          ("op", JSON_String (Changeset.string_of_op op));
        ]
    in
    let json_of_changed_refis changed_refis =
      JSON_Array
        (List.rev
           (Changeset.RefiRefSet.fold
              (fun refi_ref acc -> json_of_refi_ref refi_ref :: acc)
              changed_refis
              []))
    in
    fun (changed_vars, changed_refis) ->
      JSON_Object
        [
          ("vars", json_of_changed_vars changed_vars); ("refis", json_of_changed_refis changed_refis);
        ])

and json_of_funtype json_cx = check_depth json_of_funtype_impl json_cx

and json_of_funtype_impl
    json_cx { this_t; params; rest_param; return_t; is_predicate; closure_t; changeset; def_reason }
    =
  Hh_json.(
    let rec params_names (any, names_rev) = function
      | [] ->
        if any then
          Some names_rev
        else
          None
      | (None, _) :: xs -> params_names (any, "_" :: names_rev) xs
      | (Some name, _) :: xs -> params_names (true, name :: names_rev) xs
    in
    JSON_Object
      ( [
          ("thisType", _json_of_t json_cx this_t);
          ("paramTypes", JSON_Array (Base.List.map ~f:(fun (_, t) -> _json_of_t json_cx t) params));
        ]
      @ (match params_names (false, []) params with
        | None -> []
        | Some names_rev ->
          [("paramNames", JSON_Array (List.rev_map (fun s -> JSON_String s) names_rev))])
      @ [
          ( "restParam",
            match rest_param with
            | None -> JSON_Null
            | Some (name, _, t) ->
              JSON_Object
                ( [("restParamType", _json_of_t json_cx t)]
                @
                match name with
                | None -> []
                | Some name -> [("restParamName", JSON_String name)] ) );
          ("returnType", _json_of_t json_cx return_t);
          ("isPredicate", JSON_Bool is_predicate);
          ("closureIndex", int_ closure_t);
          ("changeset", json_of_changeset json_cx changeset);
          ("defLoc", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None def_reason);
        ] ))

and json_of_funcalltype json_cx = check_depth json_of_funcalltype_impl json_cx

and json_of_funcalltype_impl
    json_cx
    { call_this_t; call_targs; call_args_tlist; call_tout; call_closure_t; call_strict_arity } =
  Hh_json.(
    let arg_types = Base.List.map ~f:(json_of_funcallarg json_cx) call_args_tlist in
    JSON_Object
      [
        ("thisType", _json_of_t json_cx call_this_t);
        ( "typeArgs",
          match call_targs with
          | None -> JSON_Null
          | Some ts -> JSON_Array (Base.List.map ~f:(_json_of_targ json_cx) ts) );
        ("argTypes", JSON_Array arg_types);
        ("tout", _json_of_t json_cx call_tout);
        ("closureIndex", int_ call_closure_t);
        ("strictArity", JSON_Bool call_strict_arity);
      ])

and json_of_methodaction json_cx = check_depth json_of_methodaction_impl json_cx

and json_of_methodaction_impl json_cx =
  Hh_json.(
    function
    | CallM funtype -> json_of_funcalltype json_cx funtype
    | ChainM (_, _, this, t_out, void_out) ->
      JSON_Object
        [
          ("this", _json_of_t json_cx this);
          ("t_out", json_of_funcalltype json_cx t_out);
          ("voidt_out", _json_of_t json_cx void_out);
        ])

and json_of_funcallarg json_cx = check_depth json_of_funcallarg_impl json_cx

and json_of_funcallarg_impl json_cx arg =
  let (kind, t) =
    match arg with
    | Arg t -> ("argument", t)
    | SpreadArg t -> ("spread", t)
  in
  Hh_json.(JSON_Object [("argKind", JSON_String kind); ("argType", _json_of_t json_cx t)])

and json_of_insttype json_cx = check_depth json_of_insttype_impl json_cx

and json_of_insttype_impl json_cx insttype =
  Hh_json.(
    let own_props = Context.find_props json_cx.cx insttype.own_props in
    let proto_props = Context.find_props json_cx.cx insttype.proto_props in
    let inst_kind =
      match insttype.inst_kind with
      | ClassKind -> JSON_String "class"
      | InterfaceKind { inline } -> JSON_Object [("inline", JSON_Bool inline)]
    in
    JSON_Object
      [
        ("classId", json_of_aloc ~offset_table:None (insttype.class_id :> ALoc.t));
        ( "typeArgs",
          JSON_Array
            (Base.List.map
               ~f:(fun (x, _, t, p) ->
                 JSON_Object
                   [
                     ("name", JSON_String x);
                     ("type", _json_of_t json_cx t);
                     ("polarity", json_of_polarity json_cx p);
                   ])
               insttype.type_args) );
        ("fieldTypes", json_of_pmap json_cx own_props);
        ("methodTypes", json_of_pmap json_cx proto_props);
        ("mixins", JSON_Bool insttype.has_unknown_react_mixins);
        ("inst_kind", inst_kind);
      ])

and json_of_selector json_cx = check_depth json_of_selector_impl json_cx

and json_of_selector_impl json_cx =
  Hh_json.(
    function
    | Prop (x, _) -> JSON_Object [("propName", JSON_String x)]
    | Elem key -> JSON_Object [("keyType", _json_of_t json_cx key)]
    | ObjRest excludes ->
      JSON_Object
        [("excludedProps", JSON_Array (Base.List.map ~f:(fun s -> JSON_String s) excludes))]
    | ArrRest i -> JSON_Object [("index", JSON_Number (string_of_int i))]
    | Default -> JSON_Object [("default", JSON_Bool true)])

and json_of_destructor json_cx = check_depth json_of_destructor_impl json_cx

and json_of_destructor_impl json_cx =
  Hh_json.(
    function
    | NonMaybeType -> JSON_Object [("non null/void", JSON_Bool true)]
    | PropertyType x -> JSON_Object [("propName", JSON_String x)]
    | ElementType t -> JSON_Object [("elementType", _json_of_t json_cx t)]
    | Bind t -> JSON_Object [("thisType", _json_of_t json_cx t)]
    | ReadOnlyType -> JSON_Object [("readOnly", JSON_Bool true)]
    | SpreadType (target, ts, head_slice) ->
      Object.Spread.(
        JSON_Object
          ( (match target with
            | Value { make_seal } ->
              [("target", JSON_String "Value"); ("make_seal", json_of_sealtype json_cx make_seal)]
            | Annot { make_exact } ->
              [("target", JSON_String "Annot"); ("makeExact", JSON_Bool make_exact)])
          @ [
              ("spread", JSON_Array (Base.List.map ~f:(json_of_spread_operand json_cx) ts));
              ( "head_slice",
                match head_slice with
                | None -> JSON_Null
                | Some head_slice -> json_of_spread_operand_slice json_cx head_slice );
            ] ))
    | RestType (merge_mode, t) ->
      Object.Rest.(
        JSON_Object
          [
            ( "mergeMode",
              JSON_String
                (match merge_mode with
                | Sound -> "Sound"
                | IgnoreExactAndOwn -> "IgnoreExactAndOwn"
                | ReactConfigMerge _ -> "ReactConfigMerge") );
            ("restType", _json_of_t json_cx t);
          ])
    | ValuesType -> JSON_Object [("values", JSON_Bool true)]
    | CallType args ->
      JSON_Object [("args", JSON_Array (Base.List.map ~f:(_json_of_t json_cx) args))]
    | TypeMap tmap -> json_of_type_map json_cx tmap
    | ReactElementPropsType -> JSON_Object [("reactElementProps", JSON_Bool true)]
    | ReactElementConfigType -> JSON_Object [("reactElementConfig", JSON_Bool true)]
    | ReactElementRefType -> JSON_Object [("reactElementRef", JSON_Bool true)]
    | ReactConfigType t ->
      JSON_Object [("reactConfig", JSON_Bool true); ("default_props", _json_of_t json_cx t)])

and json_of_spread_operand_slice json_cx { Object.Spread.reason; prop_map; dict } =
  Hh_json.(
    JSON_Object
      [
        ("reason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None reason);
        ( "props",
          JSON_Object (SMap.fold (fun k p acc -> (k, json_of_prop json_cx p) :: acc) prop_map []) );
        ( "dict",
          match dict with
          | Some dict -> json_of_dicttype json_cx dict
          | None -> JSON_Null );
      ])

and json_of_spread_operand json_cx =
  Hh_json.(
    function
    | Object.Spread.Slice operand_slice ->
      JSON_Object
        [
          ("kind", JSON_String "slice");
          ("slice", json_of_spread_operand_slice json_cx operand_slice);
        ]
    | Object.Spread.Type t ->
      JSON_Object [("kind", JSON_String "type"); ("type", _json_of_t json_cx t)])

and json_of_type_map json_cx = check_depth json_of_type_map_impl json_cx

and json_of_type_map_impl json_cx =
  Hh_json.(
    function
    | TupleMap t -> JSON_Object [("tupleMap", _json_of_t json_cx t)]
    | ObjectMap t -> JSON_Object [("objectMap", _json_of_t json_cx t)]
    | ObjectMapi t -> JSON_Object [("objectMapi", _json_of_t json_cx t)])

and json_of_propref json_cx = check_depth json_of_propref_impl json_cx

and json_of_propref_impl json_cx =
  Hh_json.(
    function
    | Named (r, x) ->
      JSON_Object
        [
          ("reason", json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None r);
          ("name", JSON_String x);
        ]
    | Computed t -> JSON_Object [("elem", _json_of_t json_cx t)])

and json_of_loc_tmap json_cx = check_depth json_of_loc_tmap_impl json_cx

and json_of_loc_tmap_impl json_cx bindings =
  Hh_json.(
    let lst =
      SMap.fold
        (fun name (loc, t) acc -> json_of_type_binding json_cx (name, (loc, t)) :: acc)
        bindings
        []
    in
    JSON_Array (List.rev lst))

and json_of_pmap json_cx = check_depth json_of_pmap_impl json_cx

and json_of_pmap_impl json_cx bindings =
  Hh_json.(
    let lst =
      SMap.fold (fun name p acc -> json_of_prop_binding json_cx (name, p) :: acc) bindings []
    in
    JSON_Array (List.rev lst))

and json_of_defer_use_t json_cx = check_depth json_of_defer_use_t_impl json_cx

and json_of_defer_use_t_impl json_cx =
  Hh_json.(
    function
    | LatentPredT (_, p) -> JSON_Object [("predicate", json_of_pred json_cx p)]
    | TypeDestructorT (_, _, s) -> JSON_Object [("destructor", json_of_destructor json_cx s)])

and json_of_prop_binding json_cx = check_depth json_of_prop_binding_impl json_cx

and json_of_prop_binding_impl json_cx (name, p) =
  Hh_json.(JSON_Object [("name", JSON_String name); ("prop", json_of_prop json_cx p)])

and json_of_prop json_cx = check_depth json_of_prop_impl json_cx

and json_of_prop_impl json_cx p =
  Hh_json.(
    JSON_Object
      (match p with
      | Field (_loc, t, polarity) ->
        [("field", _json_of_t json_cx t); ("polarity", json_of_polarity json_cx polarity)]
      | Get (_loc, t) -> [("getter", _json_of_t json_cx t)]
      | Set (_loc, t) -> [("setter", _json_of_t json_cx t)]
      | GetSet (_loc1, t1, _loc2, t2) ->
        [("getter", _json_of_t json_cx t1); ("setter", _json_of_t json_cx t2)]
      | Method (_loc, t) -> [("method", _json_of_t json_cx t)]))

and json_of_type_binding json_cx = check_depth json_of_type_binding_impl json_cx

and json_of_type_binding_impl json_cx (name, (loc, t)) =
  Hh_json.(
    let loc_json =
      match loc with
      | None -> Hh_json.JSON_Null
      | Some loc -> json_of_aloc ~strip_root:json_cx.strip_root ~offset_table:None loc
    in
    JSON_Object [("name", JSON_String name); ("type", _json_of_t json_cx t); ("loc", loc_json)])

and json_of_pred json_cx = check_depth json_of_pred_impl json_cx

and json_of_pred_impl json_cx p =
  Hh_json.(
    JSON_Object
      ( [("kind", JSON_String (string_of_pred_ctor p))]
      @
      match p with
      | AndP (l, r)
      | OrP (l, r) ->
        [("left", json_of_pred json_cx l); ("right", json_of_pred json_cx r)]
      | NotP p -> [("pred", json_of_pred json_cx p)]
      | LeftP (b, t)
      | RightP (b, t) ->
        [("binaryTest", json_of_binary_test json_cx b); ("type", _json_of_t json_cx t)]
      | SingletonBoolP (_, value) -> [("value", JSON_Bool value)]
      | SingletonStrP (_, _, str) -> [("value", JSON_String str)]
      | SingletonNumP (_, _, (_, raw)) -> [("value", JSON_String raw)]
      | PropExistsP (key, _) -> [("propName", JSON_String key)]
      | PropNonMaybeP (key, _) -> [("propName", JSON_String key)]
      | ExistsP _
      | VoidP
      | NullP
      | MaybeP
      | BoolP _
      | StrP _
      | SymbolP _
      | NumP _
      | FunP
      | ObjP
      | ArrP ->
        []
      | LatentP (t, i) ->
        [
          ( "latent",
            JSON_Object
              [("type", _json_of_t_impl json_cx t); ("position", JSON_Number (spf "%d" i))] );
        ] ))

and json_of_binary_test json_cx = check_depth json_of_binary_test_impl json_cx

and json_of_binary_test_impl _json_cx b =
  Hh_json.(
    JSON_Object
      ( [("kind", JSON_String (string_of_binary_test_ctor b))]
      @
      match b with
      | InstanceofTest -> []
      | SentinelProp s -> [("key", JSON_String s)] ))

and json_of_node json_cx = check_depth json_of_node_impl json_cx

and json_of_node_impl json_cx id =
  Hh_json.(
    JSON_Object
      (let json_cx = { json_cx with stack = ISet.add id json_cx.stack } in
       match IMap.find id (Context.graph json_cx.cx) with
       | Constraint.Goto id -> [("kind", JSON_String "Goto")] @ [("id", int_ id)]
       | Constraint.Root root ->
         [("kind", JSON_String "Root")] @ [("root", json_of_root json_cx root)]))

and json_of_root json_cx = check_depth json_of_root_impl json_cx

and json_of_root_impl json_cx root =
  Hh_json.(
    Constraint.(
      JSON_Object
        [("rank", int_ root.rank); ("constraints", json_of_constraints json_cx root.constraints)]))

and json_of_constraints json_cx = check_depth json_of_constraints_impl json_cx

and json_of_constraints_impl json_cx constraints =
  Hh_json.(
    JSON_Object
      Constraint.(
        match constraints with
        | Resolved (_, t)
        | FullyResolved (_, t) ->
          [("kind", JSON_String "Resolved")] @ [("type", _json_of_t json_cx t)]
        | Unresolved bounds ->
          [("kind", JSON_String "Unresolved")] @ [("bounds", json_of_bounds json_cx bounds)]))

and json_of_bounds json_cx = check_depth json_of_bounds_impl json_cx

and json_of_bounds_impl json_cx bounds =
  Hh_json.(
    match bounds with
    | { Constraint.lower; upper; lowertvars; uppertvars } ->
      JSON_Object
        [
          ("lower", json_of_tkeys json_cx lower);
          ("upper", json_of_use_tkeys json_cx upper);
          ("lowertvars", json_of_tvarkeys json_cx lowertvars);
          ("uppertvars", json_of_tvarkeys json_cx uppertvars);
        ])

and json_of_tkeys json_cx = check_depth json_of_tkeys_impl json_cx

and json_of_tkeys_impl json_cx tmap =
  Hh_json.(JSON_Array (TypeMap.fold (fun t _ acc -> _json_of_t json_cx t :: acc) tmap []))

and json_of_use_tkeys json_cx = check_depth json_of_use_tkeys_impl json_cx

and json_of_use_tkeys_impl json_cx tmap =
  Hh_json.(
    let f t _ acc = _json_of_use_t json_cx t :: acc in
    JSON_Array (UseTypeMap.fold f tmap []))

and json_of_tvarkeys json_cx = check_depth json_of_tvarkeys_impl json_cx

and json_of_tvarkeys_impl _json_cx imap =
  Hh_json.(JSON_Array (IMap.fold (fun i _ acc -> int_ i :: acc) imap []))

and json_of_lookup_action json_cx = check_depth json_of_lookup_action_impl json_cx

and json_of_lookup_action_impl json_cx action =
  Hh_json.(
    JSON_Object
      (match action with
      | ReadProp { use_op = _; obj_t = _; tout } ->
        [("kind", JSON_String "ReadProp"); ("t", _json_of_t json_cx tout)]
      | WriteProp { use_op = _; obj_t = _; prop_tout = _; tin; write_ctx = _; mode = _ } ->
        [("kind", JSON_String "WriteProp"); ("t", _json_of_t json_cx tin)]
      | LookupProp (op, p) ->
        [
          ("kind", JSON_String "LookupProp");
          ("use", JSON_String (string_of_use_op op));
          ("prop", json_of_prop json_cx p);
        ]
      | SuperProp (_, p) -> [("kind", JSON_String "SuperProp"); ("prop", json_of_prop json_cx p)]
      | MatchProp (_, t) -> [("kind", JSON_String "MatchProp"); ("t", _json_of_t json_cx t)]))

and json_of_specialize_cache json_cx = check_depth json_of_specialize_cache_impl json_cx

and json_of_specialize_cache_impl json_cx cache =
  Hh_json.(
    JSON_Object
      (match cache with
      | None -> []
      | Some rs ->
        [
          ( "reasons",
            JSON_Array
              (Base.List.map
                 ~f:(json_of_reason ~strip_root:json_cx.strip_root ~offset_table:None)
                 rs) );
        ]))

and json_of_obj_assign_kind json_cx = check_depth json_of_obj_assign_kind_impl json_cx

and json_of_obj_assign_kind_impl _json_cx kind =
  Hh_json.JSON_String
    (match kind with
    | ObjAssign _ -> "normal"
    | ObjSpreadAssign -> "spread")

let json_of_t ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx t =
  let json_cx = { cx; size = ref size; depth; stack = ISet.empty; strip_root } in
  _json_of_t json_cx t

let json_of_use_t ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx use_t =
  let json_cx = { cx; size = ref size; depth; stack = ISet.empty; strip_root } in
  _json_of_use_t json_cx use_t

let jstr_of_t ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx t =
  Hh_json.json_to_multiline (json_of_t ~size ~depth ~strip_root cx t)

let jstr_of_use_t ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx use_t =
  Hh_json.json_to_multiline (json_of_use_t ~size ~depth ~strip_root cx use_t)

let json_of_graph ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx =
  Hh_json.(
    let entries =
      IMap.fold
        (fun id _ entries ->
          let json_cx = { cx; size = ref size; depth; stack = ISet.empty; strip_root } in
          (spf "%d" id, json_of_node json_cx id) :: entries)
        (Context.graph cx)
        []
    in
    JSON_Object (List.rev entries))

let jstr_of_graph ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx =
  Hh_json.json_to_multiline (json_of_graph ~size ~depth ~strip_root cx)

(* scopes *)

let json_of_scope =
  Scope.(
    Hh_json.(
      let json_of_value_impl
          json_cx
          { Entry.kind; value_state; value_declare_loc; value_assign_loc; specific; general } =
        JSON_Object
          [
            ("entry_type", JSON_String "Value");
            ("kind", JSON_String (Entry.string_of_value_kind kind));
            ("value_state", JSON_String (State.to_string value_state));
            ( "value_declare_loc",
              json_of_aloc ~strip_root:json_cx.strip_root ~offset_table:None value_declare_loc );
            ( "value_assign_loc",
              json_of_aloc ~strip_root:json_cx.strip_root ~offset_table:None value_assign_loc );
            ("specific", _json_of_t json_cx specific);
            ("general", _json_of_t json_cx general);
          ]
      in
      let json_of_value json_cx = check_depth json_of_value_impl json_cx in
      let json_of_type_impl json_cx { Entry.type_state; type_loc; type_; type_binding_kind = _ } =
        JSON_Object
          [
            ("entry_type", JSON_String "Type");
            ("type_state", JSON_String (State.to_string type_state));
            ("type_loc", json_of_aloc ~strip_root:json_cx.strip_root ~offset_table:None type_loc);
            ("type_", _json_of_t json_cx type_);
          ]
      in
      let json_of_type json_cx = check_depth json_of_type_impl json_cx in
      let json_of_class json_cx c =
        let pmap = Context.find_props json_cx.cx c.class_private_fields in
        JSON_Object
          [
            ("class_id", JSON_String (ALoc.debug_to_string (c.class_binding_id :> ALoc.t)));
            ("class_private_fields", json_of_pmap json_cx pmap);
          ]
      in
      let json_of_entry_impl json_cx =
        Entry.(
          function
          | Value r -> json_of_value json_cx r
          | Type r -> json_of_type json_cx r
          | Class r -> json_of_class json_cx r)
      in
      let json_of_entry json_cx = check_depth json_of_entry_impl json_cx in
      let json_of_entries_impl json_cx entries =
        let props =
          SMap.fold (fun name entry acc -> (name, json_of_entry json_cx entry) :: acc) entries []
          |> List.rev
        in
        JSON_Object props
      in
      let json_of_entries json_cx = check_depth json_of_entries_impl json_cx in
      let json_of_refi_impl json_cx { refi_loc; refined; original } =
        JSON_Object
          [
            ("refi_loc", json_of_aloc ~strip_root:json_cx.strip_root ~offset_table:None refi_loc);
            ("refined", _json_of_t json_cx refined);
            ("original", _json_of_t json_cx original);
          ]
      in
      let json_of_refi json_cx = check_depth json_of_refi_impl json_cx in
      let json_of_refis_impl json_cx refis =
        let props =
          Key_map.fold
            (fun key refi acc -> (Key.string_of_key key, json_of_refi json_cx refi) :: acc)
            refis
            []
          |> List.rev
        in
        JSON_Object props
      in
      let json_of_refis json_cx = check_depth json_of_refis_impl json_cx in
      fun ?(size = 5000) ?(depth = 1000) ?(strip_root = None) cx scope ->
        let json_cx = { cx; size = ref size; depth; stack = ISet.empty; strip_root } in
        JSON_Object
          [
            ("kind", JSON_String (string_of_kind scope.kind));
            ("entries", json_of_entries json_cx scope.entries);
            ("refis", json_of_refis json_cx scope.refis);
          ]))

let json_of_env ?(size = 5000) ?(depth = 1000) cx env =
  Hh_json.JSON_Array (Base.List.map ~f:(json_of_scope ~size ~depth cx) env)

(*****************************************************************)

(* debug printer *)

let lookup_trust cx id =
  Trust_constraint.(
    match Context.find_trust_graph cx id with
    | TrustResolved trust -> trust
    | TrustUnresolved b -> get_trust b)

let dump_reason cx reason =
  let strip_root =
    if Context.should_strip_root cx then
      Some (Context.root cx)
    else
      None
  in
  Reason.dump_reason ~strip_root reason

let rec dump_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") ?(trust = None) t =
    spf
      "%s %s(%s%s%s)"
      (string_of_ctor t)
      ( if not (Context.trust_tracking cx) then
        ""
      else
        Base.Option.value_map ~default:"" ~f:(lookup_trust cx |> string_of_trust_rep) trust )
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_t t))
      else
        "" )
      ( if reason && extra <> "" then
        ", "
      else
        "" )
      extra
  in
  let kid = dump_t_ (depth - 1, tvars) cx in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let defer_use expr t =
    match expr with
    | LatentPredT (_, p) -> spf "LatentPred %s on %s" (string_of_predicate p) t
    | TypeDestructorT (use_op, _, destructor) ->
      spf "%s, TypeDestruct %s on %s" (string_of_use_op use_op) (string_of_destructor destructor) t
  in
  let string_of_mixed_flavor = function
    | Mixed_everything -> "Mixed_everything"
    | Mixed_function -> "Mixed_function"
    | Mixed_truthy -> "Mixed_truthy"
    | Mixed_non_maybe -> "Mixed_non_maybe"
    | Mixed_non_null -> "Mixed_non_null"
    | Mixed_non_void -> "Mixed_non_void"
  in
  let string_of_any_source = function
    | Annotated -> "Annotated"
    | AnyError -> "Error"
    | Unsound _ -> "Unsound"
    | Untyped -> "Untyped"
  in
  let custom_fun =
    let react_prop_type =
      React.PropType.(
        let complex = function
          | ArrayOf -> "ArrayOf"
          | InstanceOf -> "InstanceOf"
          | ObjectOf -> "ObjectOf"
          | OneOf -> "OneOf"
          | OneOfType -> "OneOfType"
          | Shape -> "Shape"
        in
        function
        | Primitive (is_required, t) -> spf "Primitive (%b, %s)" is_required (kid t)
        | Complex kind -> complex kind)
    in
    function
    | ObjectAssign -> "ObjectAssign"
    | ObjectGetPrototypeOf -> "ObjectGetPrototypeOf"
    | ObjectSetPrototypeOf -> "ObjectSetPrototypeOf"
    | Compose false -> "Compose"
    | Compose true -> "ComposeReverse"
    | ReactPropType p -> spf "ReactPropType (%s)" (react_prop_type p)
    | ReactCreateElement -> "ReactCreateElement"
    | ReactCloneElement -> "ReactCloneElement"
    | ReactElementFactory _ -> "ReactElementFactory"
    | ReactCreateClass -> "ReactCreateClass"
    | Idx -> "Idx"
    | TypeAssertIs -> "TypeAssert.is"
    | TypeAssertThrows -> "TypeAssert.throws"
    | TypeAssertWraps -> "TypeAssert.wraps"
    | DebugPrint -> "DebugPrint"
    | DebugThrow -> "DebugThrow"
    | DebugSleep -> "DebugSleep"
  in
  if depth = 0 then
    string_of_ctor t
  else
    match t with
    | OpenT (_, id) -> p ~extra:(tvar id) t
    | DefT (_, trust, NumT lit) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match lit with
          | Literal (_, (_, raw)) -> raw
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, trust, StrT c) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match c with
          | Literal (_, s) -> spf "%S" s
          | Truthy -> "truthy"
          | AnyLiteral -> "")
        t
    | DefT (_, trust, BoolT c) ->
      p
        ~trust:(Some trust)
        ~extra:
          (match c with
          | Some b -> spf "%B" b
          | None -> "")
        t
    | DefT (_, trust, FunT (_, _, { params; return_t; this_t; _ })) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "<this: %s>(%s) => %s"
             (kid this_t)
             (String.concat "; " (Base.List.map ~f:(fun (_, t) -> kid t) params))
             (kid return_t))
        t
    | AnyT (_, src) -> p ~extra:(string_of_any_source src) t
    | DefT (_, trust, MixedT flavor) ->
      p ~trust:(Some trust) ~extra:(string_of_mixed_flavor flavor) t
    | DefT (_, trust, EmptyT _)
    | DefT (_, trust, SymbolT)
    | DefT (_, trust, NullT)
    | DefT (_, trust, VoidT) ->
      p ~trust:(Some trust) t
    | NullProtoT _
    | ObjProtoT _
    | FunProtoT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _ ->
      p t
    | DefT (_, trust, PolyT { tparams = tps; t_out = c; id; _ }) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "%s [%s] #%s"
             (kid c)
             (String.concat "; " (Base.List.map ~f:(fun tp -> tp.name) (Nel.to_list tps)))
             (Poly.string_of_id id))
        t
    | ThisClassT (_, inst) -> p ~extra:(kid inst) t
    | BoundT (_, name) -> p ~extra:name t
    | ExistsT _ -> p t
    | DefT (_, trust, ObjT { props_tmap; _ }) ->
      p ~trust:(Some trust) t ~extra:(Properties.string_of_id props_tmap)
    | DefT (_, trust, ArrT (ArrayAT (elemt, None))) ->
      p ~trust:(Some trust) ~extra:(spf "Array %s" (kid elemt)) t
    | DefT (_, trust, ArrT (ArrayAT (elemt, Some tup))) ->
      p
        ~trust:(Some trust)
        ~extra:
          (spf
             "Array %s, %s"
             (kid elemt)
             (spf "[%s]" (String.concat "; " (Base.List.map ~f:kid tup))))
        t
    | DefT (_, trust, ArrT (TupleAT (_, tup))) ->
      p
        ~trust:(Some trust)
        ~extra:(spf "Tuple [%s]" (String.concat ", " (Base.List.map ~f:kid tup)))
        t
    | DefT (_, trust, ArrT (ROArrayAT elemt)) ->
      p ~trust:(Some trust) ~extra:(spf "ReadOnlyArray %s" (kid elemt)) t
    | DefT (_, trust, CharSetT chars) ->
      p ~trust:(Some trust) ~extra:(spf "<%S>" (String_utils.CharSet.to_string chars)) t
    | DefT (_, trust, ClassT inst) -> p ~trust:(Some trust) ~extra:(kid inst) t
    | DefT (_, trust, InstanceT (_, _, _, { class_id; _ })) ->
      p ~trust:(Some trust) ~extra:(spf "#%s" (ALoc.debug_to_string (class_id :> ALoc.t))) t
    | DefT (_, trust, TypeT (_, arg)) -> p ~trust:(Some trust) ~extra:(kid arg) t
    | DefT (_, trust, EnumT { enum_id; enum_name; members = _; representation_t = _ })
    | DefT (_, trust, EnumObjectT { enum_id; enum_name; members = _; representation_t = _ }) ->
      p
        ~trust:(Some trust)
        ~extra:(spf "enum %s #%s" enum_name (ALoc.debug_to_string (enum_id :> ALoc.t)))
        t
    | AnnotT (_, arg, use_desc) -> p ~extra:(spf "use_desc=%b, %s" use_desc (kid arg)) t
    | OpaqueT (_, { underlying_t = Some arg; _ }) -> p ~extra:(spf "%s" (kid arg)) t
    | OpaqueT _ -> p t
    | OptionalT { reason = _; type_ = arg; use_desc = _ } -> p ~extra:(kid arg) t
    | EvalT (arg, expr, id) ->
      p ~extra:(spf "%s, %s" (defer_use expr (kid arg)) (Eval.string_of_id id)) t
    | TypeAppT (_, _, base, args) ->
      p ~extra:(spf "%s, [%s]" (kid base) (String.concat "; " (Base.List.map ~f:kid args))) t
    | ThisTypeAppT (_, base, this, args_opt) ->
      p
        ~reason:false
        ~extra:
          begin
            match args_opt with
            | Some args ->
              spf
                "%s, %s, [%s]"
                (kid base)
                (kid this)
                (String.concat "; " (Base.List.map ~f:kid args))
            | None -> spf "%s, %s" (kid base) (kid this)
          end
        t
    | ExactT (_, arg) -> p ~extra:(kid arg) t
    | MaybeT (_, arg) -> p ~extra:(kid arg) t
    | IntersectionT (_, rep) ->
      p ~extra:(spf "[%s]" (String.concat "; " (Base.List.map ~f:kid (InterRep.members rep)))) t
    | UnionT (_, rep) ->
      p
        ~extra:
          (spf
             "[%s]%s"
             (String.concat "; " (Base.List.map ~f:kid (UnionRep.members rep)))
             (UnionRep.string_of_specialization rep))
        t
    | MergedT (_, uses) ->
      p
        ~extra:
          ( "["
          ^ String.concat ", " (Base.List.map ~f:(dump_use_t_ (depth - 1, tvars) cx) uses)
          ^ "]" )
        t
    | DefT (_, trust, IdxWrapper inner_obj) -> p ~trust:(Some trust) ~extra:(kid inner_obj) t
    | DefT (_, trust, ReactAbstractComponentT _) -> p ~trust:(Some trust) t
    | ShapeT arg -> p ~reason:false ~extra:(kid arg) t
    | MatchingPropT (_, _, arg) -> p ~extra:(kid arg) t
    | KeysT (_, arg) -> p ~extra:(kid arg) t
    | DefT (_, trust, SingletonStrT s) -> p ~trust:(Some trust) ~extra:(spf "%S" s) t
    | DefT (_, trust, SingletonNumT (_, s)) -> p ~trust:(Some trust) ~extra:s t
    | DefT (_, trust, SingletonBoolT b) -> p ~trust:(Some trust) ~extra:(spf "%B" b) t
    | ModuleT (_, { exports_tmap; _ }, _) ->
      p
        t
        ~extra:
          ( Context.find_exports cx exports_tmap
          |> SMap.bindings
          |> Base.List.map ~f:(fun (name, (_, t)) -> kid t |> spf "%s: %s" name)
          |> String.concat ", "
          |> spf "[%s]" )
    | InternalT (ExtendsT (_, l, u)) -> p ~extra:(spf "%s, %s" (kid l) (kid u)) t
    | CustomFunT (_, kind) -> p ~extra:(custom_fun kind) t
    | InternalT (ChoiceKitT _) -> p t
    | TypeDestructorTriggerT (_, _, _, s, x) ->
      p ~extra:(spf "%s on upper, %s" (string_of_destructor s) (kid x)) t
    | OpenPredT { base_t = arg; m_pos = p_pos; m_neg = p_neg; reason = _ } ->
      p
        t
        ~extra:
          (spf
             "%s, {%s}, {%s}"
             (kid arg)
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun (k, p) -> spf "%s: %s" (Key.string_of_key k) (string_of_predicate p))
                   (Key_map.elements p_pos)))
             (String.concat
                "; "
                (Base.List.map
                   ~f:(fun (k, p) -> spf "%s: %s" (Key.string_of_key k) (string_of_predicate p))
                   (Key_map.elements p_neg))))
    | ReposT (_, arg)
    | InternalT (ReposUpperT (_, arg)) ->
      p ~extra:(kid arg) t

and dump_use_t_ (depth, tvars) cx t =
  let p ?(reason = true) ?(extra = "") use_t =
    spf
      "%s (%s%s%s)"
      (string_of_use_ctor use_t)
      ( if reason then
        spf "%S" (dump_reason cx (reason_of_use_t use_t))
      else
        "" )
      ( if reason && extra <> "" then
        ", "
      else
        "" )
      extra
  in
  let kid t = dump_t_ (depth - 1, tvars) cx t in
  let use_kid use_t = dump_use_t_ (depth - 1, tvars) cx use_t in
  let tvar id = dump_tvar_ (depth - 1, tvars) cx id in
  let prop p = dump_prop_ (depth - 1, tvars) cx p in
  let string_of_use_op = string_of_use_op_rec in
  let call_arg_kid = function
    | Arg t -> kid t
    | SpreadArg t -> spf "...%s" (kid t)
  in
  let tlist ts = spf "[%s]" (String.concat "; " (Base.List.map ~f:kid ts)) in
  let props map =
    spf
      "{%s}"
      (String.concat "; " (SMap.fold (fun k p acc -> spf "%s = %s" k (prop p) :: acc) map []))
  in
  let propref = function
    | Named (r, x) -> spf "%S %s" (dump_reason cx r) x
    | Computed t -> kid t
  in
  let lookup_kind = function
    | NonstrictReturning (default_opt, testid_opt) ->
      spf
        "Nonstrict%s%s"
        (Base.Option.value_map default_opt ~default:"" ~f:(fun (t, _) ->
             spf " returning %s" (kid t)))
        (Base.Option.value_map testid_opt ~default:"" ~f:(fun (id, _) -> spf " for test id %d" id))
    | Strict r -> spf "Strict %S" (dump_reason cx r)
    | ShadowRead (_, ids) ->
      spf
        "ShadowRead [%s]"
        (String.concat "; " (Nel.to_list ids |> Base.List.map ~f:Properties.string_of_id))
    | ShadowWrite ids ->
      spf
        "ShadowWrite [%s]"
        (String.concat "; " (Nel.to_list ids |> Base.List.map ~f:Properties.string_of_id))
  in
  let lookup_action = function
    | ReadProp { tout; _ } -> spf "Read %s" (kid tout)
    | WriteProp { tin; _ } -> spf "Write %s" (kid tin)
    | LookupProp (op, p) -> spf "Lookup (%s, %s)" (string_of_use_op op) (prop p)
    | SuperProp (_, p) -> spf "Super %s" (prop p)
    | MatchProp (_, t) -> spf "Match %s" (kid t)
  in
  let specialize_cache = function
    | None -> "None"
    | Some rs -> spf "Some [%s]" (String.concat "; " @@ Base.List.map ~f:(dump_reason cx) rs)
  in
  let try_flow = function
    | UnionCases (use_op, t, _rep, ts) ->
      spf
        "(%s, %s, [%s])"
        (string_of_use_op use_op)
        (kid t)
        (String.concat "; " (Base.List.map ~f:kid ts))
    | IntersectionCases (ts, use_t) ->
      spf "([%s], %s)" (String.concat "; " (Base.List.map ~f:kid ts)) (use_kid use_t)
  in
  let react_kit =
    React.(
      let resolved_object (_, pmap, _, _) = props pmap in
      let resolve_array = function
        | ResolveArray -> "ResolveArray"
        | ResolveElem (todo, done_rev) -> spf "ResolveElem (%s, %s)" (tlist todo) (tlist done_rev)
      in
      let resolve_object = function
        | ResolveObject -> "ResolveObject"
        | ResolveDict (_, todo, acc) ->
          spf "ResolveDict (%s, %s)" (props todo) (resolved_object acc)
        | ResolveProp (k, todo, acc) ->
          spf "ResolveProp (%s, %s, %s)" k (props todo) (resolved_object acc)
      in
      let simplify_prop_type =
        SimplifyPropType.(
          function
          | ArrayOf -> "ArrayOf"
          | InstanceOf -> "InstanceOf"
          | ObjectOf -> "ObjectOf"
          | OneOf tool -> spf "OneOf (%s)" (resolve_array tool)
          | OneOfType tool -> spf "OneOfType (%s)" (resolve_array tool)
          | Shape tool -> spf "Shape (%s)" (resolve_object tool))
      in
      let create_class =
        CreateClass.(
          let tool = function
            | Spec _ -> "Spec"
            | Mixins _ -> "Mixins"
            | Statics _ -> "Statics"
            | PropTypes (_, tool) -> spf "PropTypes (%s)" (resolve_object tool)
            | DefaultProps _ -> "DefaultProps"
            | InitialState _ -> "InitialState"
          in
          let knot { this; static; state_t; default_t } =
            spf
              "{this = %s; static = %s; state = %s; default = %s}"
              (kid this)
              (kid static)
              (kid state_t)
              (kid default_t)
          in
          (fun t k -> spf "%s, %s" (tool t) (knot k)))
      in
      function
      | CreateElement0 (_, config, (children, children_spread), tout)
      | CreateElement (_, _, config, (children, children_spread), tout) ->
        p
          ~extra:
            (spf
               "CreateElement (%s; %s%s) => %s"
               (kid config)
               (String.concat "; " (Base.List.map ~f:kid children))
               (match children_spread with
               | Some children_spread -> spf "; ...%s" (kid children_spread)
               | None -> "")
               (kid tout))
          t
      | ConfigCheck config -> spf "ConfigCheck (%s)" (kid config)
      | GetProps tout -> spf "GetProps (%s)" (kid tout)
      | GetConfig tout -> spf "GetConfig (%s)" (kid tout)
      | GetConfigType (default_props, tout) ->
        spf "GetConfigType (%s, %s)" (kid default_props) (kid tout)
      | GetRef tout -> spf "GetRef (%s)" (kid tout)
      | SimplifyPropType (tool, tout) ->
        spf "SimplifyPropType (%s, %s)" (simplify_prop_type tool) (kid tout)
      | CreateClass (tool, knot, tout) ->
        spf "CreateClass (%s, %s)" (create_class tool knot) (kid tout))
  in
  let slice { Object.reason = _; props; dict; flags = { exact; _ } } =
    let xs =
      match dict with
      | Some { dict_polarity = p; _ } -> [Polarity.sigil p ^ "[]"]
      | None -> []
    in
    let xs =
      SMap.fold
        (fun k (t, _) xs ->
          let opt =
            match t with
            | OptionalT _ -> "?"
            | _ -> ""
          in
          (k ^ opt) :: xs)
        props
        xs
    in
    let xs = String.concat "; " xs in
    if exact then
      spf "{|%s|}" xs
    else
      spf "{%s}" xs
  in
  let operand_slice reason prop_map dict =
    let props =
      SMap.fold
        (fun k p acc ->
          match (Type.Property.read_t p, Type.Property.write_t p) with
          | (Some t, _)
          | (_, Some t) ->
            SMap.add k (t, true) acc
          | _ -> acc)
        prop_map
        SMap.empty
    in
    let flags = { exact = true; sealed = Sealed; frozen = false } in
    slice { Object.reason; props; dict; flags }
  in
  let object_kit =
    Object.(
      let join (_loc, op) =
        match op with
        | And -> "And"
        | Or -> "Or"
      in
      let resolved xs = spf "[%s]" (String.concat "; " (Base.List.map ~f:slice (Nel.to_list xs))) in
      let resolve = function
        | Next -> "Next"
        | List0 (todo, j) ->
          spf
            "List0 ([%s], %s)"
            (String.concat "; " (Base.List.map ~f:kid (Nel.to_list todo)))
            (join j)
        | List (todo, done_rev, j) ->
          spf
            "List ([%s], [%s], %s)"
            (String.concat "; " (Base.List.map ~f:kid todo))
            (String.concat "; " (Base.List.map ~f:resolved (Nel.to_list done_rev)))
            (join j)
      in
      let resolve_tool = function
        | Resolve tool -> spf "Resolve %s" (resolve tool)
        | Super (s, tool) -> spf "Super (%s, %s)" (slice s) (resolve tool)
      in
      let acc_element = function
        | Spread.InlineSlice { Spread.reason; prop_map; dict } -> operand_slice reason prop_map dict
        | Spread.ResolvedSlice xs -> resolved xs
      in
      let spread target state =
        Object.Spread.(
          let target =
            match target with
            | Annot { make_exact } -> spf "Annot { make_exact=%b }" make_exact
            | Value { make_seal } -> spf "Value {make_seal=%b" (bool_of_sealtype make_seal)
          in
          let spread_operand = function
            | Slice { Spread.reason; prop_map; dict } -> operand_slice reason prop_map dict
            | Type t -> kid t
          in
          let state =
            let { todo_rev; acc; spread_id; union_reason; curr_resolve_idx } = state in
            spf
              "{todo_rev=[%s]; acc=[%s]; spread_id=%s; curr_resolve_idx=%s; union_reason=%s}"
              (String.concat "; " (Base.List.map ~f:spread_operand todo_rev))
              (String.concat "; " (Base.List.map ~f:acc_element acc))
              (string_of_int spread_id)
              (string_of_int curr_resolve_idx)
              (Base.Option.value_map union_reason ~default:"None" ~f:(dump_reason cx))
          in
          spf "Spread (%s, %s)" target state)
      in
      let rest merge_mode state =
        Object.Rest.(
          spf
            "Rest ({merge_mode=%s}, %s)"
            (match merge_mode with
            | Sound -> "Sound"
            | IgnoreExactAndOwn -> "IgnoreExactAndOwn"
            | ReactConfigMerge _ -> "ReactConfigMerge")
            (match state with
            | One t -> spf "One (%s)" (kid t)
            | Done o -> spf "Done (%s)" (resolved o)))
      in
      let react_props state =
        Object.ReactConfig.(
          spf
            "(%s)"
            (match state with
            | Config _ -> "Config"
            | Defaults _ -> "Defaults"))
      in
      let tool = function
        | ReadOnly -> "ReadOnly"
        | ObjectRep -> "ObjectRep"
        | ObjectWiden id -> spf "ObjectWiden (%s)" (string_of_int id)
        | Spread (options, state) -> spread options state
        | Rest (options, state) -> rest options state
        | ReactConfig state -> react_props state
      in
      (fun a b -> spf "(%s, %s)" (resolve_tool a) (tool b)))
  in
  if depth = 0 then
    string_of_use_ctor t
  else
    match t with
    | UseT (use_op, OpenT (r, id)) ->
      spf "UseT (%s, OpenT (%S, %d))" (string_of_use_op use_op) (dump_reason cx r) id
    | UseT (use_op, (DefT (_, trust, _) as t)) ->
      spf
        "UseT (%s, %s%s)"
        (string_of_use_op use_op)
        ( if Context.trust_tracking cx then
          string_of_trust_rep (lookup_trust cx) trust
        else
          "" )
        (kid t)
    | UseT (use_op, t) -> spf "UseT (%s, %s)" (string_of_use_op use_op) (kid t)
    | AdderT (use_op, _, _, x, y) ->
      p ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (kid x) (kid y)) t
    | AndT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
    | ArrRestT (use_op, _, _, _) -> p ~extra:(string_of_use_op use_op) t
    | AssertArithmeticOperandT _ -> p t
    | AssertBinaryInLHST _ -> p t
    | AssertBinaryInRHST _ -> p t
    | AssertForInRHST _ -> p t
    | AssertImportIsValueT _ -> p t
    | BecomeT (_, arg) -> p ~extra:(kid arg) t
    | BindT _ -> p t
    | CallElemT (_, _, _, _) -> p t
    | CallT (use_op, _, { call_args_tlist; call_tout; call_this_t; _ }) ->
      p
        ~extra:
          (spf
             "%s, <this: %s>(%s) => %s"
             (string_of_use_op use_op)
             (kid call_this_t)
             (String.concat "; " (Base.List.map ~f:call_arg_kid call_args_tlist))
             (kid call_tout))
        t
    | CallLatentPredT _ -> p t
    | CallOpenPredT _ -> p t
    | ChoiceKitUseT (_, TryFlow (_, spec)) -> p ~extra:(try_flow spec) t
    | ChoiceKitUseT (_, FullyResolveType id) -> p ~extra:(tvar id) t
    | CJSExtractNamedExportsT _ -> p t
    | CJSRequireT _ -> p t
    | ComparatorT (_, _, arg) -> p ~extra:(kid arg) t
    | ConstructorT _ -> p t
    | CopyNamedExportsT _ -> p t
    | CopyTypeExportsT _ -> p t
    | DebugPrintT _ -> p t
    | DebugSleepT _ -> p t
    | ElemT _ -> p t
    | EqT (_, _, arg) -> p ~extra:(kid arg) t
    | ExportNamedT (_, tmap, _export_kind, arg) ->
      p
        t
        ~extra:
          (spf
             "%s, {%s}"
             (kid arg)
             (String.concat "; " (Base.List.map ~f:(fun (x, _) -> x) (SMap.bindings tmap))))
    | ExportTypeT _ -> p t
    | AssertExportIsTypeT _ -> p t
    | GetElemT (_, _, ix, etype) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
    | GetKeysT _ -> p t
    | GetValuesT _ -> p t
    | MatchPropT (use_op, _, prop, ptype)
    | GetPropT (use_op, _, prop, ptype) ->
      p ~extra:(spf "%s, (%s), %s" (string_of_use_op use_op) (propref prop) (kid ptype)) t
    | GetPrivatePropT (_, _, prop, _, _, ptype) -> p ~extra:(spf "(%s), %s" prop (kid ptype)) t
    | GetProtoT (_, arg) -> p ~extra:(kid arg) t
    | GetStaticsT (_, arg) -> p ~extra:(kid arg) t
    | GuardT (pred, result, sink) ->
      p ~reason:false ~extra:(spf "%s, %s, %s" (string_of_predicate pred) (kid result) (kid sink)) t
    | HasOwnPropT _ -> p t
    | IdxUnMaybeifyT _ -> p t
    | IdxUnwrap _ -> p t
    | ImportDefaultT _ -> p t
    | ImportModuleNsT _ -> p t
    | ImportNamedT _ -> p t
    | ImportTypeofT _ -> p t
    | ImportTypeT _ -> p t
    | IntersectionPreprocessKitT _ -> p t
    | InvariantT _ -> p t
    | LookupT { lookup_kind = kind; propref = prop; lookup_action = action; ids; _ } ->
      p
        ~extra:
          (spf
             "%S, %s, %s, [%s]"
             (propref prop)
             (lookup_kind kind)
             (lookup_action action)
             (String.concat
                "; "
                (Properties.Set.elements ids |> Base.List.map ~f:Properties.string_of_id)))
        t
    | MakeExactT _ -> p t
    | MapTypeT _ -> p t
    | MethodT (_, _, _, prop, _, _) -> p ~extra:(spf "(%s)" (propref prop)) t
    | MixinT (_, arg) -> p ~extra:(kid arg) t
    | NotT (_, arg) -> p ~extra:(kid arg) t
    | NullishCoalesceT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
    | ObjAssignToT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjAssignFromT (_, _, arg1, arg2, _) -> p t ~extra:(spf "%s, %s" (kid arg1) (kid arg2))
    | ObjFreezeT _ -> p t
    | ObjRestT (_, xs, arg) -> p t ~extra:(spf "[%s], %s" (String.concat "; " xs) (kid arg))
    | ObjSealT _ -> p t
    | ObjTestProtoT _ -> p t
    | ObjTestT _ -> p t
    | OptionalChainT (_, _, _, t', void_t) -> p ~extra:(spf "%s, %s" (use_kid t') (kid void_t)) t
    | OrT (_, x, y) -> p ~extra:(spf "%s, %s" (kid x) (kid y)) t
    | PredicateT (pred, arg) ->
      p ~reason:false ~extra:(spf "%s, %s" (string_of_predicate pred) (kid arg)) t
    | ReactKitT (use_op, _, tool) ->
      p t ~extra:(spf "%s, %s" (string_of_use_op use_op) (react_kit tool))
    | RefineT _ -> p t
    | ReactPropsToOut (_, props)
    | ReactInToProps (_, props) ->
      p ~extra:(kid props |> spf "%s") t
    | ReposLowerT (_, use_desc, arg) -> p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid arg))
    | ReposUseT (_, use_desc, use_op, arg) ->
      p t ~extra:(spf "use_desc=%b, %s" use_desc (use_kid (UseT (use_op, arg))))
    | ResolveSpreadT (use_op, _, { rrt_resolve_to; _ }) ->
      (match rrt_resolve_to with
      | ResolveSpreadsToArrayLiteral (_, elem_t, tout)
      | ResolveSpreadsToArray (elem_t, tout) ->
        p ~extra:(spf "%s, %s, %s" (string_of_use_op use_op) (kid elem_t) (kid tout)) t
      | ResolveSpreadsToMultiflowPartial (_, _, _, tout) ->
        p ~extra:(spf "%s, %s" (string_of_use_op use_op) (kid tout)) t
      | ResolveSpreadsToCallT (_, tin) ->
        p ~extra:(spf "%s, %s" (string_of_use_op use_op) (kid tin)) t
      | ResolveSpreadsToMultiflowCallFull _
      | ResolveSpreadsToMultiflowSubtypeFull _
      | ResolveSpreadsToCustomFunCall _ ->
        p ~extra:(string_of_use_op use_op) t)
    | SentinelPropTestT (_, l, _key, sense, sentinel, result) ->
      p
        ~reason:false
        ~extra:(spf "%s, %b, %s, %s" (kid l) sense (string_of_sentinel sentinel) (kid result))
        t
    | SubstOnPredT _ -> p t
    | SuperT _ -> p t
    | ImplementsT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | SetElemT (_, _, ix, _, etype, _) -> p ~extra:(spf "%s, %s" (kid ix) (kid etype)) t
    | SetPropT (use_op, _, prop, _, _, ptype, _) ->
      p ~extra:(spf "%s, (%s), %s" (string_of_use_op use_op) (propref prop) (kid ptype)) t
    | SetPrivatePropT (_, _, prop, _, _, _, ptype, _) ->
      p ~extra:(spf "(%s), %s" prop (kid ptype)) t
    | SetProtoT (_, arg) -> p ~extra:(kid arg) t
    | SpecializeT (_, _, _, cache, args_opt, ret) ->
      p
        ~extra:
          begin
            match args_opt with
            | Some args ->
              spf
                "%s, [%s], %s"
                (specialize_cache cache)
                (String.concat "; " (Base.List.map ~f:kid args))
                (kid ret)
            | None -> spf "%s, %s" (specialize_cache cache) (kid ret)
          end
        t
    | StrictEqT { arg; _ } -> p ~extra:(kid arg) t
    | ObjKitT (use_op, _, resolve_tool, tool, tout) ->
      p
        ~extra:
          (spf "%s, %s, %s" (string_of_use_op use_op) (object_kit resolve_tool tool) (kid tout))
        t
    | TestPropT (_, _, prop, ptype) -> p ~extra:(spf "(%s), %s" (propref prop) (kid ptype)) t
    | ThisSpecializeT (_, this, _) -> p ~extra:(spf "%s" (kid this)) t
    | ToStringT (_, arg) -> p ~extra:(use_kid arg) t
    | UnaryMinusT _ -> p t
    | UnifyT (x, y) -> p ~reason:false ~extra:(spf "%s, %s" (kid x) (kid y)) t
    | VarianceCheckT (_, _, args, pol) ->
      p
        ~extra:
          (spf "[%s], %s" (String.concat "; " (Base.List.map ~f:kid args)) (Polarity.string pol))
        t
    | ConcretizeTypeAppsT _ -> p t
    | TypeAppVarianceCheckT _ -> p t
    | TypeCastT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | EnumCastT { use_op = _; enum = (reason, trust, enum) } ->
      p ~reason:false ~extra:(kid (DefT (reason, trust, EnumT enum))) t
    | FilterOptionalT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | FilterMaybeT (_, arg) -> p ~reason:false ~extra:(kid arg) t
    | CondT (_, then_t, else_t, tout) ->
      p
        t
        ~extra:
          (spf
             "%s, %s, %s"
             (match then_t with
             | None -> "None"
             | Some t -> spf "Some (%s)" (kid t))
             (kid else_t)
             (kid tout))
    | ExtendsUseT (_, _, nexts, l, u) ->
      p
        ~extra:
          (spf "[%s], %s, %s" (String.concat "; " (Base.List.map ~f:kid nexts)) (kid l) (kid u))
        t
    | DestructuringT (_, k, s, tout) ->
      p t ~extra:(spf "%s, %s, %s" (string_of_destruct_kind k) (string_of_selector s) (kid tout))
    | CreateObjWithComputedPropT { reason = _; value; tout_tvar } ->
      p t ~extra:(spf "%s %s" (kid value) (kid (OpenT tout_tvar)))
    | ResolveUnionT { resolved; unresolved; upper; id; _ } ->
      p
        t
        ~extra:
          (spf
             "%d [%s], [%s], %s"
             id
             (String.concat "; " (Base.List.map ~f:kid resolved))
             (String.concat "; " (Base.List.map ~f:kid unresolved))
             (use_kid upper))
    | ModuleExportsAssignT (_, _, _) -> p t

and dump_tvar_ (depth, tvars) cx id =
  if ISet.mem id tvars then
    spf "%d, ^" id
  else
    let stack = ISet.add id tvars in
    Constraint.(
      try
        match Context.find_tvar cx id with
        | Goto g -> spf "%d, Goto %d" id g
        | Root { constraints = Resolved (_, t) | FullyResolved (_, t); _ } ->
          spf "%d, Resolved %s" id (dump_t_ (depth - 1, stack) cx t)
        | Root { constraints = Unresolved { lower; upper; _ }; _ } ->
          if lower = TypeMap.empty && upper = UseTypeMap.empty then
            spf "%d" id
          else
            spf
              "%d, [%s], [%s]"
              id
              (String.concat
                 "; "
                 (List.rev
                    (TypeMap.fold (fun t _ acc -> dump_t_ (depth - 1, stack) cx t :: acc) lower [])))
              (String.concat
                 "; "
                 (List.rev
                    (UseTypeMap.fold
                       (fun use_t _ acc -> dump_use_t_ (depth - 1, stack) cx use_t :: acc)
                       upper
                       [])))
      with Context.Tvar_not_found _ -> spf "Not Found: %d" id)

and dump_prop_ (depth, tvars) cx p =
  let kid t = dump_t_ (depth, tvars) cx t in
  match p with
  | Field (_loc, t, polarity) -> spf "Field (%s) %s" (string_of_polarity polarity) (kid t)
  | Get (_loc, t) -> spf "Get %s" (kid t)
  | Set (_loc, t) -> spf "Set %s" (kid t)
  | GetSet (_loc1, t1, _loc2, t2) -> spf "Get %s Set %s" (kid t1) (kid t2)
  | Method (_loc, t) -> spf "Method %s" (kid t)

(* This is the type-dump debugging API.
   We should make sure these are not called recursively to avoid circumventing
   one of the termination mechanisms: depth or tvar-set.
*)
let dump_t ?(depth = 3) cx t = dump_t_ (depth, ISet.empty) cx t

let dump_use_t ?(depth = 3) cx t = dump_use_t_ (depth, ISet.empty) cx t

let dump_prop ?(depth = 3) cx p = dump_prop_ (depth, ISet.empty) cx p

let dump_tvar ?(depth = 3) cx id = dump_tvar_ (depth, ISet.empty) cx id

let dump_flow ?(depth = 3) cx (l, u) =
  spf "Lower: %s ~>\n Upper: %s" (dump_t ~depth cx l) (dump_use_t ~depth cx u)

(*****************************************************)

(* scopes and types *)

let string_of_scope_entry =
  Scope.(
    let string_of_value_binding
        cx { Entry.kind; value_state; value_declare_loc; value_assign_loc; specific; general } =
      spf
        "{ kind: %s; value_state: %s; value_declare_loc: %S; value_assign_loc: %s; specific: %s; general: %s }"
        (Entry.string_of_value_kind kind)
        (State.to_string value_state)
        (string_of_aloc value_declare_loc)
        (string_of_aloc value_assign_loc)
        (dump_t cx specific)
        (dump_t cx general)
    in
    let string_of_type_binding cx { Entry.type_state; type_loc; type_; type_binding_kind = _ } =
      spf
        "{ type_state: %s; type_loc: %S; type_: %s }"
        (State.to_string type_state)
        (string_of_aloc type_loc)
        (dump_t cx type_)
    in
    fun cx ->
      Entry.(
        function
        | Value r -> spf "Value %s" (string_of_value_binding cx r)
        | Type r -> spf "Type %s" (string_of_type_binding cx r)
        | Class r -> spf "Class %s" (ALoc.debug_to_string (r.class_binding_id :> ALoc.t))))

let string_of_scope_entries cx entries =
  let strings =
    SMap.fold
      (fun name entry acc -> spf "%s: %s" name (string_of_scope_entry cx entry) :: acc)
      entries
      []
    |> String.concat "; \n"
  in
  spf "[ %s ]" strings

let string_of_scope_refi cx { Scope.refi_loc; refined; original } =
  spf
    "{ refi_loc: %S; refined: %s; original: %s }"
    (string_of_aloc refi_loc)
    (dump_t cx refined)
    (dump_t cx original)

let string_of_scope_refis cx refis =
  let strings =
    Key_map.fold
      (fun key refi acc ->
        spf "%s: %s" (Key.string_of_key key) (string_of_scope_refi cx refi) :: acc)
      refis
      []
    |> String.concat ";\n"
  in
  spf "[ %s ]" strings

let string_of_scope cx scope =
  Scope.(
    spf
      "{ kind: %s;\nentries:\n%s\nrefis:\n%s\n}"
      (string_of_kind scope.kind)
      (string_of_scope_entries cx scope.entries)
      (string_of_scope_refis cx scope.refis))

let string_of_reason cx reason =
  let strip_root =
    if Context.should_strip_root cx then
      Some (Context.root cx)
    else
      None
  in
  Reason.string_of_reason ~strip_root reason

let string_of_file cx =
  let filename = File_key.to_string (Context.file cx) in
  match Context.is_verbose cx with
  | false -> filename
  | true ->
    let root_str = Path.to_string (Context.root cx) ^ Filename.dir_sep in
    if String_utils.string_starts_with filename root_str then
      Files.relative_path root_str filename
    else
      filename

let string_of_default =
  Default.fold
    ~expr:(fun (loc, _) -> spf "Expr %s" (string_of_loc loc))
    ~selector:(fun _ str sel -> spf "Selector (%s) (%s)" str (string_of_selector sel))
    ~cons:(fun str default -> spf "Cons (%s) (%s)" str default)

let string_of_signature_error pp_loc err =
  let open Signature_error in
  let module Sort = Signature_builder_kind.Sort in
  match err with
  | ExpectedSort (sort, x, loc) -> spf "%s @ %s is not a %s" x (pp_loc loc) (Sort.to_string sort)
  | ExpectedAnnotation (loc, sort) ->
    spf "Expected annotation at %s @ %s" (Expected_annotation_sort.to_string sort) (pp_loc loc)
  | InvalidTypeParamUse loc -> spf "Invalid use of type parameter @ %s" (pp_loc loc)
  | UnexpectedObjectKey (_loc, key_loc) -> spf "Expected simple object key @ %s" (pp_loc key_loc)
  | UnexpectedObjectSpread (_loc, spread_loc) ->
    spf "Unexpected object spread @ %s" (pp_loc spread_loc)
  | UnexpectedArraySpread (_loc, spread_loc) ->
    spf "Unexpected array spread @ %s" (pp_loc spread_loc)
  | UnexpectedArrayHole loc -> spf "Unexpected array hole @ %s" (pp_loc loc)
  | EmptyArray loc -> spf "Cannot determine the element type of an empty array @ %s" (pp_loc loc)
  | EmptyObject loc ->
    spf "Cannot determine types of initialized properties of an empty object @ %s" (pp_loc loc)
  | UnexpectedExpression (loc, esort) ->
    spf
      "Cannot determine the type of this %s @ %s"
      (Flow_ast_utils.ExpressionSort.to_string esort)
      (pp_loc loc)
  | SketchyToplevelDef loc ->
    spf "Unexpected toplevel definition that needs hoisting @ %s" (pp_loc loc)
  | UnsupportedPredicateExpression loc -> spf "Unsupported predicate expression @ %s" (pp_loc loc)
  | TODO (msg, loc) -> spf "TODO: %s @ %s" msg (pp_loc loc)

let dump_error_message =
  let open Error_message in
  let string_of_use_op = string_of_use_op_rec in
  let dump_internal_error = function
    | PackageHeapNotFound _ -> "PackageHeapNotFound"
    | AbnormalControlFlow -> "AbnormalControlFlow"
    | MethodNotAFunction -> "MethodNotAFunction"
    | OptionalMethod -> "OptionalMethod"
    | OpenPredWithoutSubst -> "OpenPredWithoutSubst"
    | PredFunWithoutParamNames -> "PredFunWithoutParamNames"
    | UnsupportedGuardPredicate _ -> "UnsupportedGuardPredicate"
    | BreakEnvMissingForCase -> "BreakEnvMissingForCase"
    | PropertyDescriptorPropertyCannotBeRead -> "PropertyDescriptorPropertyCannotBeRead"
    | ForInLHS -> "ForInLHS"
    | ForOfLHS -> "ForOfLHS"
    | InstanceLookupComputed -> "InstanceLookupComputed"
    | PropRefComputedOpen -> "PropRefComputedOpen"
    | PropRefComputedLiteral -> "PropRefComputedLiteral"
    | ShadowReadComputed -> "ShadowReadComputed"
    | ShadowWriteComputed -> "ShadowWriteComputed"
    | RestParameterNotIdentifierPattern -> "RestParameterNotIdentifierPattern"
    | InterfaceTypeSpread -> "InterfaceTypeSpread"
    | Error_message.DebugThrow -> "DebugThrow"
    | MergeTimeout _ -> "MergeTimeout"
    | MergeJobException _ -> "MergeJobException"
    | CheckTimeout _ -> "CheckTimeout"
    | CheckJobException _ -> "CheckJobException"
    | UnexpectedTypeapp _ -> "UnexpectedTypeapp"
  in
  let dump_upper_kind = function
    | IncompatibleGetPropT _ -> "IncompatibleGetPropT"
    | IncompatibleSetPropT _ -> "IncompatibleSetPropT"
    | IncompatibleMatchPropT _ -> "IncompatibleSetPropT"
    | IncompatibleGetPrivatePropT -> "IncompatibleGetPrivatePropT"
    | IncompatibleSetPrivatePropT -> "IncompatibleSetPrivatePropT"
    | IncompatibleMethodT _ -> "IncompatibleMethodT"
    | IncompatibleCallT -> "IncompatibleCallT"
    | IncompatibleMixedCallT -> "IncompatibleMixedCallT"
    | IncompatibleConstructorT -> "IncompatibleConstructorT"
    | IncompatibleGetElemT _ -> "IncompatibleGetElemT"
    | IncompatibleSetElemT _ -> "IncompatibleSetElemT"
    | IncompatibleCallElemT _ -> "IncompatibleCallElemT"
    | IncompatibleElemTOfArrT -> "IncompatibleElemTOfArrT"
    | IncompatibleObjAssignFromTSpread -> "IncompatibleObjAssignFromTSpread"
    | IncompatibleObjAssignFromT -> "IncompatibleObjAssignFromT"
    | IncompatibleObjRestT -> "IncompatibleObjRestT"
    | IncompatibleObjSealT -> "IncompatibleObjSealT"
    | IncompatibleArrRestT -> "IncompatibleArrRestT"
    | IncompatibleSuperT -> "IncompatibleSuperT"
    | IncompatibleMixinT -> "IncompatibleMixinT"
    | IncompatibleSpecializeT -> "IncompatibleSpecializeT"
    | IncompatibleThisSpecializeT -> "IncompatibleThisSpecializeT"
    | IncompatibleVarianceCheckT -> "IncompatibleVarianceCheckT"
    | IncompatibleGetKeysT -> "IncompatibleGetKeysT"
    | IncompatibleHasOwnPropT _ -> "IncompatibleHasOwnPropT"
    | IncompatibleGetValuesT -> "IncompatibleGetValuesT"
    | IncompatibleUnaryMinusT -> "IncompatibleUnaryMinusT"
    | IncompatibleMapTypeTObject -> "IncompatibleMapTypeTObject"
    | IncompatibleTypeAppVarianceCheckT -> "IncompatibleTypeAppVarianceCheckT"
    | IncompatibleGetStaticsT -> "IncompatibleGetStaticsT"
    | IncompatibleUnclassified ctor -> spf "IncompatibleUnclassified %S" ctor
  in
  fun cx err ->
    match err with
    | EIncompatible
        {
          lower = (reason_lower, _lower_kind);
          upper = (reason_upper, upper_kind);
          use_op;
          branches = _;
        } ->
      spf
        "EIncompatible { lower = (%s, _); upper = (%s, %s); use_op = %s; branches = _ }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (dump_upper_kind upper_kind)
        (match use_op with
        | None -> "None"
        | Some use_op -> spf "Some(%s)" (string_of_use_op use_op))
    | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches = _ } ->
      spf
        "EIncompatibleDefs { reason_lower = %s; reason_upper = %s; use_op = %s; branches = _ }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EIncompatibleProp { reason_prop; reason_obj; special = _; prop = _; use_op = _ } ->
      spf
        "EIncompatibleProp { reason_prop = %s; reason_obj = %s; special = _; prop = _; use_op = _ }"
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
    | EDebugPrint (reason, _) -> spf "EDebugPrint (%s, _)" (dump_reason cx reason)
    | EExportValueAsType (reason, str) ->
      spf "EExportValueAsType (%s, %s)" (dump_reason cx reason) str
    | EImportValueAsType (reason, str) ->
      spf "EImportValueAsType (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsTypeof (reason, str) ->
      spf "EImportTypeAsTypeof (%s, %s)" (dump_reason cx reason) str
    | EImportTypeAsValue (reason, str) ->
      spf "EImportTypeAsValue (%s, %s)" (dump_reason cx reason) str
    | ERefineAsValue (reason, str) -> spf "ERefineAsValue (%s, %s)" (dump_reason cx reason) str
    | ENoDefaultExport (reason, module_name, _) ->
      spf "ENoDefaultExport (%s, %s)" (dump_reason cx reason) module_name
    | EOnlyDefaultExport (reason, module_name, export_name) ->
      spf "EOnlyDefaultExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | ENoNamedExport (reason, module_name, export_name, _) ->
      spf "ENoNamedExport (%s, %s, %s)" (dump_reason cx reason) module_name export_name
    | EMissingTypeArgs { reason_tapp; reason_arity; min_arity; max_arity } ->
      spf
        "EMissingTypeArgs { reason_tapp=%s; reason_arity=%s; min_arity=%d; max_arity=%d }"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        min_arity
        max_arity
    | EValueUsedAsType { reason_use } ->
      spf "EValueUsedAsType { use = %s }" (dump_reason cx reason_use)
    | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedStringLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedNumberLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
      spf
        "EExpectedBooleanLit { reason_lower = %s; reason_upper = %s; use_op = %s }"
        (dump_reason cx reason_lower)
        (dump_reason cx reason_upper)
        (string_of_use_op use_op)
    | EPropNotFound { prop_name = prop; reason_prop; reason_obj; use_op; suggestion } ->
      spf
        "EPropNotFound (%s, %s, %s, %s, %s)"
        (match prop with
        | Some prop -> spf "Some %s" prop
        | None -> "None")
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
        (string_of_use_op use_op)
        (match suggestion with
        | Some prop -> spf "Some %s" prop
        | None -> "None")
    | EPropNotReadable { reason_prop; prop_name; use_op } ->
      spf
        "EPropNotReadable { reason_prop = %s; prop_name = %s; use_op = %s }"
        (dump_reason cx reason_prop)
        (match prop_name with
        | Some x -> spf "%S" x
        | None -> "(computed)")
        (string_of_use_op use_op)
    | EPropNotWritable { reason_prop; prop_name; use_op } ->
      spf
        "EPropNotWritable { reason_prop = %s; prop_name = %s; use_op = %s }"
        (dump_reason cx reason_prop)
        (match prop_name with
        | Some x -> spf "%S" x
        | None -> "(computed)")
        (string_of_use_op use_op)
    | EPropPolarityMismatch ((reason1, reason2), x, _, _) ->
      spf
        "EPropPolarityMismatch ((%s, %s), %s, _, _)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (match x with
        | Some x -> spf "%S" x
        | None -> "(computed)")
    | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
      spf
        "EPolarityMismatch { reason=%s; name=%S; expected_polarity=%s; actual_polarity=%s }"
        (dump_reason cx reason)
        name
        (Polarity.string expected_polarity)
        (Polarity.string actual_polarity)
    | EBuiltinLookupFailed { reason; name } ->
      spf
        "EBuiltinLookupFailed { reason = %s; name = %S }"
        (dump_reason cx reason)
        (match name with
        | Some x -> spf "Some(%S)" x
        | None -> "None")
    | EStrictLookupFailed { reason_prop; reason_obj; name; suggestion; use_op } ->
      spf
        "EStrictLookupFailed { reason_prop = %s; reason_obj = %s; name = %S; suggestion = %S; use_op = %s }"
        (dump_reason cx reason_prop)
        (dump_reason cx reason_obj)
        (match name with
        | Some x -> spf "Some(%S)" x
        | None -> "None")
        (match suggestion with
        | Some x -> spf "Some(%S)" x
        | None -> "None")
        (match use_op with
        | Some use_op -> spf "Some(%s)" (string_of_use_op use_op)
        | None -> "None")
    | EPrivateLookupFailed ((reason1, reason2), x, use_op) ->
      spf
        "EPrivateLookupFailed ((%s, %s), %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        x
        (string_of_use_op use_op)
    | EAdditionMixed (reason, use_op) ->
      spf "EAdditionMixed (%s, %s)" (dump_reason cx reason) (string_of_use_op use_op)
    | EComparison (reason1, reason2) ->
      spf "EComparison (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | ETupleArityMismatch ((reason1, reason2), arity1, arity2, use_op) ->
      spf
        "ETupleArityMismatch (%s, %s, %d, %d, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        arity1
        arity2
        (string_of_use_op use_op)
    | ENonLitArrayToTuple ((reason1, reason2), use_op) ->
      spf
        "ENonLitArrayToTuple ((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | ETupleOutOfBounds { use_op; reason; reason_op; length; index } ->
      spf
        "ETupleOutOfBounds { use_op = %s; reason = %s; reason_op = %s; length = %d; index = %s }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (dump_reason cx reason_op)
        length
        index
    | ETupleNonIntegerIndex { use_op; reason; index } ->
      spf
        "ETupleNonIntegerIndex { use_op = %s; reason = %s; index = %s }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        index
    | ETupleUnsafeWrite { reason; use_op } ->
      spf
        "ETupleUnsafeWrite { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EROArrayWrite ((reason1, reason2), use_op) ->
      spf
        "EROArrayWrite (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnionSpeculationFailed { use_op; reason; reason_op; branches = _ } ->
      spf
        "EUnionSpeculationFailed { use_op = %s; reason = %s; reason_op = %s; branches = _ }"
        (string_of_use_op use_op)
        (dump_reason cx reason)
        (dump_reason cx reason_op)
    | ESpeculationAmbiguous { reason; _ } ->
      spf "ESpeculationAmbiguous { reason = %s; _ }" (dump_reason cx reason)
    | EIncompatibleWithExact ((reason1, reason2), use_op) ->
      spf
        "EIncompatibleWithExact ((%s, %s), %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnsupportedExact (reason1, reason2) ->
      spf "EUnsupportedExact (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EIdxArity reason -> spf "EIdxArity (%s)" (dump_reason cx reason)
    | EIdxUse1 reason -> spf "EIdxUse1 (%s)" (dump_reason cx reason)
    | EIdxUse2 reason -> spf "EIdxUse2 (%s)" (dump_reason cx reason)
    | EUnexpectedThisType loc -> spf "EUnexpectedThisType (%s)" (string_of_aloc loc)
    | ETypeParamArity (loc, expected) ->
      spf "ETypeParamArity (%s, %d)" (string_of_aloc loc) expected
    | ETypeParamMinArity (loc, expected) ->
      spf "ETypeParamMinArity (%s, %d)" (string_of_aloc loc) expected
    | ECallTypeArity { call_loc; is_new; reason_arity; expected_arity } ->
      spf
        "ECallTypeArity { call_loc=%s; is_new=%b; reason_arity=%s; expected_arity=%d; }"
        (string_of_aloc call_loc)
        is_new
        (dump_reason cx reason_arity)
        expected_arity
    | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
      spf
        "ETooManyTypeArgs (%s, %s, %d)"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        maximum_arity
    | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
      spf
        "ETooFewTypeArgs (%s, %s, %d)"
        (dump_reason cx reason_tapp)
        (dump_reason cx reason_arity)
        minimum_arity
    | EInvalidTypeArgs (reason_tapp, reason_arity) ->
      spf "EInvalidTypeArgs (%s, %s)" (dump_reason cx reason_tapp) (dump_reason cx reason_arity)
    | EPropertyTypeAnnot loc -> spf "EPropertyTypeAnnot (%s)" (string_of_aloc loc)
    | EExportsAnnot loc -> spf "EExportsAnnot (%s)" (string_of_aloc loc)
    | ECharSetAnnot loc -> spf "ECharSetAnnot (%s)" (string_of_aloc loc)
    | EInvalidCharSet { invalid = (reason, _); valid; use_op } ->
      spf
        "EInvalidCharSet { invalid = (%s, _); valid = %s; use_op = %s }"
        (dump_reason cx reason)
        (dump_reason cx valid)
        (string_of_use_op use_op)
    | EUnsupportedKeyInObjectType loc -> spf "EUnsupportedKeyInObjectType (%s)" (string_of_aloc loc)
    | EPredAnnot loc -> spf "EPredAnnot (%s)" (string_of_aloc loc)
    | ERefineAnnot loc -> spf "ERefineAnnot (%s)" (string_of_aloc loc)
    | ETrustedAnnot loc -> spf "ETrustedAnnot (%s)" (string_of_aloc loc)
    | EPrivateAnnot loc -> spf "EPrivateAnnot (%s)" (string_of_aloc loc)
    | EUnexpectedTypeof loc -> spf "EUnexpectedTypeof (%s)" (string_of_aloc loc)
    | EFunPredCustom ((reason1, reason2), msg) ->
      spf "EFunPredCustom (%s, %s, %S)" (dump_reason cx reason1) (dump_reason cx reason2) msg
    | EIncompatibleWithShape (lower, upper, use_op) ->
      spf
        "EIncompatibleWithShape (%s, %s, %s)"
        (dump_reason cx lower)
        (dump_reason cx upper)
        (string_of_use_op use_op)
    | EInternal (loc, err) ->
      spf "EInternal (%s, %s)" (string_of_aloc loc) (dump_internal_error err)
    | EUnsupportedSyntax (loc, _) -> spf "EUnsupportedSyntax (%s, _)" (string_of_aloc loc)
    | EUseArrayLiteral loc -> spf "EUseArrayLiteral (%s)" (string_of_aloc loc)
    | EMissingAnnotation (reason, _) -> spf "EMissingAnnotation (%s)" (dump_reason cx reason)
    | EBindingError (_binding_error, loc, x, entry) ->
      spf "EBindingError (_, %s, %s, %s)" (string_of_aloc loc) x (Scope.Entry.string_of_kind entry)
    | ERecursionLimit (reason1, reason2) ->
      spf "ERecursionLimit (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EModuleOutsideRoot (loc, name) -> spf "EModuleOutsideRoot (%s, %S)" (string_of_aloc loc) name
    | EMalformedPackageJson (loc, error) ->
      spf "EMalformedPackageJson (%s, %S)" (string_of_aloc loc) error
    | EExperimentalDecorators loc -> spf "EExperimentalDecorators (%s)" (string_of_aloc loc)
    | EExperimentalClassProperties (loc, static) ->
      spf "EExperimentalClassProperties (%s, %b)" (string_of_aloc loc) static
    | EUnsafeGetSet loc -> spf "EUnsafeGetSet (%s)" (string_of_aloc loc)
    | EUninitializedInstanceProperty (loc, err) ->
      spf
        "EUninitializedInstanceProperty (%s, %s)"
        (string_of_aloc loc)
        Lints.(
          match err with
          | PropertyNotDefinitelyInitialized -> "PropertyNotDefinitelyInitialized"
          | ReadFromUninitializedProperty -> "ReadFromUninitializedProperty"
          | MethodCallBeforeEverythingInitialized -> "MethodCallBeforeEverythingInitialized"
          | PropertyFunctionCallBeforeEverythingInitialized ->
            "PropertyFunctionCallBeforeEverythingInitialized"
          | ThisBeforeEverythingInitialized -> "ThisBeforeEverythingInitialized")
    | EExperimentalExportStarAs loc -> spf "EExperimentalExportStarAs (%s)" (string_of_aloc loc)
    | EExperimentalEnums loc -> spf "EExperimentalEnums (%s)" (string_of_aloc loc)
    | EIndeterminateModuleType loc -> spf "EIndeterminateModuleType (%s)" (string_of_aloc loc)
    | EBadExportPosition loc -> spf "EBadExportPosition (%s)" (string_of_aloc loc)
    | EBadExportContext (name, loc) -> spf "EBadExportContext (%s, %s)" name (string_of_aloc loc)
    | EUnreachable loc -> spf "EUnreachable (%s)" (string_of_aloc loc)
    | EInvalidObjectKit { reason; reason_op; use_op } ->
      spf
        "EInvalidObjectKit { reason = %s; reason_op = %s; use_op = %s }"
        (dump_reason cx reason)
        (dump_reason cx reason_op)
        (string_of_use_op use_op)
    | EInvalidTypeof (loc, name) -> spf "EInvalidTypeof (%s, %S)" (string_of_aloc loc) name
    | EBinaryInLHS reason -> spf "EBinaryInLHS (%s)" (dump_reason cx reason)
    | EBinaryInRHS reason -> spf "EBinaryInRHS (%s)" (dump_reason cx reason)
    | EArithmeticOperand reason
    | ENullVoidAddition reason ->
      spf "EArithmeticOperand (%s)" (dump_reason cx reason)
    | EForInRHS reason -> spf "EForInRHS (%s)" (dump_reason cx reason)
    | EObjectComputedPropertyAccess (reason1, reason2) ->
      spf "EObjectComputedPropertyAccess (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EObjectComputedPropertyAssign (reason1, reason2) ->
      spf "EObjectComputedPropertyAssign (%s, %s)" (dump_reason cx reason1) (dump_reason cx reason2)
    | EInvalidLHSInAssignment loc -> spf "EInvalidLHSInAssignment (%s)" (string_of_aloc loc)
    | EIncompatibleWithUseOp (reason1, reason2, use_op) ->
      spf
        "EIncompatibleWithUseOp (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | ETrustIncompatibleWithUseOp (reason1, reason2, use_op) ->
      spf
        "ETrustIncompatibleWithUseOp (%s, %s, %s)"
        (dump_reason cx reason1)
        (dump_reason cx reason2)
        (string_of_use_op use_op)
    | EUnsupportedImplements reason -> spf "EUnsupportedImplements (%s)" (dump_reason cx reason)
    | ENotAReactComponent { reason; use_op } ->
      spf
        "ENotAReactComponent { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactConfigType { reason; use_op } ->
      spf
        "EInvalidReactConfigType { reason = %s; use_op = %s }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactPropType { reason; use_op; tool = _ } ->
      spf
        "EInvalidReactPropType { reason = %s; use_op = %s; _ }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EInvalidReactCreateClass { reason; use_op; tool = _ } ->
      spf
        "EInvalidReactCreateClass { reason = %s; use_op = %s; _ }"
        (dump_reason cx reason)
        (string_of_use_op use_op)
    | EReactElementFunArity (reason, _, _) ->
      spf "EReactElementFunArity (%s)" (dump_reason cx reason)
    | EFunctionCallExtraArg (unused_reason, def_reason, param_count, use_op) ->
      spf
        "EFunctionCallExtraArg (%s, %s, %d, %s)"
        (dump_reason cx unused_reason)
        (dump_reason cx def_reason)
        param_count
        (string_of_use_op use_op)
    | EUnsupportedSetProto reason -> spf "EUnsupportedSetProto (%s)" (dump_reason cx reason)
    | EDuplicateModuleProvider { module_name; provider; conflict } ->
      spf
        "EDuplicateModuleProvider (%S, %s, %s)"
        module_name
        (string_of_aloc provider)
        (string_of_aloc conflict)
    | EParseError (loc, _parse_error) -> spf "EParseError (%s, _)" (string_of_aloc loc)
    (* TODO: string of parse error constructor *)
    | EDocblockError (loc, err) ->
      spf
        "EDocblockError (%s, %s)"
        (string_of_aloc loc)
        (match err with
        | MultipleFlowAttributes -> "MultipleFlowAttributes"
        | MultipleProvidesModuleAttributes -> "MultipleProvidesModuleAttributes"
        | MultipleJSXAttributes -> "MultipleJSXAttributes"
        | InvalidJSXAttribute _ -> "InvalidJSXAttribute")
    | EImplicitInexactObject loc -> spf "EImplicitInexactObject (%s)" (string_of_aloc loc)
    | EAmbiguousObjectType loc -> spf "EAmbiguousObjectType (%s)" (string_of_aloc loc)
    | EUntypedTypeImport (loc, module_name) ->
      spf "EUntypedTypeImport (%s, %s)" (string_of_aloc loc) module_name
    | EUntypedImport (loc, module_name) ->
      spf "EUntypedImport (%s, %s)" (string_of_aloc loc) module_name
    | ENonstrictImport loc -> spf "ENonstrictImport (%s)" (string_of_aloc loc)
    | EUnclearType loc -> spf "EUnclearType (%s)" (string_of_aloc loc)
    | EDeprecatedUtility (loc, name) -> spf "EDeprecatedUtility (%s, %s)" (string_of_aloc loc) name
    | EDynamicExport (reason, reason') ->
      spf "EDynamicExport (%s, %s)" (dump_reason cx reason) (dump_reason cx reason')
    | EDeprecatedType loc -> spf "EDeprecatedType (%s)" (string_of_aloc loc)
    | EUnsafeGettersSetters loc -> spf "EUnclearGettersSetters (%s)" (string_of_aloc loc)
    | EUnusedSuppression loc -> spf "EUnusedSuppression (%s)" (string_of_aloc loc)
    | ELintSetting (loc, kind) ->
      LintSettings.(
        let kind_str =
          match kind with
          | Invalid_setting -> "Invalid_setting"
          | Malformed_argument -> "Malformed_argument"
          | Naked_comment -> "Naked_comment"
          | Nonexistent_rule -> "Nonexistent_rule"
          | Overwritten_argument -> "Overwritten_argument"
          | Redundant_argument -> "Redundant_argument"
        in
        spf "ELintSetting (%s, %s)" (string_of_aloc loc) kind_str)
    | ESketchyNullLint { kind; loc; null_loc; falsy_loc } ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNullBool -> "SketchyNullBool"
          | SketchyNullString -> "SketchyNullString"
          | SketchyNullNumber -> "SketchyNullNumber"
          | SketchyNullMixed -> "SketchyNullMixed"
          | SketchyNullEnumBool -> "SketchyNullEnumBool"
          | SketchyNullEnumString -> "SketchyNullEnumString"
          | SketchyNullEnumNumber -> "SketchyNullEnumNumber"
        in
        spf
          "ESketchyNullLint {kind=%s; loc=%s; null_loc=%s; falsy_loc=%s}"
          kind_str
          (string_of_aloc loc)
          (string_of_aloc null_loc)
          (string_of_aloc falsy_loc))
    | ESketchyNumberLint (kind, reason) ->
      Lints.(
        let kind_str =
          match kind with
          | SketchyNumberAnd -> "SketchyNumberAnd"
        in
        spf "ESketchyNumberLint (%s) (%s)" kind_str (dump_reason cx reason))
    | EInvalidPrototype (loc, reason) ->
      spf "EInvalidPrototype (%s) (%s)" (string_of_aloc loc) (dump_reason cx reason)
    | EExperimentalOptionalChaining loc ->
      spf "EExperimentalOptionalChaining (%s)" (string_of_aloc loc)
    | EOptionalChainingMethods loc -> spf "EOptionalChainingMethods (%s)" (string_of_aloc loc)
    | EUnnecessaryOptionalChain (loc, _) ->
      spf "EUnnecessaryOptionalChain (%s)" (string_of_aloc loc)
    | EUnnecessaryInvariant (loc, _) -> spf "EUnnecessaryInvariant (%s)" (string_of_aloc loc)
    | EUnexpectedTemporaryBaseType loc ->
      spf "EUnexpectedTemporaryBaseType (%s)" (string_of_aloc loc)
    | ECannotDelete (l1, r1) -> spf "ECannotDelete (%s, %s)" (string_of_aloc l1) (dump_reason cx r1)
    | ESignatureVerification sve ->
      let msg = string_of_signature_error ALoc.debug_to_string sve in
      spf "ESignatureVerification (%s)" msg
    | EBigIntNotYetSupported reason -> spf "EBigIntNotYetSupported (%s)" (dump_reason cx reason)
    | ECannotSpreadInterface { spread_reason; interface_reason } ->
      spf
        "ECannotSpreadInterface (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx interface_reason)
    | ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason } ->
      spf
        "ECannotSpreadIndexerOnRight (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx object_reason)
        (dump_reason cx key_reason)
    | EUnableToSpread { spread_reason; object1_reason; object2_reason; propname; error_kind = _ } ->
      spf
        "EUnableToSpread (%s) (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx object1_reason)
        (dump_reason cx object2_reason)
        propname
    | EInexactMayOverwriteIndexer { spread_reason; key_reason; value_reason; object2_reason } ->
      spf
        "EInexactMayOverwriteIndexer (%s) (%s) (%s) (%s)"
        (dump_reason cx spread_reason)
        (dump_reason cx key_reason)
        (dump_reason cx value_reason)
        (dump_reason cx object2_reason)
    | EExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 } ->
      let format_reason_group { first_reason; second_reason } =
        spf
          "[%s; %s]"
          (dump_reason cx first_reason)
          (Base.Option.value_map ~default:"None" ~f:(dump_reason cx) second_reason)
      in
      spf
        "EExponentialSpread %s ([%s]) ([%s])"
        (dump_reason cx reason)
        (format_reason_group reasons_for_operand1)
        (format_reason_group reasons_for_operand2)
    | EComputedPropertyWithMultipleLowerBounds
        { computed_property_reason; new_lower_bound_reason; existing_lower_bound_reason } ->
      spf
        "EComputedPropertyWithMultipleLowerBounds (%s) (%s) (%s)"
        (dump_reason cx computed_property_reason)
        (dump_reason cx new_lower_bound_reason)
        (dump_reason cx existing_lower_bound_reason)
    | EComputedPropertyWithUnion { computed_property_reason; union_reason } ->
      spf
        "EComputedPropertyWithUnion (%s) (%s)"
        (dump_reason cx computed_property_reason)
        (dump_reason cx union_reason)
    | EEnumInvalidMemberAccess { member_name; members; access_reason; enum_reason } ->
      spf
        "EEnumInvalidMemberAccess (%s) (%s) (%s) (%s)"
        (Base.Option.value ~default:"<None>" member_name)
        (SSet.elements members |> String.concat ", ")
        (dump_reason cx access_reason)
        (dump_reason cx enum_reason)
    | EEnumModification { loc; enum_reason } ->
      spf "EEnumModification (%s) (%s)" (string_of_aloc loc) (dump_reason cx enum_reason)
    | EEnumMemberDuplicateValue { loc; prev_use_loc; enum_reason } ->
      spf
        "EEnumMemberDuplicateValue (%s) (%s) (%s)"
        (string_of_aloc loc)
        (string_of_aloc prev_use_loc)
        (dump_reason cx enum_reason)
    | EEnumMemberAlreadyChecked { reason; prev_check_reason; enum_reason; member_name } ->
      spf
        "EEnumMemberAlreadyChecked (%s) (%s) (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx prev_check_reason)
        (dump_reason cx enum_reason)
        member_name
    | EEnumAllMembersAlreadyChecked { reason; enum_reason } ->
      spf
        "EEnumAllMembersAlreadyChecked (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
    | EEnumNotAllChecked
        { reason; enum_reason; remaining_member_to_check; number_remaining_members_to_check } ->
      spf
        "EEnumNotAllChecked (%s) (%s) (%s) (%s)"
        (dump_reason cx reason)
        (dump_reason cx enum_reason)
        remaining_member_to_check
        (string_of_int number_remaining_members_to_check)
    | EEnumInvalidCheck { reason; enum_name; members } ->
      spf
        "EEnumInvalidCheck (%s) (%s) (%s)"
        (dump_reason cx reason)
        enum_name
        (SSet.elements members |> String.concat ", ")
    | EEnumMemberUsedAsType { reason; enum_name } ->
      spf "EEnumMemberUsedAsType (%s) (%s)" (dump_reason cx reason) enum_name
    | EEnumCheckedInIf reason -> spf "EEnumCheckedInIf (%s)" (dump_reason cx reason)
    | EAssignExportedConstLikeBinding { loc; definition; binding_kind } ->
      spf
        "EAssignExportedConstLikeBinding (%s) (%s) (%s)"
        (string_of_aloc loc)
        (dump_reason cx definition)
        (Scope.Entry.string_of_let_binding_kind binding_kind)

module Verbose = struct
  let print_if_verbose_lazy cx trace ?(delim = "") ?(indent = 0) (lines : string Lazy.t list) =
    match Context.verbose cx with
    | Some { Verbose.indent = num_spaces; _ } ->
      let indent = indent + Trace.trace_depth trace - 1 in
      let prefix = String.make (indent * num_spaces) ' ' in
      let pid = Context.pid_prefix cx in
      let add_prefix line = spf "\n%s%s%s" prefix pid (Lazy.force line) in
      let lines = Base.List.map ~f:add_prefix lines in
      prerr_endline (String.concat delim lines)
    | None -> ()

  let print_if_verbose cx trace ?(delim = "") ?(indent = 0) (lines : string list) =
    match Context.verbose cx with
    | Some _ ->
      let lines = Base.List.map ~f:(fun line -> lazy line) lines in
      print_if_verbose_lazy cx trace ~delim ~indent lines
    | None -> ()

  let print_types_if_verbose cx trace ?(note : string option) ((l : Type.t), (u : Type.use_t)) =
    match Context.verbose cx with
    | Some { Verbose.depth; _ } ->
      let delim =
        match note with
        | Some x -> spf " ~> %s" x
        | None -> " ~>"
      in
      print_if_verbose cx trace ~delim [dump_t ~depth cx l; dump_use_t ~depth cx u]
    | None -> ()
end
