(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Hh_json

(* Helper function to create a JSON object with a "kind" field *)
let json_with_type type_name fields = JSON_Object (("kind", JSON_String type_name) :: fields)

(* Convert type_t_kind to JSON *)
let json_of_type_t_kind = function
  | TypeAliasKind -> JSON_String "TypeAliasKind"
  | TypeParamKind -> JSON_String "TypeParamKind"
  | OpaqueKind -> JSON_String "OpaqueKind"
  | ImportTypeofKind -> JSON_String "ImportTypeofKind"
  | ImportClassKind -> JSON_String "ImportClassKind"
  | ImportEnumKind -> JSON_String "ImportEnumKind"
  | InstanceKind -> JSON_String "InstanceKind"
  | RenderTypeKind -> JSON_String "RenderTypeKind"

(* Convert funtype to JSON *)
let rec funtype_to_json cx depth (funtype : funtype) : json =
  let (this_t_type, this_status) = funtype.this_t in
  let this_status_json =
    match this_status with
    | This_Method { unbound } ->
      JSON_Object [("kind", JSON_String "This_Method"); ("unbound", JSON_Bool unbound)]
    | This_Function -> JSON_Object [("kind", JSON_String "This_Function")]
  in

  let params_json =
    JSON_Array
      (List.map
         (fun (name_opt, t) ->
           JSON_Object
             [
               ( "name",
                 match name_opt with
                 | None -> JSON_Null
                 | Some name -> JSON_String name
               );
               ("type", type_to_json cx (depth - 1) t);
             ])
         funtype.params
      )
  in

  let rest_param_json =
    match funtype.rest_param with
    | None -> JSON_Null
    | Some (name_opt, _, t) ->
      JSON_Object
        [
          ( "name",
            match name_opt with
            | None -> JSON_Null
            | Some name -> JSON_String name
          );
          ("type", type_to_json cx (depth - 1) t);
        ]
  in

  let type_guard_json =
    match funtype.type_guard with
    | None -> JSON_Null
    | Some (TypeGuard { inferred; reason = _; param_name; type_guard; one_sided }) ->
      let (_, name) = param_name in
      JSON_Object
        [
          ("inferred", JSON_Bool inferred);
          ("param_name", JSON_String name);
          ("type_guard", type_to_json cx (depth - 1) type_guard);
          ("one_sided", JSON_Bool one_sided);
        ]
  in

  let effect_json =
    match funtype.effect_ with
    | HookDecl id ->
      JSON_Object
        [
          ("kind", JSON_String "HookDecl"); ("id", JSON_String (ALoc.debug_to_string (id :> ALoc.t)));
        ]
    | HookAnnot -> JSON_Object [("kind", JSON_String "HookAnnot")]
    | ArbitraryEffect -> JSON_Object [("kind", JSON_String "ArbitraryEffect")]
    | AnyEffect -> JSON_Object [("kind", JSON_String "AnyEffect")]
  in

  JSON_Object
    [
      ( "this_t",
        JSON_Object
          [("type", type_to_json cx (depth - 1) this_t_type); ("status", this_status_json)]
      );
      ("params", params_json);
      ("rest_param", rest_param_json);
      ("return_t", type_to_json cx (depth - 1) funtype.return_t);
      ("type_guard", type_guard_json);
      ("effect", effect_json);
    ]

(* Forward declarations for recursive types *)
and type_to_json : Context.t -> int -> t -> json =
 fun cx depth t ->
  match t with
  | OpenT _ ->
    if depth <= 0 then
      json_with_type "Open" []
    else
      let t = Flow_js.singleton_concrete_type_for_inspection cx (TypeUtil.reason_of_t t) t in
      type_to_json cx (depth - 1) t
  | DefT (r, def_t) ->
    json_with_type
      "Def"
      [
        ("def", def_t_to_json cx depth def_t);
        ( "def_loc",
          Reason.def_loc_of_reason r
          |> ALoc.update_source (Base.Option.map ~f:(File_key.map Filename.basename))
          |> ALoc.to_loc_with_tables (Context.aloc_tables cx)
          |> Reason.json_of_loc ~offset_table:None
        );
      ]
  | EvalT (t, defer_use_t, _) ->
    json_with_type
      "Eval"
      [
        ("type", type_to_json cx (depth - 1) t);
        ( "destructor",
          match defer_use_t with
          | TypeDestructorT (_, _, destructor) -> json_of_destructor cx depth destructor
        );
      ]
  | GenericT { reason = _; name; bound; no_infer; id = _ } ->
    json_with_type
      "Generic"
      [
        ("name", JSON_String (Subst_name.string_of_subst_name name));
        ("bound", type_to_json cx (depth - 1) bound);
        ("no_infer", JSON_Bool no_infer);
      ]
  | ThisInstanceT (_, instance_t, is_this, name) ->
    json_with_type
      "ThisInstance"
      [
        ("instance", json_of_instance_t cx depth instance_t);
        ("is_this", JSON_Bool is_this);
        ("name", JSON_String (Subst_name.string_of_subst_name name));
      ]
  | ThisTypeAppT (_, t1, t2, t_list_opt) ->
    let fields = [("t1", type_to_json cx (depth - 1) t1); ("t2", type_to_json cx (depth - 1) t2)] in
    let fields =
      match t_list_opt with
      | None -> fields
      | Some t_list ->
        fields @ [("t_list", JSON_Array (List.map (type_to_json cx (depth - 1)) t_list))]
    in
    json_with_type "ThisTypeApp" fields
  | TypeAppT { reason = _; use_op = _; type_; targs; from_value; use_desc } ->
    json_with_type
      "TypeApp"
      [
        ("type", type_to_json cx (depth - 1) type_);
        ("targs", JSON_Array (List.map (type_to_json cx (depth - 1)) targs));
        ("from_value", JSON_Bool from_value);
        ("use_desc", JSON_Bool use_desc);
      ]
  | FunProtoT _ -> json_with_type "FunProto" []
  | ObjProtoT _ -> json_with_type "ObjProto" []
  | NullProtoT _ -> json_with_type "NullProto" []
  | FunProtoBindT _ -> json_with_type "FunProtoBind" []
  | IntersectionT (_, inter_rep) ->
    let members = InterRep.members inter_rep in
    json_with_type
      "Intersection"
      [("members", JSON_Array (List.map (type_to_json cx (depth - 1)) members))]
  | UnionT (_, union_rep) ->
    let members = UnionRep.members union_rep in
    json_with_type
      "Union"
      [("members", JSON_Array (List.map (type_to_json cx (depth - 1)) members))]
  | MaybeT (_, t) -> json_with_type "Maybe" [("type", type_to_json cx (depth - 1) t)]
  | OptionalT { reason = _; type_; use_desc } ->
    json_with_type
      "Optional"
      [("type", type_to_json cx (depth - 1) type_); ("use_desc", JSON_Bool use_desc)]
  | KeysT (_, t) -> json_with_type "Keys" [("type", type_to_json cx (depth - 1) t)]
  | AnnotT (_, t, use_desc) ->
    json_with_type
      "Annot"
      [("type", type_to_json cx (depth - 1) t); ("use_desc", JSON_Bool use_desc)]
  | OpaqueT (_, opaquetype) ->
    json_with_type
      "Opaque"
      [
        ( "opaquetype",
          JSON_Object
            [
              ("opaque_id", JSON_String (Opaque.string_of_id opaquetype.opaque_id));
              ( "underlying_t",
                match opaquetype.underlying_t with
                | None -> JSON_Null
                | Some t -> type_to_json cx (depth - 1) t
              );
              ( "super_t",
                match opaquetype.upper_t with
                | None -> JSON_Null
                | Some t -> type_to_json cx (depth - 1) t
              );
              ( "sub_t",
                match opaquetype.lower_t with
                | None -> JSON_Null
                | Some t -> type_to_json cx (depth - 1) t
              );
              ( "opaque_type_args",
                JSON_Array
                  (List.map
                     (fun (name, _, t, polarity) ->
                       JSON_Object
                         [
                           ("name", JSON_String (Subst_name.string_of_subst_name name));
                           ("type", type_to_json cx (depth - 1) t);
                           ( "polarity",
                             JSON_String
                               (match polarity with
                               | Polarity.Positive -> "positive"
                               | Polarity.Negative -> "negative"
                               | Polarity.Neutral -> "neutral")
                           );
                         ])
                     opaquetype.opaque_type_args
                  )
              );
              ("opaque_name", JSON_String opaquetype.opaque_name);
            ]
        );
      ]
  | NamespaceT namespace_t ->
    json_with_type
      "Namespace"
      [
        ( "namespace_symbol",
          JSON_Object
            [("symbol", JSON_String (FlowSymbol.name_of_symbol namespace_t.namespace_symbol))]
        );
        ("values_type", type_to_json cx (depth - 1) namespace_t.values_type);
        ("types_tmap", json_of_property_map cx depth namespace_t.types_tmap);
      ]
  | AnyT _ -> json_with_type "Any" []
  | StrUtilT { reason = _; op; remainder } ->
    let fields =
      match op with
      | StrPrefix s -> [("op", JSON_String "StrPrefix"); ("prefix", JSON_String s)]
      | StrSuffix s -> [("op", JSON_String "StrSuffix"); ("suffix", JSON_String s)]
    in
    let fields =
      match remainder with
      | None -> fields
      | Some t -> fields @ [("remainder", type_to_json cx (depth - 1) t)]
    in
    json_with_type "StrUtil" fields

(* Convert typeparams to JSON *)
and json_of_typeparams cx depth tparams =
  JSON_Array
    (Nel.to_list tparams
    |> List.map (fun typeparam ->
           JSON_Object
             [
               ("name", JSON_String (Subst_name.string_of_subst_name typeparam.name));
               ("bound", type_to_json cx (depth - 1) typeparam.bound);
               ( "polarity",
                 JSON_String
                   (match typeparam.polarity with
                   | Polarity.Positive -> "positive"
                   | Polarity.Negative -> "negative"
                   | Polarity.Neutral -> "neutral")
               );
               ( "default",
                 match typeparam.default with
                 | None -> JSON_Null
                 | Some t -> type_to_json cx (depth - 1) t
               );
             ]
       )
    )

(* Convert enum_info to JSON *)
and json_of_enum_info cx depth = function
  | ConcreteEnum enum_concrete_info ->
    JSON_Object
      [
        ("kind", JSON_String "ConcreteEnum");
        ("enum_name", JSON_String enum_concrete_info.enum_name);
        ("enum_id", JSON_String (ALoc.debug_to_string (enum_concrete_info.enum_id :> ALoc.t)));
        ( "members",
          JSON_Array (SMap.fold (fun k _ acc -> JSON_String k :: acc) enum_concrete_info.members [])
        );
        ("representation_t", type_to_json cx (depth - 1) enum_concrete_info.representation_t);
        ("has_unknown_members", JSON_Bool enum_concrete_info.has_unknown_members);
      ]
  | AbstractEnum { representation_t } ->
    JSON_Object
      [
        ("kind", JSON_String "AbstractEnum");
        ("representation_t", type_to_json cx (depth - 1) representation_t);
      ]

(* Convert canonical_renders_form to JSON *)
and json_of_canonical_renders_form cx depth (renders : canonical_renders_form) =
  match renders with
  | IntrinsicRenders name ->
    JSON_Object [("kind", JSON_String "IntrinsicRenders"); ("name", JSON_String name)]
  | NominalRenders { renders_id; renders_name; renders_super } ->
    JSON_Object
      [
        ("kind", JSON_String "NominalRenders");
        ("renders_id", JSON_String (ALoc.debug_to_string (renders_id :> ALoc.t)));
        ("renders_name", JSON_String renders_name);
        ("renders_super", type_to_json cx (depth - 1) renders_super);
      ]
  | StructuralRenders { renders_variant; renders_structural_type } ->
    JSON_Object
      [
        ("kind", JSON_String "StructuralRenders");
        ( "renders_variant",
          match renders_variant with
          | RendersNormal -> JSON_String "RendersNormal"
          | RendersMaybe -> JSON_String "RendersMaybe"
          | RendersStar -> JSON_String "RendersStar"
        );
        ("renders_structural_type", type_to_json cx (depth - 1) renders_structural_type);
      ]
  | DefaultRenders -> JSON_Object [("kind", JSON_String "DefaultRenders")]

(* Convert instance_t to JSON *)
and json_of_instance_t cx depth (instance_t : instance_t) =
  JSON_Object
    [
      ("inst", json_of_insttype cx depth instance_t.inst);
      ("static", type_to_json cx (depth - 1) instance_t.static);
      ("super", type_to_json cx (depth - 1) instance_t.super);
      ("implements", JSON_Array (List.map (type_to_json cx (depth - 1)) instance_t.implements));
    ]

and json_of_insttype cx depth (inst : insttype) =
  JSON_Object
    [
      ( "class_name",
        match inst.class_name with
        | None -> JSON_Null
        | Some name -> JSON_String name
      );
      ("class_id", JSON_String (ALoc.debug_to_string (inst.class_id :> ALoc.t)));
      ( "type_args",
        JSON_Array
          (List.map
             (fun (name, _, t, polarity) ->
               JSON_Object
                 [
                   ("name", JSON_String (Subst_name.string_of_subst_name name));
                   ("type", type_to_json cx (depth - 1) t);
                   ( "polarity",
                     JSON_String
                       (match polarity with
                       | Polarity.Positive -> "positive"
                       | Polarity.Negative -> "negative"
                       | Polarity.Neutral -> "neutral")
                   );
                 ])
             inst.type_args
          )
      );
      ("own_props", json_of_property_map cx depth inst.own_props);
      ("proto_props", json_of_property_map cx depth inst.proto_props);
      ( "call_t",
        match inst.inst_call_t with
        | None -> JSON_Null
        | Some id ->
          let call = Context.find_call cx id in
          JSON_Object
            [("id", JSON_Number (string_of_int id)); ("call", type_to_json cx (depth - 1) call)]
      );
    ]

(* Convert def_t to JSON *)
and def_t_to_json cx depth (def_t : def_t) : json =
  match def_t with
  | NumGeneralT _ -> json_with_type "NumGeneral" []
  | StrGeneralT _ -> json_with_type "StrGeneral" []
  | BoolGeneralT -> json_with_type "BoolGeneral" []
  | BigIntGeneralT _ -> json_with_type "BigIntGeneral" []
  | EmptyT -> json_with_type "Empty" []
  | MixedT _ -> json_with_type "Mixed" []
  | NullT -> json_with_type "Null" []
  | VoidT -> json_with_type "Void" []
  | SymbolT -> json_with_type "Symbol" []
  | FunT (static, funtype) ->
    json_with_type
      "Fun"
      [
        ("static", type_to_json cx (depth - 1) static); ("funtype", funtype_to_json cx depth funtype);
      ]
  | ObjT objtype -> json_with_type "Obj" [("objtype", objtype_to_json cx depth objtype)]
  | ArrT arrtype -> json_with_type "Arr" [("arrtype", arrtype_to_json cx depth arrtype)]
  | ClassT t -> json_with_type "Class" [("type", type_to_json cx (depth - 1) t)]
  | InstanceT instance_t ->
    json_with_type "Instance" [("instance", json_of_instance_t cx depth instance_t)]
  | SingletonStrT { from_annot; value } ->
    json_with_type
      "SingletonStr"
      [
        ("from_annot", JSON_Bool from_annot);
        ("value", JSON_String (Reason.display_string_of_name value));
      ]
  | NumericStrKeyT (_, s) ->
    json_with_type "NumericStrKey" [("number", JSON_Number s); ("string", JSON_String s)]
  | SingletonNumT { from_annot; value = (_, s) } ->
    json_with_type
      "SingletonNum"
      [("from_annot", JSON_Bool from_annot); ("number", JSON_Number s); ("string", JSON_String s)]
  | SingletonBoolT { from_annot; value } ->
    json_with_type
      "SingletonBool"
      [("from_annot", JSON_Bool from_annot); ("value", JSON_Bool value)]
  | SingletonBigIntT { from_annot; value = (_, s) } ->
    json_with_type
      "SingletonBigInt"
      [("from_annot", JSON_Bool from_annot); ("value", JSON_String s)]
  | TypeT (kind, t) ->
    json_with_type
      "Type"
      [("type_kind", json_of_type_t_kind kind); ("type", type_to_json cx (depth - 1) t)]
  | PolyT { tparams_loc = _; tparams; t_out; id } ->
    json_with_type
      "Poly"
      [
        ("tparams", json_of_typeparams cx depth tparams);
        ("t_out", type_to_json cx (depth - 1) t_out);
        ("id", JSON_String (Poly.stable_string_of_id id));
      ]
  | ReactAbstractComponentT { config; instance; renders; component_kind } ->
    json_with_type
      "ReactAbstractComponent"
      [
        ("config", type_to_json cx (depth - 1) config);
        ("renders", type_to_json cx (depth - 1) renders);
        ( "instance",
          match instance with
          | ComponentInstanceAvailableAsRefSetterProp t ->
            JSON_Object
              [("kind", JSON_String "RefSetterProp"); ("type", type_to_json cx (depth - 1) t)]
          | ComponentInstanceOmitted _ -> JSON_Object [("kind", JSON_String "Omitted")]
        );
        ( "component_kind",
          match component_kind with
          | Structural -> JSON_Object [("kind", JSON_String "Structural")]
          | Nominal (id, name, types_opt) ->
            JSON_Object
              [
                ("kind", JSON_String "Nominal");
                ("id", JSON_String (ALoc.debug_to_string (id :> ALoc.t)));
                ("name", JSON_String name);
                ( "types",
                  match types_opt with
                  | None -> JSON_Null
                  | Some types -> JSON_Array (List.map (type_to_json cx (depth - 1)) types)
                );
              ]
        );
      ]
  | RendersT form ->
    json_with_type "Renders" [("form", json_of_canonical_renders_form cx depth form)]
  | EnumValueT enum_info ->
    json_with_type "EnumValue" [("enum_info", json_of_enum_info cx depth enum_info)]
  | EnumObjectT { enum_value_t; enum_info } ->
    json_with_type
      "EnumObject"
      [
        ("enum_value_t", type_to_json cx (depth - 1) enum_value_t);
        ("enum_info", json_of_enum_info cx depth enum_info);
      ]

(* Convert obj_kind to JSON *)
and json_of_obj_kind cx depth = function
  | Exact -> JSON_Object [("kind", JSON_String "Exact")]
  | Inexact -> JSON_Object [("kind", JSON_String "Inexact")]
  | Indexed dicttype ->
    JSON_Object [("kind", JSON_String "Indexed"); ("dicttype", json_of_dicttype cx depth dicttype)]

(* Convert dicttype to JSON *)
and json_of_dicttype cx depth dicttype =
  JSON_Object
    [
      ( "dict_name",
        match dicttype.dict_name with
        | None -> JSON_Null
        | Some name -> JSON_String name
      );
      ("key", type_to_json cx (depth - 1) dicttype.key);
      ("value", type_to_json cx (depth - 1) dicttype.value);
      ( "dict_polarity",
        JSON_String
          (match dicttype.dict_polarity with
          | Polarity.Positive -> "positive"
          | Polarity.Negative -> "negative"
          | Polarity.Neutral -> "neutral")
      );
    ]

(* Convert arrtype to JSON *)
and arrtype_to_json cx depth = function
  | ArrayAT { react_dro = _; elem_t; tuple_view = _ } ->
    JSON_Object [("kind", JSON_String "ArrayAT"); ("elem_t", type_to_json cx (depth - 1) elem_t)]
  | TupleAT { react_dro = _; elem_t; elements; arity = (min_arity, max_arity); inexact } ->
    JSON_Object
      [
        ("kind", JSON_String "TupleAT");
        ("elem_t", type_to_json cx (depth - 1) elem_t);
        ("elements", JSON_Array (List.map (json_of_tuple_element cx depth) elements));
        ("min_arity", JSON_Number (string_of_int min_arity));
        ("max_arity", JSON_Number (string_of_int max_arity));
        ("inexact", JSON_Bool inexact);
      ]
  | ROArrayAT (elem_t, _) ->
    JSON_Object [("kind", JSON_String "ROArrayAT"); ("elem_t", type_to_json cx (depth - 1) elem_t)]

(* Convert tuple_element to JSON *)
and json_of_tuple_element cx depth element =
  match element with
  | TupleElement { reason = _; name; t; polarity; optional } ->
    JSON_Object
      [
        ( "name",
          match name with
          | None -> JSON_Null
          | Some name -> JSON_String name
        );
        ("t", type_to_json cx (depth - 1) t);
        ( "polarity",
          JSON_String
            (match polarity with
            | Polarity.Positive -> "positive"
            | Polarity.Negative -> "negative"
            | Polarity.Neutral -> "neutral")
        );
        ("optional", JSON_Bool optional);
      ]

(* Convert flags to JSON *)
and json_of_flags cx depth flags =
  JSON_Object [("obj_kind", json_of_obj_kind cx depth flags.obj_kind)]

(* Convert property to JSON *)
and json_of_property cx depth property =
  match property with
  | Field { preferred_def_locs = _; key_loc = _; type_; polarity } ->
    JSON_Object
      [
        ("kind", JSON_String "Field");
        ("type", type_to_json cx (depth - 1) type_);
        ( "polarity",
          JSON_String
            (match polarity with
            | Polarity.Positive -> "positive"
            | Polarity.Negative -> "negative"
            | Polarity.Neutral -> "neutral")
        );
      ]
  | Get { type_; _ } ->
    JSON_Object [("kind", JSON_String "Get"); ("type", type_to_json cx (depth - 1) type_)]
  | Set { key_loc = _; type_ } ->
    JSON_Object [("kind", JSON_String "Set"); ("type", type_to_json cx (depth - 1) type_)]
  | GetSet { get_type; set_type; _ } ->
    JSON_Object
      [
        ("kind", JSON_String "GetSet");
        ("get_type", type_to_json cx (depth - 1) get_type);
        ("set_type", type_to_json cx (depth - 1) set_type);
      ]
  | Method { type_; _ } ->
    JSON_Object [("kind", JSON_String "Method"); ("type", type_to_json cx (depth - 1) type_)]

(* Convert property map to JSON *)
and json_of_property_map cx depth props_id =
  try
    let props = Context.find_props cx props_id in
    json_of_property_map_value cx depth props
  with
  | Context.Props_not_found _ -> JSON_Object [("error", JSON_String "Property map not found")]

and json_of_property_map_value cx depth props =
  let props_json =
    NameUtils.Map.fold
      (fun name prop acc ->
        (Reason.display_string_of_name name, json_of_property cx depth prop) :: acc)
      props
      []
  in
  JSON_Object props_json

(* Convert objtype to JSON *)
and objtype_to_json cx depth objtype =
  JSON_Object
    [
      ("flags", json_of_flags cx depth objtype.flags);
      ("props", json_of_property_map cx depth objtype.props_tmap);
      ("proto_t", type_to_json cx (depth - 1) objtype.proto_t);
      ( "call_t",
        match objtype.call_t with
        | None -> JSON_Null
        | Some id -> JSON_Number (string_of_int id)
      );
    ]

and json_of_destructor cx depth destructor =
  match destructor with
  | NonMaybeType -> JSON_Object [("kind", JSON_String "NonMaybeType")]
  | PropertyType { name } ->
    JSON_Object
      [
        ("kind", JSON_String "PropertyType");
        ("name", JSON_String (Reason.display_string_of_name name));
      ]
  | ElementType { index_type } ->
    JSON_Object
      [("kind", JSON_String "ElementType"); ("index_type", type_to_json cx (depth - 1) index_type)]
  | OptionalIndexedAccessNonMaybeType { index } ->
    JSON_Object
      [
        ("kind", JSON_String "OptionalIndexedAccessNonMaybeType");
        ( "index",
          match index with
          | OptionalIndexedAccessStrLitIndex name ->
            JSON_Object
              [
                ("kind", JSON_String "StrLitIndex");
                ("name", JSON_String (Reason.display_string_of_name name));
              ]
          | OptionalIndexedAccessTypeIndex t ->
            JSON_Object [("kind", JSON_String "TypeIndex"); ("type", type_to_json cx (depth - 1) t)]
        );
      ]
  | OptionalIndexedAccessResultType { void_reason = _ } ->
    JSON_Object [("kind", JSON_String "OptionalIndexedAccessResultType")]
  | ExactType -> JSON_Object [("kind", JSON_String "ExactType")]
  | ReadOnlyType -> JSON_Object [("kind", JSON_String "ReadOnlyType")]
  | PartialType -> JSON_Object [("kind", JSON_String "PartialType")]
  | RequiredType -> JSON_Object [("kind", JSON_String "RequiredType")]
  | SpreadType (target, operands, operand_slice_opt) ->
    JSON_Object
      [
        ("kind", JSON_String "SpreadType");
        ( "target",
          match target with
          | Object.Spread.Value { make_seal } ->
            JSON_Object
              [
                ("kind", JSON_String "Value");
                ( "make_seal",
                  match make_seal with
                  | Object.Spread.Sealed -> JSON_String "Sealed"
                  | Object.Spread.Frozen -> JSON_String "Frozen"
                  | Object.Spread.As_Const -> JSON_String "As_Const"
                );
              ]
          | Object.Spread.Annot { make_exact } ->
            JSON_Object [("kind", JSON_String "Annot"); ("make_exact", JSON_Bool make_exact)]
        );
        ( "operands",
          JSON_Array
            (List.map
               (fun operand ->
                 match operand with
                 | Object.Spread.Slice slice -> json_of_slice cx depth slice
                 | Object.Spread.Type t ->
                   JSON_Object
                     [("kind", JSON_String "Type"); ("type", type_to_json cx (depth - 1) t)])
               operands
            )
        );
        ( "operand_slice",
          match operand_slice_opt with
          | None -> JSON_Null
          | Some slice -> json_of_slice cx depth slice
        );
      ]
  | SpreadTupleType { reason_tuple = _; reason_spread = _; inexact; resolved_rev; unresolved } ->
    JSON_Object
      [
        ("kind", JSON_String "SpreadTupleType");
        ("inexact", JSON_Bool inexact);
        ("resolved_rev", JSON_String (string_of_int (List.length resolved_rev)));
        ("unresolved", JSON_String (string_of_int (List.length unresolved)));
      ]
  | RestType (merge_mode, t) ->
    JSON_Object
      [
        ("kind", JSON_String "RestType");
        ( "merge_mode",
          match merge_mode with
          | Object.Rest.SpreadReversal -> json_with_type "SpreadReversal" []
          | Object.Rest.ReactConfigMerge polarity ->
            JSON_Object
              [
                ("kind", JSON_String "ReactConfigMerge");
                ( "polarity",
                  match polarity with
                  | Polarity.Positive -> JSON_String "positive"
                  | Polarity.Negative -> JSON_String "negative"
                  | Polarity.Neutral -> JSON_String "neutral"
                );
              ]
          | Object.Rest.Omit -> json_with_type "Omit" []
        );
        ("type", type_to_json cx (depth - 1) t);
      ]
  | ValuesType -> JSON_Object [("kind", JSON_String "ValuesType")]
  | ConditionalType { distributive_tparam_name; infer_tparams; extends_t; true_t; false_t } ->
    JSON_Object
      [
        ("kind", JSON_String "ConditionalType");
        ( "distributive_tparam_name",
          match distributive_tparam_name with
          | None -> JSON_Null
          | Some name -> JSON_String (Subst_name.string_of_subst_name name)
        );
        ("infer_tparams", JSON_String (string_of_int (List.length infer_tparams)));
        ("extends_t", type_to_json cx (depth - 1) extends_t);
        ("true_t", type_to_json cx (depth - 1) true_t);
        ("false_t", type_to_json cx (depth - 1) false_t);
      ]
  | TypeMap ObjectKeyMirror -> JSON_Object [("kind", JSON_String "ObjectKeyMirror")]
  | ReactElementPropsType -> JSON_Object [("kind", JSON_String "ReactElementPropsType")]
  | ReactElementConfigType -> JSON_Object [("kind", JSON_String "ReactElementConfigType")]
  | ReactCheckComponentConfig props ->
    JSON_Object
      [
        ("kind", JSON_String "ReactCheckComponentConfig");
        ( "props",
          JSON_Object
            (NameUtils.Map.fold
               (fun name prop acc ->
                 (Reason.display_string_of_name name, json_of_property cx depth prop) :: acc)
               props
               []
            )
        );
      ]
  | ReactDRO (_, dro_type) ->
    JSON_Object
      [
        ("kind", JSON_String "ReactDRO");
        ( "dro_type",
          match dro_type with
          | HookReturn -> JSON_String "HookReturn"
          | HookArg -> JSON_String "HookArg"
          | Props -> JSON_String "Props"
          | ImmutableAnnot -> JSON_String "ImmutableAnnot"
          | DebugAnnot -> JSON_String "DebugAnnot"
        );
      ]
  | MakeHooklike -> JSON_Object [("kind", JSON_String "MakeHooklike")]
  | MappedType { homomorphic; distributive_tparam_name; property_type; mapped_type_flags } ->
    JSON_Object
      [
        ("kind", JSON_String "MappedType");
        ( "homomorphic",
          match homomorphic with
          | Homomorphic -> json_with_type "Homomorphic" []
          | Unspecialized -> json_with_type "Unspecialized" []
          | SemiHomomorphic t ->
            JSON_Object
              [("kind", JSON_String "SemiHomomorphic"); ("type", type_to_json cx (depth - 1) t)]
        );
        ( "distributive_tparam_name",
          match distributive_tparam_name with
          | None -> JSON_Null
          | Some name -> JSON_String (Subst_name.string_of_subst_name name)
        );
        ("property_type", type_to_json cx (depth - 1) property_type);
        ( "mapped_type_flags",
          JSON_Object
            [
              ( "variance",
                match mapped_type_flags.variance with
                | Polarity.Positive -> JSON_String "positive"
                | Polarity.Negative -> JSON_String "negative"
                | Polarity.Neutral -> JSON_String "neutral"
              );
              ( "optional",
                match mapped_type_flags.optional with
                | MakeOptional -> JSON_String "MakeOptional"
                | RemoveOptional -> JSON_String "RemoveOptional"
                | KeepOptionality -> JSON_String "KeepOptionality"
              );
            ]
        );
      ]
  | EnumType -> JSON_Object [("kind", JSON_String "EnumType")]

and json_of_slice cx depth slice =
  json_with_type
    "Slice"
    [
      ("prop_map", json_of_property_map_value cx depth slice.Type.Object.Spread.prop_map);
      ( "generics",
        JSON_Array
          (Base.List.map
             ~f:(fun gen ->
               JSON_String (Subst_name.string_of_subst_name gen.Generic.generic.Generic.name))
             slice.Type.Object.Spread.generics
          )
      );
      ( "dict",
        match slice.Type.Object.Spread.dict with
        | None -> JSON_Null
        | Some dict -> json_of_dicttype cx depth dict
      );
      ( "reachable_targs",
        JSON_Array
          (List.map
             (fun (t, polarity) ->
               JSON_Object
                 [
                   ("type", type_to_json cx (depth - 1) t);
                   ( "polarity",
                     JSON_String
                       (match polarity with
                       | Polarity.Positive -> "positive"
                       | Polarity.Negative -> "negative"
                       | Polarity.Neutral -> "neutral")
                   );
                 ])
             slice.Type.Object.Spread.reachable_targs
          )
      );
    ]
