(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module FlowError = Flow_error

open Utils_js
open Reason
open Type
open Env.LookupMode

(* AST helpers *)

let ident_name (_, name) = name

let optional_ident_name = function
| None -> "_"
| Some ident -> ident_name ident

let error_type cx loc msg =
  Flow_js.add_output cx msg;
  AnyT.at loc

let is_suppress_type cx type_name =
  SSet.mem type_name (Context.suppress_types cx)

let check_type_param_arity cx loc params n f =
  let num_params = match params with
  | None -> 0
  | Some l -> List.length l in
  if num_params = n
  then f ()
  else
    error_type cx loc (FlowError.ETypeParamArity (loc, n))

let mk_custom_fun cx loc typeParameters kind =
  check_type_param_arity cx loc typeParameters 0 (fun () ->
    let reason = mk_reason RFunctionType loc in
    CustomFunT (reason, kind)
  )

let mk_react_prop_type cx loc typeParameters kind =
  mk_custom_fun cx loc typeParameters
    (ReactPropType (React.PropType.Complex kind))

(**********************************)
(* Transform annotations to types *)
(**********************************)

(* converter *)
let rec convert cx tparams_map = Ast.Type.(function

| loc, Any -> AnyT.at loc

| loc, Mixed -> MixedT.at loc

| loc, Empty -> EmptyT.at loc

| loc, Void -> VoidT.at loc

| loc, Null -> NullT.at loc

| loc, Number -> NumT.at loc

| loc, String -> StrT.at loc

| loc, Boolean -> BoolT.at loc

| loc, Nullable t ->
    let t = convert cx tparams_map t in
    let reason = mk_reason (RMaybe (desc_of_t t)) loc in
    DefT (reason, MaybeT t)

| loc, Union (t0, t1, ts) ->
  let t0 = convert cx tparams_map t0 in
  let t1 = convert cx tparams_map t1 in
  let ts = List.map (convert cx tparams_map) ts in
  let rep = UnionRep.make t0 t1 ts in
  DefT (mk_reason RUnionType loc, UnionT rep)

| loc, Intersection (t0, t1, ts) ->
  let t0 = convert cx tparams_map t0 in
  let t1 = convert cx tparams_map t1 in
  let ts = List.map (convert cx tparams_map) ts in
  let rep = InterRep.make t0 t1 ts in
  DefT (mk_reason RIntersectionType loc, IntersectionT rep)

| loc, Typeof x ->
  begin match x with
  | (_, Generic {
      Generic.id = qualification;
      typeParameters = None
    }) ->
      let valtype = convert_qualification ~lookup_mode:ForTypeof cx
        "typeof-annotation" qualification in
      let reason = repos_reason loc (reason_of_t valtype) in
      Flow_js.mk_typeof_annotation cx reason valtype
  | _ ->
    error_type cx loc (FlowError.EUnexpectedTypeof loc)
  end

| loc, Tuple ts ->
  let tuple_types = List.map (convert cx tparams_map) ts in
  let reason = mk_reason RTupleType loc in
  let element_reason = mk_reason RTupleElement loc in
  let elemt = match tuple_types with
  | [] -> Flow_js.mk_tvar cx element_reason
  | [t] -> t
  | t0::t1::ts ->
    (* If a tuple should be viewed as an array, what would the element type of
       the array be?

       Using a union here seems appealing but is wrong: setting elements
       through arbitrary indices at the union type would be unsound, since it
       might violate the projected types of the tuple at their corresponding
       positions. This also shows why `mixed` doesn't work, either.

       On the other hand, using the empty type would prevent writes, but admit
       unsound reads.

       The correct solution is to safely case a tuple type to a covariant
       array interface whose element type would be a union. Until we have
       that, we use the following closest approximation, that behaves like a
       union as a lower bound but `any` as an upper bound.
    *)
    AnyWithLowerBoundT (DefT (element_reason, UnionT (UnionRep.make t0 t1 ts)))
  in
  DefT (reason, ArrT (TupleAT (elemt, tuple_types)))

| loc, Array t ->
  let r = mk_reason RArrayType loc in
  let elemt = convert cx tparams_map t in
  DefT (r, ArrT (ArrayAT (elemt, None)))

| loc, StringLiteral { StringLiteral.value; _ }  ->
  mk_singleton_string loc value

| loc, NumberLiteral { NumberLiteral.value; raw; _ }  ->
  mk_singleton_number loc value raw

| loc, BooleanLiteral { BooleanLiteral.value; _ }  ->
  mk_singleton_boolean loc value

(* TODO *)
| loc, Generic { Generic.id = Generic.Identifier.Qualified (_,
       { Generic.Identifier.qualification; id; }); typeParameters } ->

  let m = convert_qualification cx "type-annotation" qualification in
  let _, name = id in
  let reason = mk_reason (RCustom name) loc in
  let t = Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (m, GetPropT (reason, Named (reason, name), t));
  ) in
  let typeParameters = extract_type_param_instantiations typeParameters in
  mk_nominal_type cx reason tparams_map (t, typeParameters)

(* type applications: name < params > *)
| loc, Generic {
    Generic.id = Generic.Identifier.Unqualified (id);
    typeParameters
  } ->
  let _, name = id in
  let typeParameters = extract_type_param_instantiations typeParameters in

  let convert_type_params () = Option.value_map
    typeParameters
    ~default: []
    ~f:(List.map (convert cx tparams_map)) in

  begin match name with

  (* Array<T> *)
  | "Array" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let elemt = convert_type_params () |> List.hd in
      DefT (mk_reason RArrayType loc, ArrT (ArrayAT (elemt, None)))
    )

  (* $Either<...T> is the union of types ...T *)
  | "$Either" ->
    (match convert_type_params () with
    | t0::t1::ts ->
      let rep = UnionRep.make t0 t1 ts in
      DefT (mk_reason RUnionType loc, UnionT rep)
    | _ ->
      error_type cx loc (FlowError.ETypeParamMinArity (loc, 2)))

  (* $All<...T> is the intersection of types ...T *)
  | "$All" ->
    (match convert_type_params () with
    | t0::t1::ts ->
      let rep = InterRep.make t0 t1 ts in
      DefT (mk_reason RIntersectionType loc, IntersectionT rep)
    | _ ->
      error_type cx loc (FlowError.ETypeParamMinArity (loc, 2)))

  (* $ReadOnlyArray<T> is the supertype of all tuples and all arrays *)
  | "$ReadOnlyArray" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let elemt = convert_type_params () |> List.hd in
      DefT (mk_reason RROArrayType loc, ArrT (ROArrayAT (elemt)))
    )

  (* $Supertype<T> acts as any over supertypes of T *)
  | "$Supertype" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      AnyWithLowerBoundT t
    )

  (* $Subtype<T> acts as any over subtypes of T *)
  | "$Subtype" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      AnyWithUpperBoundT t
    )

  (* $Type<T> acts as the type of T *)
  | "$Type" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      DefT (mk_reason (RCustom "type") loc, TypeT t)
    )

  (* $PropertyType<T, 'x'> acts as the type of 'x' in object type T *)
  | "$PropertyType" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      match convert_type_params () with
      | [t; DefT (_, SingletonStrT key)] ->
        EvalT (t, TypeDestructorT
          (mk_reason (RCustom "property type") loc, PropertyType key), mk_id())
      | _ ->
        error_type cx loc (FlowError.EPropertyTypeAnnot loc)
    )

  (* $NonMaybeType<T> acts as the type T without null and void *)
  | "$NonMaybeType" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      EvalT (t, TypeDestructorT
        (mk_reason (RCustom "non-maybe type") loc, NonMaybeType), mk_id())
    )

  (* $Shape<T> matches the shape of T *)
  | "$Shape" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      ShapeT t
    )

  (* $Diff<T,S> *)
  | "$Diff" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      DiffT (t1, t2)
    )

  (* $Keys<T> is the set of keys of T *)
  (** TODO: remove $Enum **)
  | "$Keys" | "$Enum"->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      KeysT (mk_reason RKeySet loc, t)
    )

  | "$Exact" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = List.hd (convert_type_params ()) in
      let desc = RExactType (desc_of_t t) in
      ExactT (mk_reason desc loc, t)
    )

  (* $Exports<'M'> is the type of the exports of module 'M' *)
  (** TODO: use `import typeof` instead when that lands **)
  | "$Exports" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      match typeParameters with
      | Some ((_, StringLiteral { StringLiteral.value; _ })::_) ->
          let desc = RCustom (spf "exports of module `%s`" value) in
          let reason = mk_reason desc loc in
          let remote_module_t =
            Env.get_var_declared_type cx (internal_module_name value) loc
          in
          Flow_js.mk_tvar_where cx reason (fun t ->
            Flow_js.flow cx (remote_module_t, CJSRequireT(reason, t))
          )
      | _ ->
          error_type cx loc (FlowError.EExportsAnnot loc)
    )

  | "$Abstract" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RAbstract (desc_of_t t)) loc in
      AbstractT (reason, t)
    )

  | "$TupleMap" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RTupleMap loc in
      TypeMapT (reason, TupleMap, t1, t2)
    )

  | "$ObjMap" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RObjectMap loc in
      TypeMapT (reason, ObjectMap, t1, t2)
    )

  | "$ObjMapi" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RObjectMapi loc in
      TypeMapT (reason, ObjectMapi, t1, t2)
    )

  | "this" ->
    if SMap.mem "this" tparams_map then
      (* We model a this type like a type parameter. The bound on a this
         type reflects the interface of `this` exposed in the current
         environment. Currently, we only support this types in a class
         environment: a this type in class C is bounded by C. *)
      check_type_param_arity cx loc typeParameters 0 (fun () ->
        Flow_js.reposition cx loc (SMap.find_unsafe "this" tparams_map)
      )
    else (
      Flow_js.add_output cx (FlowError.EUnexpectedThisType loc);
      Locationless.AnyT.t
    )

  (* Class<T> is the type of the class whose instances are of type T *)
  | "Class" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RClassType (desc_of_t t)) loc in
      DefT (reason, ClassT t)
    )

  | "Function" | "function" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      DefT (reason, AnyFunT)
    )

  | "Object" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason RObjectType loc in
      DefT (reason, AnyObjT)
    )

  | "Function$Prototype$Apply" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoApplyT reason
    )

  | "Function$Prototype$Bind" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoBindT reason
    )

  | "Function$Prototype$Call" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoCallT reason
    )

  | "$Tainted" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let taint = TaintT (mk_reason (RCustom "taint") loc) in
      let reason = Reason.repos_reason loc (reason_of_t t) in
      DefT (reason, UnionT (UnionRep.make t taint []))
    )

  | "Object$Assign" ->
      mk_custom_fun cx loc typeParameters ObjectAssign
  | "Object$GetPrototypeOf" ->
      mk_custom_fun cx loc typeParameters ObjectGetPrototypeOf

  | "React$PropType$Primitive" ->
      check_type_param_arity cx loc typeParameters 1 (fun () ->
        let t = convert_type_params () |> List.hd in
        let prop_type = (ReactPropType (React.PropType.Primitive (false, t))) in
        mk_custom_fun cx loc None prop_type
      )
  | "React$PropType$ArrayOf" ->
      mk_react_prop_type cx loc typeParameters React.PropType.ArrayOf
  | "React$PropType$InstanceOf" ->
      mk_react_prop_type cx loc typeParameters React.PropType.InstanceOf
  | "React$PropType$ObjectOf" ->
      mk_react_prop_type cx loc typeParameters React.PropType.ObjectOf
  | "React$PropType$OneOf" ->
      mk_react_prop_type cx loc typeParameters React.PropType.OneOf
  | "React$PropType$OneOfType" ->
      mk_react_prop_type cx loc typeParameters React.PropType.OneOfType
  | "React$PropType$Shape" ->
      mk_react_prop_type cx loc typeParameters React.PropType.Shape
  | "React$CreateClass" ->
      mk_custom_fun cx loc typeParameters ReactCreateClass
  | "React$CreateElement" ->
      mk_custom_fun cx loc typeParameters ReactCreateElement
  | "$Facebookism$Merge" ->
      mk_custom_fun cx loc typeParameters Merge
  | "$Facebookism$MergeDeepInto" ->
      mk_custom_fun cx loc typeParameters MergeDeepInto
  | "$Facebookism$MergeInto" ->
      mk_custom_fun cx loc typeParameters MergeInto
  | "$Facebookism$Mixin" ->
      mk_custom_fun cx loc typeParameters Mixin
  | "$Facebookism$Idx" ->
      mk_custom_fun cx loc typeParameters Idx

  | "$Flow$DebugPrint" ->
      mk_custom_fun cx loc typeParameters DebugPrint


  (* You can specify in the .flowconfig the names of types that should be
   * treated like any<actualType>. So if you have
   * suppress_type=$FlowFixMe
   *
   * Then you can do
   *
   * var x: $FlowFixMe<number> = 123;
   *)
  (* TODO move these to type aliases once optional type args
     work properly in type aliases: #7007731 *)
  | type_name when is_suppress_type cx type_name ->
    (* Optional type params are info-only, validated then forgotten. *)
    ignore (convert_type_params ());
    AnyT.at loc

  (* TODO: presumably some existing uses of AnyT can benefit from AnyObjT
     as well: e.g., where AnyT is used to model prototypes and statics we
     don't care about; but then again, some of these uses may be internal,
     so while using AnyObjT may offer some sanity checking it might not
     reveal user-facing errors. *)

  (* in-scope type vars *)
  | _ when SMap.mem name tparams_map ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      Flow_js.reposition cx loc (SMap.find_unsafe name tparams_map)
    )

  | "$Pred" ->
    let fun_reason = mk_reason (RCustom "abstract predicate function") loc in
    let static_reason = mk_reason (RCustom "abstract predicate static") loc in
    let out_reason = mk_reason (RCustom "open predicate") loc in

    check_type_param_arity cx loc typeParameters 1 (fun () ->
      match convert_type_params () with
      | [DefT (_, SingletonNumT (f, _))] ->
        let n = Pervasives.int_of_float f in
        let key_strs =
          Utils_js.range 0 n |>
          List.map (fun i -> "x_" ^ Pervasives.string_of_int i) in
        let emp = Key_map.empty in
        let tins = Utils_js.repeat n (AnyT.at loc) in
        let tout = OpenPredT (out_reason, MixedT.at loc, emp, emp) in
        DefT (fun_reason, FunT (
          Flow_js.dummy_static static_reason,
          DefT (mk_reason RPrototype loc, AnyT),
          Flow_js.mk_functiontype tins ~rest_param:None ~def_reason:fun_reason
            ~params_names:key_strs ~is_predicate:true tout
        ))

      | _ ->
        error_type cx loc (FlowError.EPredAnnot loc)
    )

  | "$Refine" ->
    check_type_param_arity cx loc typeParameters 3 (fun () ->
      match convert_type_params () with
      | [base_t; fun_pred_t; DefT (_, SingletonNumT (f, _))] ->
          let idx = Pervasives.int_of_float f in
          let reason = mk_reason (RCustom "refined type") loc in
          let pred = LatentP (fun_pred_t, idx) in
          EvalT (base_t, DestructuringT (reason, Refine pred), mk_id())
      | _ ->
        error_type cx loc (FlowError.ERefineAnnot loc)
    )

  (* other applications with id as head expr *)
  | _ ->
    let reason = mk_reason (RCustom name) loc in
    let c = type_identifier cx name loc in
    mk_nominal_type cx reason tparams_map (c, typeParameters)

  end

| loc, Function { Function.params = (params, rest); returnType; typeParameters } ->
  let tparams, tparams_map =
    mk_type_param_declarations cx ~tparams_map typeParameters in

  let rev_params_tlist, rev_params_names =
    List.fold_left (fun (tlist, pnames) (_, param) ->
      let { Function.Param.name; typeAnnotation; optional } = param in
      let t = convert cx tparams_map typeAnnotation in
      let t = if optional then Type.optional t else t in
      (t :: tlist, optional_ident_name name :: pnames)
    ) ([], []) params in

  let reason = mk_reason RFunctionType loc in

  let rest_param = match rest with
  | Some (_, { Function.RestParam.argument = (_, param) }) ->
    let { Function.Param.name; typeAnnotation; _ } = param in
    let rest = convert cx tparams_map typeAnnotation in
    (* TODO - Use AssertRestParamT here. The big problem is that, at this
     * point, there might be some unsubstituted type parameters in the rest
     * type. Unlike expressions, which know all type parameters have been
     * substituted thanks to generate_tests, we visit types outside of
     * generate_tests.
     *
     * One solution might be to build a type visitor that runs during
     * generate_tests and does the various subst and tests then
     *)
    Some (Option.map ~f:ident_name name, loc_of_t rest, rest)
  | None -> None in

  let params_names = List.rev rev_params_names in
  let return_t = convert cx tparams_map returnType in
  let ft =
    DefT (reason, FunT (
      Flow_js.dummy_static reason,
      DefT (mk_reason RPrototype loc, AnyT),
      {
        this_t = Flow_js.mk_tvar cx (mk_reason RThis loc);
        params_tlist = (List.rev rev_params_tlist);
        params_names = Some params_names;
        rest_param;
        return_t;
        is_predicate = false;
        closure_t = 0;
        changeset = Changeset.empty;
        def_reason = reason;
      }))
  in
  poly_type tparams ft

| loc, Object { Object.exact; properties } ->
  let reason_desc = RObjectType in
  let callable = List.exists (function
    | Object.CallProperty (_, { Object.CallProperty.static; _ }) -> not static
    | _ -> false
  ) properties in
  let proto =
    if callable
    then FunProtoT (locationless_reason reason_desc)
    else ObjProtoT (locationless_reason reason_desc)
  in
  let mk_object ~exact (call_props, dict, props_map) =
    let props_map = match List.rev call_props with
      | [] -> props_map
      | [t] ->
        let p = Field (t, Positive) in
        SMap.add "$call" p props_map
      | t0::t1::ts ->
        let callable_reason = mk_reason (RCustom "callable object type") loc in
        let rep = InterRep.make t0 t1 ts in
        let t = DefT (callable_reason, IntersectionT rep) in
        let p = Field (t, Positive) in
        SMap.add "$call" p props_map
    in
    (* Use the same reason for proto and the ObjT so we can walk the proto chain
       and use the root proto reason to build an error. *)
    let pmap = Context.make_property_map cx props_map in
    let flags = {
      sealed = Sealed;
      exact;
      frozen = false
    } in
    DefT (mk_reason reason_desc loc,
      ObjT (Flow_js.mk_objecttype ~flags dict pmap proto))
  in
  let property loc prop props =
    match prop with
    | { Object.Property.
        key; value = Object.Property.Init value; optional; variance; _method;
        static = _; (* object types don't have static props *)
      } ->
      begin match key with
      | Ast.Expression.Object.Property.Literal
          (_, { Ast.Literal.value = Ast.Literal.String name; _ })
      | Ast.Expression.Object.Property.Identifier (_, name) ->
          let t = convert cx tparams_map value in
          let t = if optional then Type.optional t else t in
          let polarity = if _method then Positive else polarity variance in
          SMap.add name (Field (t, polarity)) props
      | _ ->
        Flow_js.add_output cx (FlowError.EUnsupportedKeyInObjectType loc);
        props
      end

    (* unsafe getter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (_, name);
        value = Object.Property.Get (loc, f);
        _ } when Context.enable_unsafe_getters_and_setters cx ->
      let function_type = convert cx tparams_map (loc, Ast.Type.Function f) in
      let return_t = Type.extract_getter_type function_type in
      Properties.add_getter name return_t props

    (* unsafe setter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (_, name);
        value = Object.Property.Set (loc, f);
        _ } when Context.enable_unsafe_getters_and_setters cx ->
      let function_type = convert cx tparams_map (loc, Ast.Type.Function f) in
      let param_t = Type.extract_setter_type function_type in
      Properties.add_setter name param_t props

    | { Object.Property.
        value = Object.Property.Get _ | Object.Property.Set _;
        _
      } ->
      Flow_js.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyGetSet));
      props
  in
  let add_call c = function
    | None -> Some ([c], None, SMap.empty)
    | Some (cs, d, pmap) -> Some (c::cs, d, pmap)
  in
  let make_dict { Object.Indexer.id; key; value; variance; _ } =
    Some { Type.
      dict_name = Option.map id snd;
      key = convert cx tparams_map key;
      value = convert cx tparams_map value;
      dict_polarity = polarity variance;
    }
  in
  let add_dict loc indexer = function
    | None -> Some ([], make_dict indexer, SMap.empty)
    | Some (cs, None, pmap) -> Some (cs, make_dict indexer, pmap)
    | Some (_, Some _, _) as o ->
      Flow_js.add_output cx
        FlowError.(EUnsupportedSyntax (loc, MultipleIndexers));
      o
  in
  let add_prop loc p = function
    | None -> Some ([], None, property loc p SMap.empty)
    | Some (cs, d, pmap) -> Some (cs, d, property loc p pmap)
  in
  let o, ts, spread = List.fold_left (
    fun (o, ts, spread) -> function
    | Object.CallProperty (loc, { Object.CallProperty.value = (_, ft); _ }) ->
      let t = convert cx tparams_map (loc, Ast.Type.Function ft) in
      add_call t o, ts, spread
    | Object.Indexer (loc, i) ->
      add_dict loc i o, ts, spread
    | Object.Property (loc, p) ->
      add_prop loc p o, ts, spread
    | Object.SpreadProperty (_, { Object.SpreadProperty.argument }) ->
      let ts = match o with
      | None -> ts
      | Some o -> (mk_object ~exact:true o)::ts
      in
      let o = convert cx tparams_map argument in
      None, o::ts, true
  ) (None, [], false) properties in
  let ts = match o with
  | None -> ts
  | Some o -> mk_object ~exact:spread o::ts
  in
  (match ts with
  | [] ->
    let t = mk_object ~exact ([], None, SMap.empty) in
    if exact
    then ExactT (mk_reason (RExactType reason_desc) loc, t)
    else t
  | [t] when not spread ->
    if exact
    then ExactT (mk_reason (RExactType reason_desc) loc, t)
    else t
  | t::ts ->
    let reason = mk_reason RObjectType loc in
    EvalT (t, TypeDestructorT (reason, SpreadType (exact, ts)), mk_id ()))

| loc, Exists ->
  (* Do not evaluate existential type variables when map is non-empty. This
     ensures that existential type variables under a polymorphic type remain
     unevaluated until the polymorphic type is applied. *)
  let force = SMap.is_empty tparams_map in
  let reason = derivable_reason (mk_reason RExistential loc) in
  if force then Flow_js.mk_tvar cx reason
  else ExistsT reason
)

and convert_qualification ?(lookup_mode=ForType) cx reason_prefix
  = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) ->
    let m = convert_qualification ~lookup_mode cx reason_prefix qualification in
    let name = ident_name id in
    let desc = RCustom (spf "%s '<<object>>.%s')" reason_prefix name) in
    let reason = mk_reason desc loc in
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetPropT (reason, Named (reason, name), t));
    )

  | Unqualified (loc, name) ->
    Env.get_var ~lookup_mode cx name loc
)

and mk_type cx tparams_map reason = function
  | None ->
      let t =
        if Context.is_weak cx
        then AnyT.why reason
        else Flow_js.mk_tvar cx reason
      in
      Hashtbl.replace (Context.annot_table cx) (loc_of_reason reason) t;
      t

  | Some annot ->
      convert cx tparams_map annot

and mk_type_annotation cx tparams_map reason = function
| None ->
  mk_type cx tparams_map reason None
| Some (_, typeAnnotation) ->
  mk_type cx tparams_map reason (Some typeAnnotation)

and mk_singleton_string loc key =
  let reason = mk_reason (RStringLit key) loc in
  DefT (reason, SingletonStrT key)

and mk_singleton_number loc num raw =
  let reason = mk_reason (RNumberLit raw) loc in
  DefT (reason, SingletonNumT (num, raw))

and mk_singleton_boolean loc b =
  let reason = mk_reason (RBooleanLit b) loc in
  DefT (reason, SingletonBoolT b)

(* Given the type of expression C and type arguments T1...Tn, return the type of
   values described by C<T1,...,Tn>, or C when there are no type arguments. *)
(** See comment on Flow_js.mk_instance for what the for_type flag means. **)
and mk_nominal_type ?(for_type=true) cx reason tparams_map (c, targs) =
  match targs with
  | None ->
      Flow_js.mk_instance cx reason ~for_type c
  | Some targs ->
      let tparams = List.map (convert cx tparams_map) targs in
      typeapp c tparams

(* take a list of AST type param declarations,
   do semantic checking and create types for them. *)
and mk_type_param_declarations cx ?(tparams_map=SMap.empty) typeParameters =
  let open Ast.Type.ParameterDeclaration in
  let add_type_param (tparams, tparams_map, bounds_map) = function
  | loc, { TypeParam.name; bound; variance; default; } ->
    let reason = mk_reason (RCustom name) loc in
    let bound = match bound with
    | None -> DefT (reason, MixedT Mixed_everything)
    | Some (_, u) ->
        mk_type cx tparams_map reason (Some u)
    in
    let default = match default with
    | None -> None
    | Some default ->
        let t = mk_type cx tparams_map reason (Some default) in
        Flow_js.flow_t cx (Flow_js.subst cx bounds_map t,
                           Flow_js.subst cx bounds_map bound);
        Some t in
    let polarity = polarity variance in
    let tparam = { reason; name; bound; polarity; default; } in
    (tparam :: tparams,
     SMap.add name (BoundT tparam) tparams_map,
     SMap.add name (Flow_js.subst cx bounds_map bound) bounds_map)
  in
  let tparams, tparams_map, _ =
    extract_type_param_declarations typeParameters
    |> List.fold_left add_type_param ([], tparams_map, SMap.empty)
  in
  List.rev tparams, tparams_map

and type_identifier cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else if name = "undefined"
  then VoidT.at loc
  else Env.var_ref ~lookup_mode:ForType cx name loc

and extract_type_param_declarations =
  let open Ast.Type.ParameterDeclaration in
  let f (_, typeParameters) = typeParameters.params in
  Option.value_map ~f ~default:[]

and extract_type_param_instantiations =
  let open Ast.Type.ParameterInstantiation in
  function
  | None -> None
  | Some (_, typeParameters) -> Some typeParameters.params

and polarity = Ast.Variance.(function
  | Some (_, Plus) -> Positive
  | Some (_, Minus) -> Negative
  | None -> Neutral
)
