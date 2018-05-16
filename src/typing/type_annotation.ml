(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type
open Env.LookupMode

module FlowError = Flow_error
module Flow = Flow_js

(* AST helpers *)

let qualified_name =
  let rec loop acc = Ast.Type.Generic.Identifier.(function
    | Unqualified (_, name) ->
      let parts = name::acc in
      String.concat "." parts
    | Qualified (_, { qualification; id = (_, name) }) ->
      loop (name::acc) qualification
  ) in
  loop []

let ident_name (_, name) = name

let error_type cx loc msg =
  Flow.add_output cx msg;
  AnyT.at loc

let is_suppress_type cx type_name =
  SSet.mem type_name (Context.suppress_types cx)

let check_type_arg_arity cx loc params n f =
  match params with
  | None ->
    if n = 0 then
      f ()
    else
      error_type cx loc (FlowError.ETypeParamArity (loc, n))
  | Some l ->
    if n = List.length l && n <> 0 then
      f ()
    else
      error_type cx loc (FlowError.ETypeParamArity (loc, n))

let mk_custom_fun cx loc targs kind =
  check_type_arg_arity cx loc targs 0 (fun () ->
    let reason = mk_reason RFunctionType loc in
    CustomFunT (reason, kind)
  )

let mk_react_prop_type cx loc targs kind =
  mk_custom_fun cx loc targs
    (ReactPropType (React.PropType.Complex kind))

let add_unclear_type_error_if_not_lib_file cx loc = Loc.(
  match loc.source with
    | Some file when not @@ File_key.is_lib_file file ->
      Flow_js.add_output cx (FlowError.EUnclearType loc)
    | _ -> ()
)

let add_deprecated_type_error_if_not_lib_file cx loc = Loc.(
  match loc.source with
    | Some file when not @@ File_key.is_lib_file file ->
      Flow_js.add_output cx (FlowError.EDeprecatedType loc)
    | _ -> ()
)

(**********************************)
(* Transform annotations to types *)
(**********************************)

(* converter *)
let rec convert cx tparams_map = Ast.Type.(function

| loc, Any ->
  add_unclear_type_error_if_not_lib_file cx loc;
  AnyT.at loc

| loc, Mixed -> MixedT.at loc

| loc, Empty -> EmptyT.at loc

| loc, Void -> VoidT.at loc

| loc, Null -> NullT.at loc

| loc, Number -> NumT.at loc

| loc, String -> StrT.at loc

| loc, Boolean -> BoolT.at loc

| loc, Nullable t ->
    let t = convert cx tparams_map t in
    let reason = annot_reason (mk_reason (RMaybe (desc_of_t t)) loc) in
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
      targs = None
    }) ->
      let valtype = convert_qualification ~lookup_mode:ForTypeof cx
        "typeof-annotation" qualification in
      let reason = mk_reason (RTypeof (qualified_name qualification)) loc in
      let valtype = mod_reason_of_t (fun _ -> reason) valtype in
      Flow.mk_typeof_annotation cx reason valtype
  | (loc, _) ->
    error_type cx loc (FlowError.EUnexpectedTypeof loc)
  end

| loc, Tuple ts ->
  let tuple_types = List.map (convert cx tparams_map) ts in
  let reason = annot_reason (mk_reason RTupleType loc) in
  let element_reason = mk_reason RTupleElement loc in
  let elemt = match tuple_types with
  | [] -> EmptyT.why element_reason
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

| loc, StringLiteral { Ast.StringLiteral.value; _ }  ->
  mk_singleton_string loc value

| loc, NumberLiteral { Ast.NumberLiteral.value; raw; _ }  ->
  mk_singleton_number loc value raw

| loc, BooleanLiteral value  ->
  mk_singleton_boolean loc value

(* TODO *)
| loc, Generic { Generic.id = (Generic.Identifier.Qualified (qid_loc,
       { Generic.Identifier.qualification; id; }) as qid); targs } ->

  let m = convert_qualification cx "type-annotation" qualification in
  let id_loc, name = id in
  let reason = mk_reason (RType name) loc in
  let id_reason = mk_reason (RType name) id_loc in
  let t = Tvar.mk_where cx reason (fun t ->
    let id_info = name, t, Type_table.Other in
    Env.add_type_table_info cx ~tparams_map id_loc id_info;
    let use_op = Op (GetProperty (mk_reason (RType (qualified_name qid)) qid_loc)) in
    Flow.flow cx (m, GetPropT (use_op, id_reason, Named (id_reason, name), t));
  ) in
  let targs = extract_type_param_instantiations targs in
  mk_nominal_type cx reason tparams_map (t, targs)

(* type applications: name < params > *)
| loc, Generic {
    Generic.id = Generic.Identifier.Unqualified (id);
    targs
  } ->
  let name_loc, name = id in
  let targs = extract_type_param_instantiations targs in

  let convert_type_params () = Option.value_map
    targs
    ~default: []
    ~f:(List.map (convert cx tparams_map)) in

  let use_op reason =
    Op (TypeApplication { type' = reason }) in

  begin match name with

  (* Array<T> *)
  | "Array" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let elemt = convert_type_params () |> List.hd in
      DefT (mk_reason RArrayType loc, ArrT (ArrayAT (elemt, None)))
    )

  (* $ReadOnlyArray<T> is the supertype of all tuples and all arrays *)
  | "$ReadOnlyArray" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let elemt = convert_type_params () |> List.hd in
      DefT (annot_reason (mk_reason RROArrayType loc), ArrT (ROArrayAT (elemt)))
    )

  (* $Supertype<T> acts as any over supertypes of T *)
  | "$Supertype" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      AnyWithLowerBoundT t
    )

  (* $Subtype<T> acts as any over subtypes of T *)
  | "$Subtype" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      AnyWithUpperBoundT t
    )

  (* $PropertyType<T, 'x'> acts as the type of 'x' in object type T *)
  | "$PropertyType" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      match convert_type_params () with
      | [t; DefT (_, SingletonStrT key)] ->
        let reason = mk_reason (RType "$PropertyType") loc in
        EvalT (t, TypeDestructorT
          (use_op reason, reason, PropertyType key), mk_id())
      | _ ->
        error_type cx loc (FlowError.EPropertyTypeAnnot loc)
    )

  (* $ElementType<T, string> acts as the type of the string elements in object
     type T *)
  | "$ElementType" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let ts = convert_type_params () in
      let t = List.nth ts 0 in
      let e = List.nth ts 1 in
      let reason = mk_reason (RType "$ElementType") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason, ElementType e), mk_id())
    )

  (* $NonMaybeType<T> acts as the type T without null and void *)
  | "$NonMaybeType" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "$NonMaybeType") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason, NonMaybeType), mk_id())
    )

  (* $Shape<T> matches the shape of T *)
  | "$Shape" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      ShapeT t
    )

  (* $Diff<T, S> *)
  | "$Diff" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason (RType "$Diff") loc in
      EvalT (t1, TypeDestructorT (use_op reason, reason,
        RestType (Type.Object.Rest.IgnoreExactAndOwn, t2)), mk_id ())
    )

  (* $ReadOnly<T> *)
  | "$ReadOnly" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "$ReadOnly") loc in
      EvalT (
        t,
        TypeDestructorT (
          use_op reason,
          reason,
          ReadOnlyType
        ),
        mk_id ()
      )
    )

  (* $Keys<T> is the set of keys of T *)
  (** TODO: remove $Enum **)
  | "$Keys" | "$Enum" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      KeysT (mk_reason RKeySet loc, t)
    )

  (* $Values<T> is a union of all the own enumerable value types of T *)
  | "$Values" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "$Values") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason, ValuesType), mk_id())
    )

  | "$Exact" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = List.hd (convert_type_params ()) in
      let desc = RExactType (desc_of_t t) in
      ExactT (mk_reason desc loc, t)
    )

  | "$Rest" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason (RType "$Rest") loc in
      EvalT (t1, TypeDestructorT (use_op reason, reason,
        RestType (Type.Object.Rest.Sound, t2)), mk_id ())
    )

  (* $Exports<'M'> is the type of the exports of module 'M' *)
  (** TODO: use `import typeof` instead when that lands **)
  | "$Exports" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      match targs with
      | Some ((_, StringLiteral { Ast.StringLiteral.value; _ })::_) ->
          let desc = RModule value in
          let reason = mk_reason desc loc in
          let remote_module_t =
            Env.get_var_declared_type cx (internal_module_name value) loc
          in
          Tvar.mk_where cx reason (fun t ->
            Flow.flow cx (remote_module_t, CJSRequireT(reason, t, Context.is_strict cx))
          )
      | _ ->
          error_type cx loc (FlowError.EExportsAnnot loc)
    )

  | "$Call" ->
    (match convert_type_params () with
    | fn::args ->
       let reason = mk_reason RFunctionCallType loc in
       EvalT (fn, TypeDestructorT (use_op reason, reason, CallType args), mk_id ())
    | _ ->
      error_type cx loc (FlowError.ETypeParamMinArity (loc, 1)))

  | "$TupleMap" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RTupleMap loc in
      EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (TupleMap t2)), mk_id ())
    )

  | "$ObjMap" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RObjectMap loc in
      EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (ObjectMap t2)), mk_id ())
    )

  | "$ObjMapi" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2 = match convert_type_params () with
      | [t1; t2] -> t1, t2
      | _ -> assert false in
      let reason = mk_reason RObjectMapi loc in
      EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (ObjectMapi t2)), mk_id ())
    )

  | "$CharSet" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      match targs with
    | Some [(_, StringLiteral { Ast.StringLiteral.value; _ })] ->
        let chars = String_utils.CharSet.of_string value in
        let char_str = String_utils.CharSet.to_string chars in (* sorts them *)
        let reason = mk_reason (RCustom (spf "character set `%s`" char_str)) loc in
        DefT (reason, CharSetT chars)
      | _ ->
        error_type cx loc (FlowError.ECharSetAnnot loc)
    )

  | "this" ->
    if SMap.mem "this" tparams_map then
      (* We model a this type like a type parameter. The bound on a this
         type reflects the interface of `this` exposed in the current
         environment. Currently, we only support this types in a class
         environment: a this type in class C is bounded by C. *)
      check_type_arg_arity cx loc targs 0 (fun () ->
        Flow.reposition cx loc (SMap.find_unsafe "this" tparams_map)
      )
    else (
      Flow.add_output cx (FlowError.EUnexpectedThisType loc);
      Locationless.AnyT.t
    )

  (* Class<T> is the type of the class whose instances are of type T *)
  | "Class" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RStatics (desc_of_t t)) loc in
      DefT (reason, ClassT t)
    )

  | "Function" | "function" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      DefT (reason, AnyFunT)
    )

  | "Object" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RObjectType loc in
      DefT (reason, AnyObjT)
    )

  | "Function$Prototype$Apply" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoApplyT reason
    )

  | "Function$Prototype$Bind" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoBindT reason
    )

  | "Function$Prototype$Call" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      FunProtoCallT reason
    )

  | "Object$Assign" ->
      mk_custom_fun cx loc targs ObjectAssign
  | "Object$GetPrototypeOf" ->
      mk_custom_fun cx loc targs ObjectGetPrototypeOf
  | "Object$SetPrototypeOf" ->
      mk_custom_fun cx loc targs ObjectSetPrototypeOf

  | "$Compose" ->
      mk_custom_fun cx loc targs (Compose false)
  | "$ComposeReverse" ->
      mk_custom_fun cx loc targs (Compose true)

  | "React$PropType$Primitive" ->
      check_type_arg_arity cx loc targs 1 (fun () ->
        let t = convert_type_params () |> List.hd in
        let prop_type = (ReactPropType (React.PropType.Primitive (false, t))) in
        mk_custom_fun cx loc None prop_type
      )
  | "React$PropType$ArrayOf" ->
      mk_react_prop_type cx loc targs React.PropType.ArrayOf
  | "React$PropType$InstanceOf" ->
      mk_react_prop_type cx loc targs React.PropType.InstanceOf
  | "React$PropType$ObjectOf" ->
      mk_react_prop_type cx loc targs React.PropType.ObjectOf
  | "React$PropType$OneOf" ->
      mk_react_prop_type cx loc targs React.PropType.OneOf
  | "React$PropType$OneOfType" ->
      mk_react_prop_type cx loc targs React.PropType.OneOfType
  | "React$PropType$Shape" ->
      mk_react_prop_type cx loc targs React.PropType.Shape
  | "React$CreateClass" ->
      mk_custom_fun cx loc targs ReactCreateClass
  | "React$CreateElement" ->
      mk_custom_fun cx loc targs ReactCreateElement
  | "React$CloneElement" ->
      mk_custom_fun cx loc targs ReactCloneElement
  | "React$ElementFactory" ->
      check_type_arg_arity cx loc targs 1 (fun () ->
        let t = convert_type_params () |> List.hd in
        mk_custom_fun cx loc None (ReactElementFactory t)
      )
  | "React$ElementProps" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "React$ElementProps") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason,
          ReactElementPropsType), mk_id ())
    )
  | "React$ElementConfig" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "React$ElementConfig") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason,
          ReactElementConfigType), mk_id ())
    )
  | "React$ElementRef" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = mk_reason (RType "React$ElementRef") loc in
      EvalT (t, TypeDestructorT
        (use_op reason, reason,
          ReactElementRefType), mk_id ())
    )

  | "$Facebookism$Idx" ->
      mk_custom_fun cx loc targs Idx

  | "$Flow$DebugPrint" ->
      mk_custom_fun cx loc targs DebugPrint
  | "$Flow$DebugThrow" ->
      mk_custom_fun cx loc targs DebugThrow
  | "$Flow$DebugSleep" ->
      mk_custom_fun cx loc targs DebugSleep

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
    check_type_arg_arity cx loc targs 0 (fun () ->
      let t = Flow.reposition cx loc (SMap.find_unsafe name tparams_map) in
      let id_info = name, t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map name_loc id_info;
      t
    )

  | "$Pred" ->
    let fun_reason = mk_reason (RCustom "abstract predicate function") loc in
    let static_reason = mk_reason (RCustom "abstract predicate static") loc in
    let out_reason = mk_reason (RCustom "open predicate") loc in

    check_type_arg_arity cx loc targs 1 (fun () ->
      match convert_type_params () with
      | [DefT (_, SingletonNumT (f, _))] ->
        let n = Pervasives.int_of_float f in
        let key_strs =
          ListUtils.range 0 n |>
          List.map (fun i -> Some ("x_" ^ Pervasives.string_of_int i)) in
        let emp = Key_map.empty in
        let tins = ListUtils.repeat n (AnyT.at loc) in
        let tout = OpenPredT (out_reason, MixedT.at loc, emp, emp) in
        DefT (fun_reason, FunT (
          dummy_static static_reason,
          DefT (mk_reason RPrototype loc, AnyT),
          mk_functiontype fun_reason tins tout
            ~rest_param:None ~def_reason:fun_reason
            ~params_names:key_strs ~is_predicate:true
        ))

      | _ ->
        error_type cx loc (FlowError.EPredAnnot loc)
    )

  | "$Refine" ->
    check_type_arg_arity cx loc targs 3 (fun () ->
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
    let reason = mk_reason (RType name) loc in
    let c = type_identifier cx name loc in
    let id_info = name, c, Type_table.Other in
    Env.add_type_table_info cx ~tparams_map name_loc id_info;
    mk_nominal_type cx reason tparams_map (c, targs)

  end

| loc, Function { Function.
    params = (_, { Function.Params.params; rest });
    return;
    tparams;
  } ->
  let tparams, tparams_map =
    mk_type_param_declarations cx ~tparams_map tparams in

  let rev_params = List.fold_left (fun acc (_, param) ->
    let { Function.Param.name; annot; optional } = param in
    let t = convert cx tparams_map annot in
    let t = if optional then Type.optional t else t in
    Option.iter ~f:(fun (loc, name) ->
      let id_info = name, t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map loc id_info;
    ) name;
    (Option.map ~f:ident_name name, t) :: acc
  ) [] params in

  let reason = mk_reason RFunctionType loc in

  let rest_param = match rest with
  | Some (_, { Function.RestParam.argument = (_, param) }) ->
    let { Function.Param.name; annot; _ } = param in
    let rest = convert cx tparams_map annot in
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

  let return_t = convert cx tparams_map return in
  let ft =
    DefT (reason, FunT (
      dummy_static reason,
      DefT (mk_reason RPrototype loc, AnyT),
      {
        this_t = DefT (mk_reason RThis loc, AnyT);
        params = List.rev rev_params;
        rest_param;
        return_t;
        is_predicate = false;
        closure_t = 0;
        changeset = Changeset.empty;
        def_reason = reason;
      }))
  in
  let id = Context.make_nominal cx in
  poly_type id tparams ft

| loc, Object { Object.exact; properties } ->
  let reason_desc = RObjectType in
  let callable = List.exists (function
    | Object.CallProperty (_, { Object.CallProperty.static; _ }) -> not static
    | _ -> false
  ) properties in
  let mk_object ~exact (call_props, dict, props_map, proto) =
    let props_map = match List.rev call_props with
      | [] -> props_map
      | [t] ->
        let p = Field (None, t, Positive) in
        SMap.add "$call" p props_map
      | t0::t1::ts ->
        let callable_reason = mk_reason (RCustom "callable object type") loc in
        let rep = InterRep.make t0 t1 ts in
        let t = DefT (callable_reason, IntersectionT rep) in
        let p = Field (None, t, Positive) in
        SMap.add "$call" p props_map
    in
    (* Use the same reason for proto and the ObjT so we can walk the proto chain
       and use the root proto reason to build an error. *)
    let props_map, proto = match proto with
      | Some t ->
        (* The existence of a callable property already implies that
         * __proto__ = Function.prototype. Treat __proto__ as a property *)
        if callable
        then
          SMap.add "__proto__" (Field (None, t, Neutral)) props_map,
          FunProtoT (locationless_reason RFunctionPrototype)
        else
          props_map, t
      | None ->
        props_map,
        if callable
        then FunProtoT (locationless_reason RFunctionPrototype)
        else ObjProtoT (locationless_reason RObjectPrototype)
    in
    let pmap = Context.make_property_map cx props_map in
    let flags = {
      sealed = Sealed;
      exact;
      frozen = false
    } in
    DefT (mk_reason reason_desc loc,
      ObjT (mk_objecttype ~flags dict pmap proto))
  in
  let property loc prop props proto =
    match prop with
    | { Object.Property.
        key; value = Object.Property.Init value; optional; variance; _method;
        static = _; (* object types don't have static props *)
        proto = _; (* ditto proto props *)
      } ->
      begin match key with
      | Ast.Expression.Object.Property.Literal
          (loc, { Ast.Literal.value = Ast.Literal.String name; _ })
      | Ast.Expression.Object.Property.Identifier (loc, name) ->
          Type_inference_hooks_js.dispatch_obj_prop_decl_hook cx name loc;
          let t = convert cx tparams_map value in
          if name = "__proto__" && not (_method || optional) && variance = None
          then
            let reason = mk_reason RPrototype (fst value) in
            let proto = Tvar.mk_where cx reason (fun tout ->
              Flow.flow cx (t, ObjTestProtoT (reason, tout))
            ) in
            props, Some (Flow.mk_typeof_annotation cx reason proto)
          else
            let t = if optional then Type.optional t else t in
            let key_loc = match key with
              | Ast.Expression.Object.Property.Literal (loc, _) -> loc
              | Ast.Expression.Object.Property.Identifier (loc, _) -> loc
              | Ast.Expression.Object.Property.PrivateName (loc, _) -> loc
              | Ast.Expression.Object.Property.Computed (loc, _) -> loc
            in
            let id_info = name, t, Type_table.Other in
            Env.add_type_table_info cx ~tparams_map key_loc id_info;
            let polarity = if _method then Positive else polarity variance in
            let props = SMap.add name (Field (Some loc, t, polarity)) props in
            props, proto
      | Ast.Expression.Object.Property.Literal (loc, _)
      | Ast.Expression.Object.Property.PrivateName (loc, _)
      | Ast.Expression.Object.Property.Computed (loc, _)
          ->
        Flow.add_output cx (FlowError.EUnsupportedKeyInObjectType loc);
        props, proto
      end

    (* unsafe getter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (id_loc, name);
        value = Object.Property.Get (loc, f);
        _ } ->
      Flow_js.add_output cx (FlowError.EUnsafeGettersSetters loc);
      let function_type = convert cx tparams_map (loc, Ast.Type.Function f) in
      let return_t = Type.extract_getter_type function_type in
      let id_info = name, return_t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map id_loc id_info;
      let props = Properties.add_getter name (Some id_loc) return_t props in
      props, proto

    (* unsafe setter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (id_loc, name);
        value = Object.Property.Set (loc, f);
        _ } ->
      Flow_js.add_output cx (FlowError.EUnsafeGettersSetters loc);
      let function_type = convert cx tparams_map (loc, Ast.Type.Function f) in
      let param_t = Type.extract_setter_type function_type in
      let id_info = name, param_t, Type_table.Other in
      Env.add_type_table_info cx ~tparams_map id_loc id_info;
      let props = Properties.add_setter name (Some id_loc) param_t props in
      props, proto


    | { Object.Property.
        value = Object.Property.Get _ | Object.Property.Set _;
        _
      } ->
      Flow.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyGetSet));
      props, proto
  in
  let add_call c = function
    | None -> Some ([c], None, SMap.empty, None)
    | Some (cs, d, pmap, proto) -> Some (c::cs, d, pmap, proto)
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
    | None -> Some ([], make_dict indexer, SMap.empty, None)
    | Some (cs, None, pmap, proto) -> Some (cs, make_dict indexer, pmap, proto)
    | Some (_, Some _, _, _) as o ->
      Flow.add_output cx
        FlowError.(EUnsupportedSyntax (loc, MultipleIndexers));
      o
  in
  let add_prop loc p = function
    | None ->
      let pmap, proto = property loc p SMap.empty None in
      Some ([], None, pmap, proto)
    | Some (cs, d, pmap, proto) ->
      let pmap, proto = property loc p pmap proto in
      Some (cs, d, pmap, proto)
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
    | Object.InternalSlot (loc, { Object.InternalSlot.id = (_, name); _ }) ->
      Flow.add_output cx FlowError.(
        EUnsupportedSyntax (loc, UnsupportedInternalSlot {
          name;
          static = false;
        }));
      o, ts, spread
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
    let t = mk_object ~exact ([], None, SMap.empty, None) in
    if exact
    then ExactT (mk_reason (RExactType reason_desc) loc, t)
    else t
  | [t] when not spread ->
    if exact
    then ExactT (mk_reason (RExactType reason_desc) loc, t)
    else t
  | t::ts ->
    let open Type.Object.Spread in
    let reason = mk_reason RObjectType loc in
    let target = Annot {make_exact = exact} in
    EvalT (t, TypeDestructorT (unknown_use, reason, SpreadType (target, ts)), mk_id ()))

  | loc, Interface {Interface.extends; body} ->
    let (_, {Ast.Type.Object.properties; _}) = body in
    let reason = mk_reason RInterfaceType loc in
    let iface_sig =
      let id = Context.make_nominal cx in
      let extends = List.map (mk_interface_super cx tparams_map) extends in
      let super =
        let callable = List.exists Ast.Type.Object.(function
          | CallProperty (_, { CallProperty.static; _ }) -> not static
          | _ -> false
        ) properties in
        Class_sig.Interface { extends; callable }
      in
      Class_sig.empty id reason [] tparams_map super
    in
    let iface_sig = add_interface_properties cx tparams_map properties iface_sig in
    Class_sig.generate_tests cx (fun iface_sig ->
      Class_sig.check_super cx reason iface_sig;
      Class_sig.check_implements cx reason iface_sig
    ) iface_sig;
    Class_sig.thistype cx iface_sig

| loc, Exists ->
  add_deprecated_type_error_if_not_lib_file cx loc;
  (* Do not evaluate existential type variables when map is non-empty. This
     ensures that existential type variables under a polymorphic type remain
     unevaluated until the polymorphic type is applied. *)
  let force = SMap.is_empty tparams_map in
  let reason = derivable_reason (mk_reason RExistential loc) in
  if force then Tvar.mk cx reason
  else ExistsT reason
)

and convert_qualification ?(lookup_mode=ForType) cx reason_prefix
  = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) as qualified ->
    let m = convert_qualification ~lookup_mode cx reason_prefix qualification in
    let id_loc, name = id in
    let desc = RCustom (spf "%s `%s`" reason_prefix (qualified_name qualified)) in
    let reason = mk_reason desc loc in
    let id_reason = mk_reason desc id_loc in
    Tvar.mk_where cx reason (fun t ->
      let id_info = name, t, Type_table.Other in
      Env.add_type_table_info cx id_loc id_info;
      let use_op = Op (GetProperty (mk_reason (RType (qualified_name qualified)) loc)) in
      Flow.flow cx (m, GetPropT (use_op, id_reason, Named (id_reason, name), t));
    )

  | Unqualified (loc, name) ->
    let t = Env.get_var ~lookup_mode cx name loc in
    let id_info = name, t, Type_table.Other in
    Env.add_type_table_info cx loc id_info;
    t
)

and mk_func_sig =
  let open Ast.Type.Function in
  let add_param cx tparams_map (loc, {Param.name=id; annot; optional; _}) =
    let t = convert cx tparams_map annot in
    Func_params.add_simple cx ~tparams_map ~optional loc id t
  in
  let add_rest cx tparams_map (loc, {Param.name=id; annot; _}) =
    let t = convert cx tparams_map annot in
    let () =
      let name = Option.map id ~f:snd in
      let reason = mk_reason (RRestParameter name) (loc_of_t t) in
      Flow.flow cx (t, AssertRestParamT reason)
    in
    Func_params.add_rest cx ~tparams_map loc id t
  in
  let convert_params cx tparams_map (_, {Params.params; rest}) =
    let params = List.fold_left (fun acc param ->
      add_param cx tparams_map param acc
    ) Func_params.empty params in
    match rest with
    | Some (_, { RestParam.argument }) ->
      add_rest cx tparams_map argument params
    | None -> params
  in
  fun cx tparams_map loc func ->
    let tparams, tparams_map =
      mk_type_param_declarations cx ~tparams_map func.tparams in
    { Func_sig.
      reason = mk_reason RFunctionType loc;
      kind = Func_sig.Ordinary;
      tparams;
      tparams_map;
      fparams = convert_params cx tparams_map func.Ast.Type.Function.params;
      body = None;
      return_t = convert cx tparams_map func.return;
    }

and mk_type cx tparams_map reason = function
  | None ->
      let t =
        if Context.is_weak cx
        then AnyT.why reason
        else Tvar.mk cx reason
      in
      Hashtbl.replace (Context.annot_table cx) (loc_of_reason reason) t;
      t

  | Some annot ->
      convert cx tparams_map annot

and mk_type_annotation cx tparams_map reason = function
| None ->
  mk_type cx tparams_map reason None
| Some (_, annot) ->
  mk_type cx tparams_map reason (Some annot)

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
(** See comment on Flow.mk_instance for what the for_type flag means. **)
and mk_nominal_type ?(for_type=true) cx reason tparams_map (c, targs) =
  let reason = annot_reason reason in
  let c = mod_reason_of_t (fun reason ->
    annot_reason (replace_reason (function
    (* Unwrapping RStatics aligns error messages that look at the top level
     * reason. mk_instance unwraps ClassT so the result of mk_nominal_type will
     * not be the "statics of". *)
    | RStatics desc -> desc
    | desc -> desc
    ) reason)
  ) c in
  match targs with
  | None ->
      Flow.mk_instance cx reason ~for_type c
  | Some targs ->
      let tparams = List.map (convert cx tparams_map) targs in
      typeapp c tparams

(* take a list of AST type param declarations,
   do semantic checking and create types for them. *)
and mk_type_param_declarations cx ?(tparams_map=SMap.empty) tparams =
  let open Ast.Type.ParameterDeclaration in
  let add_type_param (tparams, tparams_map, bounds_map) = function
  | _, { TypeParam.name = (loc, name); bound; variance; default; } ->
    let reason = mk_reason (RType name) loc in
    let bound = match bound with
    | None -> DefT (reason, MixedT Mixed_everything)
    | Some (_, u) ->
        mk_type cx tparams_map reason (Some u)
    in
    let default = match default with
    | None -> None
    | Some default ->
        let t = mk_type cx tparams_map reason (Some default) in
        Flow.flow_t cx (Flow.subst cx bounds_map t,
                           Flow.subst cx bounds_map bound);
        Some t in
    let polarity = polarity variance in
    let tparam = { reason; name; bound; polarity; default; } in
    let t = BoundT tparam in
    let id_info = name, t, Type_table.Other in
    (* TODO PV we shouldn't need to add the 'tparam' here. This is the only
       case where this is necessary. Instead use a dedicated Type_table.id_kind
       for the declaration of type parameters.
    *)
    Env.add_type_table_info cx ~tparams_map ~tparam loc id_info;
    (tparam :: tparams,
     SMap.add name t tparams_map,
     SMap.add name (Flow.subst cx bounds_map bound) bounds_map)
  in
  let tparams, tparams_map, _ =
    extract_type_param_declarations tparams
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
  let f (_, params) = params in
  Option.value_map ~f ~default:[]

and extract_type_param_instantiations =
  function
  | None -> None
  | Some (_, args) -> Some args

and polarity = Ast.Variance.(function
  | Some (_, Plus) -> Positive
  | Some (_, Minus) -> Negative
  | None -> Neutral
)

and mk_interface_super cx tparams_map (loc, {Ast.Type.Generic.id; targs}) =
  let lookup_mode = Env.LookupMode.ForType in
  let i = convert_qualification ~lookup_mode cx "extends" id in
  let loc = match targs with
  | Some (targs, _) -> Loc.btwn loc targs
  | None -> loc
  in
  let r = mk_reason (RType (qualified_name id)) loc in
  let targs = extract_type_param_instantiations targs in
  let t = mk_nominal_type cx r tparams_map (i, targs) in
  Flow.reposition cx loc ~annot_loc:loc t

and add_interface_properties cx tparams_map properties s =
  let open Class_sig in
  List.fold_left Ast.Type.Object.(fun x -> function
    | CallProperty (loc, { CallProperty.value = (_, func); static }) ->
      let fsig = mk_func_sig cx tparams_map loc func in
      append_method ~static "$call" None fsig x
    | Indexer (loc, { Indexer.static; _ }) when mem_field ~static "$key" x ->
      Flow.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, MultipleIndexers));
      x
    | Indexer (_, { Indexer.key; value; static; variance; _ }) ->
      let k = convert cx tparams_map key in
      let v = convert cx tparams_map value in
      let polarity = polarity variance in
      x
        |> add_field ~static "$key" (None, polarity, Annot k)
        |> add_field ~static "$value" (None, polarity, Annot v)
    | Property (loc, { Property.key; value; static; proto; _method; optional; variance; }) ->
      if optional && _method
      then Flow.add_output cx Flow_error.(EInternal (loc, OptionalMethod));
      let polarity = polarity variance in
      Ast.Expression.Object.(match _method, key, value with
      | _, Property.Literal (loc, _), _
      | _, Property.PrivateName (loc, _), _
      | _, Property.Computed (loc, _), _ ->
          Flow.add_output cx (Flow_error.EUnsupportedSyntax (loc, Flow_error.IllegalName));
          x
      | true, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Init (_, Ast.Type.Function func) ->
          let fsig = mk_func_sig cx tparams_map loc func in
          let append_method = match static, name with
          | false, "constructor" -> append_constructor (Some id_loc)
          | _ -> append_method ~static name (Some id_loc)
          in
          append_method fsig x

      | true, Property.Identifier _, _ ->
          Flow.add_output cx
            Flow_error.(EInternal (loc, MethodNotAFunction));
          x

      | false, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Init value ->
          let t = convert cx tparams_map value in
          let t = if optional then Type.optional t else t in
          let add = if proto then add_proto_field else add_field ~static in
          add name (Some id_loc, polarity, Annot t) x

      (* unsafe getter property *)
      | _, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Get (_, func) ->
          Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
          let fsig = mk_func_sig cx tparams_map loc func in
          add_getter ~static name (Some id_loc) fsig x

      (* unsafe setter property *)
      | _, Property.Identifier (id_loc, name),
          Ast.Type.Object.Property.Set (_, func) ->
          Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
          let fsig = mk_func_sig cx tparams_map loc func in
          add_setter ~static name (Some id_loc) fsig x

      )

    | InternalSlot (loc, { InternalSlot.id = (_, name); static; _ }) ->
      Flow.add_output cx Flow_error.(
        EUnsupportedSyntax (loc, UnsupportedInternalSlot {
          name;
          static;
        }));
      x

    | SpreadProperty (loc, _) ->
      Flow.add_output cx Flow_error.(EInternal (loc, InterfaceTypeSpread));
      x
  ) s properties

let mk_super cx tparams_map c targs = Type.(
  (* A super class must be parameterized by This, so that it can be
     specialized to this class and its subclasses when properties are looked
     up on their instances. *)
  let targs = extract_type_param_instantiations targs in
  let this = SMap.find_unsafe "this" tparams_map in
  match targs with
  | None ->
      (* No type args, but `c` could still be a polymorphic class that must
         be implicitly instantiated. We need to do this before we try to
         this-specialize `c`. *)
      let reason = reason_of_t c in
      let c = Tvar.mk_derivable_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (unknown_use, reason, reason, None, None, tvar))
      ) in
      this_typeapp c this None
  | Some targs ->
      let targs = List.map (convert cx tparams_map) targs in
      this_typeapp c this (Some targs)
)

let mk_interface_sig cx reason decl =
  let open Class_sig in
  let { Ast.Statement.Interface.
    id = (id_loc, id_name);
    tparams;
    body = (_, { Ast.Type.Object.properties; _ });
    extends;
    _;
  } = decl in

  let self = Tvar.mk cx reason in

  let tparams, tparams_map =
    mk_type_param_declarations cx tparams in

  let id_info = id_name, self, Type_table.Other in
  Env.add_type_table_info cx ~tparams_map id_loc id_info;

  let iface_sig =
    let id = Context.make_nominal cx in
    let extends = List.map (mk_interface_super cx tparams_map) extends in
    let super =
      let callable = List.exists Ast.Type.Object.(function
        | CallProperty (_, { CallProperty.static; _ }) -> not static
        | _ -> false
      ) properties in
      Interface { extends; callable }
    in
    empty id reason tparams tparams_map super
  in

  let iface_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (None, Type.Neutral, Annot t) iface_sig
  in

  let iface_sig = add_interface_properties cx tparams_map properties iface_sig in

  iface_sig, self

let mk_declare_class_sig =
  let open Class_sig in

  let extract_mixins _cx =
    List.map (fun (loc, {Ast.Type.Generic.id; targs}) ->
      let name = qualified_name id in
      let r = mk_reason (RType name) loc in
      r, id, targs
    )
  in

  let mk_mixins cx tparams_map (r, id, targs) =
    let i =
      let lookup_mode = Env.LookupMode.ForValue in
      convert_qualification ~lookup_mode cx "mixins" id
    in
    let props_bag = Tvar.mk_derivable_where cx r (fun tvar ->
      Flow.flow cx (i, Type.MixinT (r, tvar))
    ) in
    mk_super cx tparams_map props_bag targs
  in

  let is_object_builtin_libdef (loc, name) =
    name = "Object" &&
    match Loc.source loc with
    | None -> false
    | Some source -> File_key.is_lib_file source
  in

  fun cx reason decl ->
    let { Ast.Statement.DeclareClass.
      id = (id_loc, id_name) as ident;
      tparams;
      body = (_, { Ast.Type.Object.properties; _ });
      extends;
      mixins;
      implements;
    } = decl in

    let self = Tvar.mk cx reason in

    let tparams, tparams_map =
      mk_type_param_declarations cx tparams in

    let id_info = id_name, self, Type_table.Other in
    Env.add_type_table_info cx ~tparams_map id_loc id_info;

    let tparams, tparams_map = Class_sig.add_this self cx reason tparams tparams_map in

    let iface_sig =
      let id = Context.make_nominal cx in
      let extends =
        match extends with
        | Some (_, {Ast.Type.Generic.id; targs}) ->
          let lookup_mode = Env.LookupMode.ForValue in
          let i = convert_qualification ~lookup_mode cx "mixins" id in
          Some (mk_super cx tparams_map i targs)
        | None ->
          None
      in
      let mixins =
        mixins
        |> extract_mixins cx
        |> List.map (mk_mixins cx tparams_map)
      in
      let implements = List.map (fun (_, i) ->
        let { Ast.Class.Implements.id = (loc, name); targs } = i in
        let reason = mk_reason (RCustom "implements") loc in
        let c = Env.get_var ~lookup_mode:Env.LookupMode.ForType cx name loc in
        let targs = extract_type_param_instantiations targs in
        mk_nominal_type cx reason tparams_map (c, targs)
      ) implements in
      let super =
        let extends = match extends with
        | None -> Implicit { null = is_object_builtin_libdef ident }
        | Some t -> Explicit t
        in
        Class { extends; mixins; implements }
      in
      empty id reason tparams tparams_map super
    in

    let iface_sig =
      let reason = replace_reason (fun desc -> RNameProperty desc) reason in
      let t = Type.StrT.why reason in
      add_field ~static:true "name" (None, Type.Neutral, Annot t) iface_sig
    in

    let iface_sig = add_interface_properties cx tparams_map properties iface_sig in

    (* Add a default ctor if we don't have a ctor and won't inherit one from a super *)
    let iface_sig =
      if mem_constructor iface_sig || extends <> None || mixins <> [] then
        iface_sig
      else
        let reason = replace_reason_const RDefaultConstructor reason in
        Class_sig.add_default_constructor reason iface_sig
    in
    iface_sig, self
