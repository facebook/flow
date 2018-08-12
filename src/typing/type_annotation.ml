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

let snd_fst ((_, x), _) = x

let error_type cx loc msg =
  Flow.add_output cx msg;
  (loc, AnyT.at loc), Typed_ast.Type.error

let is_suppress_type cx type_name =
  SSet.mem type_name (Context.suppress_types cx)

let check_type_arg_arity cx loc params n f =
  match params with
  | None ->
    if n = 0 then
      f ()
    else
      error_type cx loc (FlowError.ETypeParamArity (loc, n))
  | Some (_, l) ->
    if n = List.length l && n <> 0 then
      f ()
    else
      error_type cx loc (FlowError.ETypeParamArity (loc, n))

let mk_custom_fun cx loc targs id kind =
  check_type_arg_arity cx loc targs 0 (fun () ->
    let reason = mk_reason RFunctionType loc in
    (loc, CustomFunT (reason, kind)),
    Ast.Type.(Generic {
      Generic.id = Generic.Identifier.Unqualified id;
      targs = None
    })
  )

let mk_react_prop_type cx loc targs id kind =
  mk_custom_fun cx loc targs id
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

| loc, (Any as t_ast) ->
  add_unclear_type_error_if_not_lib_file cx loc;
  (loc, AnyT.at loc), t_ast

| loc, (Mixed as t_ast) -> (loc, MixedT.at loc), t_ast

| loc, (Empty as t_ast) -> (loc, EmptyT.at loc), t_ast

| loc, (Void as t_ast) -> (loc, VoidT.at loc), t_ast

| loc, (Null as t_ast) -> (loc, NullT.at loc), t_ast

| loc, (Number as t_ast) -> (loc, NumT.at loc), t_ast

| loc, (String as t_ast) -> (loc, StrT.at loc), t_ast

| loc, (Boolean as t_ast) -> (loc, BoolT.at loc), t_ast

| loc, Nullable t ->
    let (_, t), _ as t_ast = convert cx tparams_map t in
    let reason = annot_reason (mk_reason (RMaybe (desc_of_t t)) loc) in
    (loc, DefT (reason, MaybeT t)), Nullable t_ast

| loc, Union (t0, t1, ts) ->
  let (_, t0), _ as t0_ast = convert cx tparams_map t0 in
  let (_, t1), _ as t1_ast = convert cx tparams_map t1 in
  let ts_ast = List.map (convert cx tparams_map) ts in
  let ts = List.map snd_fst ts_ast in
  let rep = UnionRep.make t0 t1 (ts) in
  (loc, DefT (mk_reason RUnionType loc, UnionT rep)),
  Union (t0_ast, t1_ast, ts_ast)

| loc, Intersection (t0, t1, ts) ->
  let (_, t0), _ as t0_ast = convert cx tparams_map t0 in
  let (_, t1), _ as t1_ast = convert cx tparams_map t1 in
  let ts_ast = List.map (convert cx tparams_map) ts in
  let ts = List.map snd_fst ts_ast in
  let rep = InterRep.make t0 t1 ts in
  (loc, DefT (mk_reason RIntersectionType loc, IntersectionT rep)),
  Intersection (t0_ast, t1_ast, ts_ast)

| loc, Typeof x ->
  begin match x with
  | q_loc, Generic {
      Generic.id = qualification;
      targs = None
    } ->
      let valtype, qualification_ast = convert_qualification
        ~lookup_mode:ForTypeof cx "typeof-annotation" qualification in
      let reason = mk_reason (RTypeof (qualified_name qualification)) loc in
      let valtype = mod_reason_of_t (fun _ -> reason) valtype in
      (loc, Flow.mk_typeof_annotation cx reason valtype),
      Typeof ((q_loc, valtype), Generic { Generic.id = qualification_ast; targs = None })
  | loc, _ ->
    error_type cx loc (FlowError.EUnexpectedTypeof loc)
  end

| loc, Tuple ts ->
  let ts_ast = List.map (convert cx tparams_map) ts in
  let tuple_types = List.map snd_fst ts_ast in
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
  (loc, DefT (reason, ArrT (TupleAT (elemt, tuple_types)))), Tuple ts_ast

| loc, Array t ->
  let r = mk_reason RArrayType loc in
  let (_, elemt), _ as t_ast = convert cx tparams_map t in
  (loc, DefT (r, ArrT (ArrayAT (elemt, None)))), Array t_ast

| loc, (StringLiteral { Ast.StringLiteral.value; _ } as t_ast) ->
  (loc, mk_singleton_string loc value), t_ast

| loc, (NumberLiteral { Ast.NumberLiteral.value; raw } as t_ast) ->
  (loc, mk_singleton_number loc value raw), t_ast

| loc, (BooleanLiteral value as t_ast) ->
  (loc, mk_singleton_boolean loc value), t_ast

(* TODO *)
| loc, Generic { Generic.id = (Generic.Identifier.Qualified (qid_loc,
       { Generic.Identifier.qualification; id; }) as qid); targs } ->
  let m, qualification_ast =
    convert_qualification cx "type-annotation" qualification in
  let id_loc, name = id in
  let reason = mk_reason (RType name) loc in
  let id_reason = mk_reason (RType name) id_loc in
  let t = Tvar.mk_where cx reason (fun t ->
    let id_info = name, t, Type_table.Other in
    Type_table.set_info id_loc id_info (Context.type_table cx);
    let use_op = Op (GetProperty (mk_reason (RType (qualified_name qid)) qid_loc)) in
    Flow.flow cx (m, GetPropT (use_op, id_reason, Named (id_reason, name), t));
  ) in
  let t, targs = mk_nominal_type cx reason tparams_map (t, targs) in
  (loc, t),
  Generic {
    Generic.id = Generic.Identifier.Qualified (qid_loc, {
      Generic.Identifier.qualification = qualification_ast;
      id;
    });
    targs
  }

(* type applications: name < params > *)
| loc, Generic {
    Generic.id = Generic.Identifier.Unqualified (name_loc, name as ident) as id;
    targs
  } ->

  let convert_type_params () =
    let targs =
      Option.map ~f:(fun (loc, targs) -> loc, List.map (convert cx tparams_map) targs) targs in
    let elemts =
      Option.value_map targs ~default:[] ~f:(Fn.compose (List.map snd_fst) snd) in
    elemts, targs
  in

  let reconstruct_ast targs = Generic { Generic.id; targs; } in

  let use_op reason =
    Op (TypeApplication { type' = reason }) in

  begin match name with

  (* Array<T> *)
  | "Array" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let elemts, targs = convert_type_params () in
      let elemt = List.hd elemts in
      (loc, DefT (mk_reason RArrayType loc, ArrT (ArrayAT (elemt, None)))),
      reconstruct_ast targs
    )

  (* $ReadOnlyArray<T> is the supertype of all tuples and all arrays *)
  | "$ReadOnlyArray" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let elemts, targs = convert_type_params () in
      let elemt = List.hd elemts in
      (loc, DefT (annot_reason (mk_reason RROArrayType loc), ArrT (ROArrayAT (elemt)))),
      reconstruct_ast targs
    )

  (* $Supertype<T> acts as any over supertypes of T *)
  | "$Supertype" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      (loc, AnyWithLowerBoundT t),
      reconstruct_ast targs
    )

  (* $Subtype<T> acts as any over subtypes of T *)
  | "$Subtype" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      (loc, AnyWithUpperBoundT t),
      reconstruct_ast targs
    )

  (* $PropertyType<T, 'x'> acts as the type of 'x' in object type T *)
  | "$PropertyType" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      match convert_type_params () with
      | ([t; DefT (_, SingletonStrT key)], targs) ->
        let reason = mk_reason (RType "$PropertyType") loc in
        (
          loc,
          EvalT (t, TypeDestructorT
            (use_op reason, reason, PropertyType key), mk_id())
        ),
        reconstruct_ast targs
      | _ ->
        error_type cx loc (FlowError.EPropertyTypeAnnot loc)
    )

  (* $ElementType<T, string> acts as the type of the string elements in object
     type T *)
  | "$ElementType" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      match convert_type_params () with
      | ([t; e], targs) ->
        let reason = mk_reason (RType "$ElementType") loc in
        (
          loc,
          EvalT (t, TypeDestructorT
            (use_op reason, reason, ElementType e), mk_id())
        ),
        reconstruct_ast targs
      | _ -> assert false
    )

  (* $NonMaybeType<T> acts as the type T without null and void *)
  | "$NonMaybeType" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "$NonMaybeType") loc in
      (
        loc,
        EvalT (t, TypeDestructorT
          (use_op reason, reason, NonMaybeType), mk_id())
      ),
      reconstruct_ast targs
    )

  (* $Shape<T> matches the shape of T *)
  | "$Shape" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      (loc, ShapeT t),
      reconstruct_ast targs
    )

  (* $Diff<T, S> *)
  | "$Diff" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2, targs = match convert_type_params () with
      | [t1; t2], targs -> t1, t2, targs
      | _ -> assert false in
      let reason = mk_reason (RType "$Diff") loc in
      (
        loc,
        EvalT (t1, TypeDestructorT (use_op reason, reason,
          RestType (Type.Object.Rest.IgnoreExactAndOwn, t2)), mk_id ())
      ),
      reconstruct_ast targs
    )

  (* $ReadOnly<T> *)
  | "$ReadOnly" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "$ReadOnly") loc in
      (
        loc,
        EvalT (
          t,
          TypeDestructorT (
            use_op reason,
            reason,
            ReadOnlyType
          ),
          mk_id ()
        )
      ),
      reconstruct_ast targs
    )

  (* $Keys<T> is the set of keys of T *)
  (** TODO: remove $Enum **)
  | "$Keys" | "$Enum" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      (loc, KeysT (mk_reason RKeySet loc, t)),
      reconstruct_ast targs
    )

  (* $Values<T> is a union of all the own enumerable value types of T *)
  | "$Values" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "$Values") loc in
      (
        loc,
        EvalT (t, TypeDestructorT
          (use_op reason, reason, ValuesType), mk_id())
      ),
      reconstruct_ast targs
    )

  | "$Exact" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let desc = RExactType (desc_of_t t) in
      (loc, ExactT (mk_reason desc loc, t)),
      reconstruct_ast targs
    )

  | "$Rest" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2, targs = match convert_type_params () with
      | [t1; t2], targs -> t1, t2, targs
      | _ -> assert false in
      let reason = mk_reason (RType "$Rest") loc in
      (
        loc,
        EvalT (t1, TypeDestructorT (use_op reason, reason,
          RestType (Type.Object.Rest.Sound, t2)), mk_id ())
      ),
      reconstruct_ast targs
    )

  (* $Exports<'M'> is the type of the exports of module 'M' *)
  (** TODO: use `import typeof` instead when that lands **)
  | "$Exports" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      match targs with
      | Some (targs_loc, (str_loc, StringLiteral { Ast.StringLiteral.value; raw })::_) ->
          let desc = RModule value in
          let reason = mk_reason desc loc in
          let remote_module_t =
            Env.get_var_declared_type cx (internal_module_name value) loc
          in
          let str_t = mk_singleton_string str_loc value in
          (
            loc,
            Tvar.mk_where cx reason (fun t ->
              Flow.flow cx (remote_module_t, CJSRequireT(reason, t, Context.is_strict cx))
            )
          ),
          reconstruct_ast (Some (
            targs_loc,
            [ (str_loc, str_t),  StringLiteral { Ast.StringLiteral.value; raw } ]
          ))
      | _ ->
          error_type cx loc (FlowError.EExportsAnnot loc)
    )

  | "$Call" ->
    (match convert_type_params () with
    | fn::args, targs ->
      let reason = mk_reason RFunctionCallType loc in
      (
        loc,
        EvalT (fn, TypeDestructorT (use_op reason, reason, CallType args), mk_id ())
      ),
      reconstruct_ast targs
    | _ ->
      error_type cx loc (FlowError.ETypeParamMinArity (loc, 1)))

  | "$TupleMap" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2, targs = match convert_type_params () with
      | [t1; t2], targs -> t1, t2, targs
      | _ -> assert false in
      let reason = mk_reason RTupleMap loc in
      (
        loc,
        EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (TupleMap t2)), mk_id ())
      ),
      reconstruct_ast targs
    )

  | "$ObjMap" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2, targs = match convert_type_params () with
      | [t1; t2], targs -> t1, t2, targs
      | _ -> assert false in
      let reason = mk_reason RObjectMap loc in
      (
        loc,
        EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (ObjectMap t2)), mk_id ())
      ),
      reconstruct_ast targs
    )

  | "$ObjMapi" ->
    check_type_arg_arity cx loc targs 2 (fun () ->
      let t1, t2, targs = match convert_type_params () with
      | [t1; t2], targs -> t1, t2, targs
      | _ -> assert false in
      let reason = mk_reason RObjectMapi loc in
      (
        loc,
        EvalT (t1, TypeDestructorT (use_op reason, reason, TypeMap (ObjectMapi t2)), mk_id ())
      ),
      reconstruct_ast targs
    )

  | "$CharSet" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      match targs with
    | Some (targs_loc, [ str_loc, StringLiteral { Ast.StringLiteral.value; raw } ]) ->
        let str_t = mk_singleton_string str_loc value in
        let chars = String_utils.CharSet.of_string value in
        let char_str = String_utils.CharSet.to_string chars in (* sorts them *)
        let reason = mk_reason (RCustom (spf "character set `%s`" char_str)) loc in
        (loc, DefT (reason, CharSetT chars)),
        reconstruct_ast (Some (
          targs_loc,
          [ (str_loc, str_t), StringLiteral { Ast.StringLiteral.value; raw } ]
        ))
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
        (loc, Flow.reposition cx loc (SMap.find_unsafe "this" tparams_map)),
        reconstruct_ast None
      )
    else (
      Flow.add_output cx (FlowError.EUnexpectedThisType loc);
      (loc, Locationless.AnyT.t), Any (* why locationless? *)
    )

  (* Class<T> is the type of the class whose instances are of type T *)
  | "Class" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RStatics (desc_of_t t)) loc in
      (loc, DefT (reason, ClassT t)),
      reconstruct_ast targs
    )

  | "Function" | "function" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      (loc, DefT (reason, AnyFunT)),
      reconstruct_ast None
    )

  | "Object" ->
    add_unclear_type_error_if_not_lib_file cx loc;
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RObjectType loc in
      (loc, DefT (reason, AnyObjT)),
      reconstruct_ast None
    )

  | "Function$Prototype$Apply" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      (loc, FunProtoApplyT reason),
      reconstruct_ast None
    )

  | "Function$Prototype$Bind" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      (loc, FunProtoBindT reason),
      reconstruct_ast None
    )

  | "Function$Prototype$Call" ->
    check_type_arg_arity cx loc targs 0 (fun () ->
      let reason = mk_reason RFunctionType loc in
      (loc, FunProtoCallT reason),
      reconstruct_ast None
    )

  | "Object$Assign" ->
      mk_custom_fun cx loc targs ident ObjectAssign
  | "Object$GetPrototypeOf" ->
      mk_custom_fun cx loc targs ident ObjectGetPrototypeOf
  | "Object$SetPrototypeOf" ->
      mk_custom_fun cx loc targs ident ObjectSetPrototypeOf

  | "$Compose" ->
      mk_custom_fun cx loc targs ident (Compose false)
  | "$ComposeReverse" ->
      mk_custom_fun cx loc targs ident (Compose true)

  | "React$PropType$Primitive" ->
      check_type_arg_arity cx loc targs 1 (fun () ->
        let ts, targs = convert_type_params () in
        let t = List.hd ts in
        let prop_type = (ReactPropType (React.PropType.Primitive (false, t))) in
        fst (mk_custom_fun cx loc None ident prop_type), reconstruct_ast targs
      )
  | "React$PropType$ArrayOf" ->
      mk_react_prop_type cx loc targs ident React.PropType.ArrayOf
  | "React$PropType$InstanceOf" ->
      mk_react_prop_type cx loc targs ident React.PropType.InstanceOf
  | "React$PropType$ObjectOf" ->
      mk_react_prop_type cx loc targs ident React.PropType.ObjectOf
  | "React$PropType$OneOf" ->
      mk_react_prop_type cx loc targs ident React.PropType.OneOf
  | "React$PropType$OneOfType" ->
      mk_react_prop_type cx loc targs ident React.PropType.OneOfType
  | "React$PropType$Shape" ->
      mk_react_prop_type cx loc targs ident React.PropType.Shape
  | "React$CreateClass" ->
      mk_custom_fun cx loc targs ident ReactCreateClass
  | "React$CreateElement" ->
      mk_custom_fun cx loc targs ident ReactCreateElement
  | "React$CloneElement" ->
      mk_custom_fun cx loc targs ident ReactCloneElement
  | "React$ElementFactory" ->
      check_type_arg_arity cx loc targs 1 (fun () ->
        let t = match convert_type_params () with
          | [t], _ -> t
          | _ -> assert false in
        mk_custom_fun cx loc None ident (ReactElementFactory t)
      )
  | "React$ElementProps" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "React$ElementProps") loc in
      (
        loc,
        EvalT (t, TypeDestructorT
        (use_op reason, reason,
          ReactElementPropsType), mk_id ())
      ),
      reconstruct_ast targs
    )
  | "React$ElementConfig" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "React$ElementConfig") loc in
      (
        loc,
        EvalT (t, TypeDestructorT (
          use_op reason, reason, ReactElementConfigType), mk_id ()
        )
      ),
      reconstruct_ast targs
    )
  | "React$ElementRef" ->
    check_type_arg_arity cx loc targs 1 (fun () ->
      let ts, targs = convert_type_params () in
      let t = List.hd ts in
      let reason = mk_reason (RType "React$ElementRef") loc in
      (
        loc,
        EvalT (t, TypeDestructorT (
          use_op reason, reason, ReactElementRefType), mk_id ()
        )
      ),
      reconstruct_ast targs
    )

  | "$Facebookism$Idx" ->
      mk_custom_fun cx loc targs ident Idx
  | "$Facebookism$TypeAssertIs" ->
      mk_custom_fun cx loc targs ident TypeAssertIs
  | "$Facebookism$TypeAssertThrows" ->
      mk_custom_fun cx loc targs ident TypeAssertThrows
  | "$Facebookism$TypeAssertWraps" ->
      mk_custom_fun cx loc targs ident TypeAssertWraps

  | "$Flow$DebugPrint" ->
      mk_custom_fun cx loc targs ident DebugPrint
  | "$Flow$DebugThrow" ->
      mk_custom_fun cx loc targs ident DebugThrow
  | "$Flow$DebugSleep" ->
      mk_custom_fun cx loc targs ident DebugSleep

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
    let _, targs = convert_type_params () in
    (loc, AnyT.at loc),
    reconstruct_ast targs

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
      Type_table.set_info name_loc id_info (Context.type_table cx);
      (loc, t),
      reconstruct_ast None
    )

  | "$Pred" ->
    let fun_reason = mk_reason (RCustom "abstract predicate function") loc in
    let static_reason = mk_reason (RCustom "abstract predicate static") loc in
    let out_reason = mk_reason (RCustom "open predicate") loc in

    check_type_arg_arity cx loc targs 1 (fun () ->
      match convert_type_params () with
      | [DefT (_, SingletonNumT (f, _))], targs ->
        let n = Pervasives.int_of_float f in
        let key_strs =
          ListUtils.range 0 n |>
          List.map (fun i -> Some ("x_" ^ Pervasives.string_of_int i)) in
        let emp = Key_map.empty in
        let tins = ListUtils.repeat n (AnyT.at loc) in
        let tout = OpenPredT (out_reason, MixedT.at loc, emp, emp) in
        (loc, DefT (fun_reason, FunT (
          dummy_static static_reason,
          DefT (mk_reason RPrototype loc, AnyT),
          mk_functiontype fun_reason tins tout
            ~rest_param:None ~def_reason:fun_reason
            ~params_names:key_strs ~is_predicate:true
        ))),
        reconstruct_ast targs

      | _ ->
        error_type cx loc (FlowError.EPredAnnot loc)
    )

  | "$Refine" ->
    check_type_arg_arity cx loc targs 3 (fun () ->
      match convert_type_params () with
      | [base_t; fun_pred_t; DefT (_, SingletonNumT (f, _))], targs ->
          let idx = Pervasives.int_of_float f in
          let reason = mk_reason (RCustom "refined type") loc in
          let pred = LatentP (fun_pred_t, idx) in
          (loc, EvalT (base_t, DestructuringT (reason, Refine pred), mk_id())),
          reconstruct_ast targs
      | _ ->
        error_type cx loc (FlowError.ERefineAnnot loc)
    )

  (* other applications with id as head expr *)
  | _ ->
    let reason = mk_reason (RType name) loc in
    let c = type_identifier cx name loc in
    let id_info = name, c, Type_table.Other in
    Type_table.set_info name_loc id_info (Context.type_table cx);
    let t, targs = mk_nominal_type cx reason tparams_map (c, targs) in
    (loc, t), reconstruct_ast targs

  end

| loc, Function { Function.
    params = (params_loc, { Function.Params.params; rest });
    return;
    tparams;
  } ->
  let tparams, tparams_map, tparams_ast =
    mk_type_param_declarations cx ~tparams_map tparams in

  let rev_params, rev_param_asts = List.fold_left (fun (params_acc, asts_acc) (param_loc, param) ->
    let { Function.Param.name; annot; optional } = param in
    let (_, t), _ as annot_ast = convert cx tparams_map annot in
    let t = if optional then Type.optional t else t in
    Option.iter ~f:(fun (loc, name) ->
      let id_info = name, t, Type_table.Other in
      Type_table.set_info ~extra_tparams:tparams loc id_info (Context.type_table cx);
    ) name;
    (Option.map ~f:ident_name name, t) :: params_acc,
    (param_loc, {
      Function.Param.name;
      annot = annot_ast;
      optional
    }) :: asts_acc
  ) ([], []) params in

  let reason = mk_reason RFunctionType loc in

  let rest_param, rest_param_ast = match rest with
  | Some (rest_loc, { Function.RestParam.argument = (param_loc, param) }) ->
    let { Function.Param.name; annot; optional } = param in
    let (_, rest), _ as annot_ast = convert cx tparams_map annot in
    (* TODO - Use AssertRestParamT here. The big problem is that, at this
     * point, there might be some unsubstituted type parameters in the rest
     * type. Unlike expressions, which know all type parameters have been
     * substituted thanks to generate_tests, we visit types outside of
     * generate_tests.
     *
     * One solution might be to build a type visitor that runs during
     * generate_tests and does the various subst and tests then
     *)
    Some (Option.map ~f:ident_name name, loc_of_t rest, rest),
    Some (rest_loc, {
      Function.RestParam.argument = (param_loc, {
        Function.Param.name;
        annot = annot_ast;
        optional
      });
    })
  | None -> None, None in

  let (_, return_t), _ as return_ast = convert cx tparams_map return in
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
  (loc, poly_type id tparams ft),
  Function {
    Function.params = (params_loc, {
      Function.Params.params = List.rev rev_param_asts;
      rest = rest_param_ast;
    });
    return = return_ast;
    tparams = tparams_ast;
  }

| loc, Object { Object.exact; properties } ->
  let reason_desc = RObjectType in
  let callable = List.exists (function
    | Object.CallProperty (_, { Object.CallProperty.static; _ }) -> not static
    | _ -> false
  ) properties in
  let mk_object ~exact (call_props, dict, props_map, proto, call_deprecated) =
    let call = match List.rev call_props with
      | [] ->
        (* Note that call properties using the call property syntax always override
           $call properties. Previously, if both were present, the $call property
           was ignored, but is now left as a named property. *)
        call_deprecated
      | [t] -> Some t
      | t0::t1::ts ->
        let callable_reason = mk_reason (RCustom "callable object type") loc in
        let rep = InterRep.make t0 t1 ts in
        let t = DefT (callable_reason, IntersectionT rep) in
        Some t
    in
    (* Previously, call properties were stored in the props map under the key
       $call. Unfortunately, this made it possible to specify call properties
       using this syntax in object types, and some libraries adopted this
       syntax.

       Note that call properties using the call property syntax always override
       $call properties. Previously, if both were present, the $call property
       was ignored, but is now left as a named property. *)
    let props_map, call =
      if call <> None then props_map, call
      else match SMap.get "$call" props_map with
      | Some (Field (_, t, (Positive | Neutral))) ->
        SMap.remove "$call" props_map, Some t
      | _ -> props_map, call
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
    let call = Option.map call ~f:(Context.make_call_prop cx) in
    let pmap = Context.make_property_map cx props_map in
    let flags = {
      sealed = Sealed;
      exact;
      frozen = false
    } in
    DefT (mk_reason reason_desc loc,
      ObjT (mk_objecttype ~flags ~dict ~call pmap proto))
  in
  let property loc prop props proto call_deprecated =
    match prop with
    | { Object.Property.
        key; value = Object.Property.Init value; optional; variance; _method; _
      } ->
      begin match key with
      (* Previously, call properties were stored in the props map under the key
         $call. Unfortunately, this made it possible to specify call properties
         using this syntax in object types, and some libraries adopted this
         syntax.

         Note that call properties using the call property syntax always override
         $call properties. Previously, if both were present, the $call property
         was ignored, but is now left as a named property. *)
      | Ast.Expression.Object.Property.Identifier (loc, "$call") as key ->
          Flow.add_output cx Flow_error.(EDeprecatedCallSyntax loc);
          let (_, t), _ as value_ast = convert cx tparams_map value in
          let t = if optional then Type.optional t else t in
          props, proto, Some t,
          { prop with Object.Property.key; value = Object.Property.Init value_ast }
      | Ast.Expression.Object.Property.Literal
          (loc, { Ast.Literal.value = Ast.Literal.String name; _ })
      | Ast.Expression.Object.Property.Identifier (loc, name) as key ->
          Type_inference_hooks_js.dispatch_obj_prop_decl_hook cx name loc;
          let (_, t), _ as value_ast = convert cx tparams_map value in
          let prop_ast =
            { prop with Object.Property.key; value = Object.Property.Init value_ast; } in
          if name = "__proto__" && not (_method || optional) && variance = None
          then
            let reason = mk_reason RPrototype (fst value) in
            let proto = Tvar.mk_where cx reason (fun tout ->
              Flow.flow cx (t, ObjTestProtoT (reason, tout))
            ) in
            let proto = Some (Flow.mk_typeof_annotation cx reason proto) in
            props, proto, call_deprecated, prop_ast
          else
            let t = if optional then Type.optional t else t in
            let id_info = name, t, Type_table.Other in
            Type_table.set_info loc id_info (Context.type_table cx);
            let polarity = if _method then Positive else polarity variance in
            let props = SMap.add name (Field (Some loc, t, polarity)) props in
            props, proto, call_deprecated, prop_ast
      | Ast.Expression.Object.Property.Literal (loc, _)
      | Ast.Expression.Object.Property.PrivateName (loc, _)
      | Ast.Expression.Object.Property.Computed (loc, _)
          ->
        Flow.add_output cx (FlowError.EUnsupportedKeyInObjectType loc);
        props, proto, call_deprecated, Typed_ast.Type.Object.Property.error
      end

    (* unsafe getter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (id_loc, name) as key;
        value = Object.Property.Get (loc, f);
        _method; _ } ->
      Flow_js.add_output cx (FlowError.EUnsafeGettersSetters loc);
      let function_type, f_ast =
        match convert cx tparams_map (loc, Ast.Type.Function f) with
        | (_, function_type), Ast.Type.Function f_ast -> function_type, f_ast
        | _ -> assert false
      in
      let return_t = Type.extract_getter_type function_type in
      let id_info = name, return_t, Type_table.Other in
      Type_table.set_info id_loc id_info (Context.type_table cx);
      let props = Properties.add_getter name (Some id_loc) return_t props in
      props, proto, call_deprecated,
      { prop with Object.Property.key; value = Object.Property.Get (loc, f_ast); }
    (* unsafe setter property *)
    | { Object.Property.
        key = Ast.Expression.Object.Property.Identifier (id_loc, name) as key;
        value = Object.Property.Set (loc, f);
        _method; _ } ->
      Flow_js.add_output cx (FlowError.EUnsafeGettersSetters loc);
      let function_type, f_ast =
        match convert cx tparams_map (loc, Ast.Type.Function f) with
        | (_, function_type), Ast.Type.Function f_ast -> function_type, f_ast
        | _ -> assert false
      in
      let param_t = Type.extract_setter_type function_type in
      let id_info = name, param_t, Type_table.Other in
      Type_table.set_info id_loc id_info (Context.type_table cx);
      let props = Properties.add_setter name (Some id_loc) param_t props in
      props, proto, call_deprecated,
      { prop with Object.Property.key; value = Object.Property.Set (loc, f_ast); }
    | { Object.Property.
        value = Object.Property.Get _ | Object.Property.Set _; _ } ->
      Flow.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyGetSet));
      props, proto, call_deprecated, Typed_ast.Type.Object.Property.error
  in
  let add_call c = function
    | None -> Some ([c], None, SMap.empty, None, None)
    | Some (cs, d, pmap, proto, _) ->
      (* Note that call properties using the call property syntax always override
         $call properties. Previously, if both were present, the $call property
         was ignored, but is now left as a named property. *)
      Some (c::cs, d, pmap, proto, None)
  in
  let make_dict ({ Object.Indexer.id; key; value; variance; _ } as indexer) =
    let (_, key), _ as key_ast = convert cx tparams_map key in
    let (_, value), _ as value_ast = convert cx tparams_map value in
    Some { Type.
      dict_name = Option.map ~f:snd id;
      key;
      value;
      dict_polarity = polarity variance;
    },
    { indexer with Object.Indexer.key = key_ast; value = value_ast; }
  in
  let add_dict loc indexer = function
    | None ->
      let dict, indexer_ast = make_dict indexer in
      Some ([], dict, SMap.empty, None, None), indexer_ast
    | Some (cs, None, pmap, proto, call_deprecated) ->
      let dict, indexer_ast = make_dict indexer in
      Some (cs, dict, pmap, proto, call_deprecated), indexer_ast
    | Some (_, Some _, _, _, _) as o ->
      Flow.add_output cx
        FlowError.(EUnsupportedSyntax (loc, MultipleIndexers));
      o, Typed_ast.Type.Object.Indexer.error
  in
  let add_prop loc p = function
    | None ->
      let pmap, proto, call_deprecated, p_ast = property loc p SMap.empty None None in
      Some ([], None, pmap, proto, call_deprecated), p_ast
    | Some (cs, d, pmap, proto, call_deprecated) ->
      let pmap, proto, call_deprecated, p_ast = property loc p pmap proto call_deprecated in
      Some (cs, d, pmap, proto, call_deprecated), p_ast
  in
  let o, ts, spread, rev_prop_asts = List.fold_left Object.(
    fun (o, ts, spread, rev_prop_asts) -> function
    | CallProperty (loc, { CallProperty.value = (value_loc, ft); static }) ->
      let t, ft_ast = match convert cx tparams_map (loc, Ast.Type.Function ft) with
        | (_, t), Ast.Type.Function ft_ast -> t, ft_ast
        | _ -> assert false
      in
      let prop_ast = CallProperty (loc, { CallProperty.value = value_loc, ft_ast; static }) in
      add_call t o, ts, spread, prop_ast::rev_prop_asts
    | Indexer (loc, i) ->
      let o, i_ast = add_dict loc i o in
      o, ts, spread, Indexer (loc, i_ast)::rev_prop_asts
    | Property (loc, p) ->
      let o, p_ast = add_prop loc p o in
      o, ts, spread, Property (loc, p_ast)::rev_prop_asts
    | InternalSlot (loc, slot) ->
      let { Object.InternalSlot.
        id = (_, name);
        value;
        static=_; (* object props are never static *)
        optional;
        _method=_;
      } = slot in
      if name = "call" then
        let (_, t), _ as value_ast = convert cx tparams_map value in
        let t = if optional then Type.optional t else t in
        add_call t o, ts, spread,
        InternalSlot (loc, { slot with Object.InternalSlot.value = value_ast })::rev_prop_asts
      else (
        Flow.add_output cx FlowError.(
          EUnsupportedSyntax (loc, UnsupportedInternalSlot {
            name;
            static = false;
          }));
        o, ts, spread, InternalSlot (loc, Typed_ast.Type.Object.InternalSlot.error)::rev_prop_asts
      )
    | SpreadProperty (loc, { Object.SpreadProperty.argument }) ->
      let ts = match o with
      | None -> ts
      | Some o -> (mk_object ~exact:true o)::ts
      in
      let (_, o), _ as argument_ast = convert cx tparams_map argument in
      None, o::ts, true,
      SpreadProperty (loc, { SpreadProperty.argument = argument_ast })::rev_prop_asts
  ) (None, [], false, []) properties in
  let ts = match o with
  | None -> ts
  | Some o -> mk_object ~exact:spread o::ts
  in (
  loc,
  match ts with
  | [] ->
    let t = mk_object ~exact ([], None, SMap.empty, None, None) in
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
    EvalT (t, TypeDestructorT (unknown_use, reason, SpreadType (target, ts)), mk_id ())
  ), Object { Object.exact; properties = List.rev rev_prop_asts }

| loc, Interface {Interface.extends; body} ->
  let body_loc, {Ast.Type.Object.properties; exact} = body in
  let reason = mk_reason RInterfaceType loc in
  let iface_sig, extend_asts =
    let id = Context.make_nominal cx in
    let extends, extend_asts = extends
      |> List.map (mk_interface_super cx tparams_map)
      |> List.split
    in
    let super =
      let callable = List.exists Ast.Type.Object.(function
        | CallProperty (_, { CallProperty.static; _ }) -> not static
        | _ -> false
      ) properties in
      Class_sig.Interface { extends; callable }
    in
    Class_sig.empty id reason [] tparams_map super, extend_asts
  in
  let iface_sig, property_asts =
    add_interface_properties cx tparams_map properties iface_sig in
  Class_sig.generate_tests cx (fun iface_sig ->
    Class_sig.check_super cx reason iface_sig;
    Class_sig.check_implements cx reason iface_sig
  ) iface_sig |> ignore;
  (loc, Class_sig.thistype cx iface_sig),
  Interface { Interface.
    body = body_loc, { Object.
      exact;
      properties = property_asts;
    };
    extends = extend_asts;
  }

| loc, Exists ->
  add_deprecated_type_error_if_not_lib_file cx loc;
  (* Do not evaluate existential type variables when map is non-empty. This
     ensures that existential type variables under a polymorphic type remain
     unevaluated until the polymorphic type is applied. *)
  let force = SMap.is_empty tparams_map in
  let reason = derivable_reason (mk_reason RExistential loc) in
  if force then begin
    let tvar = Tvar.mk cx reason in
    Type_table.set_info loc ("Star", tvar, Type_table.Exists) (Context.type_table cx);
    (loc, tvar), Exists
  end
  else (loc, ExistsT reason), Exists
)

and convert_qualification ?(lookup_mode=ForType) cx reason_prefix
  = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) as qualified ->
    let m, qualification =
      convert_qualification ~lookup_mode cx reason_prefix qualification in
    let id_loc, name = id in
    let desc = RCustom (spf "%s `%s`" reason_prefix (qualified_name qualified)) in
    let reason = mk_reason desc loc in
    let id_reason = mk_reason desc id_loc in
    Tvar.mk_where cx reason (fun t ->
      let id_info = name, t, Type_table.Other in
      Type_table.set_info id_loc id_info (Context.type_table cx);
      let use_op = Op (GetProperty (mk_reason (RType (qualified_name qualified)) loc)) in
      Flow.flow cx (m, GetPropT (use_op, id_reason, Named (id_reason, name), t));
    ),
    Qualified (loc, { qualification; id; })

  | Unqualified ((loc, name) as id) ->
    let t = Env.get_var ~lookup_mode cx name loc in
    let id_info = name, t, Type_table.Other in
    Type_table.set_info loc id_info (Context.type_table cx);
    t, Unqualified id
)

and mk_func_sig =
  let open Ast.Type.Function in
  let add_param cx tparams_map (x, rev_param_asts) (loc, param) =
    let { Param.name=id; annot; optional } = param in
    let (_, t), _ as annot = convert cx tparams_map annot in
    Func_params.add_simple cx ~optional loc id t x,
    (loc, { param with Param.annot })::rev_param_asts
  in
  let add_rest cx tparams_map (loc, param) x =
    let { Param.name=id; annot; _ } = param in
    let (_, t), _ as annot = convert cx tparams_map annot in
    let () =
      let name = Option.map id ~f:snd in
      let reason = mk_reason (RRestParameter name) (loc_of_t t) in
      Flow.flow cx (t, AssertRestParamT reason)
    in
    Func_params.add_rest cx loc id t x, (loc, { param with Param.annot })
  in
  let convert_params cx tparams_map (loc, {Params.params; rest}) =
    let params, rev_param_asts =
      List.fold_left (add_param cx tparams_map) (Func_params.empty, []) params in
    match rest with
    | Some (rest_loc, { RestParam.argument }) ->
      let params, argument = add_rest cx tparams_map argument params in
      params, (
        loc,
        { Params.
          params = List.rev rev_param_asts;
          rest = Some (rest_loc, { RestParam.argument; })
        }
      )
    | None ->
      params, (loc, { Params.params = List.rev rev_param_asts; rest = None; })
  in
  fun cx tparams_map loc func ->
    let tparams, tparams_map, tparams_ast =
      mk_type_param_declarations cx ~tparams_map func.tparams in
    Type_table.with_typeparams tparams (Context.type_table cx) @@ fun _ ->
    let fparams, params_ast = convert_params cx tparams_map func.Ast.Type.Function.params in
    let (_, return_t), _ as return_ast = convert cx tparams_map func.return in
    { Func_sig.
      reason = mk_reason RFunctionType loc;
      kind = Func_sig.Ordinary;
      tparams;
      tparams_map;
      fparams;
      body = None;
      return_t;
    }, { Ast.Type.Function.
      tparams = tparams_ast;
      params = params_ast;
      return = return_ast;
    }

and mk_type cx tparams_map reason = function
  | None ->
      let t =
        if Context.is_weak cx
        then AnyT.why reason
        else Tvar.mk cx reason
      in
      Hashtbl.replace (Context.annot_table cx) (loc_of_reason reason) t;
      t, None

  | Some annot ->
      let (_, t), _ as annot_ast = convert cx tparams_map annot in
      t, Some annot_ast

and mk_type_annotation cx tparams_map reason = function
| None ->
  fst (mk_type cx tparams_map reason None), None
| Some (loc, annot) ->
  let t, annot_ast = mk_type cx tparams_map reason (Some annot) in
  t, Option.map ~f:(fun ast -> loc, ast) annot_ast

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
      Flow.mk_instance cx reason ~for_type c, None
  | Some (loc, targs) ->
      let targs_ast = List.map (convert cx tparams_map) targs in
      let targs = List.map snd_fst targs_ast in
      typeapp c targs, Some (loc, targs_ast)

(* take a list of AST type param declarations,
   do semantic checking and create types for them. *)
and mk_type_param_declarations cx ?(tparams_map=SMap.empty) tparams =
  let open Ast.Type.ParameterDeclaration in
  let add_type_param (tparams, tparams_map, bounds_map, rev_asts) (loc, type_param) =
    let { TypeParam.name = name_loc, name as id; bound; variance; default; } = type_param in
    let reason = mk_reason (RType name) name_loc in
    let bound, bound_ast = match bound with
    | None -> DefT (reason, MixedT Mixed_everything), None
    | Some (bound_loc, u) ->
        let bound, bound_ast = mk_type cx tparams_map reason (Some u) in
        bound, Option.map ~f:(fun ast -> bound_loc, ast) bound_ast
    in
    let default, default_ast = match default with
    | None -> None, None
    | Some default ->
        let t, default_ast = mk_type cx tparams_map reason (Some default) in
        Flow.flow_t cx (Flow.subst cx bounds_map t,
                           Flow.subst cx bounds_map bound);
        Some t, default_ast in
    let polarity = polarity variance in
    let tparam = { reason; name; bound; polarity; default; } in
    let t = BoundT tparam in
    let id_info = name, t, Type_table.Other in
    let ast = (loc, {
      TypeParam.name = id;
      bound = bound_ast;
      variance;
      default = default_ast
    }) in
    let tparams = tparam :: tparams in
    Type_table.set_info ~extra_tparams:tparams name_loc id_info (Context.type_table cx);
    tparams,
    SMap.add name t tparams_map,
    SMap.add name (Flow.subst cx bounds_map bound) bounds_map,
    ast :: rev_asts
  in
  let rev_tparams, tparams_map, _, rev_asts =
    tparams
    |> Option.value_map ~f:snd ~default:[]
    |> List.fold_left add_type_param ([], tparams_map, SMap.empty, [])
  in
  let tparams_ast =
    Option.map ~f:(fun (tparams_loc, _) -> tparams_loc, List.rev rev_asts) tparams in
  List.rev rev_tparams, tparams_map, tparams_ast

and type_identifier cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else if name = "undefined"
  then VoidT.at loc
  else Env.var_ref ~lookup_mode:ForType cx name loc

and polarity = Ast.Variance.(function
  | Some (_, Plus) -> Positive
  | Some (_, Minus) -> Negative
  | None -> Neutral
)

and mk_interface_super cx tparams_map (loc, {Ast.Type.Generic.id; targs}) =
  let lookup_mode = Env.LookupMode.ForType in
  let i, id_ast = convert_qualification ~lookup_mode cx "extends" id in
  let loc = match targs with
  | Some (targs, _) -> Loc.btwn loc targs
  | None -> loc
  in
  let r = mk_reason (RType (qualified_name id)) loc in
  let t, targs_ast = mk_nominal_type cx r tparams_map (i, targs) in
  Flow.reposition cx loc ~annot_loc:loc t,
  (loc, { Ast.Type.Generic.id = id_ast; targs = targs_ast })

and add_interface_properties cx tparams_map properties s =
  let open Class_sig in
  let x, rev_prop_asts =
    List.fold_left Ast.Type.Object.(fun (x, rev_prop_asts) -> function
    | CallProperty (loc, { CallProperty.value = value_loc, ft; static }) ->
      let (_, t), ft = convert cx tparams_map (loc, Ast.Type.Function ft) in
      let ft = match ft with Ast.Type.Function ft -> ft | _ -> assert false in
      append_call ~static t x,
      CallProperty (loc, { CallProperty.
        value = value_loc, ft;
        static;
      })::rev_prop_asts
    | Indexer (loc, { Indexer.static; _ })
      when mem_field ~static "$key" x ->
      Flow.add_output cx
        Flow_error.(EUnsupportedSyntax (loc, MultipleIndexers));
      x, Indexer (loc, Typed_ast.Type.Object.Indexer.error)::rev_prop_asts
    | Indexer (loc, indexer) ->
      let { Indexer.key; value; static; variance; _ } = indexer in
      let (_, k), _ as key = convert cx tparams_map key in
      let (_, v), _ as value = convert cx tparams_map value in
      let polarity = polarity variance in
      x |> add_field ~static "$key" (None, polarity, Annot k)
        |> add_field ~static "$value" (None, polarity, Annot v),
      Indexer (loc, { indexer with Indexer.key; value; })::rev_prop_asts
    | Property (loc, ({ Property.
        key; value; static; proto; optional; _method; variance;
      } as prop)) ->
      if optional && _method
      then Flow.add_output cx Flow_error.(EInternal (loc, OptionalMethod));
      let polarity = polarity variance in
      let x, prop = Ast.Expression.Object.(
        match _method, key, value with
        | _, Property.Literal (loc, _), _
        | _, Property.PrivateName (loc, _), _
        | _, Property.Computed (loc, _), _ ->
            Flow.add_output cx (Flow_error.EUnsupportedSyntax (loc, Flow_error.IllegalName));
            x, (loc, Typed_ast.Type.Object.Property.error)

        (* Previously, call properties were stored in the props map under the key
           $call. Unfortunately, this made it possible to specify call properties
           using this syntax in interfaces, declared classes, and even normal classes.

           Note that $call properties always override the call property syntax.
           As before, if both are present, the $call property is used and the call
           property is ignored. *)
        | _, (Property.Identifier (id_loc, "$call") as key),
            Ast.Type.Object.Property.Init value when not proto ->
            Flow.add_output cx Flow_error.(EDeprecatedCallSyntax id_loc);
            let (_, t), _ as value_ast = convert cx tparams_map value in
            let t = if optional then Type.optional t else t in
            add_call_deprecated ~static t x,
            Ast.Type.(loc, { prop with Object.Property.
              key;
              value = Object.Property.Init value_ast;
            })

        | true, (Property.Identifier (id_loc, name) as key),
            Ast.Type.Object.Property.Init (func_loc, Ast.Type.Function func) ->
            let fsig, func_ast = mk_func_sig cx tparams_map loc func in
            let ft = Func_sig.methodtype cx fsig in
            let append_method = match static, name with
            | false, "constructor" -> append_constructor (Some id_loc)
            | _ -> append_method ~static name (Some id_loc)
            in
            append_method fsig x,
            Ast.Type.(loc, { prop with Object.Property.
              key;
              value = Object.Property.Init ((func_loc, ft), Function func_ast);
            })

        | true, Property.Identifier _, _ ->
            Flow.add_output cx
              Flow_error.(EInternal (loc, MethodNotAFunction));
            x, (loc, Typed_ast.Type.Object.Property.error)

        | false, (Property.Identifier (id_loc, name) as key),
            Ast.Type.Object.Property.Init value ->
            let (_, t), _ as value_ast = convert cx tparams_map value in
            let t = if optional then Type.optional t else t in
            let add = if proto then add_proto_field else add_field ~static in
            add name (Some id_loc, polarity, Annot t) x,
            Ast.Type.(loc, { prop with Object.Property.
              key;
              value = Object.Property.Init value_ast;
            })

        (* unsafe getter property *)
        | _, (Property.Identifier (id_loc, name) as key),
            Ast.Type.Object.Property.Get (get_loc, func) ->
            Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
            let fsig, func_ast = mk_func_sig cx tparams_map loc func in
            add_getter ~static name (Some id_loc) fsig x,
            Ast.Type.(loc, { prop with Object.Property.
              key;
              value = Object.Property.Get (get_loc, func_ast);
            })

        (* unsafe setter property *)
        | _, (Property.Identifier (id_loc, name) as key),
            Ast.Type.Object.Property.Set (set_loc, func) ->
            Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
            let fsig, func_ast = mk_func_sig cx tparams_map loc func in
            add_setter ~static name (Some id_loc) fsig x,
            Ast.Type.(loc, { prop with Object.Property.
              key;
              value = Object.Property.Set (set_loc, func_ast);
            })
        )
      in
      x, Ast.Type.Object.Property prop :: rev_prop_asts

    | InternalSlot (loc, slot) ->
      let { InternalSlot.
        id = _, name;
        value;
        optional;
        static;
        _method;
      } = slot in
      if name = "call" then
        let (_, t), _ as value = convert cx tparams_map value in
        let t = if optional then Type.optional t else t in
        append_call ~static t x,
        InternalSlot (loc, { slot with InternalSlot.value })::rev_prop_asts
      else (
        Flow.add_output cx Flow_error.(
          EUnsupportedSyntax (loc, UnsupportedInternalSlot {
            name;
            static;
          }));
        x, InternalSlot (loc, Typed_ast.Type.Object.InternalSlot.error)::rev_prop_asts
      )

    | SpreadProperty (loc, _) ->
      Flow.add_output cx Flow_error.(EInternal (loc, InterfaceTypeSpread));
      x,
      SpreadProperty (loc, Typed_ast.Type.Object.SpreadProperty.error)::rev_prop_asts
  ) (s, []) properties
  in
  x, List.rev rev_prop_asts

let mk_super cx tparams_map c targs = Type.(
  (* A super class must be parameterized by This, so that it can be
     specialized to this class and its subclasses when properties are looked
     up on their instances. *)
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
      this_typeapp c this None, None
  | Some (loc, targs) ->
      let targ_asts = List.map (convert cx tparams_map) targs in
      let targs = List.map snd_fst targ_asts in
      this_typeapp c this (Some targs),
      Some (loc, targ_asts)
)

let mk_interface_sig cx reason decl =
  let open Class_sig in
  let { Ast.Statement.Interface.
    id = id_loc, id_name as id;
    tparams;
    body = (body_loc, { Ast.Type.Object.properties; exact });
    extends;
    _;
  } = decl in

  let self = Tvar.mk cx reason in

  let tparams, tparams_map, tparams_ast =
    mk_type_param_declarations cx tparams in

  let id_info = id_name, self, Type_table.Other in
  Type_table.set_info id_loc id_info (Context.type_table cx);

  let iface_sig, extends_ast =
    let id = Context.make_nominal cx in
    let extends, extends_ast =
      extends
      |> List.map (mk_interface_super cx tparams_map)
      |> List.split in
    let super =
      let callable = List.exists Ast.Type.Object.(function
        | CallProperty (_, { CallProperty.static; _ }) -> not static
        | _ -> false
      ) properties in
      Interface { extends; callable }
    in
    empty id reason tparams tparams_map super, extends_ast
  in

  let iface_sig =
    let reason = replace_reason (fun desc -> RNameProperty desc) reason in
    let t = Type.StrT.why reason in
    add_field ~static:true "name" (None, Type.Neutral, Annot t) iface_sig
  in

  let iface_sig, properties = add_interface_properties cx tparams_map properties iface_sig in

  iface_sig, self,
  { Ast.Statement.Interface.
    id;
    tparams = tparams_ast;
    extends = extends_ast;
    body = body_loc, { Ast.Type.Object.exact; properties };
  }

let mk_declare_class_sig =
  let open Class_sig in

  let mk_mixins cx tparams_map (loc, {Ast.Type.Generic.id; targs}) =
    let name = qualified_name id in
    let r = mk_reason (RType name) loc in
    let i, id =
      let lookup_mode = Env.LookupMode.ForValue in
      convert_qualification ~lookup_mode cx "mixins" id
    in
    let props_bag = Tvar.mk_derivable_where cx r (fun tvar ->
      Flow.flow cx (i, Type.MixinT (r, tvar))
    ) in
    let t, targs = mk_super cx tparams_map props_bag targs in
    t, (loc, { Ast.Type.Generic.id; targs })
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
      body = body_loc, { Ast.Type.Object.properties; exact };
      extends;
      mixins;
      implements;
    } = decl in

    let self = Tvar.mk cx reason in

    let tparams, tparams_map, tparam_asts =
      mk_type_param_declarations cx tparams in

    let id_info = id_name, self, Type_table.Other in
    Type_table.set_info id_loc id_info (Context.type_table cx);

    let tparams, tparams_map = Class_sig.add_this self cx reason tparams tparams_map in

    Type_table.with_typeparams tparams (Context.type_table cx) @@ fun _ ->

    let iface_sig, extends_ast, mixins_ast, implements_ast =
      let id = Context.make_nominal cx in
      let extends, extends_ast =
        match extends with
        | Some (extends_loc, {Ast.Type.Generic.id; targs}) ->
          let lookup_mode = Env.LookupMode.ForValue in
          let i, id =
            convert_qualification ~lookup_mode cx "mixins" id in
          let t, targs = mk_super cx tparams_map i targs in
          Some t, Some (extends_loc, { Ast.Type.Generic.id; targs })
        | None ->
          None, None
      in
      let mixins, mixins_ast =
        mixins
        |> List.map (mk_mixins cx tparams_map)
        |> List.split
      in
      let implements, implements_ast =
        implements
        |> List.map (fun (i_loc, i) ->
            let { Ast.Class.Implements.id = loc, name as id; targs } = i in
            let reason = mk_reason (RCustom "implements") loc in
            let c = Env.get_var ~lookup_mode:Env.LookupMode.ForType cx name loc in
            let t, targs = mk_nominal_type cx reason tparams_map (c, targs) in
            t, (i_loc, { Ast.Class.Implements.id; targs; })
        )
        |> List.split in
      let super =
        let extends = match extends with
        | None -> Implicit { null = is_object_builtin_libdef ident }
        | Some t -> Explicit t
        in
        Class { extends; mixins; implements }
      in
      empty id reason tparams tparams_map super,
      extends_ast, mixins_ast, implements_ast
    in

    let iface_sig =
      let reason = replace_reason (fun desc -> RNameProperty desc) reason in
      let t = Type.StrT.why reason in
      add_field ~static:true "name" (None, Type.Neutral, Annot t) iface_sig
    in

    let iface_sig, properties =
      add_interface_properties cx tparams_map properties iface_sig in

    (* Add a default ctor if we don't have a ctor and won't inherit one from a super *)
    let iface_sig =
      if mem_constructor iface_sig || extends <> None || mixins <> [] then
        iface_sig
      else
        let reason = replace_reason_const RDefaultConstructor reason in
        Class_sig.add_default_constructor reason iface_sig
    in
    iface_sig, self,
    { Ast.Statement.DeclareClass.
      id = ident;
      tparams = tparam_asts;
      body = body_loc, { Ast.Type.Object.properties; exact };
      extends = extends_ast;
      mixins = mixins_ast;
      implements = implements_ast;
    }
