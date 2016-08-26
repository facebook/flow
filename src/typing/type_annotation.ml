(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast
module FlowError = Flow_error

open Utils_js
open Reason
open Type
open Env.LookupMode

(* AST helpers *)

let ident_name (_, ident) =
  ident.Ast.Identifier.name

let error_type cx loc msg =
  FlowError.add_error cx (loc, [msg]);
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
    let msg = spf "Incorrect number of type parameters (expected %n)" n in
    error_type cx loc msg

let mk_custom_fun cx loc typeParameters kind =
  check_type_param_arity cx loc typeParameters 0 (fun () ->
    let reason = mk_reason "function type" loc in
    CustomFunT (reason, kind)
  )

(**********************************)
(* Transform annotations to types *)
(**********************************)

(* converter *)
let rec convert cx tparams_map = Ast.Type.(function

| loc, Any -> AnyT.at loc

| loc, Void -> VoidT.at loc

| loc, Null -> NullT.at loc

| loc, Number -> NumT.at loc

| loc, String -> StrT.at loc

| loc, Boolean -> BoolT.at loc

| _, Nullable t -> MaybeT (convert cx tparams_map t)

| loc, Union ts ->
  let ts = List.map (convert cx tparams_map) ts in
  UnionT (mk_reason "union type" loc, UnionRep.make ts)

| loc, Intersection ts ->
  let ts = List.map (convert cx tparams_map) ts in
  let rep = InterRep.make ts in
  IntersectionT (mk_reason "intersection type" loc, rep)

| loc, Typeof x ->
  begin match x with
  | (_, Generic {
      Generic.id = qualification;
      typeParameters = None
    }) ->
      let valtype = convert_qualification ~lookup_mode:ForTypeof cx
        "typeof-annotation" qualification in
      Flow_js.mk_typeof_annotation cx valtype
  | _ ->
    error_type cx loc "Unexpected typeof expression"
  end

| loc, Tuple ts ->
  let elts = List.map (convert cx tparams_map) ts in
  let reason = mk_reason "tuple type" loc in
  let element_reason = mk_reason "tuple element" loc in
  let tx =
    if ts = [] then Flow_js.mk_tvar cx element_reason
    else
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
      AnyWithLowerBoundT (UnionT (element_reason, UnionRep.make elts)) in
  ArrT (reason, tx, elts)

| loc, Array t ->
  let r = mk_reason "array type" loc in
  let t = convert cx tparams_map t in
  ArrT (r, t, [])

| loc, StringLiteral { StringLiteral.value; _ }  ->
  let reason = mk_reason "string literal type" loc in
  mk_singleton_string reason value

| loc, NumberLiteral { NumberLiteral.value; raw; _ }  ->
  let reason = mk_reason "number literal type" loc in
  mk_singleton_number reason value raw

| loc, BooleanLiteral { BooleanLiteral.value; _ }  ->
  let reason = mk_reason "boolean literal type" loc in
  mk_singleton_boolean reason value

(* TODO *)
| loc, Generic { Generic.id = Generic.Identifier.Qualified (_,
       { Generic.Identifier.qualification; id; }); typeParameters } ->

  let m = convert_qualification cx "type-annotation" qualification in
  let _, { Ast.Identifier.name; _ } = id in
  let reason = mk_reason name loc in
  let t = Flow_js.mk_tvar_where cx reason (fun t ->
    Flow_js.flow cx (m, GetPropT (reason, (reason, name), t));
  ) in
  let typeParameters = extract_type_param_instantiations typeParameters in
  mk_nominal_type cx reason tparams_map (t, typeParameters)

(* type applications: name < params > *)
| loc, Generic {
    Generic.id = Generic.Identifier.Unqualified (id);
    typeParameters
  } ->
  let _, { Ast.Identifier.name; _ } = id in
  let typeParameters = extract_type_param_instantiations typeParameters in

  let convert_type_params () = Option.value_map
    typeParameters
    ~default: []
    ~f:(List.map (convert cx tparams_map)) in

  begin match name with

  (* TODO Type.Mixed *)
  | "mixed" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      MixedT.at loc
    )

  (* Array<T> *)
  | "Array" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      ArrT (mk_reason "array type" loc, t, [])
    )

  (* $Either<...T> is the union of types ...T *)
  | "$Either" ->
    let ts = convert_type_params () in
    UnionT (mk_reason "union type" loc, UnionRep.make ts)

  (* $All<...T> is the intersection of types ...T *)
  | "$All" ->
    let ts = convert_type_params () in
    let rep = InterRep.make ts in
    IntersectionT (mk_reason "intersection type" loc, rep)

  (* $Tuple<...T> is the tuple of types ...T *)
  | "$Tuple" ->
    let ts = convert_type_params () in
    ArrT (mk_reason "tuple type" loc, AnyT.t, ts)

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
      TypeT (mk_reason "type" loc, t)
    )

  (* $PropertyType<T, 'x'> acts as the type of 'x' in object type T *)
  | "$PropertyType" ->
    check_type_param_arity cx loc typeParameters 2 (fun () ->
      match convert_type_params () with
      | [t; SingletonStrT (_, key)] ->
        EvalT (t, TypeDestructorT
          (mk_reason "property type" loc, PropertyType key), mk_id())
      | _ -> error_type cx loc
        "expected object type and string literal as arguments to $PropertyType"
    )

  (* $NonMaybeType<T> acts as the type T without null and void *)
  | "$NonMaybeType" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      EvalT (t, TypeDestructorT
        (mk_reason "non-maybe type" loc, NonMaybeType), mk_id())
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
      KeysT (mk_reason "key set" loc, t)
    )

  | "$Exact" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = List.hd (convert_type_params ()) in
      ExactT (mk_reason "exact type" loc, t)
    )

  (* $Exports<'M'> is the type of the exports of module 'M' *)
  (** TODO: use `import typeof` instead when that lands **)
  | "$Exports" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      match typeParameters with
      | Some ((_, StringLiteral { StringLiteral.value; _ })::_) ->
          let reason = (mk_reason (spf "exports of module `%s`" value) loc) in
          let remote_module_t =
            Env.get_var_declared_type cx (internal_module_name value) reason
          in
          Flow_js.mk_tvar_where cx reason (fun t ->
            Flow_js.flow cx (remote_module_t, CJSRequireT(reason, t))
          )
      | _ ->
          let msg = "$Exports requires a string literal" in
          FlowError.add_error cx (loc, [msg]);
          AnyT.t
    )

  | "$Abstract" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      AbstractT t
    )

  | "this" ->
    if SMap.mem "this" tparams_map then
      (* We model a this type like a type parameter. The bound on a this
         type reflects the interface of `this` exposed in the current
         environment. Currently, we only support this types in a class
         environment: a this type in class C is bounded by C. *)
      check_type_param_arity cx loc typeParameters 0 (fun () ->
        let reason = mk_reason thistype_desc loc in
        Flow_js.reposition cx reason (SMap.find_unsafe "this" tparams_map)
      )
    else (
      FlowError.add_warning cx (loc, ["Unexpected use of `this` type"]);
      AnyT.t
    )

  (* Class<T> is the type of the class whose instances are of type T *)
  | "Class" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      ClassT t
    )

  | "Function" | "function" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason "function type" loc in
      AnyFunT reason
    )

  | "Object" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason "object type" loc in
      AnyObjT reason
    )

  | "Function$Prototype$Apply" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason "function type" loc in
      FunProtoApplyT reason
    )

  | "Function$Prototype$Bind" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason "function type" loc in
      FunProtoBindT reason
    )

  | "Function$Prototype$Call" ->
    check_type_param_arity cx loc typeParameters 0 (fun () ->
      let reason = mk_reason "function type" loc in
      FunProtoCallT reason
    )

  | "$Tainted" ->
    check_type_param_arity cx loc typeParameters 1 (fun () ->
      let t = convert_type_params () |> List.hd in
      let reason = Reason.repos_reason loc (reason_of_t t) in
      UnionT (reason, UnionRep.make [t; TaintT (mk_reason "taint" loc)])
    )

  | "Object$Assign" ->
      mk_custom_fun cx loc typeParameters ObjectAssign
  | "Object$GetPrototypeOf" ->
      mk_custom_fun cx loc typeParameters ObjectGetPrototypeOf
  | "Promise$All" ->
      mk_custom_fun cx loc typeParameters PromiseAll
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
      Flow_js.reposition cx (mk_reason name loc)
        (SMap.find_unsafe name tparams_map)
    )

  | "$Pred" ->
    let fun_reason = mk_reason "abstract predicate function" loc in
    let static_reason = mk_reason "abstract predicate static" loc in
    let out_reason = mk_reason "open predicate" loc in

    check_type_param_arity cx loc typeParameters 1 (fun () ->
      match convert_type_params () with
      | [SingletonNumT (_, (f, _))] ->
        let n = Pervasives.int_of_float f in
        let key_strs =
          Utils_js.range 0 n |>
          List.map (fun i -> "x_" ^ Pervasives.string_of_int i) in
        let emp = Key_map.empty in
        let tins = Utils_js.repeat n (AnyT.at loc) in
        let tout = OpenPredT (out_reason, MixedT.t, emp, emp) in
        FunT (
          fun_reason,
          Flow_js.dummy_static static_reason,
          AnyT (mk_reason "prototype" loc),
          Flow_js.mk_functiontype tins ~params_names:key_strs
            ~is_predicate:true tout
        )

      | _ -> error_type cx loc "expected number of refined variables\
              (currently only supporting one variable)"
    )

  | "$Refine" ->
    check_type_param_arity cx loc typeParameters 3 (fun () ->
      match convert_type_params () with
      | [base_t; fun_pred_t; SingletonNumT (_, (f, _))] ->
          let idx = Pervasives.int_of_float f in
          let reason = mk_reason "refined type" loc in
          let pred = LatentP (fun_pred_t, idx) in
          EvalT (base_t, DestructuringT (reason, Refine pred), mk_id())
      | _ -> error_type cx loc
        "expected base type and predicate type as arguments to $Refine"
    )

  (* other applications with id as head expr *)
  | _ ->
    let reason = mk_reason name loc in
    let c = type_identifier cx name loc in
    mk_nominal_type cx reason tparams_map (c, typeParameters)

  end

| loc, Function { Function.params; returnType; rest; typeParameters } ->
  let tparams, tparams_map =
    mk_type_param_declarations cx ~tparams_map typeParameters in

  let rev_params_tlist, rev_params_names =
    (let rev_tlist, rev_pnames =
      List.fold_left (fun (tlist, pnames) param ->
      match param with
      | _, { Function.Param.name;
             Function.Param.typeAnnotation; optional = false; _ } ->
          (convert cx tparams_map typeAnnotation) :: tlist,
          (ident_name name) :: pnames
      | _, { Function.Param.name;
             Function.Param.typeAnnotation; optional = true; _ } ->
          (OptionalT (convert cx tparams_map typeAnnotation)) :: tlist,
          (ident_name name) :: pnames
    ) ([], []) params in
    match rest with
      | Some (_, { Function.Param.name;
                   Function.Param.typeAnnotation; _ }) ->
          let rest = mk_rest cx (convert cx tparams_map typeAnnotation) in
          rest :: rev_tlist,
          (ident_name name) :: rev_pnames
      | None -> rev_tlist, rev_pnames
    ) in
  let reason = mk_reason "function type" loc in
  let params_names = List.rev rev_params_names in
  let return_t = convert cx tparams_map returnType in
  let ft =
    FunT (
      reason,
      Flow_js.dummy_static reason,
      AnyT (mk_reason "prototype" loc),
      {
        this_t = Flow_js.mk_tvar cx (mk_reason "this" loc);
        params_tlist = (List.rev rev_params_tlist);
        params_names = Some params_names;
        return_t;
        is_predicate = false;
        closure_t = 0;
        changeset = Changeset.empty
      })
  in
  if (tparams = []) then ft else PolyT(tparams, ft)

| loc, Object { Object.exact; properties; indexers; callProperties; } ->
  let props_map = List.fold_left (
    fun props_map (loc, { Object.Property.key; value; optional; _ }) ->
      (match key with
        | Ast.Expression.Object.Property.Literal
            (_, { Ast.Literal.value = Ast.Literal.String name; _ })
        | Ast.Expression.Object.Property.Identifier
            (_, { Ast.Identifier.name; _ }) ->
            let t = convert cx tparams_map value in
            if optional
            then
              (* wrap types of optional properties, just like we do for
                 optional parameters *)
              SMap.add name (OptionalT t) props_map
            else
              SMap.add name t props_map
        | _ ->
          let msg = "Unsupported key in object type" in
          FlowError.add_error cx (loc, [msg]);
          props_map
    )
  ) SMap.empty properties
  in
  let props_map = match callProperties with
    | [] -> props_map
    | [loc, { Object.CallProperty.value = (_, ft); _; }] ->
        SMap.add "$call" (
          convert cx tparams_map (loc, Ast.Type.Function ft)) props_map
    | fts ->
        let fts = List.map
          (fun (loc, { Object.CallProperty.value = (_, ft); _; }) ->
              convert cx tparams_map (loc, Ast.Type.Function ft))
          fts in
        let callable_reason = mk_reason "callable object type" loc in
        let rep = InterRep.make fts in
        SMap.add "$call" (IntersectionT (callable_reason, rep)) props_map
  in
  (* Seal an object type unless it specifies an indexer. *)
  let sealed, dict =
    match indexers with
    | [] ->
        true,
        None
    | (
        _,
        { Object.Indexer.id = (_, { Ast.Identifier.name; _ }); key; value; _;}
      )::rest ->
        (* TODO *)
        List.iter (fun (indexer_loc, _) ->
          let msg = "multiple indexers are not supported" in
          FlowError.add_error cx (indexer_loc, [msg]);
        ) rest;

        let keyt = convert cx tparams_map key in
        let valuet = convert cx tparams_map value in
        false,
        Some { Type.
          dict_name = Some name;
          key = keyt;
          value = valuet
        }
  in
  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let reason_desc = "object type" in
  let pmap = Flow_js.mk_propmap cx props_map in
  let proto = MixedT (reason_of_string reason_desc, Mixed_everything) in
  let flags = {
    sealed = if sealed then Sealed else UnsealedInFile (Loc.source loc);
    exact = not sealed || exact;
    frozen = false;
  } in
  let t = ObjT (mk_reason reason_desc loc,
    Flow_js.mk_objecttype ~flags dict pmap proto) in
  if exact
  then ExactT (mk_reason "exact type" loc, t)
  else t

| loc, Exists ->
  (* Do not evaluate existential type variables when map is non-empty. This
     ensures that existential type variables under a polymorphic type remain
     unevaluated until the polymorphic type is applied. *)
  let force = SMap.is_empty tparams_map in
  let reason = derivable_reason (mk_reason existential_desc loc) in
  if force then Flow_js.mk_tvar cx reason
  else ExistsT reason
)

and convert_qualification ?(lookup_mode=ForType) cx reason_prefix
  = Ast.Type.Generic.Identifier.(function
  | Qualified (loc, { qualification; id; }) ->
    let m = convert_qualification ~lookup_mode cx reason_prefix qualification in
    let _, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s '<<object>>.%s')" reason_prefix name) loc in
    Flow_js.mk_tvar_where cx reason (fun t ->
      Flow_js.flow cx (m, GetPropT (reason, (reason, name), t));
    )

  | Unqualified (id) ->
    let loc, { Ast.Identifier.name; _ } = id in
    let reason = mk_reason (spf "%s `%s`" reason_prefix name) loc in
    Env.get_var ~lookup_mode cx name reason
)

(** Like `destructuring`, the following function operates on types that might
    contain unsubstituted type parameters, so we must be careful not to emit
    constraints in the general case. In fact, there does not seem to be any need
    at all to allow general types to appear as annotations of a rest parameter,
    we can make our lives simpler by disallowing them. **)
and mk_rest cx = function
  | ArrT (_, t, []) -> RestT t
  | AnyT _ as t -> RestT t
  | OpenT _ as t ->
      (* unify t with Array<e>, return (RestT e) *)
      let reason = prefix_reason "element of " (reason_of_t t) in
      let tvar = Flow_js.mk_tvar cx reason in
      let arrt = ArrT(reason, tvar, []) in
      Flow_js.unify cx t arrt;
      RestT tvar
  | t ->
      let r = reason_of_t t in
      let msg =
        "rest parameter should have an explicit array type (or type `any`)" in
      FlowError.(add_warning cx (mk_info r [msg]));
      RestT (AnyT.why r)

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

(* Model a set of keys as the union of their singleton types. *)
and mk_keys_type reason = function
| [key] ->
  mk_singleton_string reason key
| keys ->
  UnionT (reason,
    UnionRep.make (List.map (mk_singleton_string reason) keys))

and mk_singleton_string reason key =
  let reason = replace_reason (spf "string literal `%s`" key) reason in
  SingletonStrT (reason, key)

and mk_singleton_number reason num raw =
  let reason = replace_reason (spf "number literal `%.16g`" num) reason in
  SingletonNumT (reason, (num, raw))

and mk_singleton_boolean reason b =
  let reason = replace_reason (spf "boolean literal `%b`" b) reason in
  SingletonBoolT (reason, b)

(* Given the type of expression C and type arguments T1...Tn, return the type of
   values described by C<T1,...,Tn>, or C when there are no type arguments. *)
(** See comment on Flow_js.mk_instance for what the for_type flag means. **)
and mk_nominal_type ?(for_type=true) cx reason tparams_map (c, targs) =
  match targs with
  | None ->
      Flow_js.mk_instance cx reason ~for_type c
  | Some targs ->
      let tparams = List.map (convert cx tparams_map) targs in
      TypeAppT (c, tparams)

(* take a list of AST type param declarations,
   do semantic checking and create types for them. *)
and mk_type_param_declarations cx ?(tparams_map=SMap.empty) typeParameters =
  let open Ast.Type.ParameterDeclaration in
  let add_type_param (tparams, tparams_map, bounds_map) = function
  | loc, { TypeParam.name; bound; variance; default; } ->
    let reason = mk_reason name loc in
    let bound = match bound with
    | None -> MixedT (reason, Mixed_everything)
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
    let polarity = TypeParam.Variance.(match variance with
    | Some Plus -> Positive
    | Some Minus -> Negative
    | None -> Neutral
    ) in
    let tparam = { reason; name; bound; polarity; default; } in
    (tparam :: tparams,
     SMap.add name (BoundT tparam) tparams_map,
     SMap.add name (Flow_js.subst cx bounds_map bound) bounds_map)
  in
  let tparams, tparams_map, _ = extract_type_param_declarations typeParameters
    |> List.fold_left add_type_param ([], tparams_map, SMap.empty)
  in
  List.rev tparams, tparams_map

and type_identifier cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    if name = "undefined"
    then VoidT.at loc
    else (
      let reason = mk_reason (spf "identifier `%s`" name) loc in
      let t = Env.var_ref ~lookup_mode:ForType cx name reason in
      t
    )
  )

and extract_type_param_declarations =
  let open Ast.Type.ParameterDeclaration in
  let f (_, typeParameters) = typeParameters.params in
  Option.value_map ~f ~default:[]

and extract_type_param_instantiations =
  let open Ast.Type.ParameterInstantiation in
  function
  | None -> None
  | Some (_, typeParameters) -> Some typeParameters.params
