(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Tast_utils = Typed_ast_utils
module Generic_ID = Generic
open Utils_js
open Reason
open Type
open TypeUtil
open Trust_helpers
module Flow = Flow_js
module T = Ast.Type

module type C = sig
  val mk_typeof_annotation : Context.t -> ?trace:Type.trace -> Reason.t -> Type.t -> Type.t

  val mk_instance : Context.t -> ?trace:Type.trace -> reason -> ?use_desc:bool -> Type.t -> Type.t

  val cjs_require : Context.t -> Type.t -> Reason.t -> bool -> Type.t

  val get_prop :
    Context.t -> Type.use_op -> Reason.t -> ?op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val reposition : Context.t -> ALoc.t -> ?annot_loc:ALoc.t -> Type.t -> Type.t

  val get_builtin : Context.t -> ?trace:Type.trace -> name -> reason -> Type.t

  val obj_test_proto : Context.t -> Reason.t -> Type.t -> Type.t

  val widen_obj_type :
    Context.t -> ?trace:Type.trace -> use_op:Type.use_op -> Reason.reason -> Type.t -> Type.t

  val mixin : Context.t -> Reason.t -> Type.t -> Type.t

  val subtype_check : Context.t -> Type.t -> Type.t -> unit
end

module FlowJS : C = struct
  include Flow

  let reposition = reposition ?trace:None ?desc:None

  let subtype_check cx l u = Flow.flow_t cx (l, u)

  let mixin cx reason i =
    Tvar.mk_where cx reason (fun tout -> Flow.flow cx (i, Type.MixinT (reason, tout)))

  let cjs_require cx remote_module_t reason is_strict =
    Tvar.mk_where cx reason (fun tout ->
        Flow.flow cx (remote_module_t, CJSRequireT (reason, tout, is_strict))
    )

  let obj_test_proto cx reason t =
    Tvar.mk_where cx reason (fun tout -> Flow.flow cx (t, ObjTestProtoT (reason, tout)))

  let get_prop cx use_op reason ?(op_reason = reason) name l =
    Tvar.mk_no_wrap_where cx op_reason (fun tout ->
        Flow.flow cx (l, GetPropT (use_op, op_reason, None, Named (reason, name), tout))
    )
end

module Annot : C = struct
  include Annotation_inference.ConsGen

  let subtype_check _ _ _ = (* TODO *) ()
end

module Make (ConsGen : C) (Statement : Statement_sig.S) : Type_annotation_sig.S = struct
  open Env.LookupMode

  module Func_type_params_config_types = struct
    type 'T ast = (ALoc.t, 'T) Ast.Type.Function.Params.t

    type 'T param_ast = (ALoc.t, 'T) Ast.Type.Function.Param.t

    type 'T rest_ast = (ALoc.t, 'T) Ast.Type.Function.RestParam.t

    type 'T this_ast = (ALoc.t, ALoc.t * Type.t) Ast.Type.Function.ThisParam.t

    type param = Type.t * (ALoc.t * Type.t) param_ast

    type rest = Type.t * (ALoc.t * Type.t) rest_ast

    type this_param = Type.t * (ALoc.t * Type.t) this_ast

    type pattern = unit
  end

  module Func_type_params_config = struct
    let id_name (_, { Ast.Identifier.name; _ }) = name

    let param_type (t, (_, { Ast.Type.Function.Param.name; optional; _ })) =
      let name = Base.Option.map name ~f:id_name in
      let t =
        if optional then
          TypeUtil.optional t
        else
          t
      in
      (name, t)

    let rest_type (t, (loc, { Ast.Type.Function.RestParam.argument; comments = _ })) =
      let (_, { Ast.Type.Function.Param.name; _ }) = argument in
      let name = Base.Option.map name ~f:id_name in
      (name, loc, t)

    let this_type (t, _) = t

    let is_param_type_annotated _ = true

    let is_rest_type_annotated _ = true

    let subst_param cx map (t, tast) =
      let t = Subst.subst cx map t in
      (t, tast)

    let subst_rest cx map (t, tast) =
      let t = Subst.subst cx map t in
      (t, tast)

    let subst_this cx map (t, tast) =
      let t = Subst.subst cx map t in
      (t, tast)

    let eval_param _cx (_, tast) = tast

    let eval_rest _cx (_, tast) = tast

    let eval_this _cx (_, tast) = tast
  end

  module Func_type_params_types = Func_class_sig_types.Param.Make (Func_type_params_config_types)
  module Func_type_params =
    Func_params.Make (Func_type_params_config_types) (Func_type_params_config)
      (Func_type_params_types)
  module Func_type_sig_types =
    Func_class_sig_types.Func.Make (Func_type_params_config_types) (Func_type_params_types)
  module Func_type_sig =
    Func_sig.Make (Statement) (Func_type_params_config_types) (Func_type_params_config)
      (Func_type_params)
      (Func_type_sig_types)
  module Class_type_sig_types =
    Func_class_sig_types.Class.Make (Func_type_params_config_types) (Func_type_params_types)
      (Func_type_sig_types)
  module Class_type_sig =
    Class_sig.Make (Func_type_params_config_types) (Func_type_params_config) (Func_type_params)
      (Func_type_sig)
      (Class_type_sig_types)

  (* AST helpers *)

  let qualified_name =
    let rec loop acc =
      let open Ast.Type.Generic.Identifier in
      function
      | Unqualified (_, { Ast.Identifier.name; comments = _ }) ->
        let parts = name :: acc in
        String.concat "." parts
      | Qualified (_, { qualification; id = (_, { Ast.Identifier.name; comments = _ }) }) ->
        loop (name :: acc) qualification
    in
    loop []

  let typeof_name =
    let rec loop acc =
      let open Ast.Type.Typeof.Target in
      function
      | Unqualified (_, { Ast.Identifier.name; comments = _ }) ->
        let parts = name :: acc in
        String.concat "." parts
      | Qualified (_, { qualification; id = (_, { Ast.Identifier.name; comments = _ }) }) ->
        loop (name :: acc) qualification
    in
    loop []

  let ident_name (_, { Ast.Identifier.name; comments = _ }) = name

  let error_type cx loc msg t_in =
    Flow_js_utils.add_output cx msg;
    let t_out = Tast_utils.error_mapper#type_ t_in |> snd in
    ((loc, AnyT.at (AnyError None) loc), t_out)

  let is_suppress_type cx type_name = SSet.mem type_name (Context.suppress_types cx)

  let check_type_arg_arity cx loc t_ast params n f =
    match params with
    | None ->
      if n = 0 then
        f ()
      else
        error_type cx loc (Error_message.ETypeParamArity (loc, n)) t_ast
    | Some (_, { Ast.Type.TypeArgs.arguments = l; comments = _ }) ->
      if n = List.length l && n <> 0 then
        f ()
      else
        error_type cx loc (Error_message.ETypeParamArity (loc, n)) t_ast

  let mk_custom_fun cx loc t_ast targs (id_loc, name, comments) kind =
    check_type_arg_arity cx loc t_ast targs 0 (fun () ->
        let reason = mk_reason RFunctionType loc in
        let t = CustomFunT (reason, kind) in
        ( (loc, t),
          let open Ast.Type in
          Generic
            {
              Generic.id =
                Generic.Identifier.Unqualified ((id_loc, t), { Ast.Identifier.name; comments });
              targs = None;
              comments = None;
            }
        )
    )

  let mk_eval_id cx loc =
    if Env.in_toplevel_scope cx then
      Context.make_aloc_id cx loc |> Eval.id_of_aloc_id
    else
      Eval.generate_id ()

  let mk_react_prop_type cx loc t_ast targs id kind =
    mk_custom_fun cx loc t_ast targs id (ReactPropType (React.PropType.Complex kind))

  let add_unclear_type_error_if_not_lib_file cx loc =
    match ALoc.source loc with
    | Some file when not @@ File_key.is_lib_file file ->
      Flow_js_utils.add_output cx (Error_message.EUnclearType loc)
    | _ -> ()

  let polarity cx variance =
    (match variance with
    | Some (loc, { Ast.Variance.kind = Ast.Variance.Readonly; _ }) ->
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSReadonlyVariance; loc })
    | Some (loc, { Ast.Variance.kind = Ast.Variance.In; _ }) ->
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `In; loc })
    | Some (loc, { Ast.Variance.kind = Ast.Variance.Out; _ }) ->
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `Out; loc })
    | Some (loc, { Ast.Variance.kind = Ast.Variance.InOut; _ }) ->
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSInOutVariance `InOut; loc })
    | _ -> ());
    Typed_ast_utils.polarity variance

  (**********************************)
  (* Transform annotations to types *)
  (**********************************)

  (* converter *)
  let rec convert cx tparams_map =
    let open Ast.Type in
    function
    | (loc, (Any _ as t_ast)) ->
      add_unclear_type_error_if_not_lib_file cx loc;
      ((loc, AnyT.at AnnotatedAny loc), t_ast)
    | (loc, (Mixed _ as t_ast)) -> ((loc, MixedT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Empty _ as t_ast)) -> ((loc, EmptyT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Void _ as t_ast)) -> ((loc, VoidT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Null _ as t_ast)) -> ((loc, NullT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Symbol _ as t_ast)) -> ((loc, SymbolT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Number _ as t_ast)) -> ((loc, NumT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (BigInt _ as t_ast)) -> ((loc, BigIntT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (String _ as t_ast)) -> ((loc, StrT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Boolean { raw; comments = _ } as t_ast)) ->
      (match raw with
      | `Bool -> Flow_js_utils.add_output cx (Error_message.EDeprecatedBool loc)
      | `Boolean -> ());
      ((loc, BoolT.at loc |> with_trust_inference cx), t_ast)
    | (loc, (Unknown _ as t_ast)) ->
      Flow_js_utils.add_output cx (Error_message.ETSSyntax { kind = Error_message.TSUnknown; loc });
      ((loc, AnyT.at (AnyError None) loc), t_ast)
    | (loc, (Never _ as t_ast)) ->
      Flow_js_utils.add_output cx (Error_message.ETSSyntax { kind = Error_message.TSNever; loc });
      ((loc, AnyT.at (AnyError None) loc), t_ast)
    | (loc, (Undefined _ as t_ast)) ->
      Flow_js_utils.add_output cx (Error_message.ETSSyntax { kind = Error_message.TSUndefined; loc });
      ((loc, AnyT.at (AnyError None) loc), t_ast)
    | (loc, Nullable { Nullable.argument = t; comments }) ->
      let (((_, t), _) as t_ast) = convert cx tparams_map t in
      let reason = mk_annot_reason (RMaybe (desc_of_t t)) loc in
      ((loc, MaybeT (reason, t)), Nullable { Nullable.argument = t_ast; comments })
    | (loc, Union { Union.types = (t0, t1, ts); comments }) ->
      let (((_, t0), _) as t0_ast) = convert cx tparams_map t0 in
      let (((_, t1), _) as t1_ast) = convert cx tparams_map t1 in
      let (ts, ts_ast) = convert_list cx tparams_map ts in
      let rep = UnionRep.make t0 t1 ts in
      ( (loc, UnionT (mk_annot_reason RUnionType loc, rep)),
        Union { Union.types = (t0_ast, t1_ast, ts_ast); comments }
      )
    | (loc, Intersection { Intersection.types = (t0, t1, ts); comments }) ->
      let (((_, t0), _) as t0_ast) = convert cx tparams_map t0 in
      let (((_, t1), _) as t1_ast) = convert cx tparams_map t1 in
      let (ts, ts_ast) = convert_list cx tparams_map ts in
      let rep = InterRep.make t0 t1 ts in
      ( (loc, IntersectionT (mk_annot_reason RIntersectionType loc, rep)),
        Intersection { Intersection.types = (t0_ast, t1_ast, ts_ast); comments }
      )
    | (loc, Typeof { Typeof.argument = qualification; comments }) ->
      let (valtype, qualification_ast) = convert_typeof cx "typeof-annotation" qualification in
      let desc = RTypeof (typeof_name qualification) in
      let reason = mk_reason desc loc in
      ( (loc, ConsGen.mk_typeof_annotation cx reason valtype),
        Typeof { Typeof.argument = qualification_ast; comments }
      )
    | (loc, Keyof keyof) ->
      Flow_js_utils.add_output cx (Error_message.ETSSyntax { kind = Error_message.TSKeyof; loc });
      let t = AnyT.at (AnyError None) loc in
      ((loc, t), Keyof (Tast_utils.error_mapper#keyof_type keyof))
    | (loc, ReadOnly ro) ->
      let { ReadOnly.argument; _ } = ro in
      let arg_kind =
        match argument with
        | (_, Array _) -> Some `Array
        | (_, Tuple _) -> Some `Tuple
        | _ -> None
      in
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSReadonlyType arg_kind; loc });
      let t = AnyT.at (AnyError None) loc in
      ((loc, t), ReadOnly (Tast_utils.error_mapper#readonly_type ro))
    | (loc, Tuple { Tuple.elements; comments }) ->
      let (ts_rev, els_rev, els_ast_rev) =
        Base.List.fold elements ~init:([], [], []) ~f:(fun (ts, els, els_ast) element ->
            let (t, el, el_ast) = convert_tuple_element cx tparams_map element in
            (t :: ts, el :: els, el_ast :: els_ast)
        )
      in
      let (ts, els, els_ast) = (List.rev ts_rev, List.rev els_rev, List.rev els_ast_rev) in
      let reason = mk_annot_reason RTupleType loc in
      let element_reason = mk_annot_reason RTupleElement loc in
      let elem_t =
        match ts with
        | [] -> EmptyT.why element_reason |> with_trust bogus_trust
        | [t] -> t
        | t0 :: t1 :: ts ->
          (* If a tuple should be viewed as an array, what would the element type of
             the array be?

             Using a union here seems appealing but is wrong: setting elements
             through arbitrary indices at the union type would be unsound, since it
             might violate the projected types of the tuple at their corresponding
             positions. This also shows why `mixed` doesn't work, either.

             On the other hand, using the empty type would prevent writes, but admit
             unsound reads.

             The correct solution is to safely case a tuple type to a covariant
             array interface whose element type would be a union.
          *)
          UnionT (element_reason, UnionRep.make t0 t1 ts)
      in
      ( (loc, DefT (reason, infer_trust cx, ArrT (TupleAT { elem_t; elements = els }))),
        Tuple { Tuple.elements = els_ast; comments }
      )
    | (loc, Array { Array.argument = t; comments }) ->
      let r = mk_annot_reason RArrayType loc in
      let (((_, elemt), _) as t_ast) = convert cx tparams_map t in
      ( (loc, DefT (r, infer_trust cx, ArrT (ArrayAT (elemt, None)))),
        Array { Array.argument = t_ast; comments }
      )
    | (loc, (StringLiteral { Ast.StringLiteral.value; _ } as t_ast)) ->
      let t =
        if Type_inference_hooks_js.dispatch_literal_hook cx loc then
          Tvar.mk cx (mk_reason (RCustom "literal") loc)
        else
          mk_singleton_string cx loc value
      in
      ((loc, t), t_ast)
    | (loc, (NumberLiteral { Ast.NumberLiteral.value; raw; _ } as t_ast)) ->
      ((loc, mk_singleton_number cx loc value raw), t_ast)
    | (loc, (BigIntLiteral { Ast.BigIntLiteral.value; raw; _ } as t_ast)) ->
      ((loc, mk_singleton_bigint cx loc value raw), t_ast)
    | (loc, (BooleanLiteral { Ast.BooleanLiteral.value; _ } as t_ast)) ->
      ((loc, mk_singleton_boolean cx loc value), t_ast)
    | (loc, IndexedAccess { IndexedAccess._object; index; comments }) ->
      let reason = mk_reason (RIndexedAccess { optional = false }) loc in
      let (((_, object_type), _) as _object) = convert cx tparams_map _object in
      let (((_, index_type), _) as index) = convert cx tparams_map index in
      let t =
        let use_op =
          Op
            (IndexedTypeAccess { _object = reason_of_t object_type; index = reason_of_t index_type })
        in
        let destructor =
          match index with
          | (_, StringLiteral { Ast.StringLiteral.value; _ }) ->
            PropertyType { name = OrdinaryName value }
          | _ -> ElementType { index_type }
        in
        EvalT (object_type, TypeDestructorT (use_op, reason, destructor), mk_eval_id cx loc)
      in
      ((loc, t), IndexedAccess { IndexedAccess._object; index; comments })
    | (loc, OptionalIndexedAccess ia) ->
      let (_, ast) = optional_indexed_access cx loc ~tparams_map ia in
      ast
    (* TODO *)
    | ( loc,
        Generic
          {
            Generic.id =
              Generic.Identifier.Qualified (qid_loc, { Generic.Identifier.qualification; id }) as
              qid;
            targs;
            comments;
          }
      ) ->
      let (m, qualification_ast) = convert_qualification cx "type-annotation" qualification in
      let (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name)) = id in
      let reason = mk_reason (RType (OrdinaryName name)) loc in
      let id_reason = mk_reason (RType (OrdinaryName name)) id_loc in
      let qid_reason = mk_reason (RType (OrdinaryName (qualified_name qid))) qid_loc in
      let use_op = Op (GetProperty qid_reason) in
      let t_unapplied =
        ConsGen.get_prop cx use_op id_reason ~op_reason:qid_reason (OrdinaryName name) m
      in
      let (t, targs) = mk_nominal_type cx reason tparams_map (t_unapplied, targs) in
      ( (loc, t),
        Generic
          {
            Generic.id =
              Generic.Identifier.Qualified
                ( qid_loc,
                  {
                    Generic.Identifier.qualification = qualification_ast;
                    id = ((id_loc, t_unapplied), id_name);
                  }
                );
            targs;
            comments;
          }
      )
    (* type applications: name < params > *)
    | ( loc,
        Generic
          {
            Generic.id =
              Generic.Identifier.Unqualified
                (name_loc, ({ Ast.Identifier.name; comments = id_comments } as id_name));
            targs;
            comments;
          }
      ) as t_ast ->
      (* Comments are innecessary, so they can be stripped to meet the generic requirements *)
      let ident = (name_loc, name, id_comments) in
      let convert_type_params () =
        match targs with
        | None -> ([], None)
        | Some (loc, { TypeArgs.arguments = targs; comments }) ->
          let (elemts, targs) = convert_list cx tparams_map targs in
          (elemts, Some (loc, { TypeArgs.arguments = targs; comments }))
      in
      let reconstruct_ast t ?id_t targs =
        ( (loc, t),
          Generic
            {
              Generic.id =
                Generic.Identifier.Unqualified
                  ((name_loc, Base.Option.value id_t ~default:t), id_name);
              targs;
              comments;
            }
        )
      in
      let use_op reason = Op (TypeApplication { type' = reason }) in
      begin
        match name with
        (* Temporary base types with literal information *)
        | "$TEMPORARY$number" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              match List.hd elemts with
              | DefT (r, trust, SingletonNumT num_lit) ->
                reconstruct_ast
                  (DefT (replace_desc_reason RNumber r, trust, NumT (Literal (None, num_lit))))
                  targs
              | _ -> error_type cx loc (Error_message.EUnexpectedTemporaryBaseType loc) t_ast
          )
        | "$TEMPORARY$string" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              match List.hd elemts with
              | DefT (r, trust, SingletonStrT s) ->
                let max_literal_length = Context.max_literal_length cx in
                let (lit, r_desc) =
                  if
                    max_literal_length = 0
                    || String.length (display_string_of_name s) <= max_literal_length
                  then
                    (Literal (None, s), RString)
                  else
                    (AnyLiteral, RLongStringLit max_literal_length)
                in
                reconstruct_ast (DefT (replace_desc_reason r_desc r, trust, StrT lit)) targs
              | _ -> error_type cx loc (Error_message.EUnexpectedTemporaryBaseType loc) t_ast
          )
        | "$TEMPORARY$boolean" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              match List.hd elemts with
              | DefT (r, trust, SingletonBoolT bool) ->
                reconstruct_ast
                  (DefT (replace_desc_reason RBoolean r, trust, BoolT (Some bool)))
                  targs
              | _ -> error_type cx loc (Error_message.EUnexpectedTemporaryBaseType loc) t_ast
          )
        | "$TEMPORARY$object" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = convert_temporary_object (List.hd ts) in
              reconstruct_ast t targs
          )
        | "$TEMPORARY$array" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              let elemt = List.hd elemts in
              reconstruct_ast
                (DefT (mk_annot_reason RArrayLit loc, infer_trust cx, ArrT (ArrayAT (elemt, None))))
                targs
          )
        (* Array<T> *)
        | "Array" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              let elemt = List.hd elemts in
              reconstruct_ast
                (DefT (mk_annot_reason RArrayType loc, infer_trust cx, ArrT (ArrayAT (elemt, None))))
                targs
          )
        (* $ReadOnlyArray<T> is the supertype of all tuples and all arrays *)
        | "$ReadOnlyArray" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (elemts, targs) = convert_type_params () in
              let elemt = List.hd elemts in
              reconstruct_ast
                (DefT (mk_annot_reason RROArrayType loc, infer_trust cx, ArrT (ROArrayAT elemt)))
                targs
          )
        (* $PropertyType<T, 'x'> acts as the type of 'x' in object type T *)
        | "$PropertyType" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              match convert_type_params () with
              | ([t; DefT (_, _, SingletonStrT key)], targs) ->
                let reason = mk_reason (RType (OrdinaryName "$PropertyType")) loc in
                reconstruct_ast
                  (EvalT
                     ( t,
                       TypeDestructorT (use_op reason, reason, PropertyType { name = key }),
                       mk_eval_id cx loc
                     )
                  )
                  targs
              | _ -> error_type cx loc (Error_message.EPropertyTypeAnnot loc) t_ast
          )
        (* $ElementType<T, string> acts as the type of the string elements in object
           type T *)
        | "$ElementType" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              match convert_type_params () with
              | ([t; e], targs) ->
                let reason = mk_reason (RType (OrdinaryName "$ElementType")) loc in
                reconstruct_ast
                  (EvalT
                     ( t,
                       TypeDestructorT (use_op reason, reason, ElementType { index_type = e }),
                       mk_eval_id cx loc
                     )
                  )
                  targs
              | _ -> assert false
          )
        (* $NonMaybeType<T> acts as the type T without null and void *)
        | "$NonMaybeType" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "$NonMaybeType")) loc in
              reconstruct_ast
                (EvalT (t, TypeDestructorT (use_op reason, reason, NonMaybeType), mk_eval_id cx loc))
                targs
          )
        (* $Partial<T> makes all of `T`'s properties optional *)
        | "$Partial" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RPartialOf (desc_of_t t)) (loc_of_t t) in
              reconstruct_ast
                (EvalT (t, TypeDestructorT (use_op reason, reason, PartialType), mk_eval_id cx loc))
                targs
          )
        (* $Shape<T> matches the shape of T *)
        | "$Shape" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RShapeOf (desc_of_t t)) (loc_of_t t) in
              reconstruct_ast (ShapeT (reason, t)) targs
          )
        (* $Diff<T, S> *)
        | "$Diff" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason (RType (OrdinaryName "$Diff")) loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT
                       (use_op reason, reason, RestType (Type.Object.Rest.IgnoreExactAndOwn, t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        (* $ReadOnly<T> *)
        | "$ReadOnly" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason RReadOnlyType loc in
              reconstruct_ast
                (EvalT (t, TypeDestructorT (use_op reason, reason, ReadOnlyType), mk_eval_id cx loc))
                targs
          )
        (* $Keys<T> is the set of keys of T *)
        | "$Keys" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              reconstruct_ast (KeysT (mk_reason RKeySet loc, t)) targs
          )
        (* $Values<T> is a union of all the own enumerable value types of T *)
        | "$Values" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "$Values")) loc in
              reconstruct_ast
                (EvalT (t, TypeDestructorT (use_op reason, reason, ValuesType), mk_eval_id cx loc))
                targs
          )
        | "$Exact" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let desc = RExactType (desc_of_t t) in
              reconstruct_ast (ExactT (mk_annot_reason desc loc, t)) targs
          )
        | "$Rest" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason (RType (OrdinaryName "$Rest")) loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, RestType (Type.Object.Rest.Sound, t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        (* $Exports<'M'> is the type of the exports of module 'M' *)
        (* TODO: use `import typeof` instead when that lands **)
        | "$Exports" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              match targs with
              | Some
                  ( targs_loc,
                    {
                      Ast.Type.TypeArgs.arguments =
                        ( str_loc,
                          StringLiteral { Ast.StringLiteral.value; raw; comments = str_comments }
                        )
                        :: _;
                      comments;
                    }
                  ) ->
                let desc = RModule (OrdinaryName value) in
                let reason = mk_annot_reason desc loc in
                let remote_module_t = ConsGen.get_builtin cx (internal_module_name value) reason in
                let str_t = mk_singleton_string cx str_loc value in
                reconstruct_ast
                  (ConsGen.cjs_require cx remote_module_t reason (Context.is_strict cx))
                  (Some
                     ( targs_loc,
                       {
                         Ast.Type.TypeArgs.arguments =
                           [
                             ( (str_loc, str_t),
                               StringLiteral
                                 { Ast.StringLiteral.value; raw; comments = str_comments }
                             );
                           ];
                         comments;
                       }
                     )
                  )
              | _ -> error_type cx loc (Error_message.EExportsAnnot loc) t_ast
          )
        | "$Call" ->
          (match convert_type_params () with
          | (fn :: args, targs) ->
            let reason = mk_reason RFunctionCallType loc in
            reconstruct_ast
              (EvalT (fn, TypeDestructorT (use_op reason, reason, CallType args), mk_eval_id cx loc))
              targs
          | _ -> error_type cx loc (Error_message.ETypeParamMinArity (loc, 1)) t_ast)
        | "$TupleMap" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason RTupleMap loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, TypeMap (TupleMap t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$ObjMap" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason RObjectMap loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, TypeMap (ObjectMap t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$ObjMapi" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason RObjectMapi loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, TypeMap (ObjectMapi t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$KeyMirror" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (t1, targs) =
                match convert_type_params () with
                | ([t], targs) -> (t, targs)
                | _ -> assert false
              in
              let reason = mk_reason RObjectKeyMirror loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, TypeMap ObjectKeyMirror),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$ObjMapConst" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (t1, t2, targs) =
                match convert_type_params () with
                | ([t1; t2], targs) -> (t1, t2, targs)
                | _ -> assert false
              in
              let reason = mk_reason RObjectMapi loc in
              reconstruct_ast
                (EvalT
                   ( t1,
                     TypeDestructorT (use_op reason, reason, TypeMap (ObjectMapConst t2)),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$CharSet" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              match targs with
              | Some
                  ( targs_loc,
                    {
                      Ast.Type.TypeArgs.arguments =
                        [
                          ( str_loc,
                            StringLiteral { Ast.StringLiteral.value; raw; comments = str_comments }
                          );
                        ];
                      comments;
                    }
                  ) ->
                let str_t = mk_singleton_string cx str_loc value in
                let chars = String_utils.CharSet.of_string value in
                let char_str = String_utils.CharSet.to_string chars in
                (* sorts them *)
                let reason = mk_annot_reason (RCustom (spf "character set `%s`" char_str)) loc in
                reconstruct_ast
                  (DefT (reason, infer_trust cx, CharSetT chars))
                  (Some
                     ( targs_loc,
                       {
                         Ast.Type.TypeArgs.arguments =
                           [
                             ( (str_loc, str_t),
                               StringLiteral
                                 { Ast.StringLiteral.value; raw; comments = str_comments }
                             );
                           ];
                         comments;
                       }
                     )
                  )
              | _ -> error_type cx loc (Error_message.ECharSetAnnot loc) t_ast
          )
        | "this" ->
          if Subst_name.Map.mem (Subst_name.Name "this") tparams_map then
            (* We model a this type like a type parameter. The bound on a this
               type reflects the interface of `this` exposed in the current
               environment. Currently, we only support this types in a class
               environment: a this type in class C is bounded by C. *)
            check_type_arg_arity cx loc t_ast targs 0 (fun () ->
                reconstruct_ast
                  (ConsGen.reposition
                     cx
                     loc
                     ~annot_loc:loc
                     (Subst_name.Map.find (Subst_name.Name "this") tparams_map)
                  )
                  None
            )
          else (
            Flow_js_utils.add_output cx (Error_message.EUnexpectedThisType loc);
            Tast_utils.error_mapper#type_ t_ast
          )
        (* Class<T> is the type of the class whose instances are of type T *)
        | "Class" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RStatics (desc_of_t t)) loc in
              reconstruct_ast (DefT (reason, infer_trust cx, ClassT t)) targs
          )
        | "Function"
        | "function" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              add_unclear_type_error_if_not_lib_file cx loc;
              let reason = mk_annot_reason RFunctionType loc in
              reconstruct_ast (AnyT.make AnnotatedAny reason) None
          )
        | "Object" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              add_unclear_type_error_if_not_lib_file cx loc;
              let reason = mk_annot_reason RObjectType loc in
              reconstruct_ast (AnyT.make AnnotatedAny reason) None
          )
        | "Function$Prototype$Apply" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              let reason = mk_annot_reason RFunctionType loc in
              reconstruct_ast (FunProtoApplyT reason) None
          )
        | "Function$Prototype$Bind" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              let reason = mk_annot_reason RFunctionType loc in
              reconstruct_ast (FunProtoBindT reason) None
          )
        | "Function$Prototype$Call" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              let reason = mk_annot_reason RFunctionType loc in
              reconstruct_ast (FunProtoCallT reason) None
          )
        | "Object$Assign" -> mk_custom_fun cx loc t_ast targs ident ObjectAssign
        | "Object$GetPrototypeOf" -> mk_custom_fun cx loc t_ast targs ident ObjectGetPrototypeOf
        | "Object$SetPrototypeOf" -> mk_custom_fun cx loc t_ast targs ident ObjectSetPrototypeOf
        | "$Compose" -> mk_custom_fun cx loc t_ast targs ident (Compose false)
        | "$ComposeReverse" -> mk_custom_fun cx loc t_ast targs ident (Compose true)
        | "React$AbstractComponent" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (ts, targs) = convert_type_params () in
              let config = List.nth ts 0 in
              let instance = List.nth ts 1 in
              reconstruct_ast
                (DefT
                   ( mk_reason (RCustom "AbstractComponent") loc,
                     infer_trust cx,
                     ReactAbstractComponentT { config; instance }
                   )
                )
                targs
          )
        | "React$Config" ->
          check_type_arg_arity cx loc t_ast targs 2 (fun () ->
              let (ts, targs) = convert_type_params () in
              let props = List.nth ts 0 in
              let default_props = List.nth ts 1 in
              let reason = mk_reason RReactConfig loc in
              reconstruct_ast
                (EvalT
                   ( props,
                     TypeDestructorT (use_op reason, reason, ReactConfigType default_props),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "React$PropType$Primitive" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, typed_targs) = convert_type_params () in
              let t = List.hd ts in
              let prop_type = ReactPropType (React.PropType.Primitive (false, t)) in
              let targ =
                match targs with
                | Some (_, { Ast.Type.TypeArgs.arguments = [t]; comments = _ }) -> t
                | Some _
                | None ->
                  assert false
              in
              let ((_, prop_t), _) = mk_custom_fun cx loc targ None ident prop_type in
              reconstruct_ast prop_t typed_targs
          )
        | "React$PropType$Primitive$Required" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, typed_targs) = convert_type_params () in
              let t = List.hd ts in
              let prop_type = ReactPropType (React.PropType.Primitive (true, t)) in
              let targ =
                match targs with
                | Some (_, { Ast.Type.TypeArgs.arguments = [t]; comments = _ }) -> t
                | Some _
                | None ->
                  assert false
              in
              let ((_, prop_t), _) = mk_custom_fun cx loc targ None ident prop_type in
              reconstruct_ast prop_t typed_targs
          )
        | "React$PropType$ArrayOf" ->
          mk_react_prop_type cx loc t_ast targs ident React.PropType.ArrayOf
        | "React$PropType$InstanceOf" ->
          mk_react_prop_type cx loc t_ast targs ident React.PropType.InstanceOf
        | "React$PropType$ObjectOf" ->
          mk_react_prop_type cx loc t_ast targs ident React.PropType.ObjectOf
        | "React$PropType$OneOf" -> mk_react_prop_type cx loc t_ast targs ident React.PropType.OneOf
        | "React$PropType$OneOfType" ->
          mk_react_prop_type cx loc t_ast targs ident React.PropType.OneOfType
        | "React$PropType$Shape" -> mk_react_prop_type cx loc t_ast targs ident React.PropType.Shape
        | "React$CreateClass" ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              let t = AnyT.at Untyped loc in
              reconstruct_ast t None
          )
        | "React$CreateElement" -> mk_custom_fun cx loc t_ast targs ident ReactCreateElement
        | "React$CloneElement" -> mk_custom_fun cx loc t_ast targs ident ReactCloneElement
        | "React$ElementFactory" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let t =
                match convert_type_params () with
                | ([t], _) -> t
                | _ -> assert false
              in
              let targ =
                match targs with
                | Some (_, { Ast.Type.TypeArgs.arguments = [t]; comments = _ }) -> t
                | Some _
                | None ->
                  assert false
              in
              mk_custom_fun cx loc targ None ident (ReactElementFactory t)
          )
        | "React$ElementProps" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "React$ElementProps")) loc in
              reconstruct_ast
                (EvalT
                   ( t,
                     TypeDestructorT (use_op reason, reason, ReactElementPropsType),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "React$ElementConfig" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "React$ElementConfig")) loc in
              reconstruct_ast
                (EvalT
                   ( t,
                     TypeDestructorT (use_op reason, reason, ReactElementConfigType),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "React$ElementRef" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "React$ElementRef")) loc in
              reconstruct_ast
                (EvalT
                   ( t,
                     TypeDestructorT (use_op reason, reason, ReactElementRefType),
                     mk_eval_id cx loc
                   )
                )
                targs
          )
        | "$Facebookism$IdxUnwrapper" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "$Facebookism$IdxUnwrapper")) loc in
              reconstruct_ast
                (EvalT (t, TypeDestructorT (use_op reason, reason, IdxUnwrapType), mk_eval_id cx loc)
                )
                targs
          )
        | "$Facebookism$IdxWrapper" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              let (ts, targs) = convert_type_params () in
              let t = List.hd ts in
              let reason = mk_reason (RType (OrdinaryName "$Facebookism$IdxWrapper")) loc in
              reconstruct_ast (DefT (reason, infer_trust cx, IdxWrapper t)) targs
          )
        | "$Flow$DebugPrint" -> mk_custom_fun cx loc t_ast targs ident DebugPrint
        | "$Flow$DebugThrow" -> mk_custom_fun cx loc t_ast targs ident DebugThrow
        | "$Flow$DebugSleep" -> mk_custom_fun cx loc t_ast targs ident DebugSleep
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
          let (_, targs) = convert_type_params () in
          reconstruct_ast (AnyT.at AnnotatedAny loc) targs
        (* in-scope type vars *)
        | _ when Subst_name.Map.mem (Subst_name.Name name) tparams_map ->
          check_type_arg_arity cx loc t_ast targs 0 (fun () ->
              let t =
                ConsGen.reposition
                  cx
                  loc
                  ~annot_loc:loc
                  (Subst_name.Map.find (Subst_name.Name name) tparams_map)
              in
              reconstruct_ast t None
          )
        | "$Pred" ->
          let fun_reason = mk_annot_reason (RCustom "abstract predicate function") loc in
          let static_reason = mk_reason (RCustom "abstract predicate static") loc in
          let out_reason = mk_reason (RCustom "open predicate") loc in
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              match convert_type_params () with
              | ([DefT (_, _, SingletonNumT (f, _))], targs) ->
                let n = Base.Int.of_float f in
                let key_strs = Base.List.init n ~f:(fun i -> Some ("x_" ^ Base.Int.to_string i)) in
                let emp = Key_map.empty in
                let tins = Base.List.init n ~f:(fun _ -> Unsoundness.at FunctionPrototype loc) in
                let tout =
                  OpenPredT
                    {
                      reason = out_reason;
                      base_t = MixedT.at loc |> with_trust bogus_trust;
                      m_pos = emp;
                      m_neg = emp;
                    }
                in
                reconstruct_ast
                  (DefT
                     ( fun_reason,
                       infer_trust cx,
                       FunT
                         ( dummy_static static_reason,
                           mk_functiontype
                             fun_reason
                             tins
                             tout
                             ~rest_param:None
                             ~def_reason:fun_reason
                             ~params_names:key_strs
                             ~is_predicate:true
                         )
                     )
                  )
                  targs
              | _ -> error_type cx loc (Error_message.EPredAnnot loc) t_ast
          )
        | "$Refine" ->
          check_type_arg_arity cx loc t_ast targs 3 (fun () ->
              match convert_type_params () with
              | ([base_t; fun_pred_t; DefT (_, _, SingletonNumT (f, _))], targs) ->
                let idx = Base.Int.of_float f in
                let reason = mk_reason (RCustom "refined type") loc in
                let pred = LatentP (fun_pred_t, idx) in
                reconstruct_ast
                  (EvalT (base_t, LatentPredT (reason, pred), mk_eval_id cx loc))
                  targs
              | _ -> error_type cx loc (Error_message.ERefineAnnot loc) t_ast
          )
        | "$Trusted" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              match convert_type_params () with
              | ([AnyT _], _) -> error_type cx loc (Error_message.ETrustedAnnot loc) t_ast
              | ([DefT (rs, trust, ty)], targs) ->
                let trust = make_trusted cx trust (Error_message.ETrustedAnnot loc) in
                reconstruct_ast
                  (DefT (mk_annot_reason (RTrusted (desc_of_reason rs)) loc, trust, ty))
                  targs
              | _ -> error_type cx loc (Error_message.ETrustedAnnot loc) t_ast
          )
        | "$Private" ->
          check_type_arg_arity cx loc t_ast targs 1 (fun () ->
              match convert_type_params () with
              | ([AnyT _], _) -> error_type cx loc (Error_message.EPrivateAnnot loc) t_ast
              | ([DefT (rs, trust, ty)], targs) ->
                let trust = make_private cx trust (Error_message.EPrivateAnnot loc) in
                reconstruct_ast
                  (DefT (mk_annot_reason (RPrivate (desc_of_reason rs)) loc, trust, ty))
                  targs
              | _ -> error_type cx loc (Error_message.EPrivateAnnot loc) t_ast
          )
        (* other applications with id as head expr *)
        | _ ->
          let reason = mk_reason (RType (OrdinaryName name)) loc in
          let c = type_identifier cx name name_loc in
          let (t, targs) = mk_nominal_type cx reason tparams_map (c, targs) in
          reconstruct_ast t ~id_t:c targs
      end
    | ( loc,
        Function
          {
            Function.params =
              (params_loc, { Function.Params.params; rest; this_; comments = params_comments });
            return;
            tparams;
            comments = func_comments;
          }
      ) ->
      let (tparams, tparams_map, tparams_ast) =
        mk_type_param_declarations cx ~tparams_map tparams
      in
      let (rev_params, rev_param_asts) =
        List.fold_left
          (fun (params_acc, asts_acc) (param_loc, param) ->
            let { Function.Param.name; annot; optional } = param in
            let (((_, t), _) as annot_ast) = convert cx tparams_map annot in
            let t =
              if optional then
                TypeUtil.optional t
              else
                t
            in
            let name = Base.Option.map ~f:(fun (loc, id_name) -> ((loc, t), id_name)) name in
            ( (Base.Option.map ~f:ident_name name, t) :: params_acc,
              (param_loc, { Function.Param.name; annot = annot_ast; optional }) :: asts_acc
            ))
          ([], [])
          params
      in
      let (this_t, this_param_ast) =
        match this_ with
        | None -> (bound_function_dummy_this params_loc, None)
        | Some (this_loc, { Function.ThisParam.annot = (loc, annot); comments }) ->
          let (((_, this_t), _) as annot) = convert cx tparams_map annot in
          (this_t, Some (this_loc, { Function.ThisParam.annot = (loc, annot); comments }))
      in
      let reason = mk_annot_reason RFunctionType loc in
      let (rest_param, rest_param_ast) =
        match rest with
        | Some (rest_loc, { Function.RestParam.argument = (param_loc, param); comments }) ->
          let { Function.Param.name; annot; optional } = param in
          let (((_, rest), _) as annot_ast) = convert cx tparams_map annot in
          ( Some (Base.Option.map ~f:ident_name name, loc_of_t rest, rest),
            Some
              ( rest_loc,
                {
                  Function.RestParam.argument =
                    ( param_loc,
                      {
                        Function.Param.name =
                          Base.Option.map ~f:(fun (loc, id_name) -> ((loc, rest), id_name)) name;
                        annot = annot_ast;
                        optional;
                      }
                    );
                  comments;
                }
              )
          )
        | None -> (None, None)
      in
      let (((_, return_t), _) as return_ast) = convert cx tparams_map return in
      let statics_t =
        let reason = update_desc_reason (fun d -> RStatics d) reason in
        Obj_type.mk_with_proto cx reason (FunProtoT reason) ~obj_kind:Inexact ?call:None
      in
      let ft =
        DefT
          ( reason,
            infer_trust cx,
            FunT
              ( statics_t,
                {
                  this_t = (this_t, This_Function);
                  params = List.rev rev_params;
                  rest_param;
                  return_t;
                  is_predicate = false;
                  def_reason = reason;
                }
              )
          )
      in
      let t =
        match tparams with
        | None -> ft
        | Some (tparams_loc, tparams_nel) ->
          let id = Context.make_source_poly_id cx tparams_loc in
          poly_type id tparams_loc tparams_nel ft
      in
      ( (loc, t),
        Function
          {
            Function.params =
              ( params_loc,
                {
                  Function.Params.params = List.rev rev_param_asts;
                  rest = rest_param_ast;
                  this_ = this_param_ast;
                  comments = params_comments;
                }
              );
            return = return_ast;
            tparams = tparams_ast;
            comments = func_comments;
          }
      )
    | (loc, Object { Object.exact; properties; inexact; comments }) ->
      let exact_by_default = Context.exact_by_default cx in
      let exact_type = exact || ((not inexact) && exact_by_default) in
      let (t, properties) = convert_object cx tparams_map loc ~exact:exact_type properties in
      let has_indexer =
        properties
        |> List.exists (fun property ->
               match property with
               | Ast.Type.Object.Indexer _ -> true
               | _ -> false
           )
      in
      if (not exact) && (not inexact) && not has_indexer then (
        Flow_js_utils.add_output cx Error_message.(EAmbiguousObjectType loc);
        if not exact_by_default then
          Flow_js_utils.add_output cx Error_message.(EImplicitInexactObject loc)
      );
      ((loc, t), Object { Object.exact; properties; inexact; comments })
    | (loc, Interface { Interface.extends; body; comments }) ->
      let ( body_loc,
            { Ast.Type.Object.properties; exact; inexact = _inexact; comments = object_comments }
          ) =
        body
      in
      let reason = mk_annot_reason RInterfaceType loc in
      let (iface_sig, extend_asts) =
        let id = ALoc.id_none in
        let (extends, extend_asts) =
          extends |> Base.List.map ~f:(mk_interface_super cx tparams_map) |> List.split
        in
        let super =
          let callable =
            List.exists
              Ast.Type.Object.(
                function
                | CallProperty (_, { CallProperty.static; _ }) -> not static
                | _ -> false
              )
              properties
          in
          Class_type_sig.Types.Interface { Class_type_sig.Types.inline = true; extends; callable }
        in
        (Class_type_sig.empty id loc reason None tparams_map super, extend_asts)
      in
      let (iface_sig, property_asts) =
        add_interface_properties
          cx
          tparams_map
          properties
          ~this:(implicit_mixed_this reason)
          iface_sig
      in
      Class_type_sig.check_super cx reason iface_sig;
      Class_type_sig.check_implements cx reason iface_sig;
      Class_type_sig.check_methods cx reason iface_sig;
      ( (loc, Class_type_sig.thistype cx iface_sig),
        Interface
          {
            Interface.body =
              ( body_loc,
                {
                  Object.exact;
                  inexact = false;
                  properties = property_asts;
                  comments = object_comments;
                }
              );
            extends = extend_asts;
            comments;
          }
      )
    | (loc, (Exists _ as t_ast)) ->
      add_unclear_type_error_if_not_lib_file cx loc;
      ((loc, AnyT.at AnnotatedAny loc), t_ast)

  and convert_list =
    let rec loop (ts, tasts) cx tparams_map = function
      | [] -> (List.rev ts, List.rev tasts)
      | ast :: asts ->
        let (((_, t), _) as tast) = convert cx tparams_map ast in
        loop (t :: ts, tast :: tasts) cx tparams_map asts
    in
    (fun cx tparams_map asts -> loop ([], []) cx tparams_map asts)

  and convert_opt cx tparams_map ast_opt =
    let tast_opt = Base.Option.map ~f:(convert cx tparams_map) ast_opt in
    let t_opt = Base.Option.map ~f:(fun ((_, x), _) -> x) tast_opt in
    (t_opt, tast_opt)

  and convert_temporary_object = function
    | ExactT (_, DefT (r, trust, ObjT o))
    | DefT (r, trust, ObjT o) ->
      let r = replace_desc_reason RObjectLit r in
      let obj_kind =
        match o.flags.obj_kind with
        | Indexed _ -> o.flags.obj_kind
        | _ -> Exact
      in
      DefT (r, trust, ObjT { o with flags = { obj_kind; frozen = false } })
    | EvalT (l, TypeDestructorT (use_op, r, SpreadType (_, ts, head_slice)), id) ->
      let r = replace_desc_reason RObjectLit r in
      let target =
        let open Type.Object.Spread in
        Value { make_seal = Sealed }
      in
      EvalT (l, TypeDestructorT (use_op, r, SpreadType (target, ts, head_slice)), id)
    | t -> t

  and convert_qualification ?(lookup_mode = ForType) cx reason_prefix =
    let open Ast.Type.Generic.Identifier in
    function
    | Qualified (loc, { qualification; id }) as qualified ->
      let (m, qualification) = convert_qualification ~lookup_mode cx reason_prefix qualification in
      let (id_loc, id_name) = id in
      let { Ast.Identifier.name; comments = _ } = id_name in
      let desc = RCustom (spf "%s `%s`" reason_prefix (qualified_name qualified)) in
      let id_reason = mk_reason desc id_loc in
      let use_op =
        Op (GetProperty (mk_reason (RType (OrdinaryName (qualified_name qualified))) loc))
      in
      let t = ConsGen.get_prop cx use_op id_reason (OrdinaryName name) m in
      (t, Qualified (loc, { qualification; id = ((id_loc, t), id_name) }))
    | Unqualified (loc, ({ Ast.Identifier.name; comments = _ } as id_name)) ->
      let t = Env.get_var ~lookup_mode cx name loc in
      (t, Unqualified ((loc, t), id_name))

  and convert_typeof cx reason_prefix =
    let open Ast.Type.Typeof.Target in
    function
    | Qualified (loc, { qualification; id }) as qualified ->
      let (m, qualification) = convert_typeof cx reason_prefix qualification in
      let (id_loc, id_name) = id in
      let { Ast.Identifier.name; comments = _ } = id_name in
      let desc = RCustom (spf "%s `%s`" reason_prefix (typeof_name qualified)) in
      let id_reason = mk_reason desc id_loc in
      let use_op =
        Op (GetProperty (mk_reason (RType (OrdinaryName (typeof_name qualified))) loc))
      in
      let t = ConsGen.get_prop cx use_op id_reason (OrdinaryName name) m in
      (t, Qualified ((loc, t), { qualification; id = ((id_loc, t), id_name) }))
    | Unqualified (loc, ({ Ast.Identifier.name; comments = _ } as id_name)) ->
      let t =
        if Type_inference_hooks_js.dispatch_id_hook cx name loc then
          Unsoundness.at InferenceHooks loc
        else
          Env.get_var ~lookup_mode:ForTypeof cx name loc
      in
      (t, Unqualified ((loc, t), id_name))

  and convert_object =
    let obj_proto_t = ObjProtoT (locationless_reason RObjectPrototype) in
    let fun_proto_t = FunProtoT (locationless_reason RFunctionPrototype) in
    let module Acc = struct
      type element =
        | Spread of Type.t
        | Slice of {
            dict: Type.dicttype option;
            pmap: Type.Properties.t;
          }

      type t = {
        dict: Type.dicttype option;
        pmap: Type.Properties.t;
        tail: element list;
        proto: Type.t option;
        calls: Type.t list;
      }

      let empty = { dict = None; pmap = NameUtils.Map.empty; tail = []; proto = None; calls = [] }

      let empty_slice = Slice { dict = None; pmap = NameUtils.Map.empty }

      let head_slice { dict; pmap; _ } =
        if dict = None && NameUtils.Map.is_empty pmap then
          None
        else
          Some (Slice { dict; pmap })

      let add_call c = function
        | { proto = Some _; _ } -> Error Error_message.ExplicitCallAfterProto
        | acc -> Ok { acc with calls = c :: acc.calls }

      let add_dict d = function
        | { dict = Some _; _ } -> Error Error_message.MultipleIndexers
        | acc -> Ok { acc with dict = Some d }

      let add_prop f acc = { acc with pmap = f acc.pmap }

      let add_proto p = function
        | { proto = Some _; _ } -> Error Error_message.MultipleProtos
        | { calls = _ :: _; _ } -> Error Error_message.ExplicitProtoAfterCall
        | acc -> Ok { acc with proto = Some p }

      let add_spread t acc =
        let tail =
          match head_slice acc with
          | None -> acc.tail
          | Some slice -> slice :: acc.tail
        in
        { acc with dict = None; pmap = NameUtils.Map.empty; tail = Spread t :: tail }

      let elements_rev acc =
        match head_slice acc with
        | Some slice -> (slice, acc.tail)
        | None ->
          (match acc.tail with
          | [] -> (empty_slice, [])
          | x :: xs -> (x, xs))

      let proto = function
        | { proto = Some t; _ } -> t
        | { calls = _ :: _; _ } -> fun_proto_t
        | _ -> obj_proto_t

      let calls_rev acc = acc.calls
    end in
    let mk_object cx loc ~src_loc ~exact call dict pmap proto =
      let pmap =
        if src_loc && Env.in_toplevel_scope cx then
          Context.make_source_property_map cx pmap loc
        else
          Context.generate_property_map cx pmap
      in
      let call = Base.Option.map ~f:(Context.make_call_prop cx) call in
      let obj_kind =
        match dict with
        | Some d -> Indexed d
        | None ->
          if exact then
            Exact
          else
            Inexact
      in
      let flags = { obj_kind; frozen = false } in
      DefT
        ( mk_annot_reason RObjectType loc,
          infer_trust cx,
          ObjT (mk_objecttype ~flags ~call pmap proto)
        )
    in
    let mk_object_annot cx loc ~exact call dict pmap proto =
      let exact = exact && dict = None in
      let t = mk_object cx loc ~src_loc:true ~exact call dict pmap proto in
      if exact then
        ExactT (mk_annot_reason (RExactType RObjectType) loc, t)
      else
        t
    in
    let open Ast.Type in
    let named_property cx tparams_map loc acc prop =
      match prop with
      | { Object.Property.key; value = Object.Property.Init value; optional; variance; _method; _ }
        -> begin
        match key with
        | Ast.Expression.Object.Property.Literal
            (loc, { Ast.Literal.value = Ast.Literal.String name; _ })
        | Ast.Expression.Object.Property.Identifier (loc, { Ast.Identifier.name; comments = _ }) ->
          Type_inference_hooks_js.dispatch_obj_type_prop_decl_hook cx name loc;
          let (((_, t), _) as value_ast) = convert cx tparams_map value in
          let prop_ast t =
            {
              prop with
              Object.Property.key =
                begin
                  match key with
                  | Ast.Expression.Object.Property.Literal (_, lit) ->
                    Ast.Expression.Object.Property.Literal ((loc, t), lit)
                  | Ast.Expression.Object.Property.Identifier
                      (_loc, { Ast.Identifier.name = _; comments = comments_inner }) ->
                    Ast.Expression.Object.Property.Identifier
                      ((loc, t), { Ast.Identifier.name; comments = comments_inner })
                  | _ -> assert_false "branch invariant"
                end;
              value = Object.Property.Init value_ast;
            }
          in
          if name = "__proto__" && (not (_method || optional)) && variance = None then
            let reason = mk_reason RPrototype (fst value) in
            let proto = ConsGen.obj_test_proto cx reason t in
            let acc =
              match Acc.add_proto (ConsGen.mk_typeof_annotation cx reason proto) acc with
              | Ok acc -> acc
              | Error err ->
                Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, err));
                acc
            in
            (acc, prop_ast proto)
          else
            let t =
              if optional then
                TypeUtil.optional t
              else
                t
            in
            let prop =
              if _method then
                Properties.add_method (OrdinaryName name) (Some loc) t
              else
                Properties.add_field (OrdinaryName name) (polarity cx variance) (Some loc) t
            in
            (Acc.add_prop prop acc, prop_ast t)
        | Ast.Expression.Object.Property.Literal (loc, _)
        | Ast.Expression.Object.Property.PrivateName (loc, _)
        | Ast.Expression.Object.Property.Computed (loc, _) ->
          Flow_js_utils.add_output cx (Error_message.EUnsupportedKeyInObjectType loc);
          let (_, prop_ast) = Tast_utils.error_mapper#object_property_type (loc, prop) in
          (acc, prop_ast)
      end
      (* unsafe getter property *)
      | {
       Object.Property.key =
         Ast.Expression.Object.Property.Identifier
           (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name));
       value = Object.Property.Get ((loc, _) as getter);
       _method;
       _;
      } ->
        Flow_js_utils.add_output cx (Error_message.EUnsafeGettersSetters loc);
        let (function_type, getter_ast) = mk_function_type_annotation cx tparams_map getter in
        let return_t = Type.extract_getter_type function_type in
        ( Acc.add_prop (Properties.add_getter (OrdinaryName name) (Some id_loc) return_t) acc,
          {
            prop with
            Object.Property.key =
              Ast.Expression.Object.Property.Identifier ((id_loc, return_t), id_name);
            value = Object.Property.Get getter_ast;
          }
        )
      (* unsafe setter property *)
      | {
       Object.Property.key =
         Ast.Expression.Object.Property.Identifier
           (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name));
       value = Object.Property.Set ((loc, _) as setter);
       _method;
       _;
      } ->
        Flow_js_utils.add_output cx (Error_message.EUnsafeGettersSetters loc);
        let (function_type, setter_ast) = mk_function_type_annotation cx tparams_map setter in
        let param_t = Type.extract_setter_type function_type in
        ( Acc.add_prop (Properties.add_setter (OrdinaryName name) (Some id_loc) param_t) acc,
          {
            prop with
            Object.Property.key =
              Ast.Expression.Object.Property.Identifier ((id_loc, param_t), id_name);
            value = Object.Property.Set setter_ast;
          }
        )
      | { Object.Property.value = Object.Property.Get _ | Object.Property.Set _; _ } ->
        Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, ObjectPropertyGetSet));
        let (_, prop_ast) = Tast_utils.error_mapper#object_property_type (loc, prop) in
        (acc, prop_ast)
    in
    let make_call cx tparams_map loc call =
      let { Object.CallProperty.value = (fn_loc, fn); static; comments } = call in
      (* note: this uses [loc] instead of [fn_loc]. not sure if this is intentional. *)
      let (t, (_, fn)) = mk_function_type_annotation cx tparams_map (loc, fn) in
      (t, { Object.CallProperty.value = (fn_loc, fn); static; comments })
    in
    let make_dict cx tparams_map indexer =
      let { Object.Indexer.id; key; value; static; variance; comments } = indexer in
      let (((_, key), _) as key_ast) = convert cx tparams_map key in
      let (((_, value), _) as value_ast) = convert cx tparams_map value in
      ( {
          Type.dict_name = Base.Option.map ~f:ident_name id;
          key;
          value;
          dict_polarity = polarity cx variance;
        },
        { Object.Indexer.id; key = key_ast; value = value_ast; static; variance; comments }
      )
    in
    let property cx tparams_map acc =
      Object.(
        function
        | CallProperty (loc, call) ->
          let (t, call) = make_call cx tparams_map loc call in
          let acc =
            match Acc.add_call t acc with
            | Ok acc -> acc
            | Error err ->
              Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, err));
              acc
          in
          (acc, CallProperty (loc, call))
        | Indexer (loc, i) ->
          let (d, i) = make_dict cx tparams_map i in
          let acc =
            match Acc.add_dict d acc with
            | Ok acc -> acc
            | Error err ->
              Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, err));
              acc
          in
          (acc, Indexer (loc, i))
        | Property (loc, p) ->
          let (acc, p) = named_property cx tparams_map loc acc p in
          (acc, Property (loc, p))
        | InternalSlot (loc, slot) as prop ->
          let {
            Object.InternalSlot.id = (_, { Ast.Identifier.name; comments = _ });
            value;
            static = _;
            (* object props are never static *)
            optional;
            _method = _;
            comments = _;
          } =
            slot
          in
          if name = "call" then
            let (((_, t), _) as value_ast) = convert cx tparams_map value in
            let t =
              if optional then
                TypeUtil.optional t
              else
                t
            in
            let acc =
              match Acc.add_call t acc with
              | Ok acc -> acc
              | Error err ->
                Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, err));
                acc
            in
            (acc, InternalSlot (loc, { slot with Object.InternalSlot.value = value_ast }))
          else (
            Flow_js_utils.add_output
              cx
              Error_message.(
                EUnsupportedSyntax (loc, UnsupportedInternalSlot { name; static = false })
              );
            (acc, Tast_utils.error_mapper#object_type_property prop)
          )
        | SpreadProperty (loc, { Object.SpreadProperty.argument; comments }) ->
          let (((_, t), _) as argument_ast) = convert cx tparams_map argument in
          ( Acc.add_spread t acc,
            SpreadProperty (loc, { SpreadProperty.argument = argument_ast; comments })
          )
      )
    in
    fun cx tparams_map loc ~exact properties ->
      let (acc, rev_prop_asts) =
        List.fold_left
          (fun (acc, rev_prop_asts) p ->
            let (acc, prop_ast) = property cx tparams_map acc p in
            (acc, prop_ast :: rev_prop_asts))
          (Acc.empty, [])
          properties
      in
      let proto = Acc.proto acc in
      let calls_rev = Acc.calls_rev acc in
      let t =
        match Acc.elements_rev acc with
        | (Acc.Slice { dict; pmap }, []) ->
          let ts =
            List.rev_map
              (fun call -> mk_object_annot cx loc ~exact (Some call) dict pmap proto)
              calls_rev
          in
          (match ts with
          | [] -> mk_object_annot cx loc ~exact None dict pmap proto
          | [t] -> t
          | t0 :: t1 :: ts ->
            let callable_reason = mk_annot_reason (RCustom "callable object type") loc in
            let rep = InterRep.make t0 t1 ts in
            IntersectionT (callable_reason, rep))
        | os ->
          Type.Object.Spread.(
            let reason = mk_reason RObjectType loc in
            let target = Annot { make_exact = exact } in
            let (t, ts, head_slice) =
              let (t, ts) = os in
              (* We don't need to do this recursively because every pair of slices must be separated
               * by a spread *)
              match (t, ts) with
              | (Acc.Spread t, ts) ->
                let ts =
                  Base.List.map
                    ~f:(function
                      | Acc.Spread t -> Type t
                      | Acc.Slice { dict; pmap } ->
                        Slice
                          {
                            Type.Object.Spread.reason;
                            prop_map = pmap;
                            dict;
                            generics = Generic_ID.spread_empty;
                          })
                    ts
                in
                (t, ts, None)
              | (Acc.Slice { dict; pmap = prop_map }, Acc.Spread t :: ts) ->
                let head_slice =
                  { Type.Object.Spread.reason; prop_map; dict; generics = Generic_ID.spread_empty }
                in
                let ts =
                  Base.List.map
                    ~f:(function
                      | Acc.Spread t -> Type t
                      | Acc.Slice { dict; pmap } ->
                        Slice
                          {
                            Type.Object.Spread.reason;
                            prop_map = pmap;
                            dict;
                            generics = Generic_ID.spread_empty;
                          })
                    ts
                in
                (t, ts, Some head_slice)
              | _ -> failwith "Invariant Violation: spread list has two slices in a row"
            in
            let l = ConsGen.widen_obj_type cx ~use_op:unknown_use reason t in
            EvalT
              ( l,
                TypeDestructorT (unknown_use, reason, SpreadType (target, ts, head_slice)),
                Type.Eval.generate_id ()
              )
          )
      in
      (t, List.rev rev_prop_asts)

  and convert_tuple_element cx tparams_map (loc, el) =
    match el with
    | Ast.Type.Tuple.UnlabeledElement annot ->
      let (((_, t), _) as annot_ast) = convert cx tparams_map annot in
      let element_ast = (loc, Ast.Type.Tuple.UnlabeledElement annot_ast) in
      (t, TupleElement { name = None; t; polarity = Polarity.Neutral }, element_ast)
    | Ast.Type.Tuple.LabeledElement
        { Ast.Type.Tuple.LabeledElement.name; annot; variance; optional } ->
      let (((_, annot_t), _) as annot_ast) = convert cx tparams_map annot in
      let t =
        if optional then (
          Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, TupleOptionalElement));
          AnyT.at (AnyError None) loc
        ) else
          annot_t
      in
      let (name_loc, ({ Ast.Identifier.name = str_name; _ } as name_ast)) = name in
      let id_name = ((name_loc, t), name_ast) in
      let element_ast =
        ( loc,
          Ast.Type.Tuple.LabeledElement
            { Ast.Type.Tuple.LabeledElement.name = id_name; annot = annot_ast; variance; optional }
        )
      in
      (t, TupleElement { name = Some str_name; t; polarity = polarity cx variance }, element_ast)
    | Ast.Type.Tuple.SpreadElement spread_el ->
      Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, TupleSpreadElement));
      let t = AnyT.at (AnyError None) loc in
      let element_ast =
        (loc, Ast.Type.Tuple.SpreadElement (Tast_utils.error_mapper#tuple_spread_element spread_el))
      in
      (t, TupleElement { name = None; t; polarity = Polarity.Neutral }, element_ast)

  and mk_func_sig =
    let open Ast.Type.Function in
    let add_param cx tparams_map x param =
      let (loc, { Param.name; annot; optional }) = param in
      let (((_, t), _) as annot) = convert cx tparams_map annot in
      let name = Base.Option.map ~f:(fun (loc, id_name) -> ((loc, t), id_name)) name in
      let param = (t, (loc, { Param.name; annot; optional })) in
      Func_type_params.add_param param x
    in
    let add_rest cx tparams_map x rest_param =
      let (rest_loc, { RestParam.argument = (loc, { Param.name; annot; optional }); comments }) =
        rest_param
      in
      let (((_, t), _) as annot) = convert cx tparams_map annot in
      let name = Base.Option.map ~f:(fun (loc, id_name) -> ((loc, t), id_name)) name in
      let rest =
        (t, (rest_loc, { RestParam.argument = (loc, { Param.name; annot; optional }); comments }))
      in
      Func_type_params.add_rest rest x
    in
    let add_this cx tparams_map x this_param =
      let (this_loc, { ThisParam.annot = (loc, annot); comments }) = this_param in
      let (((_, t), _) as annot') = convert cx tparams_map annot in
      let this = (t, (this_loc, { Ast.Type.Function.ThisParam.annot = (loc, annot'); comments })) in
      Func_type_params.add_this this x
    in
    let convert_params cx tparams_map (loc, { Params.params; rest; this_; comments }) =
      let fparams =
        Func_type_params.empty (fun params rest this_ ->
            Some (loc, { Params.params; rest; this_; comments })
        )
      in
      let fparams = List.fold_left (add_param cx tparams_map) fparams params in
      let fparams = Base.Option.fold ~f:(add_rest cx tparams_map) ~init:fparams rest in
      let fparams = Base.Option.fold ~f:(add_this cx tparams_map) ~init:fparams this_ in
      let params_ast = Func_type_params.eval cx fparams in
      (fparams, Base.Option.value_exn params_ast)
    in
    fun cx tparams_map loc func ->
      let (tparams, tparams_map, tparams_ast) =
        mk_type_param_declarations cx ~tparams_map func.tparams
      in
      let (fparams, params_ast) = convert_params cx tparams_map func.Ast.Type.Function.params in
      let (((_, return_t), _) as return_ast) = convert cx tparams_map func.return in
      let reason = mk_annot_reason RFunctionType loc in
      ( {
          Func_type_sig.Types.reason;
          kind = Func_class_sig_types.Func.Ordinary;
          tparams;
          tparams_map;
          fparams;
          body = None;
          return_t = Annotated return_t;
          statics = None;
        },
        {
          Ast.Type.Function.tparams = tparams_ast;
          params = params_ast;
          return = return_ast;
          comments = None;
        }
      )

  and mk_type cx tparams_map reason = function
    | None ->
      let t = Tvar.mk cx reason in
      (t, None)
    | Some annot ->
      let (((_, t), _) as annot_ast) = convert cx tparams_map annot in
      (t, Some annot_ast)

  and mk_type_annotation cx tparams_map reason = function
    | T.Missing loc ->
      let t =
        if Context.in_synthesis_mode cx then
          Context.mk_placeholder cx reason
        else
          Tvar.mk cx reason
      in
      (Inferred t, T.Missing (loc, t))
    | T.Available annot ->
      let (t, ast_annot) = mk_type_available_annotation cx tparams_map annot in
      (Annotated t, T.Available ast_annot)

  and mk_return_type_annotation cx tparams_map reason ~void_return ~async annot =
    match annot with
    | T.Missing loc when void_return ->
      let void_t = VoidT.why reason |> with_trust literal_trust in
      let t =
        if async then
          let reason = mk_annot_reason (RType (OrdinaryName "Promise")) (aloc_of_reason reason) in
          Flow.get_builtin_typeapp cx reason (OrdinaryName "Promise") [void_t]
        else
          void_t
      in
      (Inferred t, T.Missing (loc, t))
    (* TODO we could probably take the same shortcut for functions with an explicit `void` annotation
       and no explicit returns *)
    | _ -> mk_type_annotation cx tparams_map reason annot

  and mk_type_available_annotation cx tparams_map (loc, annot) =
    let node_cache = Context.node_cache cx in
    let (((_, t), _) as annot_ast) =
      match (Node_cache.get_annotation node_cache loc, annot) with
      | ( _,
          ( _,
            Ast.Type.Generic
              {
                Ast.Type.Generic.id =
                  Ast.Type.Generic.Identifier.Unqualified (_, { Ast.Identifier.name; comments = _ });
                targs = _;
                comments = _;
              }
          )
        )
        when Subst_name.Map.mem (Subst_name.Name name) tparams_map ->
        (* If the type we're converting is in the tparams map, we prefer that over
           the node cache *)
        convert cx tparams_map annot
      | (Some (_, node), _) ->
        Debug_js.Verbose.print_if_verbose_lazy
          cx
          (lazy [spf "Annotation cache hit at %s" (ALoc.debug_to_string loc)]);
        node
      | (None, _) -> convert cx tparams_map annot
    in
    (t, (loc, annot_ast))

  and mk_function_type_annotation cx tparams_map (loc, f) =
    match convert cx tparams_map (loc, Ast.Type.Function f) with
    | ((_, function_type), Ast.Type.Function f_ast) -> (function_type, (loc, f_ast))
    | _ -> assert false

  and mk_singleton_string cx loc key =
    let reason = mk_annot_reason (RStringLit (OrdinaryName key)) loc in
    DefT (reason, infer_trust cx, SingletonStrT (OrdinaryName key))

  and mk_singleton_number cx loc num raw =
    let reason = mk_annot_reason (RNumberLit raw) loc in
    DefT (reason, infer_trust cx, SingletonNumT (num, raw))

  and mk_singleton_boolean cx loc b =
    let reason = mk_annot_reason (RBooleanLit b) loc in
    DefT (reason, infer_trust cx, SingletonBoolT b)

  and mk_singleton_bigint cx loc num raw =
    let reason = mk_annot_reason (RBigIntLit raw) loc in
    DefT (reason, infer_trust cx, SingletonBigIntT (num, raw))

  (* Given the type of expression C and type arguments T1...Tn, return the type of
     values described by C<T1,...,Tn>, or C when there are no type arguments. *)
  and mk_nominal_type cx reason tparams_map (c, targs) =
    let annot_loc = aloc_of_reason reason in
    match targs with
    | None ->
      let reason = annot_reason ~annot_loc reason in
      (ConsGen.mk_instance cx reason c, None)
    | Some (loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
      let (targs, targs_ast) = convert_list cx tparams_map targs in
      ( typeapp_annot annot_loc c targs,
        Some (loc, { Ast.Type.TypeArgs.arguments = targs_ast; comments })
      )

  and mk_type_param cx tparams_map (loc, type_param) =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_tparam node_cache loc with
    | Some x -> x
    | None ->
      let {
        Ast.Type.TypeParam.name = (name_loc, { Ast.Identifier.name; comments = _ }) as id;
        bound;
        bound_kind;
        variance;
        default;
      } =
        type_param
      in
      let reason = mk_annot_reason (RType (OrdinaryName name)) name_loc in
      let polarity = polarity cx variance in
      (match bound_kind with
      | Ast.Type.TypeParam.Extends ->
        Flow_js_utils.add_output
          cx
          (Error_message.ETSSyntax { kind = Error_message.TSTypeParamExtends; loc });
        let t = AnyT.at (AnyError None) loc in
        let tparam =
          {
            reason;
            name = Subst_name.Name name;
            bound = t;
            polarity;
            default = None;
            is_this = false;
          }
        in
        let ast = Tast_utils.error_mapper#type_param (loc, type_param) in
        (ast, tparam, t)
      | Ast.Type.TypeParam.Colon ->
        let (bound, bound_ast) =
          match bound with
          | Ast.Type.Missing loc ->
            let t = DefT (reason, infer_trust cx, MixedT Mixed_everything) in
            (t, Ast.Type.Missing (loc, t))
          | Ast.Type.Available (bound_loc, u) ->
            let (bound, bound_ast) = mk_type cx tparams_map reason (Some u) in
            let bound_ast =
              match bound_ast with
              | Some ast -> Ast.Type.Available (bound_loc, ast)
              | None -> Ast.Type.Missing (bound_loc, bound)
            in
            (bound, bound_ast)
        in
        let (default, default_ast) =
          match default with
          | None -> (None, None)
          | Some default ->
            let (t, default_ast) = mk_type cx tparams_map reason (Some default) in
            ConsGen.subtype_check cx t bound;
            (Some t, default_ast)
        in
        let tparam =
          { reason; name = Subst_name.Name name; bound; polarity; default; is_this = false }
        in
        let t = Flow_js_utils.generic_of_tparam ~f:(fun x -> x) cx tparam in
        let name_ast =
          let (loc, id_name) = id in
          (loc, id_name)
        in
        let ast =
          ( loc,
            {
              Ast.Type.TypeParam.name = name_ast;
              bound = bound_ast;
              bound_kind;
              variance;
              default = default_ast;
            }
          )
        in
        (ast, tparam, t))

  (* take a list of AST type param declarations,
     do semantic checking and create types for them. *)
  and mk_type_param_declarations cx ?(tparams_map = Subst_name.Map.empty) tparams =
    let add_type_param (tparams, tparams_map, bounds_map, rev_asts) (loc, type_param) =
      let (ast, ({ name; bound; _ } as tparam), t) =
        mk_type_param cx tparams_map (loc, type_param)
      in

      let tparams = tparam :: tparams in
      ( tparams,
        Subst_name.Map.add name t tparams_map,
        Subst_name.Map.add name (Subst.subst cx bounds_map bound) bounds_map,
        ast :: rev_asts
      )
    in
    match tparams with
    | None -> (None, tparams_map, None)
    | Some (tparams_loc, { Ast.Type.TypeParams.params = tparams; comments }) ->
      let (rev_tparams, tparams_map, _, rev_asts) =
        List.fold_left add_type_param ([], tparams_map, Subst_name.Map.empty, []) tparams
      in
      let tparams_ast =
        Some (tparams_loc, { Ast.Type.TypeParams.params = List.rev rev_asts; comments })
      in
      let tparams =
        match List.rev rev_tparams with
        | [] -> None
        | hd :: tl -> Some (tparams_loc, (hd, tl))
      in
      (tparams, tparams_map, tparams_ast)

  and type_identifier cx name loc =
    if Type_inference_hooks_js.dispatch_id_hook cx name loc then
      Unsoundness.at InferenceHooks loc
    else if name = "undefined" then
      VoidT.at loc |> with_trust_inference cx
    else
      Env.var_ref ~lookup_mode:ForType cx (OrdinaryName name) loc

  and mk_interface_super cx tparams_map (loc, { Ast.Type.Generic.id; targs; comments }) =
    let lookup_mode = Env.LookupMode.ForType in
    let (c, id) = convert_qualification ~lookup_mode cx "extends" id in
    let (typeapp, targs) =
      match targs with
      | None -> ((loc, c, None), None)
      | Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
        let (ts, targs_ast) = convert_list cx tparams_map targs in
        ((loc, c, Some ts), Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs_ast; comments }))
    in
    (typeapp, (loc, { Ast.Type.Generic.id; targs; comments }))

  and add_interface_properties cx ~this tparams_map properties s =
    let open Class_type_sig in
    let open Class_type_sig.Types in
    let (x, rev_prop_asts) =
      List.fold_left
        Ast.Type.Object.(
          fun (x, rev_prop_asts) -> function
            | CallProperty (loc, { CallProperty.value; static; comments }) ->
              let (t, value) = mk_function_type_annotation cx tparams_map value in
              ( append_call ~static t x,
                CallProperty (loc, { CallProperty.value; static; comments }) :: rev_prop_asts
              )
            | Indexer (loc, { Indexer.static; _ }) as indexer_prop when mem_field ~static "$key" x
              ->
              Flow_js_utils.add_output cx Error_message.(EUnsupportedSyntax (loc, MultipleIndexers));
              (x, Tast_utils.error_mapper#object_type_property indexer_prop :: rev_prop_asts)
            | Indexer (loc, indexer) ->
              let { Indexer.key; value; static; variance; _ } = indexer in
              let (((_, k), _) as key) = convert cx tparams_map key in
              let (((_, v), _) as value) = convert cx tparams_map value in
              let polarity = polarity cx variance in
              ( add_indexer ~static polarity ~key:k ~value:v x,
                Indexer (loc, { indexer with Indexer.key; value }) :: rev_prop_asts
              )
            | Property
                ( loc,
                  ( { Property.key; value; static; proto; optional; _method; variance; comments = _ }
                  as prop
                  )
                ) ->
              if optional && _method then
                Flow_js_utils.add_output cx Error_message.(EInternal (loc, OptionalMethod));
              let polarity = polarity cx variance in
              let (x, prop) =
                Ast.Expression.Object.(
                  match (_method, key, value) with
                  | (_, Property.Literal (loc, _), _)
                  | (_, Property.PrivateName (loc, _), _)
                  | (_, Property.Computed (loc, _), _) ->
                    Flow_js_utils.add_output
                      cx
                      (Error_message.EUnsupportedSyntax (loc, Error_message.IllegalName));
                    (x, Tast_utils.error_mapper#object_property_type (loc, prop))
                  | ( true,
                      Property.Identifier
                        (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name)),
                      Ast.Type.Object.Property.Init (func_loc, Ast.Type.Function func)
                    ) ->
                    let (fsig, func_ast) = mk_func_sig cx tparams_map loc func in
                    let this_write_loc = None in
                    let ft = Func_type_sig.methodtype cx this_write_loc this fsig in
                    let append_method =
                      match (static, name) with
                      | (false, "constructor") -> append_constructor ~id_loc:(Some id_loc)
                      | _ -> append_method ~static name ~id_loc ~this_write_loc
                    in
                    let open Ast.Type in
                    ( append_method ~func_sig:fsig x,
                      ( loc,
                        {
                          prop with
                          Object.Property.key = Property.Identifier ((id_loc, ft), id_name);
                          value = Object.Property.Init ((func_loc, ft), Function func_ast);
                        }
                      )
                    )
                  | (true, Property.Identifier _, _) ->
                    Flow_js_utils.add_output cx Error_message.(EInternal (loc, MethodNotAFunction));
                    (x, Tast_utils.error_mapper#object_property_type (loc, prop))
                  | ( false,
                      Property.Identifier
                        (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name)),
                      Ast.Type.Object.Property.Init value
                    ) ->
                    let (((_, t), _) as value_ast) = convert cx tparams_map value in
                    let t =
                      if optional then
                        TypeUtil.optional t
                      else
                        t
                    in
                    let add =
                      if proto then
                        add_proto_field
                      else
                        add_field ~static
                    in
                    let open Ast.Type in
                    ( add name id_loc polarity (Annot t) x,
                      ( loc,
                        {
                          prop with
                          Object.Property.key = Property.Identifier ((id_loc, t), id_name);
                          value = Object.Property.Init value_ast;
                        }
                      )
                    )
                  (* unsafe getter property *)
                  | ( _,
                      Property.Identifier
                        (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name)),
                      Ast.Type.Object.Property.Get (get_loc, func)
                    ) ->
                    Flow_js_utils.add_output cx (Error_message.EUnsafeGettersSetters loc);
                    let (fsig, func_ast) = mk_func_sig cx tparams_map loc func in
                    let prop_t =
                      TypeUtil.type_t_of_annotated_or_inferred fsig.Func_type_sig.Types.return_t
                    in
                    let open Ast.Type in
                    ( add_getter ~static name ~id_loc ~this_write_loc:None ~func_sig:fsig x,
                      ( loc,
                        {
                          prop with
                          Object.Property.key = Property.Identifier ((id_loc, prop_t), id_name);
                          value = Object.Property.Get (get_loc, func_ast);
                        }
                      )
                    )
                  (* unsafe setter property *)
                  | ( _,
                      Property.Identifier
                        (id_loc, ({ Ast.Identifier.name; comments = _ } as id_name)),
                      Ast.Type.Object.Property.Set (set_loc, func)
                    ) ->
                    Flow_js_utils.add_output cx (Error_message.EUnsafeGettersSetters loc);
                    let (fsig, func_ast) = mk_func_sig cx tparams_map loc func in
                    let prop_t =
                      match fsig with
                      | { Func_type_sig.Types.tparams = None; fparams; _ } ->
                        (match Func_type_params.value fparams with
                        | [(_, t)] -> t
                        | _ -> AnyT.at (AnyError None) id_loc)
                      (* error case: report any ok *)
                      | _ -> AnyT.at (AnyError None) id_loc
                      (* error case: report any ok *)
                    in
                    let open Ast.Type in
                    ( add_setter ~static name ~id_loc ~this_write_loc:None ~func_sig:fsig x,
                      ( loc,
                        {
                          prop with
                          Object.Property.key = Property.Identifier ((id_loc, prop_t), id_name);
                          value = Object.Property.Set (set_loc, func_ast);
                        }
                      )
                    )
                )
              in
              (x, Ast.Type.Object.Property prop :: rev_prop_asts)
            | InternalSlot (loc, slot) as prop ->
              let {
                InternalSlot.id = (_, { Ast.Identifier.name; comments = _ });
                value;
                optional;
                static;
                _method;
                comments = _;
              } =
                slot
              in
              if name = "call" then
                let (((_, t), _) as value) = convert cx tparams_map value in
                let t =
                  if optional then
                    TypeUtil.optional t
                  else
                    t
                in
                ( append_call ~static t x,
                  InternalSlot (loc, { slot with InternalSlot.value }) :: rev_prop_asts
                )
              else (
                Flow_js_utils.add_output
                  cx
                  Error_message.(EUnsupportedSyntax (loc, UnsupportedInternalSlot { name; static }));
                (x, Tast_utils.error_mapper#object_type_property prop :: rev_prop_asts)
              )
            | SpreadProperty (loc, _) as prop ->
              Flow_js_utils.add_output cx Error_message.(EInternal (loc, InterfaceTypeSpread));
              (x, Tast_utils.error_mapper#object_type_property prop :: rev_prop_asts)
        )
        (s, [])
        properties
    in
    (x, List.rev rev_prop_asts)

  and optional_indexed_access
      cx loc ~tparams_map { T.OptionalIndexedAccess.indexed_access; optional } =
    let reason = mk_reason (RIndexedAccess { optional }) loc in
    let { T.IndexedAccess._object; index; comments } = indexed_access in
    let (((_, index_type), _) as index) = convert cx tparams_map index in
    let index_reason = reason_of_t index_type in
    let (object_t, object_ast) =
      match _object with
      | (loc, T.OptionalIndexedAccess ia) -> optional_indexed_access cx loc ~tparams_map ia
      | _ ->
        let (((_, object_t), _) as object_ast) = convert cx tparams_map _object in
        (object_t, object_ast)
    in
    let lhs_reason = reason_of_t object_t in
    let use_op = Op (IndexedTypeAccess { _object = lhs_reason; index = index_reason }) in
    let non_maybe_destructor =
      match index with
      | (_, Ast.Type.StringLiteral { Ast.StringLiteral.value; _ }) ->
        let name = OrdinaryName value in
        if optional then
          OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessStrLitIndex name }
        else
          PropertyType { name }
      | _ ->
        if optional then
          OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessTypeIndex index_type }
        else
          ElementType { index_type }
    in
    let non_maybe_result_t =
      EvalT (object_t, TypeDestructorT (use_op, reason, non_maybe_destructor), mk_eval_id cx loc)
    in
    let void_reason = replace_desc_reason RVoid lhs_reason in
    let result_t =
      EvalT
        ( non_maybe_result_t,
          TypeDestructorT
            (unknown_use (* not used *), reason, OptionalIndexedAccessResultType { void_reason }),
          Eval.generate_id ()
        )
    in
    ( non_maybe_result_t,
      ( (loc, result_t),
        T.OptionalIndexedAccess
          {
            T.OptionalIndexedAccess.indexed_access =
              { T.IndexedAccess._object = object_ast; index; comments };
            optional;
          }
      )
    )

  let mk_super cx tparams_map loc c targs =
    match targs with
    | None -> ((loc, c, None), None)
    | Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
      let (ts, targs_ast) = convert_list cx tparams_map targs in
      ((loc, c, Some ts), Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs_ast; comments }))

  let mk_interface_sig cx intf_loc reason decl =
    let open Class_type_sig in
    let open Class_type_sig.Types in
    let {
      Ast.Statement.Interface.id = (id_loc, id_name);
      tparams;
      body =
        ( body_loc,
          { Ast.Type.Object.properties; exact; inexact = _inexact; comments = object_comments }
        );
      extends;
      comments;
    } =
      decl
    in
    let self = Tvar.mk cx reason in
    let (tparams, tparams_map, tparams_ast) = mk_type_param_declarations cx tparams in
    let (iface_sig, extends_ast) =
      let id = Context.make_aloc_id cx id_loc in
      let (extends, extends_ast) =
        extends |> Base.List.map ~f:(mk_interface_super cx tparams_map) |> List.split
      in
      let super =
        let callable =
          List.exists
            Ast.Type.Object.(
              function
              | CallProperty (_, { CallProperty.static; _ }) -> not static
              | _ -> false
            )
            properties
        in
        Interface { inline = false; extends; callable }
      in
      (empty id intf_loc reason tparams tparams_map super, extends_ast)
    in
    (* TODO: interfaces don't have a name field, or even statics *)
    let iface_sig = add_name_field iface_sig in
    let (iface_sig, properties) =
      add_interface_properties
        cx
        tparams_map
        properties
        ~this:(implicit_mixed_this reason)
        iface_sig
    in
    ( iface_sig,
      self,
      {
        Ast.Statement.Interface.id = ((id_loc, self), id_name);
        tparams = tparams_ast;
        extends = extends_ast;
        body =
          ( body_loc,
            { Ast.Type.Object.exact; properties; inexact = false; comments = object_comments }
          );
        comments;
      }
    )

  let mk_declare_class_sig =
    let open Class_type_sig in
    let open Class_type_sig.Types in
    let mk_mixins cx tparams_map (loc, { Ast.Type.Generic.id; targs; comments }) =
      let name = qualified_name id in
      let r = mk_annot_reason (RType (OrdinaryName name)) loc in
      let (i, id) =
        let lookup_mode = Env.LookupMode.ForValue in
        convert_qualification ~lookup_mode cx "mixins" id
      in
      let props_bag = ConsGen.mixin cx r i in
      let (t, targs) = mk_super cx tparams_map loc props_bag targs in
      (t, (loc, { Ast.Type.Generic.id; targs; comments }))
    in
    let is_object_builtin_libdef (loc, { Ast.Identifier.name; comments = _ }) =
      name = "Object"
      &&
      match ALoc.source loc with
      | None -> false
      | Some source -> File_key.is_lib_file source
    in
    fun cx class_loc reason decl ->
      let {
        Ast.Statement.DeclareClass.id = (id_loc, id_name) as ident;
        tparams;
        body =
          ( body_loc,
            { Ast.Type.Object.properties; exact; inexact = _inexact; comments = object_comments }
          );
        extends;
        mixins;
        implements;
        comments;
      } =
        decl
      in
      let self = Tvar.mk cx reason in
      let (tparams, tparams_map, tparam_asts) = mk_type_param_declarations cx tparams in
      let (this_tparam, this_t) = mk_this self cx reason tparams in
      let tparams_map_with_this = Subst_name.Map.add (Subst_name.Name "this") this_t tparams_map in
      let (iface_sig, extends_ast, mixins_ast, implements_ast) =
        let id = Context.make_aloc_id cx id_loc in
        let (extends, extends_ast) =
          match extends with
          | Some (loc, { Ast.Type.Generic.id; targs; comments }) ->
            let lookup_mode = Env.LookupMode.ForValue in
            let (i, id) = convert_qualification ~lookup_mode cx "mixins" id in
            let (t, targs) = mk_super cx tparams_map_with_this loc i targs in
            (Some t, Some (loc, { Ast.Type.Generic.id; targs; comments }))
          | None -> (None, None)
        in
        let (mixins, mixins_ast) =
          mixins |> Base.List.map ~f:(mk_mixins cx tparams_map_with_this) |> List.split
        in
        let (implements, implements_ast) =
          let open Ast.Class.Implements in
          match implements with
          | None -> ([], None)
          | Some (implements_loc, { interfaces; comments }) ->
            let (implements, interfaces_ast) =
              interfaces
              |> Base.List.map ~f:(fun (loc, i) ->
                     let { Interface.id = (id_loc, id_name_inner); targs } = i in
                     let { Ast.Identifier.name; comments = _ } = id_name_inner in
                     let c = Env.get_var ~lookup_mode:Env.LookupMode.ForType cx name id_loc in
                     let (typeapp, targs) =
                       match targs with
                       | None -> ((loc, c, None), None)
                       | Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
                         let (ts, targs_ast) = convert_list cx tparams_map_with_this targs in
                         ( (loc, c, Some ts),
                           Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs_ast; comments })
                         )
                     in
                     (typeapp, (loc, { Interface.id = ((id_loc, c), id_name_inner); targs }))
                 )
              |> List.split
            in
            (implements, Some (implements_loc, { interfaces = interfaces_ast; comments }))
        in
        let super =
          let extends =
            match extends with
            | None -> Implicit { null = is_object_builtin_libdef ident }
            | Some extends -> Explicit extends
          in
          Class { Class_type_sig.Types.extends; mixins; implements; this_t; this_tparam }
        in
        ( empty id class_loc reason tparams tparams_map super,
          extends_ast,
          mixins_ast,
          implements_ast
        )
      in
      (* All classes have a static "name" property. *)
      let iface_sig = add_name_field iface_sig in
      let (iface_sig, properties) =
        add_interface_properties
          cx
          tparams_map_with_this
          properties
          ~this:(implicit_mixed_this (reason_of_t this_t))
          iface_sig
      in
      (* Add a default ctor if we don't have a ctor and won't inherit one from a super *)
      let iface_sig =
        if mem_constructor iface_sig || extends <> None || mixins <> [] then
          iface_sig
        else
          let reason = replace_desc_reason RDefaultConstructor reason in
          add_default_constructor reason iface_sig
      in
      ( iface_sig,
        self,
        {
          Ast.Statement.DeclareClass.id = ((id_loc, self), id_name);
          tparams = tparam_asts;
          body =
            ( body_loc,
              { Ast.Type.Object.properties; exact; inexact = false; comments = object_comments }
            );
          extends = extends_ast;
          mixins = mixins_ast;
          implements = implements_ast;
          comments;
        }
      )
end
