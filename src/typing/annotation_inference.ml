(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type_subst
open Type
open Type.AConstraint
open TypeUtil

let object_like_op = function
  | Annot_SpecializeT _
  | Annot_ThisSpecializeT _
  | Annot_UseT_TypeT _
  | Annot_ConcretizeForImportsExports _
  | Annot_CJSRequireT _
  | Annot_ImportTypeofT _
  | Annot_ImportNamedT _
  | Annot_ImportDefaultT _
  | Annot_ImportModuleNsT _
  | Annot_CJSExtractNamedExportsT _
  | Annot_ExportNamedT _
  | Annot_ExportTypeT _
  | Annot_AssertExportIsTypeT _
  | Annot_CopyNamedExportsT _
  | Annot_CopyTypeExportsT _
  | Annot_ElemT _
  | Annot_GetStaticsT _
  | Annot_MakeExactT _
  | Annot_MixinT _
  | Annot_ObjKitT _
  | Annot_ObjTestProtoT _
  | Annot_ArithT _
  | Annot_UnaryArithT _
  | Annot_NotT _
  | Annot_ObjKeyMirror _
  | Annot_ObjMapConst _
  | Annot_GetKeysT _
  | Annot_GetEnumT _
  | Annot_DeepReadOnlyT _
  | Annot_ToStringT _
  | Annot__Future_added_value__ _ ->
    false
  | Annot_GetTypeFromNamespaceT _
  | Annot_GetPropT _
  | Annot_GetElemT _
  | Annot_LookupT _
  | Annot_ObjRestT _
  | Annot_GetValuesT _ ->
    true

let primitive_promoting_op = function
  | Annot_GetPropT _
  | Annot_GetElemT _
  | Annot_LookupT _ ->
    true
  (* TODO: enumerate all use types *)
  | _ -> false

let function_like_op op = object_like_op op

let get_fully_resolved_type cx id =
  let (_, constraints) = Context.find_constraints cx id in
  match constraints with
  | Constraint.FullyResolved s -> Context.force_fully_resolved_tvar cx s
  | Constraint.Resolved _
  | Constraint.Unresolved _ ->
    failwith "unexpected unresolved constraint in annotation inference"

let get_builtin_typeapp cx reason x targs =
  let t = Flow_js_utils.lookup_builtin_type cx x reason in
  TypeUtil.typeapp ~from_value:false ~use_desc:false reason t targs

module type S = sig
  val mk_type_reference : Context.t -> type_t_kind:Type.type_t_kind -> Reason.t -> Type.t -> Type.t

  val mk_instance :
    Context.t -> ?type_t_kind:Type.type_t_kind -> reason -> ?use_desc:bool -> Type.t -> Type.t

  val reposition : Context.t -> ALoc.t -> Type.t -> Type.t

  val get_prop :
    Context.t -> Type.use_op -> Reason.t -> ?op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val get_elem : Context.t -> Type.use_op -> Reason.t -> key:Type.t -> Type.t -> Type.t

  val get_builtin_type : Context.t -> reason -> ?use_desc:bool -> string -> Type.t

  val qualify_type :
    Context.t -> Type.use_op -> Reason.t -> op_reason:Reason.t -> Reason.name -> Type.t -> Type.t

  val assert_export_is_type : Context.t -> Reason.t -> string -> Type.t -> Type.t

  val mk_sig_tvar : Context.t -> Reason.t -> Type.t Lazy.t -> Type.t

  val cjs_require : Context.t -> Type.t -> Reason.t -> FlowSymbol.symbol -> bool -> bool -> Type.t

  val export_named :
    Context.t ->
    Reason.reason ->
    Type.export_kind ->
    Type.named_symbol NameUtils.Map.t ->
    Type.named_symbol NameUtils.Map.t ->
    Type.t ->
    Type.t

  val cjs_extract_named_exports : Context.t -> Reason.reason -> Type.moduletype -> Type.t -> Type.t

  val import_default :
    Context.t -> Reason.t -> Type.import_kind -> string -> string -> bool -> Type.t -> Type.t

  val import_named :
    Context.t -> Reason.t -> Type.import_kind -> string -> string -> bool -> Type.t -> Type.t

  val import_ns : Context.t -> Reason.t -> FlowSymbol.symbol -> bool -> Type.t -> Type.t

  val import_typeof : Context.t -> Reason.t -> string -> Type.t -> Type.t

  val specialize :
    Context.t ->
    Type.t ->
    Type.use_op ->
    Reason.t ->
    Reason.t ->
    Type.t list Base.Option.t ->
    Type.t

  val copy_named_exports : Context.t -> from_ns:Type.t -> Reason.t -> module_t:Type.t -> Type.t

  val copy_type_exports : Context.t -> from_ns:Type.t -> Reason.t -> module_t:Type.t -> Type.t

  val arith : Context.t -> Reason.t -> Type.t -> Type.t -> Type.ArithKind.t -> Type.t

  val unary_arith : Context.t -> Reason.t -> Type.t -> Type.UnaryArithKind.t -> Type.t

  val unary_not : Context.t -> Reason.t -> Type.t -> Type.t

  val mixin : Context.t -> Reason.t -> Type.t -> Type.t

  val object_spread :
    Context.t ->
    Type.use_op ->
    Reason.reason ->
    Type.Object.Spread.target ->
    Type.Object.Spread.state ->
    Type.t ->
    Type.t

  val obj_test_proto : Context.t -> Reason.t -> Type.t -> Type.t

  val obj_rest : Context.t -> Reason.t -> string list -> Type.t -> Type.t

  val arr_rest : Context.t -> Type.use_op -> Reason.t -> int -> Type.t -> Type.t

  val set_dst_cx : Context.t -> unit

  val elab_t : Context.t -> ?seen:ISet.t -> Type.t -> Type.AConstraint.op -> Type.t
end

module rec ConsGen : S = struct
  (* Annotation inference is performed in the context of the definition module (this
   * is what the input `cx` in elab_t etc. represents). However, in order to be
   * able to raise errors during annotation inference, we need to have access to the
   * destination context. This is what this reference is for. `dst_cx_ref` is set
   * Check_serivce.mk_check_file once per file right after the destination context
   * is created. *)
  let dst_cx_ref = ref None

  let set_dst_cx cx = dst_cx_ref := Some cx

  (* Errors created with [error_unsupported] are actually reported. Compare this to
   * errors created with Flow_js_utils.add_output which are recorded in the context
   * of the source of the annotations, and are therefore ignored. This function checks
   * that dst_cx_ref has been set and uses that as the target context.
   *
   * The only kind of errors that are reported here are "unsupported" cases. These
   * are mostly cases that rely on subtyping, which is not implemented here; most
   * commonly evaluating call-like EvalTs and speculation. *)
  let error_unsupported_reason ?suggestion cx reason reason_op =
    let loc = Reason.loc_of_reason reason_op in
    let msg = Error_message.EAnnotationInference (loc, reason_op, reason, suggestion) in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason_op

  let error_unsupported ?suggestion cx reason op =
    let reason_op = AConstraint.display_reason_of_op op in
    error_unsupported_reason ?suggestion cx reason reason_op

  let error_recursive cx reason =
    let loc = Reason.loc_of_reason reason in
    let msg = Error_message.ETrivialRecursiveDefinition (loc, reason) in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason

  let error_internal_reason cx msg reason_op =
    let loc = Reason.loc_of_reason reason_op in
    let msg = Error_message.(EInternal (loc, UnexpectedAnnotationInference msg)) in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason_op

  let error_internal cx msg op =
    let reason_op = AConstraint.display_reason_of_op op in
    error_internal_reason cx msg reason_op

  let dummy_trace = DepthTrace.dummy_trace

  (* Repositioning does not seem to have any perceptible impact in annotation
   * inference. Instead of replicating the convoluted implementation of Flow_js
   * here, we just return the same type intact. *)
  let reposition _cx _loc t = t

  (*****************)
  (* Instantiation *)
  (*****************)
  module Instantiation_helper = struct
    (* We will not be solving implicit instantiation problems here. The only case
     * where we will need to use this function is when a PolyT needs to be used
     * as a monomorphic type. In this case, the only sensible thing to do is to
     * use the bound of each parameter as the argument to the intantiation. *)
    let mk_targ _cx typeparam _reason_op _reason_tapp = typeparam.Type.bound

    let is_subtype _cx _trace ~use_op:_ (_t1, _t2) = ()

    let unify _cx _trace ~use_op:_ (_t1, _t2) = ()

    let reposition cx ?trace:_ loc t = reposition cx loc t
  end

  module InstantiationKit = Flow_js_utils.Instantiation_kit (Instantiation_helper)

  let instantiate_poly cx = InstantiationKit.instantiate_poly cx dummy_trace

  let mk_typeapp_of_poly cx = InstantiationKit.mk_typeapp_of_poly cx dummy_trace

  (***********)
  (* Imports *)
  (***********)
  module Import_export_helper : Flow_js_utils.Import_export_helper_sig with type r = Type.t = struct
    type r = Type.t

    let reposition cx loc t = reposition cx loc t

    let return _cx t = t

    let export_named cx (reason, values, types, kind) t =
      ConsGen.export_named cx reason kind values types t

    let export_named_fresh_var = export_named

    let export_type cx (reason, name_loc, preferred_def_locs, export_name, target_module_t) export_t
        =
      ConsGen.elab_t
        cx
        export_t
        (Annot_ExportTypeT { reason; name_loc; preferred_def_locs; export_name; target_module_t })

    let cjs_extract_named_exports cx (reason, local_module) t =
      ConsGen.cjs_extract_named_exports cx reason local_module t
  end

  let with_concretized_type cx r f t = ConsGen.elab_t cx t (Annot_ConcretizeForImportsExports (r, f))

  module CJSRequireTKit = Flow_js_utils.CJSRequireTKit
  module ImportModuleNsTKit = Flow_js_utils.ImportModuleNsTKit
  module ImportDefaultTKit = Flow_js_utils.ImportDefaultTKit
  module ImportNamedTKit = Flow_js_utils.ImportNamedTKit
  module ImportTypeofTKit = Flow_js_utils.ImportTypeofTKit
  module ExportNamedTKit = Flow_js_utils.ExportNamedTKit
  module AssertExportIsTypeTKit = Flow_js_utils.AssertExportIsTypeT_kit (Import_export_helper)
  module CopyNamedExportsTKit = Flow_js_utils.CopyNamedExportsT_kit (Import_export_helper)
  module CopyTypeExportsTKit = Flow_js_utils.CopyTypeExportsT_kit (Import_export_helper)
  module ExportTypeTKit = Flow_js_utils.ExportTypeT_kit (Import_export_helper)
  module CJSExtractNamedExportsTKit =
    Flow_js_utils.CJSExtractNamedExportsT_kit (Import_export_helper)

  (***********)
  (* GetProp *)
  (***********)

  module Get_prop_helper = struct
    type r = Type.t

    let cg_lookup_ cx use_op t reason_op propref objt =
      ConsGen.elab_t cx t (Annot_LookupT (reason_op, use_op, propref, objt))

    let error_type _ _ = AnyT.error

    (* We could have just returned `t` here. The OpenT indirection is for compatibility
     * with Flow_js. Specifically, without the OpenT the transformation in
     * https://github.com/facebook/flow/blob/8c3825a1be188e9ade4ad4ed515361bb28c65d8a/src/typing/flow_js.ml#L1744-L1755
     * would fire, causing a divergence in the behavior of this module and Flow_js. *)
    let return cx ~use_op:_ _trace t =
      match t with
      | OpenT _ -> t
      | _ -> Tvar.mk_fully_resolved cx (reason_of_t t) t

    (* We will not be doing subtyping checks in annotation inference. *)
    let dict_read_check _ _ ~use_op:_ _ = ()

    let reposition cx ?trace:_ loc t = reposition cx loc t

    let cg_lookup cx _trace ~obj_t ~method_accessible:_ t (reason_op, _kind, propref, use_op, _ids)
        =
      cg_lookup_ cx use_op t reason_op propref obj_t

    let cg_get_prop cx _trace t (use_op, access_reason, _, (prop_reason, name)) =
      ConsGen.elab_t
        cx
        t
        (Annot_GetPropT (access_reason, use_op, mk_named_prop ~reason:prop_reason name))

    let mk_react_dro cx _use_op (props_loc, dro_t) t =
      ConsGen.elab_t cx t (Annot_DeepReadOnlyT (reason_of_t t, props_loc, dro_t))

    let mk_hooklike _cx _use_op t = t
  end

  module GetPropTKit = Flow_js_utils.GetPropT_kit (Get_prop_helper)

  (** [ensure_annot_resolved cx reason id] ensures that the annotation constraint
   *  associated with [id] has been resolved. If the respective constraint is already
   *  resolved then it returns immediately. Otherwise, it resolves [id] immediately
   *  to the 'any' type. In the case of an [Anno_op (_, _, dep_id)] constraint we also
   *  update the "dependents" set of [dep_id], so that we don't attempt to resolve
   *  [id] once again when [dep_id] gets resolved.
   *)
  let rec ensure_annot_resolved cx reason id =
    let module A = Type.AConstraint in
    match Context.find_avar_opt cx id with
    | None -> get_fully_resolved_type cx id
    | Some (A.Annot_unresolved _) ->
      let t = error_recursive cx reason in
      resolve_id cx reason id t;
      t
    | Some (A.Annot_op { id = dep_id; _ }) ->
      let dep_constraint = Context.find_avar cx dep_id in
      A.update_deps_of_constraint dep_constraint ~f:(fun deps ->
          ISet.filter (fun id2 -> id <> id2) deps
      );
      let t = error_recursive cx reason in
      resolve_id cx reason id t;
      t

  and mk_lazy_tvar cx reason f =
    let id = Reason.mk_id () in
    let tvar = OpenT (reason, id) in
    let t =
      lazy
        ( Avar.unresolved_with_id cx id reason;
          f id;
          (* Before forcing the type constraint of [id] we need to make sure the
           * respective annotation constraint has been processed. If not we infer
           * the empty type. *)
          ensure_annot_resolved cx reason id
        )
    in
    let node =
      Constraint.create_root
        (Constraint.FullyResolved (Constraint.ForcingState.of_lazy_t ~error_reason:reason t))
    in
    Context.add_tvar cx id node;
    tvar

  and mk_sig_tvar cx reason (resolved : Type.t Lazy.t) =
    let f id =
      let t = Lazy.force resolved in
      resolve_id cx reason id t
    in
    mk_lazy_tvar cx reason f

  (** [resolve_id cx id1 t] resolves an annotation tvar [id1] to a type [t] *
   *  - If [t] is a concrete type, we mark [id1] as a resolved annotation tvar and
   *    record it as fully resolved in the type graph. *
   *  - If [t] is an OpenT (_, id2), then we unify [id1] and [id2]. (See merge_ids.)
   *)
  and resolve_id cx reason id t =
    match Context.find_avar_opt cx id with
    | None ->
      (* The avar is already resolved. This happens when the avar is recursively
       * reachable and becomes resolved to any. *)
      ()
    | Some constraints1 ->
      let t =
        match t with
        | Type.OpenT (_, id2) -> ensure_annot_resolved cx reason id2
        | _ -> t
      in
      Context.remove_avar cx id;
      Context.add_tvar
        cx
        id
        Type.Constraint.(create_root (FullyResolved (ForcingState.of_non_lazy_t t)));
      let dependents1 = deps_of_constraint constraints1 in
      resolve_dependent_set cx reason dependents1 t

  and resolve_dependent_set cx reason dependents t =
    Context.iter_annot_dependent_set
      cx
      (fun id op -> resolve_id cx reason id (elab_t cx t op))
      dependents

  and elab_open cx ~seen reason id op =
    if ISet.mem id seen then
      error_recursive cx reason
    else
      let module A = Type.AConstraint in
      match Context.find_avar_opt cx id with
      | None ->
        (* [id] may refer to a lazily resolved constraint (e.g. created through
         * [mk_lazy_tvar]). To protect against trying to force recursive lazy
         * structures, we introduce a lazy indirection around the resulting
         * constraint. An example that would have cause this unwanted behavior is
         *
         *   declare var x: {
         *     p: number;
         *     q: typeof (x.p);
         *   };
         *
         * This lazy indirection allows the type of `x` to be resolved, before we
         * attempt to force the constraint for `x.p`. *)
        let resolved =
          lazy
            ((* resolved ids definitelly appear in the type graph *)
             let t = get_fully_resolved_type cx id in
             elab_t cx ~seen:(ISet.add id seen) t op
            )
        in
        mk_sig_tvar cx (AConstraint.reason_of_op op) resolved
      | Some (A.Annot_unresolved _)
      | Some (A.Annot_op _) ->
        let fresh_id = Avar.constrained cx op id in
        OpenT (reason, fresh_id)

  and elab_t cx ?(seen = ISet.empty) t op =
    match (t, op) with
    | (EvalT (t, TypeDestructorT (use_op, reason, ReadOnlyType), _), _) ->
      let t = make_readonly cx use_op reason t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, reason, ReactDRO (dro_loc, dro_kind)), _), _) ->
      let t = elab_t cx t (Annot_DeepReadOnlyT (reason, dro_loc, dro_kind)) in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, _, MakeHooklike), _), _) -> t
    | (EvalT (t, TypeDestructorT (use_op, reason, PartialType), _), _) ->
      let t = make_partial cx use_op reason t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, RequiredType), _), _) ->
      let t = make_required cx use_op reason t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, SpreadType (target, todo_rev, head_slice)), _), _)
      ->
      let state =
        {
          Object.Spread.todo_rev;
          acc =
            Base.Option.value_map ~f:(fun x -> [Object.Spread.InlineSlice x]) ~default:[] head_slice;
          spread_id = Reason.mk_id ();
          union_reason = None;
          curr_resolve_idx = 0;
        }
      in

      let t = object_spread cx use_op reason target state t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, RestType (options, r)), _), _) ->
      let state = Object.Rest.One r in
      let t = object_rest cx use_op reason options state t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, reason, TypeMap ObjectKeyMirror), _), _) ->
      let t = elab_t cx t (Annot_ObjKeyMirror reason) in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, reason, TypeMap (ObjectMapConst t')), _), _) ->
      let t = elab_t cx t (Annot_ObjMapConst (reason, t')) in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, reason, ValuesType), _), _) ->
      let t = elab_t cx t (Annot_GetValuesT reason) in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, PropertyType { name }), _), _) ->
      let reason_op = replace_desc_reason (RProperty (Some name)) reason in
      let t =
        elab_t
          cx
          t
          (Annot_GetPropT (reason_op, use_op, Named { reason; name; from_indexed_access = true }))
      in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, ElementType { index_type }), _), _) ->
      let t = elab_t cx t (Annot_GetElemT (reason, use_op, index_type)) in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (_, reason, EnumType), _), _) ->
      let t = elab_t cx t (Annot_GetEnumT reason) in
      elab_t cx t op
    | (EvalT (_, TypeDestructorT (_, reason, TypeMap (ObjectMap _)), _), _) ->
      error_unsupported ~suggestion:"$ObjMapConst" cx reason op
    | (EvalT (_, TypeDestructorT (_, reason, TypeMap (ObjectMapi _)), _), _) ->
      error_unsupported ~suggestion:"$KeyMirror" cx reason op
    | (EvalT (_, TypeDestructorT (_, reason, _), _), _) -> error_unsupported cx reason op
    | (OpenT (reason, id), _) -> elab_open cx ~seen reason id op
    | (InternalT (ExtendsT (reason, _, _) | EnforceUnionOptimized reason), _) ->
      error_unsupported cx reason op
    | (AnnotT (r, t, _), _) ->
      let t = reposition cx (loc_of_reason r) t in
      elab_t cx ~seen t op
    (*********************************************************************)
    (* UseT TypeT (runtime types derive static types through annotation) *)
    (*********************************************************************)
    (* First handle catch-all cases of subtyping_kit.ml *)
    | ((MaybeT (reason, _) | OptionalT { reason; _ }), Annot_UseT_TypeT _) ->
      error_unsupported cx reason op
    | (ThisTypeAppT (reason_tapp, c, this, ts), Annot_UseT_TypeT _) ->
      let reason_op = Type.AConstraint.reason_of_op op in
      let tc = specialize_class cx c reason_op reason_tapp ts in
      let t = this_specialize cx reason_tapp this tc in
      elab_t cx t op
    | ( TypeAppT
          { reason = reason_tapp; use_op = typeapp_use_op; type_; targs; from_value; use_desc = _ },
        Annot_UseT_TypeT _
      ) ->
      (* NOTE omitting TypeAppExpansion.push_unless_loop check. *)
      let reason_op = Type.AConstraint.reason_of_op op in
      let t =
        mk_typeapp_instance
          cx
          ~use_op:typeapp_use_op
          ~reason_op
          ~reason_tapp
          ~from_value
          type_
          targs
      in
      elab_t cx t op
    | ( DefT (_, PolyT { tparams = ids; t_out = DefT (_, ReactAbstractComponentT _) as t; _ }),
        Annot_UseT_TypeT (reason_op, RenderTypeKind)
      ) ->
      let subst_map =
        Nel.fold_left
          (fun acc tparam -> Subst_name.Map.add tparam.name (AnyT.untyped reason_op) acc)
          Subst_name.Map.empty
          ids
      in
      let t_ = subst cx subst_map t in
      elab_t cx t_ op
    | (DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; _ }), Annot_UseT_TypeT (reason, _)) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EMissingTypeArgs
           {
             reason_op = reason;
             reason_tapp;
             arity_loc = tparams_loc;
             min_arity = Flow_js_utils.poly_minimum_arity ids;
             max_arity = Nel.length ids;
           }
        );
      AnyT.error reason
    | ( DefT (class_r, ClassT (ThisInstanceT (r, i, is_this, this_name))),
        Annot_UseT_TypeT (reason, _)
      ) ->
      let c =
        DefT (class_r, ClassT (Flow_js_utils.fix_this_instance cx reason (r, i, is_this, this_name)))
      in
      elab_t cx c op
    | (DefT (_, ClassT it), Annot_UseT_TypeT (reason, _)) ->
      (* a class value annotation becomes the instance type *)
      reposition cx (loc_of_reason reason) it
    | ((DefT (_, ReactAbstractComponentT _) as l), Annot_UseT_TypeT (reason, _)) ->
      (* a component syntax value annotation becomes an element of that component *)
      get_builtin_typeapp cx reason "React$Element" [l]
    | (DefT (_, TypeT (_, l)), Annot_UseT_TypeT _) -> l
    | (DefT (_, EnumObjectT { enum_value_t; _ }), Annot_UseT_TypeT _) ->
      (* an enum object value annotation becomes the enum value type *)
      enum_value_t
    | (DefT (enum_reason, EnumValueT _), Annot_UseT_TypeT (reason, _)) ->
      Flow_js_utils.add_output cx Error_message.(EEnumMemberUsedAsType { reason; enum_reason });
      AnyT.error reason
    | (l, Annot_UseT_TypeT (reason_use, _)) ->
      (match l with
      (* Short-circut as we already error on the unresolved name. *)
      | AnyT (_, AnyError _) -> ()
      | AnyT _ -> Flow_js_utils.add_output cx Error_message.(EAnyValueUsedAsType { reason_use })
      | _ -> Flow_js_utils.add_output cx Error_message.(EValueUsedAsType { reason_use }));
      AnyT.error reason_use
    | (l, Annot_ConcretizeForImportsExports (_, f)) -> f l
    (*******************)
    (* `import typeof` *)
    (*******************)
    | (_, Annot_ImportTypeofT (reason, export_name)) ->
      ImportTypeofTKit.on_concrete_type cx reason export_name t
    (******************)
    (* Module exports *)
    (******************)
    | ( ModuleT m,
        Annot_ExportNamedT { reason = _; value_exports_tmap; type_exports_tmap; export_kind }
      ) ->
      ExportNamedTKit.mod_ModuleT cx (value_exports_tmap, type_exports_tmap, export_kind) m;
      ModuleT m
    | (_, Annot_AssertExportIsTypeT (_, name)) -> AssertExportIsTypeTKit.on_concrete_type cx name t
    | (ModuleT m, Annot_CopyNamedExportsT (reason, target_module_t)) ->
      CopyNamedExportsTKit.on_ModuleT cx (reason, target_module_t) m
    | (ModuleT m, Annot_CopyTypeExportsT (reason, target_module_t)) ->
      CopyTypeExportsTKit.on_ModuleT cx (reason, target_module_t) m
    | (_, Annot_ExportTypeT { reason; name_loc; preferred_def_locs; export_name; target_module_t })
      ->
      ExportTypeTKit.on_concrete_type
        cx
        (reason, name_loc, preferred_def_locs, export_name, target_module_t)
        t
    | (AnyT (_, _), Annot_CopyNamedExportsT (_, target_module)) ->
      CopyNamedExportsTKit.on_AnyT cx target_module
    | (AnyT (_, _), Annot_CopyTypeExportsT (_, target_module)) ->
      CopyTypeExportsTKit.on_AnyT cx target_module
    | (_, Annot_CJSExtractNamedExportsT (reason, local_module)) ->
      CJSExtractNamedExportsTKit.on_concrete_type cx (reason, local_module) t
    (******************)
    (* Module imports *)
    (******************)
    | (ModuleT m, Annot_CJSRequireT { reason; namespace_symbol; is_strict; legacy_interop }) ->
      CJSRequireTKit.on_ModuleT
        cx
        ~reposition:(fun _ _ t -> t)
        (reason, namespace_symbol, is_strict, legacy_interop)
        m
    | (ModuleT m, Annot_ImportModuleNsT (reason, namespace_symbol, is_strict)) ->
      let (values_type, types_tmap) = ImportModuleNsTKit.on_ModuleT cx (reason, is_strict) m in
      NamespaceT { namespace_symbol; values_type; types_tmap }
    | (ModuleT m, Annot_ImportDefaultT (reason, import_kind, local, is_strict)) ->
      let (_name_loc_opt, t) =
        ImportDefaultTKit.on_ModuleT
          cx
          ~with_concretized_type
          (reason, import_kind, local, is_strict)
          m
      in
      t
    | (ModuleT m, Annot_ImportNamedT (reason, import_kind, export_name, module_name, is_strict)) ->
      let (_name_loc_opt, t) =
        ImportNamedTKit.on_ModuleT
          cx
          ~with_concretized_type
          (reason, import_kind, export_name, module_name, is_strict)
          m
      in
      t
    | (AnyT (_, src), (Annot_CJSRequireT { reason; _ } | Annot_ImportModuleNsT (reason, _, _))) ->
      AnyT.why src reason
    | (AnyT (_, src), Annot_ImportDefaultT (reason, _, _, _)) -> AnyT.why src reason
    | (AnyT (_, src), Annot_ImportNamedT (reason, _, _, _, _)) -> AnyT.why src reason
    (************************************)
    (* Wildcards (idx, maybe, optional) *)
    (************************************)
    | (MaybeT (reason, _), _)
    | (OptionalT { reason; _ }, _) ->
      (* These are rare in practice. Will consider adding support if we hit this
       * error case. *)
      error_unsupported cx reason op
    (*********************)
    (* Type applications *)
    (*********************)
    | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
      let reason_op = Type.AConstraint.reason_of_op op in
      let tc = specialize_class cx c reason_op reason_tapp ts in
      let t = this_specialize cx reason_tapp this tc in
      elab_t cx t op
    | ( TypeAppT
          { reason = reason_tapp; use_op = typeapp_use_op; type_; targs; from_value; use_desc = _ },
        _
      ) ->
      (* NOTE omitting TypeAppExpansion.push_unless_loop check. *)
      let reason_op = Type.AConstraint.reason_of_op op in
      let t =
        mk_typeapp_instance
          cx
          ~use_op:typeapp_use_op
          ~reason_op
          ~reason_tapp
          ~from_value
          type_
          targs
      in
      elab_t cx t op
    (****************)
    (* Opaque types *)
    (****************)
    | (OpaqueT (_, { super_t = Some super_t; _ }), Annot_ToStringT { reason; _ }) ->
      elab_t cx super_t (Annot_ToStringT { orig_t = Some t; reason })
    | (OpaqueT (r, { underlying_t = Some t; _ }), _)
      when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
      elab_t cx ~seen t op
    (********)
    (* Keys *)
    (********)
    | (KeysT _, Annot_ToStringT _) -> t
    | (KeysT (reason, t), _) ->
      let t = elab_t cx t (Annot_GetKeysT reason) in
      elab_t cx t op
    | (DefT (_, ObjT { flags; props_tmap; _ }), Annot_GetKeysT reason_op) ->
      let dict_t = Obj_type.get_dict_opt flags.obj_kind in
      (* flow the union of keys of l to keys *)
      let keylist = Flow_js_utils.keylist_of_props (Context.find_props cx props_tmap) reason_op in
      let keylist =
        match dict_t with
        | None -> keylist
        | Some { key; _ } ->
          let key = elab_t cx key (Annot_ToStringT { orig_t = None; reason = reason_op }) in
          key :: keylist
      in
      union_of_ts reason_op keylist
    | (DefT (_, InstanceT { inst; _ }), Annot_GetKeysT reason_op) ->
      (* methods are not enumerable, so only walk fields *)
      let own_props = Context.find_props cx inst.own_props in
      let keylist = Flow_js_utils.keylist_of_props own_props reason_op in
      union_of_ts reason_op keylist
    | (AnyT _, Annot_GetKeysT reason_op) -> StrT.why reason_op
    (***********)
    (* $Values *)
    (***********)
    | (DefT (_, ObjT o), Annot_GetValuesT reason) ->
      Flow_js_utils.get_values_type_of_obj_t cx o reason
    | (DefT (_, InstanceT { inst = { own_props; inst_dict; _ }; _ }), Annot_GetValuesT reason) ->
      Flow_js_utils.get_values_type_of_instance_t cx own_props inst_dict reason
    (* Any will always be ok *)
    | (AnyT (_, src), Annot_GetValuesT reason) -> AnyT.why src reason
    (********************************)
    (* Union and intersection types *)
    (********************************)
    | (UnionT (reason, rep), Annot_MakeExactT reason_op) ->
      let ts = UnionRep.members rep in
      let f t = ExactT (reason_op, t) in
      let ts' = Base.List.map ts ~f in
      let reason' = repos_reason (loc_of_reason reason_op) reason in
      union_of_ts reason' ts'
    | (UnionT _, Annot_ObjKitT (reason, use_op, resolve_tool, tool)) ->
      object_kit_concrete cx use_op op reason resolve_tool tool t
    | (UnionT (_, rep), _) ->
      let reason = Type.AConstraint.reason_of_op op in
      let ts = UnionRep.members rep in
      let ts = Base.List.map ~f:(fun t -> elab_t cx ~seen t op) ts in
      union_of_ts reason ts
    | (IntersectionT _, Annot_ObjKitT (reason, use_op, resolve_tool, tool)) ->
      object_kit_concrete cx use_op op reason resolve_tool tool t
    | (IntersectionT (reason, _), _) ->
      (* Handling intersections as inputs would require use of speculation. Instead,
       * we ask the user to provide a simpler type. *)
      error_unsupported cx reason op
    (*************)
    (* Unary not *)
    (*************)
    (* any propagation *)
    | (AnyT _, Annot_NotT _) -> t
    (* !x when x is of unknown truthiness *)
    | (DefT (_, BoolT None), Annot_NotT reason)
    | (DefT (_, StrT AnyLiteral), Annot_NotT reason)
    | (DefT (_, NumT AnyLiteral), Annot_NotT reason) ->
      BoolT.at (loc_of_reason reason)
    (* !x when x is falsy *)
    | (DefT (_, BoolT (Some false)), Annot_NotT reason)
    | (DefT (_, SingletonBoolT false), Annot_NotT reason)
    | (DefT (_, StrT (Literal (_, OrdinaryName ""))), Annot_NotT reason)
    | (DefT (_, SingletonStrT (OrdinaryName "")), Annot_NotT reason)
    | (DefT (_, NumT (Literal (_, (0., _)))), Annot_NotT reason)
    | (DefT (_, SingletonNumT (0., _)), Annot_NotT reason)
    | (DefT (_, NullT), Annot_NotT reason)
    | (DefT (_, VoidT), Annot_NotT reason) ->
      let reason = replace_desc_reason (RBooleanLit true) reason in
      DefT (reason, BoolT (Some true))
    (* !x when x is truthy *)
    | (_, Annot_NotT reason) ->
      let reason = replace_desc_reason (RBooleanLit false) reason in
      DefT (reason, BoolT (Some false))
    (*****************************)
    (* Singleton primitive types *)
    (*****************************)
    | (DefT (reason, NumericStrKeyT (_, s)), _) ->
      elab_t cx (DefT (reason, StrT (Literal (None, OrdinaryName s)))) op
    | (DefT (reason, SingletonStrT key), _) ->
      elab_t cx (DefT (reason, StrT (Literal (None, key)))) op
    | (DefT (reason, SingletonNumT lit), _) ->
      elab_t cx (DefT (reason, NumT (Literal (None, lit)))) op
    | (DefT (reason, SingletonBoolT b), _) -> elab_t cx (DefT (reason, BoolT (Some b))) op
    | (NullProtoT reason, _) -> elab_t cx (DefT (reason, NullT)) op
    (*********)
    (* Exact *)
    (*********)
    | (ExactT (r, t), _) ->
      let t = push_type_alias_reason r t in
      let t = make_exact cx r t in
      elab_t cx t op
    | (DefT (reason_obj, ObjT obj), Annot_MakeExactT reason_op) ->
      TypeUtil.make_exact_object ~reason_obj obj ~reason_op
    | (AnyT (_, src), Annot_MakeExactT reason_op) -> AnyT.why src reason_op
    | (DefT (_, VoidT), Annot_MakeExactT reason_op) -> VoidT.why reason_op
    | (DefT (_, EmptyT), Annot_MakeExactT reason_op) -> EmptyT.why reason_op
    | (_, Annot_MakeExactT reason_op) ->
      Flow_js_utils.add_output cx (Error_message.EUnsupportedExact (reason_op, reason_of_t t));
      AnyT.error reason_op
    (**********)
    (* Mixins *)
    (**********)
    | ( DefT (class_r, ClassT (ThisInstanceT (inst_r, { inst; _ }, is_this, this_name))),
        Annot_MixinT r
      ) ->
      (* A class can be viewed as a mixin by extracting its immediate properties,
       * and "erasing" its static and super *)
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      DefT
        ( class_r,
          ClassT
            (ThisInstanceT (inst_r, { static; super; implements = []; inst }, is_this, this_name))
        )
    | ( DefT
          ( _,
            PolyT
              {
                tparams_loc;
                tparams = xs;
                t_out =
                  DefT (class_r, ClassT (ThisInstanceT (inst_r, { inst; _ }, is_this, this_name)));
                _;
              }
          ),
        Annot_MixinT r
      ) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      let instance = { static; super; implements = []; inst } in
      poly_type
        (Type.Poly.generate_id ())
        tparams_loc
        xs
        (DefT (class_r, ClassT (ThisInstanceT (inst_r, instance, is_this, this_name))))
    | (AnyT (_, src), Annot_MixinT r) -> AnyT.why src r
    (***********************)
    (* Type specialization *)
    (***********************)
    | ( DefT (_, PolyT { tparams_loc; tparams = xs; t_out = t; id }),
        Annot_SpecializeT (use_op, reason_op, reason_tapp, ts)
      ) ->
      let ts = Base.Option.value ts ~default:[] in
      mk_typeapp_of_poly cx ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts
    | (DefT (_, ClassT _), Annot_SpecializeT (_, _, _, None)) -> t
    | (AnyT _, Annot_SpecializeT _) -> t
    | (DefT (_, ClassT (ThisInstanceT (r, i, _, this_name))), Annot_ThisSpecializeT (reason, this))
      ->
      let i = subst_instance_type cx (Subst_name.Map.singleton this_name this) i in
      reposition cx (loc_of_reason reason) (DefT (r, InstanceT i))
    (* this-specialization of non-this-abstracted classes is a no-op *)
    | (DefT (_, ClassT i), Annot_ThisSpecializeT (reason, _this)) ->
      reposition cx (loc_of_reason reason) i
    | (AnyT _, Annot_ThisSpecializeT (reason, _)) -> reposition cx (loc_of_reason reason) t
    (**********************)
    (* Type instantiation *)
    (**********************)
    | (DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
      let use_op = unknown_use in
      let reason_op = Type.AConstraint.reason_of_op op in
      let (t, _) = instantiate_poly cx ~use_op ~reason_op ~reason_tapp (tparams_loc, ids, t) in
      elab_t cx t op
    | (ThisInstanceT (r, i, is_this, this_name), _) ->
      let reason = Type.AConstraint.reason_of_op op in
      let t = Flow_js_utils.fix_this_instance cx reason (r, i, is_this, this_name) in
      elab_t cx t op
    (*****************************)
    (* React Abstract Components *)
    (*****************************)
    | (DefT (r, ReactAbstractComponentT _), (Annot_GetPropT _ | Annot_GetElemT _)) ->
      let statics = Flow_js_utils.lookup_builtin_type cx "React$AbstractComponentStatics" r in
      elab_t cx statics op
    (****************)
    (* Custom types *)
    (****************)
    | (CustomFunT (r, _), _) when function_like_op op -> elab_t cx (FunProtoT r) op
    (*****************)
    (* ObjTestProtoT *)
    (*****************)
    | (AnyT (_, src), Annot_ObjTestProtoT reason_op) -> AnyT.why src reason_op
    | (DefT (_, NullT), Annot_ObjTestProtoT reason_op) -> NullProtoT.why reason_op
    | (_, Annot_ObjTestProtoT reason_op) ->
      if Flow_js_utils.object_like t then
        reposition cx (loc_of_reason reason_op) t
      else
        let () =
          Flow_js_utils.add_output
            cx
            (Error_message.EInvalidPrototype (loc_of_reason reason_op, reason_of_t t))
        in
        ObjProtoT.why reason_op
    (***************)
    (* Get statics *)
    (***************)
    | (DefT (_, InstanceT { static; _ }), Annot_GetStaticsT reason_op) ->
      reposition cx (loc_of_reason reason_op) static
    | (AnyT (_, src), Annot_GetStaticsT reason_op) -> AnyT.why src reason_op
    | (ObjProtoT _, Annot_GetStaticsT reason_op) ->
      (* ObjProtoT not only serves as the instance type of the root class, but
       * also as the statics of the root class. *)
      reposition cx (loc_of_reason reason_op) t
    (***************)
    (* LookupT pt1 *)
    (***************)
    | ( DefT (_lreason, InstanceT { super; inst; _ }),
        Annot_LookupT (reason_op, use_op, (Named _ as propref), objt)
      ) ->
      let react_dro =
        match objt with
        | DefT (_, ObjT o) -> o.flags.react_dro
        | _ -> None
      in
      (match
         GetPropTKit.get_instance_prop
           cx
           dummy_trace
           ~use_op
           ~ignore_dicts:true
           inst
           propref
           reason_op
       with
      | Some (p, _) ->
        GetPropTKit.perform_read_prop_action
          cx
          dummy_trace
          use_op
          propref
          (Property.type_ p)
          reason_op
          react_dro
      | None -> Get_prop_helper.cg_lookup_ cx use_op super reason_op propref objt)
    | (DefT (reason, InstanceT _), Annot_LookupT (_, _, Computed _, _)) ->
      error_unsupported cx reason op
    | (DefT (_, ObjT o), Annot_LookupT (reason_op, use_op, propref, objt)) ->
      let react_dro =
        match objt with
        | DefT (_, ObjT o) -> o.flags.react_dro
        | _ -> None
      in
      (match GetPropTKit.get_obj_prop cx dummy_trace o propref reason_op with
      | Some (p, _) ->
        GetPropTKit.perform_read_prop_action cx dummy_trace use_op propref p reason_op react_dro
      | None -> Get_prop_helper.cg_lookup_ cx use_op o.proto_t reason_op propref objt)
    | (AnyT _, Annot_LookupT (reason_op, _use_op, _propref, _)) -> AnyT.untyped reason_op
    (************)
    (* DRO *)
    (************)
    | (DefT (r, ObjT ({ Type.flags; _ } as o)), Annot_DeepReadOnlyT (_, dro_loc, dro_kind)) ->
      DefT (r, ObjT { o with Type.flags = { flags with react_dro = Some (dro_loc, dro_kind) } })
    | ( DefT (r, ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro = _ })),
        Annot_DeepReadOnlyT (_, dro_loc, dro_kind)
      ) ->
      DefT
        ( r,
          ArrT (TupleAT { elem_t; elements; arity; inexact; react_dro = Some (dro_loc, dro_kind) })
        )
    | ( DefT (r, ArrT (ArrayAT { elem_t; tuple_view; react_dro = _ })),
        Annot_DeepReadOnlyT (_, dro_loc, dro_kind)
      ) ->
      DefT (r, ArrT (ArrayAT { elem_t; tuple_view; react_dro = Some (dro_loc, dro_kind) }))
    | (DefT (r, ArrT (ROArrayAT (t, _))), Annot_DeepReadOnlyT (_, dro_loc, dro_kind)) ->
      DefT (r, ArrT (ROArrayAT (t, Some (dro_loc, dro_kind))))
    (************)
    (* ObjRestT *)
    (************)
    | ( DefT (reason_obj, ObjT { props_tmap; flags = { obj_kind; _ }; _ }),
        Annot_ObjRestT (reason_op, xs)
      ) ->
      Flow_js_utils.objt_to_obj_rest
        cx
        props_tmap
        ~reachable_targs:[]
        ~obj_kind
        ~reason_op
        ~reason_obj
        xs
    | (DefT (reason, InstanceT _), Annot_ObjRestT _) ->
      (* This implementation relies on unsealed objects and set-prop logic that is
       * hard to implement in annotation inference. *)
      error_unsupported cx reason op
    | (AnyT (_, src), Annot_ObjRestT (reason, _)) -> AnyT.why src reason
    | (ObjProtoT _, Annot_ObjRestT (reason, _)) ->
      Obj_type.mk_with_proto cx reason ~obj_kind:Exact t
    | (DefT (_, (NullT | VoidT)), Annot_ObjRestT (reason, _)) ->
      Obj_type.mk ~obj_kind:Exact cx reason
    (************************************)
    (* Namespace and type qualification *)
    (************************************)
    | ( NamespaceT { namespace_symbol = _; values_type; types_tmap },
        Annot_GetTypeFromNamespaceT
          { reason = reason_op; use_op; prop_ref = (prop_ref_reason, prop_name) }
      ) ->
      (match
         NameUtils.Map.find_opt prop_name (Context.find_props cx types_tmap)
         |> Base.Option.bind ~f:Type.Property.read_t
       with
      | Some prop -> prop
      | None ->
        elab_t
          cx
          ~seen
          values_type
          (Annot_GetPropT
             ( reason_op,
               use_op,
               Named { reason = prop_ref_reason; name = prop_name; from_indexed_access = false }
             )
          ))
    | (NamespaceT { namespace_symbol = _; values_type; types_tmap = _ }, _) ->
      elab_t cx ~seen values_type op
    | ( _,
        Annot_GetTypeFromNamespaceT
          { reason = reason_op; use_op; prop_ref = (prop_ref_reason, prop_name) }
      ) ->
      elab_t
        cx
        ~seen
        t
        (Annot_GetPropT
           ( reason_op,
             use_op,
             Named { reason = prop_ref_reason; name = prop_name; from_indexed_access = false }
           )
        )
    (************)
    (* GetPropT *)
    (************)
    | ( DefT (reason_instance, InstanceT { super; inst; _ }),
        Annot_GetPropT (reason_op, use_op, (Named _ as propref))
      ) ->
      GetPropTKit.read_instance_prop
        cx
        dummy_trace
        ~use_op
        ~instance_t:t
        ~id:None
        ~method_accessible:false
        ~super
        ~lookup_kind:(Strict reason_instance)
        ~hint:hint_unavailable
        inst
        propref
        reason_op
    | (DefT (reason, InstanceT _), Annot_GetPropT (_, _, Computed _)) ->
      error_unsupported cx reason op
    | ( DefT (_, ObjT _),
        Annot_GetPropT (reason_op, _, Named { name = OrdinaryName "constructor"; _ })
      ) ->
      Unsoundness.why Constructor reason_op
    | (DefT (reason_obj, ObjT o), Annot_GetPropT (reason_op, use_op, propref)) ->
      GetPropTKit.read_obj_prop cx dummy_trace ~use_op o propref reason_obj reason_op None
    | (AnyT _, Annot_GetPropT (reason_op, _, _)) -> AnyT (reason_op, Untyped)
    | ( DefT (reason, ClassT instance),
        Annot_GetPropT (_, _, Named { name = OrdinaryName "prototype"; _ })
      ) ->
      reposition cx (loc_of_reason reason) instance
    (**************)
    (* Object Kit *)
    (**************)
    | (_, Annot_ObjKitT (reason, use_op, resolve_tool, tool)) ->
      object_kit_concrete cx use_op op reason resolve_tool tool t
    (********************)
    (* GetElemT / ElemT *)
    (********************)
    | (DefT (_, StrT _), Annot_GetElemT (reason_op, _use_op, _index)) ->
      (* NOTE bypassing check that index is a number *)
      StrT.why reason_op
    | ((DefT (_, (ObjT _ | ArrT _ | InstanceT _)) | AnyT _), Annot_GetElemT (reason_op, use_op, key))
      ->
      elab_t cx key (Annot_ElemT (reason_op, use_op, t))
    | (_, Annot_ElemT (reason_op, use_op, (DefT (_, (ObjT _ | InstanceT _)) as obj))) ->
      let propref = Flow_js_utils.propref_for_elem_t t in
      elab_t cx obj (Annot_GetPropT (reason_op, use_op, propref))
    | (_, Annot_ElemT (reason_op, _use_op, (AnyT _ as _obj))) ->
      let value = AnyT.untyped reason_op in
      reposition cx (loc_of_reason reason_op) value
    | (AnyT _, Annot_ElemT (reason_op, _, DefT (_, ArrT arrtype))) ->
      let value = elemt_of_arrtype arrtype in
      reposition cx (loc_of_reason reason_op) value
    | (DefT (_, NumT _), Annot_ElemT (reason_op, use_op, DefT (reason_tup, ArrT arrtype))) ->
      let (value, _, _, _) =
        Flow_js_utils.array_elem_check ~write_action:false cx t use_op reason_op reason_tup arrtype
      in
      reposition cx (loc_of_reason reason_op) value
    | (DefT (_, ObjT o), Annot_ObjKeyMirror reason_op) ->
      Flow_js_utils.obj_key_mirror cx o reason_op
    | (DefT (_, ObjT o), Annot_ObjMapConst (reason_op, target)) ->
      Flow_js_utils.obj_map_const cx o reason_op target
    | (DefT (_, EnumValueT enum_info), Annot_GetEnumT reason) ->
      DefT (reason, EnumObjectT { enum_value_t = t; enum_info })
    (***********************)
    (* Opaque types (pt 2) *)
    (***********************)
    | (OpaqueT (_, { super_t = Some t; _ }), _) -> elab_t cx t op
    (**************************)
    (* Binary arith operators *)
    (**************************)
    | (lhs_t, Annot_ArithT { reason; flip; rhs_t; kind }) ->
      if Flow_js_utils.needs_resolution rhs_t || Flow_js_utils.is_generic rhs_t then
        elab_t cx rhs_t (Annot_ArithT { reason; flip = not flip; rhs_t = lhs_t; kind })
      else
        let (lhs_t, rhs_t) =
          if flip then
            (rhs_t, lhs_t)
          else
            (lhs_t, rhs_t)
        in
        Flow_js_utils.flow_arith cx reason lhs_t rhs_t kind
    (*************************)
    (* Unary arith operators *)
    (*************************)
    | (l, Annot_UnaryArithT (reason, kind)) -> Flow_js_utils.flow_unary_arith cx l reason kind
    (********************)
    (* Function Statics *)
    (********************)
    | (DefT (reason, FunT (static, _)), _) when object_like_op op ->
      let static = reposition cx (loc_of_reason reason) static in
      elab_t cx static op
    (*****************)
    (* Class statics *)
    (*****************)
    | (DefT (reason, ClassT instance), _) when object_like_op op ->
      let t = get_statics cx reason instance in
      elab_t cx t op
    (*********)
    (* Enums *)
    (*********)
    | ( DefT (enum_reason, EnumObjectT { enum_value_t; enum_info = ConcreteEnum enum_info }),
        Annot_GetPropT (access_reason, use_op, Named { reason = prop_reason; name; _ })
      ) ->
      let access = (use_op, access_reason, None, (prop_reason, name)) in
      GetPropTKit.on_EnumObjectT
        cx
        dummy_trace
        enum_reason
        ~enum_object_t:t
        ~enum_value_t
        ~enum_info
        access
    | (DefT (enum_reason, EnumObjectT _), Annot_GetElemT (reason_op, _, elem)) ->
      let reason = reason_of_t elem in
      Flow_js_utils.add_output
        cx
        (Error_message.EEnumInvalidMemberAccess
           { member_name = None; suggestion = None; reason; enum_reason }
        );
      AnyT.error reason_op
    (***************)
    (* LookupT pt2 *)
    (***************)
    | (ObjProtoT _, Annot_LookupT (reason_op, _, Named { name; _ }, _))
      when Flow_js_utils.is_object_prototype_method name ->
      Flow_js_utils.lookup_builtin_value cx "Object" reason_op
    | (FunProtoT _, Annot_LookupT (reason_op, _, Named { name; _ }, _))
      when Flow_js_utils.is_function_prototype name ->
      Flow_js_utils.lookup_builtin_value cx "Function" reason_op
    | ( (DefT (_, NullT) | ObjProtoT _ | FunProtoT _),
        Annot_LookupT (reason_op, use_op, Named { reason = reason_prop; name; _ }, _)
      ) ->
      let error_message =
        Error_message.EPropNotFound
          { reason_prop; reason_obj = reason_op; prop_name = Some name; use_op; suggestion = None }
      in
      Flow_js_utils.add_output cx error_message;
      AnyT.error_of_kind UnresolvedName reason_op
    (****************************************)
    (* Object, function, etc. library calls *)
    (****************************************)
    | (ObjProtoT reason, _) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx reason ~use_desc "Object" in
      elab_t cx obj_proto op
    | (FunProtoT reason, _) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx reason ~use_desc "Function" in
      elab_t cx fun_proto op
    (*************)
    (* ToStringT *)
    (*************)
    | (DefT (_, StrT _), Annot_ToStringT _) -> t
    | (_, Annot_ToStringT { reason; _ }) -> StrT.why reason
    (************)
    (* GetPropT *)
    (************)
    | (DefT (reason, ArrT (ArrayAT { elem_t; _ })), (Annot_GetPropT _ | Annot_LookupT _)) ->
      let arr = get_builtin_typeapp cx reason "Array" [elem_t] in
      elab_t cx arr op
    | ( DefT (reason, ArrT (TupleAT { arity; inexact; _ })),
        Annot_GetPropT (reason_op, _, Named { name = OrdinaryName "length"; _ })
      ) ->
      GetPropTKit.on_array_length cx dummy_trace reason ~inexact arity reason_op
    | ( DefT (reason, ArrT ((TupleAT _ | ROArrayAT _) as arrtype)),
        (Annot_GetPropT _ | Annot_LookupT _)
      ) ->
      let t = elemt_of_arrtype arrtype in
      elab_t cx (get_builtin_typeapp cx reason "$ReadOnlyArray" [t]) op
    (************************)
    (* Promoting primitives *)
    (************************)
    | (DefT (reason, StrT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true "String" in
      elab_t cx builtin op
    | (DefT (reason, NumT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true "Number" in
      elab_t cx builtin op
    | (DefT (reason, BoolT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true "Boolean" in
      elab_t cx builtin op
    | (DefT (reason, SymbolT), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true "Symbol" in
      elab_t cx builtin op
    | (DefT (lreason, MixedT Mixed_function), (Annot_GetPropT _ | Annot_LookupT _)) ->
      elab_t cx (FunProtoT lreason) op
    | (_, _) ->
      let open Error_message in
      let reason_op = reason_of_op op in
      let lower = (reason_of_t t, Flow_js_utils.error_message_kind_of_lower t) in
      let upper = (reason_op, IncompatibleUnclassified (string_of_operation op)) in
      let use_op = use_op_of_operation op in
      Flow_js_utils.add_output cx (EIncompatible { lower; upper; use_op; branches = [] });
      AnyT.error reason_op

  and get_builtin_type cx reason ?(use_desc = false) name =
    let t = Flow_js_utils.lookup_builtin_type cx name reason in
    mk_instance_raw cx reason ~use_desc ~reason_type:(reason_of_t t) t

  and specialize cx t use_op reason_op reason_tapp ts =
    elab_t cx t (Annot_SpecializeT (use_op, reason_op, reason_tapp, ts))

  and this_specialize cx reason this t = elab_t cx t (Annot_ThisSpecializeT (reason, this))

  and specialize_class cx c reason_op reason_tapp ts =
    match ts with
    | None -> c
    | Some ts -> specialize cx c unknown_use reason_op reason_tapp (Some ts)

  and mk_type_reference cx ~type_t_kind reason c =
    let f id = resolve_id cx reason id (elab_t cx c (Annot_UseT_TypeT (reason, type_t_kind))) in
    let tvar = mk_lazy_tvar cx reason f in
    AnnotT (reason, tvar, false)

  and mk_instance cx ?type_t_kind instance_reason ?use_desc c =
    mk_instance_raw cx ?type_t_kind instance_reason ?use_desc ~reason_type:instance_reason c

  and mk_instance_raw
      cx ?(type_t_kind = InstanceKind) instance_reason ?(use_desc = false) ~reason_type c =
    let source = elab_t cx c (Annot_UseT_TypeT (reason_type, type_t_kind)) in
    AnnotT (instance_reason, source, use_desc)

  and mk_typeapp_instance cx ~use_op ~reason_op ~reason_tapp ~from_value c ts =
    let t = specialize cx c use_op reason_op reason_tapp (Some ts) in
    if from_value then
      mod_reason_of_t (fun _ -> reason_tapp) t
    else
      mk_instance_raw cx reason_tapp ~reason_type:(reason_of_t c) t

  and get_statics cx reason t = elab_t cx t (Annot_GetStaticsT reason)

  and get_prop cx use_op reason ?(op_reason = reason) name t =
    elab_t cx t (Annot_GetPropT (op_reason, use_op, mk_named_prop ~reason name))

  and get_elem cx use_op reason ~key t = elab_t cx t (Annot_GetElemT (reason, use_op, key))

  and qualify_type cx use_op reason ~op_reason prop_name t =
    let f id =
      let t =
        elab_t
          cx
          t
          (Annot_GetTypeFromNamespaceT
             { reason = op_reason; use_op; prop_ref = (reason, prop_name) }
          )
      in
      resolve_id cx op_reason id t
    in
    mk_lazy_tvar cx op_reason f

  and assert_export_is_type cx reason name t =
    let f id =
      let t = elab_t cx t (Annot_AssertExportIsTypeT (reason, Reason.OrdinaryName name)) in
      resolve_id cx reason id t
    in
    mk_lazy_tvar cx reason f

  and cjs_require cx t reason namespace_symbol is_strict legacy_interop =
    elab_t cx t (Annot_CJSRequireT { reason; namespace_symbol; is_strict; legacy_interop })

  and export_named cx reason export_kind value_exports_tmap type_exports_tmap t =
    elab_t cx t (Annot_ExportNamedT { reason; value_exports_tmap; type_exports_tmap; export_kind })

  and cjs_extract_named_exports cx reason local_module t =
    elab_t cx t (Annot_CJSExtractNamedExportsT (reason, local_module))

  and import_typeof cx reason export_name t = elab_t cx t (Annot_ImportTypeofT (reason, export_name))

  and import_default cx reason import_kind export_name module_name is_strict t =
    elab_t cx t (Annot_ImportDefaultT (reason, import_kind, (export_name, module_name), is_strict))

  and import_named cx reason import_kind export_name module_name is_strict t =
    elab_t cx t (Annot_ImportNamedT (reason, import_kind, export_name, module_name, is_strict))

  and import_ns cx reason namespace_symbol is_strict t =
    elab_t cx t (Annot_ImportModuleNsT (reason, namespace_symbol, is_strict))

  and copy_named_exports cx ~from_ns reason ~module_t =
    elab_t cx from_ns (Annot_CopyNamedExportsT (reason, module_t))

  and copy_type_exports cx ~from_ns reason ~module_t =
    elab_t cx from_ns (Annot_CopyTypeExportsT (reason, module_t))

  and arith cx reason lhs_t rhs_t kind =
    elab_t cx lhs_t (Annot_ArithT { reason; flip = false; rhs_t; kind })

  and unary_arith cx reason_op t kind = elab_t cx t (Annot_UnaryArithT (reason_op, kind))

  and unary_not cx reason_op t = elab_t cx t (Annot_NotT reason_op)

  and mixin cx reason t = elab_t cx t (Annot_MixinT reason)

  and obj_rest cx reason xs t = elab_t cx t (Annot_ObjRestT (reason, xs))

  and arr_rest cx _use_op reason_op _i t = error_unsupported_reason cx (reason_of_t t) reason_op

  and object_kit_concrete =
    let add_output cx msg : unit = Flow_js_utils.add_output cx msg in
    let return _cx _use_op t = t in
    let recurse cx use_op reason resolve_tool tool x =
      object_kit cx use_op reason resolve_tool tool x
    in
    let object_spread options state cx =
      let dict_check _cx _use_op _d1 _d2 = () in
      Slice_utils.object_spread ~dict_check ~add_output ~return ~recurse options state cx
    in
    let object_rest options state cx =
      let return _ _ _ t = t in
      (* No subtyping checks in annotation inference *)
      let subt_check ~use_op:_ _ _ = () in
      Slice_utils.object_rest ~add_output ~return ~recurse ~subt_check options state cx
    in
    let check_component_config cx pmap =
      let return _ _ t = t in
      Slice_utils.check_component_config ~add_output ~return pmap cx
    in
    let object_read_only cx _use_op = Slice_utils.object_read_only cx in
    let object_partial cx _use_op = Slice_utils.object_update_optionality `Partial cx in
    let object_required cx _use_op = Slice_utils.object_update_optionality `Required cx in
    let next op cx use_op tool reason x =
      Object.(
        match tool with
        | Spread (options, state) -> object_spread options state cx use_op reason x
        | Rest (options, state) -> object_rest options state cx use_op reason x
        | Partial -> object_partial cx use_op reason x
        | Required -> object_required cx use_op reason x
        | ReadOnly -> object_read_only cx use_op reason x
        | ReactConfig _ -> error_internal cx "ReactConfig" op
        | Object.ReactCheckComponentConfig pmap -> check_component_config cx pmap use_op reason x
        | ObjectRep -> error_internal cx "ObjectRep" op
        | Object.ObjectMap _ ->
          (* TODO(jmbrown): Annotation inference for Mapped Types *)
          error_internal cx "ObjectMap" op
      )
    in
    let statics = get_statics in
    fun cx use_op op reason resolve_tool tool t ->
      Slice_utils.run
        ~add_output
        ~return
        ~next:(next op)
        ~recurse
        ~statics
        cx
        use_op
        reason
        resolve_tool
        tool
        t

  and object_kit cx use_op reason resolve_tool tool t =
    elab_t cx t (Annot_ObjKitT (reason, use_op, resolve_tool, tool))

  and object_spread cx use_op reason target state t =
    let resolve_tool = Type.Object.(Resolve Next) in
    let tool = Type.Object.Spread (target, state) in
    object_kit cx use_op reason resolve_tool tool t

  and object_rest cx use_op reason target state t =
    let resolve_tool = Type.Object.(Resolve Next) in
    let tool = Type.Object.Rest (target, state) in
    object_kit cx use_op reason resolve_tool tool t

  and make_readonly cx use_op reason t =
    let resolve_tool = Type.Object.(Resolve Next) in
    object_kit cx use_op reason resolve_tool Type.Object.ReadOnly t

  and make_partial cx use_op reason t =
    let resolve_tool = Type.Object.(Resolve Next) in
    object_kit cx use_op reason resolve_tool Type.Object.Partial t

  and make_required cx use_op reason t =
    let resolve_tool = Type.Object.(Resolve Next) in
    object_kit cx use_op reason resolve_tool Type.Object.Required t

  and make_exact cx reason t = elab_t cx t (Annot_MakeExactT reason)

  and obj_test_proto cx reason_op t = elab_t cx t (Annot_ObjTestProtoT reason_op)
end
