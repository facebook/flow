(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Subst
open Type
open Type.AConstraint
open TypeUtil

let object_like_op = function
  | Annot_SpecializeT _
  | Annot_ThisSpecializeT _
  | Annot_UseT_TypeT _
  | Annot_CJSRequireT _
  | Annot_ImportTypeT _
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
  | Annot_UnaryMinusT _
  | Annot_NotT _
  | Annot_ObjKeyMirror _
  | Annot_ObjMapConst _
  | Annot_GetKeysT _
  | Annot_ToStringT _
  | Annot__Future_added_value__ _ ->
    false
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
  let (_, node) = Context.find_root cx id in
  match node.Constraint.constraints with
  | Constraint.FullyResolved (_, (lazy t)) -> t
  | Constraint.Resolved _
  | Constraint.Unresolved _ ->
    failwith "unexpected unresolved constraint in annotation inference"

let get_builtin_typeapp cx reason x targs =
  let t = Flow_js_utils.lookup_builtin_strict cx x reason in
  TypeUtil.typeapp reason t targs

module type S = sig
  val unresolved_tvar : Context.t -> Reason.t -> int

  val mk_typeof_annotation : Context.t -> ?trace:Type.trace -> Reason.t -> Type.t -> Type.t

  val mk_type_reference : Context.t -> Reason.t -> Type.t -> Type.t

  val get_prop : Context.t -> Type.use_op -> Reason.t -> Reason.name -> Type.t -> Type.t

  val get_elem : Context.t -> Type.use_op -> Reason.t -> key:Type.t -> Type.t -> Type.t

  val qualify_type :
    Context.t -> Type.use_op -> Reason.t -> Reason.t * Reason.name -> Type.t -> Type.t

  val assert_export_is_type : Context.t -> Reason.t -> string -> Type.t -> Type.t

  val resolve_id : Context.t -> Type.ident -> Type.t -> unit

  val mk_sig_tvar : Context.t -> Reason.t -> Type.t Lazy.t -> Type.t

  val cjs_require : Context.t -> Type.t -> Reason.t -> bool -> Type.t

  val export_named :
    Context.t ->
    Reason.reason ->
    Type.export_kind ->
    (ALoc.t option * Type.t) NameUtils.Map.t ->
    Type.t ->
    Type.t

  val cjs_extract_named_exports :
    Context.t -> Reason.reason -> Reason.reason * Type.exporttypes * bool -> Type.t -> Type.t

  val import_default :
    Context.t -> Reason.t -> Type.import_kind -> string -> string -> bool -> Type.t -> Type.t

  val import_named :
    Context.t -> Reason.t -> Type.import_kind -> string -> string -> bool -> Type.t -> Type.t

  val import_ns : Context.t -> Reason.t -> bool -> Type.t -> Type.t

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

  val unary_minus : Context.t -> Reason.t -> Type.t -> Type.t

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
  let error_unsupported_reason ?suggestion cx t reason_op =
    let loc = Reason.aloc_of_reason reason_op in
    let msg =
      Error_message.EAnnotationInference (loc, reason_op, TypeUtil.reason_of_t t, suggestion)
    in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason_op

  let error_unsupported ?suggestion cx t op =
    let reason_op = AConstraint.display_reason_of_op op in
    error_unsupported_reason ?suggestion cx t reason_op

  let error_recursive cx reason =
    let loc = Reason.aloc_of_reason reason in
    let msg = Error_message.EAnnotationInferenceRecursive (loc, reason) in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason

  let error_internal_reason cx msg reason_op =
    let loc = Reason.aloc_of_reason reason_op in
    let msg = Error_message.(EInternal (loc, UnexpectedAnnotationInference msg)) in
    (match !dst_cx_ref with
    | None -> assert false
    | Some dst_cx -> Flow_js_utils.add_annot_inference_error ~src_cx:cx ~dst_cx msg);
    AnyT.error reason_op

  let error_internal cx msg op =
    let reason_op = AConstraint.display_reason_of_op op in
    error_internal_reason cx msg reason_op

  let dummy_trace = Trace.dummy_trace

  (* Repositioning does not seem to have any perceptible impact in annotation
   * inference. Instead of replicating the convoluted implementation of Flow_js
   * here, we just return the same type intact. *)
  let reposition _cx _loc t = t

  (*****************)
  (* Instantiation *)
  (*****************)
  module Instantiation_helper = struct
    let cache_instantiate _cx _trace ~use_op:_ ?cache:_ _typeparam _reason_op _reason_tapp t = t

    (* We will not be solving implicit instantiation problems here. The only case
     * where we will need to use this function is when a PolyT needs to be used
     * as a monomorphic type. In this case, the only sensible thing to do is to
     * use the bound of each parameter as the argument to the intantiation. *)
    let mk_targ _cx typeparam _reason_op _reason_tapp = typeparam.Type.bound

    let is_subtype _cx _trace ~use_op:_ (_t1, _t2) = ()

    let reposition cx ?trace:_ loc ?desc:_ ?annot_loc:_ t = reposition cx loc t

    let unresolved_id = Avar.unresolved

    let resolve_id cx _trace ~use_op:_ (_, id) t = ConsGen.resolve_id cx id t
  end

  module InstantiationKit = Flow_js_utils.Instantiation_kit (Instantiation_helper)

  let instantiate_poly cx = InstantiationKit.instantiate_poly cx dummy_trace

  let mk_typeapp_of_poly cx = InstantiationKit.mk_typeapp_of_poly cx dummy_trace

  let fix_this_class cx = InstantiationKit.fix_this_class cx dummy_trace

  (***********)
  (* Imports *)
  (***********)
  module Import_export_helper = struct
    type r = Type.t

    let reposition cx ?trace:_ loc ?desc:_ ?annot_loc:_ t = reposition cx loc t

    let return _cx ~use_op:_ _trace t = t

    let import_type cx _ reason export_name t =
      ConsGen.elab_t cx t (Annot_ImportTypeT (reason, export_name))

    let import_typeof cx _trace reason export_name t = ConsGen.import_typeof cx reason export_name t

    let export_named cx _trace (reason, named, kind) t = ConsGen.export_named cx reason kind named t

    let export_named_fresh_var = export_named

    let export_type cx _trace (reason, export_name, target_module_t) export_t =
      ConsGen.elab_t cx export_t (Annot_ExportTypeT (reason, export_name, target_module_t))

    let cjs_extract_named_exports cx _ (reason, local_module) t =
      ConsGen.cjs_extract_named_exports cx reason local_module t

    (* This check is bypassed in annotation inference *)
    let assert_import_is_value _cx _trace _reason _name _export_t = ()

    let error_type = AnyT.error

    let fix_this_class = InstantiationKit.fix_this_class

    let mk_typeof_annotation = ConsGen.mk_typeof_annotation
  end

  module CJSRequireTKit = Flow_js_utils.CJSRequireT_kit (Import_export_helper)
  module ImportModuleNsTKit = Flow_js_utils.ImportModuleNsT_kit (Import_export_helper)
  module ImportDefaultTKit = Flow_js_utils.ImportDefaultT_kit (Import_export_helper)
  module ImportNamedTKit = Flow_js_utils.ImportNamedT_kit (Import_export_helper)
  module ImportTypeTKit = Flow_js_utils.ImportTypeT_kit (Import_export_helper)
  module ImportTypeofTKit = Flow_js_utils.ImportTypeofT_kit (Import_export_helper)
  module ExportNamedTKit = Flow_js_utils.ExportNamedT_kit (Import_export_helper)
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

    let perform_read_prop_action cx use_op propref p ureason =
      match Property.read_t p with
      | Some t -> reposition cx (aloc_of_reason ureason) t
      | None ->
        let (reason_prop, prop_name) =
          match propref with
          | Named (r, x) -> (r, Some x)
          | Computed t -> (reason_of_t t, None)
        in
        let msg = Error_message.EPropNotReadable { reason_prop; prop_name; use_op } in
        Flow_js_utils.add_output cx msg;
        AnyT.error ureason

    let cg_lookup_ cx use_op t reason_op propref =
      ConsGen.elab_t cx t (Annot_LookupT (reason_op, use_op, propref))

    let read_prop cx _trace options reason_prop reason_op l _super x pmap =
      let { Flow_js_utils.Access_prop_options.use_op; _ } = options in
      let propref = Named (reason_prop, x) in
      match NameUtils.Map.find_opt x pmap with
      | Some p -> perform_read_prop_action cx use_op propref p reason_op
      | None ->
        let l =
          (* munge names beginning with single _ *)
          if Flow_js_utils.is_munged_prop_name cx x then
            ObjProtoT (reason_of_t l)
          else
            l
        in
        cg_lookup_ cx use_op l reason_op propref

    let error_type = AnyT.error

    (* We could have just returned `t` here. The OpenT indirection is for compatibility
     * with Flow_js. Specifically, without the OpenT the transformation in
     * https://github.com/facebook/flow/blob/8c3825a1be188e9ade4ad4ed515361bb28c65d8a/src/typing/flow_js.ml#L1744-L1755
     * would fire, causing a divergence in the behavior of this module and Flow_js. *)
    let return cx ~use_op _trace t =
      match t with
      | OpenT _ -> t
      | _ -> Tvar.mk_fully_resolved cx use_op (reason_of_t t) t

    (* We will not be doing subtyping checks in annotation inference. *)
    let dict_read_check _ _ ~use_op:_ _ = ()

    let reposition cx ?trace:_ loc ?desc:_ ?annot_loc:_ t = reposition cx loc t

    let enum_proto cx _trace ~reason (enum_reason, trust, enum) =
      let enum_t = DefT (enum_reason, trust, EnumT enum) in
      let { representation_t; _ } = enum in
      get_builtin_typeapp cx reason (OrdinaryName "$EnumProto") [enum_t; representation_t]

    let cg_lookup cx _trace ~obj_t:_ t (reason_op, _kind, propref, use_op, _ids) =
      cg_lookup_ cx use_op t reason_op propref

    let cg_get_prop cx _trace t (use_op, access_reason, _, (prop_reason, name)) =
      ConsGen.elab_t cx t (Annot_GetPropT (access_reason, use_op, Named (prop_reason, name)))
  end

  module GetPropTKit = Flow_js_utils.GetPropT_kit (Get_prop_helper)

  let unresolved_tvar cx reason = Avar.unresolved cx reason

  (** [ensure_annot_resolved cx reason id] ensures that the annotation constraint
   *  associated with [id] has been resolved. If the respective constraint is already
   *  resolved then it returns immediately. Otherwise, it resolves [id] immediately
   *  to the 'any' type. In the case of an [Anno_op (_, _, dep_id)] constraint we also
   *  update the "dependents" set of [dep_id], so that we don't attempt to resolve
   *  [id] once again when [dep_id] gets resolved.
   *)
  let rec ensure_annot_resolved cx reason id =
    let module A = Type.AConstraint in
    match Context.find_avar cx id with
    | (_, { A.constraints = A.Annot_resolved; _ }) -> get_fully_resolved_type cx id
    | (_, { A.constraints = A.Annot_unresolved _; _ }) ->
      let t = error_recursive cx reason in
      resolve_id cx id t;
      t
    | (root_id, { A.constraints = A.Annot_op { id = dep_id; _ }; _ }) ->
      let (_, { A.constraints = dep_constraint; _ }) = Context.find_avar cx dep_id in
      A.update_deps_of_constraint dep_constraint ~f:(fun deps ->
          ISet.filter
            (fun id2 ->
              let (root_id2, _) = Context.find_avar cx id2 in
              root_id <> root_id2)
            deps
      );
      let t = error_recursive cx reason in
      resolve_id cx id t;
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
    let constraints = Constraint.FullyResolved (unknown_use, t) in
    Context.add_tvar cx id (Constraint.Root { Constraint.rank = 0; constraints });
    tvar

  and mk_sig_tvar cx reason (resolved : Type.t Lazy.t) =
    let f id =
      let t = Lazy.force resolved in
      resolve_id cx id t
    in
    mk_lazy_tvar cx reason f

  (** [resolve_id cx id1 t] resolves an annotation tvar [id1] to a type [t] *
   *  - If [t] is a concrete type, we mark [id1] as a resolved annotation tvar and
   *    record it as fully resolved in the type graph. *
   *  - If [t] is an OpenT (_, id2), then we unify [id1] and [id2]. (See merge_ids.)
   *)
  and resolve_id cx id t =
    let module A = Type.AConstraint in
    let module C = Type.Constraint in
    match t with
    | Type.OpenT (_, id2) -> merge_ids cx id id2
    | _ ->
      let (root_id1, root1) = Context.find_avar cx id in
      Context.add_avar cx root_id1 A.fully_resolved_node;
      Context.add_tvar cx root_id1 (C.fully_resolved_node t);
      let dependents1 = deps_of_constraint root1.A.constraints in
      resolve_dependent_set cx dependents1 t

  (** Makes id1 a goto node to id2. It also appends depndents of id1 to those of id2.
   *  If id2 is a resolved node, then dependents can be immediately resolved using
   *  the resolved type of id2. *)
  and goto cx id1 dependents1 (id2, root2) =
    let module A = Type.AConstraint in
    let module T = Type.Constraint in
    Context.add_tvar cx id1 (T.Goto id2);
    Context.add_avar cx id1 (A.Goto id2);
    match root2.A.constraints with
    | (A.Annot_op _ | A.Annot_unresolved _) as constraint_ ->
      update_deps_of_constraint ~f:(ISet.union dependents1) constraint_
    | A.Annot_resolved ->
      let t = get_fully_resolved_type cx id2 in
      resolve_dependent_set cx dependents1 t

  (** Similar to Flow_js.merge_ids. Uses rank information to determine which one
   *  of [id1] and [id2] will become the goto node and which one the root. *)
  and merge_ids cx id1 id2 =
    let module A = Type.AConstraint in
    let ((id1, root1), (id2, root2)) = (Context.find_avar cx id1, Context.find_avar cx id2) in
    if id1 = id2 then
      ()
    else if root1.A.rank < root2.A.rank then
      let deps1 = deps_of_constraint root1.A.constraints in
      goto cx id1 deps1 (id2, root2)
    else if root2.A.rank < root1.A.rank then
      let deps2 = deps_of_constraint root2.A.constraints in
      goto cx id2 deps2 (id1, root1)
    else (
      Context.add_avar cx id2 (A.Root { root2 with A.rank = root1.A.rank + 1 });
      let deps1 = deps_of_constraint root1.A.constraints in
      goto cx id1 deps1 (id2, root2)
    )

  and resolve_dependent_set cx dependents t =
    Context.iter_annot_dependent_set cx (fun id op -> resolve_id cx id (elab_t cx t op)) dependents

  and elab_open cx ~seen reason id op =
    if ISet.mem id seen then
      error_recursive cx reason
    else
      let module A = Type.AConstraint in
      let (_, { A.constraints; _ }) = Context.find_avar cx id in
      match constraints with
      | A.Annot_resolved ->
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
            ((* Annot_resolved ids definitelly appear in the type graph *)
             let t = get_fully_resolved_type cx id in
             elab_t cx ~seen:(ISet.add id seen) t op
            )
        in
        mk_sig_tvar cx (AConstraint.reason_of_op op) resolved
      | A.Annot_unresolved _
      | A.Annot_op _ ->
        let fresh_id = Avar.constrained cx op id in
        OpenT (reason, fresh_id)

  and elab_t cx ?(seen = ISet.empty) t op =
    match (t, op) with
    | (EvalT (t, TypeDestructorT (use_op, reason, ReadOnlyType), _), _) ->
      let t = make_readonly cx use_op reason t in
      elab_t cx t op
    | (EvalT (t, TypeDestructorT (use_op, reason, SpreadType (target, todo_rev, head_slice)), _), _)
      ->
      let state =
        Object.(
          Spread.
            {
              todo_rev;
              acc = Base.Option.value_map ~f:(fun x -> [InlineSlice x]) ~default:[] head_slice;
              spread_id = Reason.mk_id ();
              union_reason = None;
              curr_resolve_idx = 0;
            }
          
        )
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
    | (EvalT (_, TypeDestructorT (_, _, TypeMap (ObjectMap _)), _), _) ->
      error_unsupported ~suggestion:"$ObjMapConst" cx t op
    | (EvalT (_, TypeDestructorT (_, _, TypeMap (ObjectMapi _)), _), _) ->
      error_unsupported ~suggestion:"$KeyMirror" cx t op
    | (EvalT _, _) -> error_unsupported cx t op
    | (OpenT (reason, id), _) -> elab_open cx ~seen reason id op
    | (TypeDestructorTriggerT _, _)
    | (InternalT _, _) ->
      error_unsupported cx t op
    | (AnnotT (r, t, _), _) ->
      let t = reposition cx (aloc_of_reason r) t in
      elab_t cx ~seen t op
    (*********************************************************************)
    (* UseT TypeT (runtime types derive static types through annotation) *)
    (*********************************************************************)
    (* First handle catch-all cases of subtyping_kit.ml *)
    | ((MaybeT _ | OptionalT _), Annot_UseT_TypeT _) -> error_unsupported cx t op
    | (ThisTypeAppT (reason_tapp, c, this, ts), Annot_UseT_TypeT _) ->
      let reason_op = Type.AConstraint.reason_of_op op in
      let tc = specialize_class cx c reason_op reason_tapp ts in
      let t = this_specialize cx reason_tapp this tc in
      elab_t cx t op
    | (TypeAppT (reason_tapp, typeapp_use_op, c, ts), Annot_UseT_TypeT _) ->
      (* NOTE omitting TypeAppExpansion.push_unless_loop check. *)
      let reason_op = Type.AConstraint.reason_of_op op in
      let t = mk_typeapp_instance cx ~use_op:typeapp_use_op ~reason_op ~reason_tapp c ts in
      elab_t cx t op
    | (DefT (reason_tapp, _, PolyT { tparams_loc; tparams = ids; _ }), Annot_UseT_TypeT reason) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EMissingTypeArgs
           {
             reason_op = reason;
             reason_tapp;
             reason_arity = Flow_js_utils.mk_poly_arity_reason tparams_loc;
             min_arity = Flow_js_utils.poly_minimum_arity ids;
             max_arity = Nel.length ids;
           }
        );
      AnyT.error reason
    | (ThisClassT (r, i, is_this, this_name), Annot_UseT_TypeT reason) ->
      let c = fix_this_class cx reason (r, i, is_this, this_name) in
      elab_t cx c op
    | (DefT (_, _, ClassT it), Annot_UseT_TypeT reason) ->
      (* a class value annotation becomes the instance type *)
      reposition cx (aloc_of_reason reason) it
    | (DefT (_, _, TypeT (_, l)), Annot_UseT_TypeT _) -> l
    | (DefT (lreason, trust, EnumObjectT enum), Annot_UseT_TypeT _) ->
      (* an enum object value annotation becomes the enum type *)
      mk_enum_type ~trust lreason enum
    | (DefT (enum_reason, _, EnumT _), Annot_UseT_TypeT reason) ->
      Flow_js_utils.add_output cx Error_message.(EEnumMemberUsedAsType { reason; enum_reason });
      AnyT.error reason
    | (l, Annot_UseT_TypeT reason_use) ->
      (match l with
      (* Short-circut as we already error on the unresolved name. *)
      | AnyT (_, AnyError (Some UnresolvedName)) -> ()
      | AnyT _ -> Flow_js_utils.add_output cx Error_message.(EAnyValueUsedAsType { reason_use })
      | _ -> Flow_js_utils.add_output cx Error_message.(EValueUsedAsType { reason_use }));
      AnyT.error reason_use
    (*****************)
    (* `import type` *)
    (*****************)
    | (_, Annot_ImportTypeT (reason, export_name)) ->
      ImportTypeTKit.on_concrete_type cx dummy_trace reason export_name t
    (*******************)
    (* `import typeof` *)
    (*******************)
    | (_, Annot_ImportTypeofT (reason, export_name)) ->
      ImportTypeofTKit.on_concrete_type cx dummy_trace reason export_name t
    (******************)
    (* Module exports *)
    (******************)
    | (ModuleT m, Annot_ExportNamedT (reason, tmap, export_kind)) ->
      ExportNamedTKit.on_ModuleT cx dummy_trace (reason, tmap, export_kind) t m
    | (_, Annot_AssertExportIsTypeT (_, name)) ->
      AssertExportIsTypeTKit.on_concrete_type cx dummy_trace name t
    | (ModuleT m, Annot_CopyNamedExportsT (reason, target_module_t)) ->
      CopyNamedExportsTKit.on_ModuleT cx dummy_trace (reason, target_module_t) m
    | (ModuleT m, Annot_CopyTypeExportsT (reason, target_module_t)) ->
      CopyTypeExportsTKit.on_ModuleT cx dummy_trace (reason, target_module_t) m
    | (_, Annot_ExportTypeT (reason, export_name, target_module_t)) ->
      ExportTypeTKit.on_concrete_type cx dummy_trace (reason, export_name, target_module_t) t
    | (AnyT (lreason, _), Annot_CopyNamedExportsT (reason, target_module)) ->
      CopyNamedExportsTKit.on_AnyT cx dummy_trace lreason (reason, target_module)
    | (AnyT (lreason, _), Annot_CopyTypeExportsT (reason, target_module)) ->
      CopyTypeExportsTKit.on_AnyT cx dummy_trace lreason (reason, target_module)
    | (_, Annot_CJSExtractNamedExportsT (reason, local_module)) ->
      CJSExtractNamedExportsTKit.on_concrete_type cx dummy_trace (reason, local_module) t
    (******************)
    (* Module imports *)
    (******************)
    | (ModuleT m, Annot_CJSRequireT (reason, is_strict)) ->
      CJSRequireTKit.on_ModuleT cx dummy_trace (reason, is_strict) m
    | (ModuleT m, Annot_ImportModuleNsT (reason, is_strict)) ->
      ImportModuleNsTKit.on_ModuleT cx dummy_trace (reason, is_strict) m
    | (ModuleT m, Annot_ImportDefaultT (reason, import_kind, local, is_strict)) ->
      ImportDefaultTKit.on_ModuleT cx dummy_trace (reason, import_kind, local, is_strict) m
    | (ModuleT m, Annot_ImportNamedT (reason, import_kind, export_name, module_name, is_strict)) ->
      ImportNamedTKit.on_ModuleT
        cx
        dummy_trace
        (reason, import_kind, export_name, module_name, is_strict)
        m
    | (AnyT (lreason, src), (Annot_CJSRequireT (reason, _) | Annot_ImportModuleNsT (reason, _))) ->
      Flow_js_utils.check_untyped_import cx ImportValue lreason reason;
      AnyT.why src reason
    | (AnyT (lreason, src), Annot_ImportDefaultT (reason, import_kind, _, _)) ->
      Flow_js_utils.check_untyped_import cx import_kind lreason reason;
      AnyT.why src reason
    | (AnyT (lreason, src), Annot_ImportNamedT (reason, import_kind, _, _, _)) ->
      Flow_js_utils.check_untyped_import cx import_kind lreason reason;
      AnyT.why src reason
    (************************************)
    (* Wildcards (idx, maybe, optional) *)
    (************************************)
    | (DefT (_, _, IdxWrapper _), _)
    | (MaybeT _, _)
    | (OptionalT _, _) ->
      (* These are rare in practice. Will consider adding support if we hit this
       * error case. *)
      error_unsupported cx t op
    (*********************)
    (* Type applications *)
    (*********************)
    | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
      let reason_op = Type.AConstraint.reason_of_op op in
      let tc = specialize_class cx c reason_op reason_tapp ts in
      let t = this_specialize cx reason_tapp this tc in
      elab_t cx t op
    | (TypeAppT (reason_tapp, typeapp_use_op, c, ts), _) ->
      (* NOTE omitting TypeAppExpansion.push_unless_loop check. *)
      let reason_op = Type.AConstraint.reason_of_op op in
      let t = mk_typeapp_instance cx ~use_op:typeapp_use_op ~reason_op ~reason_tapp c ts in
      elab_t cx t op
    (****************)
    (* Opaque types *)
    (****************)
    | (OpaqueT (r, { underlying_t = Some t; _ }), _)
      when ALoc.source (aloc_of_reason r) = ALoc.source (def_aloc_of_reason r) ->
      elab_t cx ~seen t op
    (********)
    (* Keys *)
    (********)
    | (KeysT _, Annot_ToStringT _) -> t
    | (KeysT (reason, t), _) ->
      let t = elab_t cx t (Annot_GetKeysT reason) in
      elab_t cx t op
    | (DefT (_, _, ObjT { flags; props_tmap; _ }), Annot_GetKeysT reason_op) ->
      begin
        match flags.obj_kind with
        | UnsealedInFile _ -> with_trust bogus_trust (StrT.why reason_op)
        | _ ->
          let dict_t = Obj_type.get_dict_opt flags.obj_kind in
          (* flow the union of keys of l to keys *)
          let keylist =
            Flow_js_utils.keylist_of_props (Context.find_props cx props_tmap) reason_op
          in
          let keylist =
            match dict_t with
            | None -> keylist
            | Some { key; _ } ->
              let key = elab_t cx key (Annot_ToStringT reason_op) in
              key :: keylist
          in
          union_of_ts reason_op keylist
      end
    | (DefT (_, _, InstanceT (_, _, _, instance)), Annot_GetKeysT reason_op) ->
      (* methods are not enumerable, so only walk fields *)
      let own_props = Context.find_props cx instance.own_props in
      let keylist = Flow_js_utils.keylist_of_props own_props reason_op in
      union_of_ts reason_op keylist
    | (AnyT _, Annot_GetKeysT reason_op) -> with_trust literal_trust (StrT.why reason_op)
    (***********)
    (* $Values *)
    (***********)
    | (DefT (_, _, ObjT o), Annot_GetValuesT reason) ->
      Flow_js_utils.get_values_type_of_obj_t cx o reason
    | (DefT (_, _, InstanceT (_, _, _, { own_props; _ })), Annot_GetValuesT reason) ->
      Flow_js_utils.get_values_type_of_instance_t cx own_props reason
    (* Any will always be ok *)
    | (AnyT (_, src), Annot_GetValuesT reason) -> AnyT.why src reason
    (********************************)
    (* Union and intersection types *)
    (********************************)
    | (UnionT (reason, rep), Annot_MakeExactT reason_op) ->
      let ts = UnionRep.members rep in
      let f t = ExactT (reason_op, t) in
      let ts' = Base.List.map ts ~f in
      let reason' = repos_reason (aloc_of_reason reason_op) reason in
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
    | (IntersectionT _, _) ->
      (* Handling intersections as inputs would require use of speculation. Instead,
       * we ask the user to provide a simpler type. *)
      error_unsupported cx t op
    (*************)
    (* Unary not *)
    (*************)
    (* any propagation *)
    | (AnyT _, Annot_NotT _) -> t
    (* !x when x is of unknown truthiness *)
    | (DefT (_, trust, BoolT None), Annot_NotT reason)
    | (DefT (_, trust, StrT AnyLiteral), Annot_NotT reason)
    | (DefT (_, trust, NumT AnyLiteral), Annot_NotT reason) ->
      BoolT.at (aloc_of_reason reason) trust
    (* !x when x is falsy *)
    | (DefT (_, trust, BoolT (Some false)), Annot_NotT reason)
    | (DefT (_, trust, SingletonBoolT false), Annot_NotT reason)
    | (DefT (_, trust, StrT (Literal (_, OrdinaryName ""))), Annot_NotT reason)
    | (DefT (_, trust, SingletonStrT (OrdinaryName "")), Annot_NotT reason)
    | (DefT (_, trust, NumT (Literal (_, (0., _)))), Annot_NotT reason)
    | (DefT (_, trust, SingletonNumT (0., _)), Annot_NotT reason)
    | (DefT (_, trust, NullT), Annot_NotT reason)
    | (DefT (_, trust, VoidT), Annot_NotT reason) ->
      let reason = replace_desc_reason (RBooleanLit true) reason in
      DefT (reason, trust, BoolT (Some true))
    (* !x when x is truthy *)
    | (_, Annot_NotT reason) ->
      let reason = replace_desc_reason (RBooleanLit false) reason in
      DefT (reason, bogus_trust (), BoolT (Some false))
    (*****************************)
    (* Singleton primitive types *)
    (*****************************)
    | (DefT (reason, trust, SingletonStrT key), _) ->
      elab_t cx (DefT (reason, trust, StrT (Literal (None, key)))) op
    | (DefT (reason, trust, SingletonNumT lit), _) ->
      elab_t cx (DefT (reason, trust, NumT (Literal (None, lit)))) op
    | (DefT (reason, trust, SingletonBoolT b), _) ->
      elab_t cx (DefT (reason, trust, BoolT (Some b))) op
    | (NullProtoT reason, _) -> elab_t cx (DefT (reason, bogus_trust (), NullT)) op
    (*********)
    (* Exact *)
    (*********)
    | (ExactT (r, t), _) ->
      let t = push_type_alias_reason r t in
      let t = make_exact cx r t in
      elab_t cx t op
    | (ShapeT (_, o), Annot_MakeExactT _) -> elab_t cx o op
    | (DefT (reason_obj, trust, ObjT obj), Annot_MakeExactT reason_op) ->
      TypeUtil.make_exact_object ~reason_obj trust obj ~reason_op
    | (AnyT (_, src), Annot_MakeExactT reason_op) -> AnyT.why src reason_op
    | (DefT (_, trust, VoidT), Annot_MakeExactT reason_op) -> VoidT.why reason_op trust
    | (DefT (_, trust, EmptyT), Annot_MakeExactT reason_op) -> EmptyT.why reason_op trust
    | (_, Annot_MakeExactT reason_op) ->
      Flow_js_utils.add_output cx (Error_message.EUnsupportedExact (reason_op, reason_of_t t));
      AnyT.error reason_op
    (**********)
    (* Mixins *)
    (**********)
    | ( ThisClassT (_, DefT (_, trust, InstanceT (_, _, _, instance)), is_this, this_name),
        Annot_MixinT r
      ) ->
      (* A class can be viewed as a mixin by extracting its immediate properties,
       * and "erasing" its static and super *)
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      this_class_type (DefT (r, trust, InstanceT (static, super, [], instance))) is_this this_name
    | ( DefT
          ( _,
            _,
            PolyT
              {
                tparams_loc;
                tparams = xs;
                t_out =
                  ThisClassT (_, DefT (_, trust, InstanceT (_, _, _, insttype)), is_this, this_name);
                _;
              }
          ),
        Annot_MixinT r
      ) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      let instance = DefT (r, trust, InstanceT (static, super, [], insttype)) in
      poly_type
        (Type.Poly.generate_id ())
        tparams_loc
        xs
        (this_class_type instance is_this this_name)
    | (AnyT (_, src), Annot_MixinT r) -> AnyT.why src r
    (***********************)
    (* Type specialization *)
    (***********************)
    | ( DefT (_, _, PolyT { tparams_loc; tparams = xs; t_out = t; id }),
        Annot_SpecializeT (use_op, reason_op, reason_tapp, ts)
      ) ->
      let ts = Base.Option.value ts ~default:[] in
      mk_typeapp_of_poly cx ~use_op ~reason_op ~reason_tapp id tparams_loc xs t ts
    | ((DefT (_, _, ClassT _) | ThisClassT _), Annot_SpecializeT (_, _, _, None)) -> t
    | (AnyT _, Annot_SpecializeT _) -> t
    | (ThisClassT (_, i, _, this_name), Annot_ThisSpecializeT (reason, this)) ->
      let i = subst cx (Subst_name.Map.singleton this_name this) i in
      reposition cx (aloc_of_reason reason) i
    (* this-specialization of non-this-abstracted classes is a no-op *)
    | (DefT (_, _, ClassT i), Annot_ThisSpecializeT (reason, _this)) ->
      reposition cx (aloc_of_reason reason) i
    | (AnyT _, Annot_ThisSpecializeT (reason, _)) -> reposition cx (aloc_of_reason reason) t
    (**********************)
    (* Type instantiation *)
    (**********************)
    | (DefT (reason_tapp, _, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
      let use_op = unknown_use in
      let reason_op = Type.AConstraint.reason_of_op op in
      let t = instantiate_poly cx ~use_op ~reason_op ~reason_tapp (tparams_loc, ids, t) in
      elab_t cx t op
    | (ThisClassT (r, i, is_this, this_name), _) ->
      let reason = Type.AConstraint.reason_of_op op in
      let t = fix_this_class cx reason (r, i, is_this, this_name) in
      elab_t cx t op
    (*****************************)
    (* React Abstract Components *)
    (*****************************)
    | (DefT (r, _, ReactAbstractComponentT _), (Annot_GetPropT _ | Annot_GetElemT _)) ->
      let statics =
        Flow_js_utils.lookup_builtin_strict cx (OrdinaryName "React$AbstractComponentStatics") r
      in
      elab_t cx statics op
    (****************)
    (* Custom types *)
    (****************)
    | (DefT (reason, trust, CharSetT _), _) -> elab_t cx (StrT.why reason trust) op
    | ( CustomFunT (_, ReactPropType (React.PropType.Primitive (false, t))),
        Annot_GetPropT (reason_op, _, Named (_, OrdinaryName "isRequired"))
      ) ->
      let prop_type = React.PropType.Primitive (true, t) in
      CustomFunT (reason_op, ReactPropType prop_type)
    | (CustomFunT (reason, ReactPropType (React.PropType.Primitive (req, _))), _)
      when function_like_op op ->
      let builtin_name =
        if req then
          "ReactPropsCheckType"
        else
          "ReactPropsChainableTypeChecker"
      in
      let l = get_builtin_type cx reason (OrdinaryName builtin_name) in
      elab_t cx l op
    | (CustomFunT (reason, ReactPropType (React.PropType.Complex kind)), _) when function_like_op op
      ->
      let l = get_builtin_prop_type cx reason kind in
      elab_t cx l op
    | (CustomFunT (r, _), _) when function_like_op op -> elab_t cx (FunProtoT r) op
    (**************)
    (* Shape type *)
    (**************)
    | (ShapeT (r, o), _) -> elab_t cx ~seen (reposition cx (aloc_of_reason r) o) op
    (*****************)
    (* ObjTestProtoT *)
    (*****************)
    | (AnyT (_, src), Annot_ObjTestProtoT reason_op) -> AnyT.why src reason_op
    | (DefT (_, trust, NullT), Annot_ObjTestProtoT reason_op) -> NullProtoT.why reason_op trust
    | (_, Annot_ObjTestProtoT reason_op) ->
      if Flow_js_utils.object_like t then
        reposition cx (aloc_of_reason reason_op) t
      else
        let () =
          Flow_js_utils.add_output
            cx
            (Error_message.EInvalidPrototype (aloc_of_reason reason_op, reason_of_t t))
        in
        ObjProtoT.why reason_op |> with_trust bogus_trust
    (***************)
    (* Get statics *)
    (***************)
    | (DefT (_, _, InstanceT (static, _, _, _)), Annot_GetStaticsT reason_op) ->
      reposition cx (aloc_of_reason reason_op) static
    | (AnyT (_, src), Annot_GetStaticsT reason_op) -> AnyT.why src reason_op
    | (ObjProtoT _, Annot_GetStaticsT reason_op) ->
      (* ObjProtoT not only serves as the instance type of the root class, but
       * also as the statics of the root class. *)
      reposition cx (aloc_of_reason reason_op) t
    (***************)
    (* LookupT pt1 *)
    (***************)
    | ( DefT (_lreason, _, InstanceT (_, super, _, instance)),
        Annot_LookupT (reason_op, use_op, (Named (_, x) as propref))
      ) ->
      let own_props = Context.find_props cx instance.own_props in
      let proto_props = Context.find_props cx instance.proto_props in
      let pmap = NameUtils.Map.union own_props proto_props in
      (match NameUtils.Map.find_opt x pmap with
      | None -> Get_prop_helper.cg_lookup_ cx use_op super reason_op propref
      | Some p -> GetPropTKit.perform_read_prop_action cx dummy_trace use_op propref p reason_op)
    | (DefT (_, _, InstanceT _), Annot_LookupT (reason_op, _, Computed _)) ->
      let loc = aloc_of_reason reason_op in
      Flow_js_utils.add_output cx Error_message.(EInternal (loc, InstanceLookupComputed));
      AnyT.error reason_op
    | (DefT (_, _, ObjT o), Annot_LookupT (reason_op, use_op, propref)) ->
      (match GetPropTKit.get_obj_prop cx dummy_trace o propref reason_op with
      | Some (p, _) ->
        GetPropTKit.perform_read_prop_action cx dummy_trace use_op propref p reason_op
      | None -> Get_prop_helper.cg_lookup_ cx use_op o.proto_t reason_op propref)
    | (AnyT _, Annot_LookupT (reason_op, use_op, propref)) ->
      let p = Field (None, AnyT.untyped reason_op, Polarity.Neutral) in
      GetPropTKit.perform_read_prop_action cx dummy_trace use_op propref p reason_op
    (************)
    (* ObjRestT *)
    (************)
    | (DefT (_, _, ObjT { props_tmap; flags; _ }), Annot_ObjRestT (reason, xs)) ->
      Flow_js_utils.objt_to_obj_rest cx props_tmap flags reason xs
    | (DefT (_, _, InstanceT _), Annot_ObjRestT _) ->
      (* This implementation relies on unsealed objects and set-prop logic that is
       * hard to implement in annotation inference. *)
      error_unsupported cx t op
    | (AnyT (_, src), Annot_ObjRestT (reason, _)) -> AnyT.why src reason
    | (ObjProtoT _, Annot_ObjRestT (reason, _)) -> Obj_type.mk_unsealed cx reason ~proto:t
    | (DefT (_, _, (NullT | VoidT)), Annot_ObjRestT (reason, _)) ->
      (* mirroring Object.assign semantics, treat null/void as empty objects *)
      Obj_type.mk_unsealed cx reason
    (************)
    (* GetPropT *)
    (************)
    | (DefT (r, _, InstanceT (_, super, _, insttype)), Annot_GetPropT (reason_op, use_op, propref))
      ->
      GetPropTKit.on_InstanceT
        cx
        dummy_trace
        ~l:t
        ~id:None
        r
        super
        insttype
        use_op
        reason_op
        propref
    | (DefT (_, _, ObjT _), Annot_GetPropT (reason_op, _, Named (_, OrdinaryName "constructor"))) ->
      Unsoundness.why Constructor reason_op
    | (DefT (reason_obj, _, ObjT o), Annot_GetPropT (reason_op, use_op, propref)) ->
      GetPropTKit.read_obj_prop cx dummy_trace ~use_op o propref reason_obj reason_op None
    | (AnyT _, Annot_GetPropT (reason_op, _, _)) -> AnyT (reason_op, Untyped)
    | (DefT (reason, _, ClassT instance), Annot_GetPropT (_, _, Named (_, OrdinaryName "prototype")))
      ->
      reposition cx (aloc_of_reason reason) instance
    (**************)
    (* Object Kit *)
    (**************)
    | (_, Annot_ObjKitT (reason, use_op, resolve_tool, tool)) ->
      object_kit_concrete cx use_op op reason resolve_tool tool t
    (********************)
    (* GetElemT / ElemT *)
    (********************)
    | (DefT (_, trust, StrT _), Annot_GetElemT (reason_op, _use_op, _index)) ->
      (* NOTE bypassing check that index is a number *)
      StrT.why reason_op trust
    | ((DefT (_, _, (ObjT _ | ArrT _)) | AnyT _), Annot_GetElemT (reason_op, use_op, key)) ->
      elab_t cx key (Annot_ElemT (reason_op, use_op, t))
    | (DefT (_, _, InstanceT _), Annot_GetElemT (reason, use_op, _i)) ->
      (* NOTE bypassing key check *)
      elab_t cx t (Annot_GetPropT (reason, use_op, Named (reason, OrdinaryName "$value")))
    | (_, Annot_ElemT (reason_op, use_op, (DefT (_, _, ObjT _) as obj))) ->
      let propref = Flow_js_utils.propref_for_elem_t t in
      elab_t cx obj (Annot_GetPropT (reason_op, use_op, propref))
    | (_, Annot_ElemT (reason_op, _use_op, (AnyT _ as _obj))) ->
      let value = AnyT.untyped reason_op in
      reposition cx (aloc_of_reason reason_op) value
    | (AnyT _, Annot_ElemT (reason_op, _, DefT (_, _, ArrT arrtype))) ->
      let value = elemt_of_arrtype arrtype in
      reposition cx (aloc_of_reason reason_op) value
    | (l, Annot_ElemT (reason_op, use_op, DefT (reason_tup, _, ArrT arrtype)))
      when Flow_js_utils.numeric l ->
      let (value, _) =
        Flow_js_utils.array_elem_check
          ~write_action:false
          cx
          dummy_trace
          l
          use_op
          reason_op
          reason_tup
          arrtype
      in
      reposition cx (aloc_of_reason reason_op) value
    | (DefT (_, trust, ObjT o), Annot_ObjKeyMirror reason_op) ->
      Flow_js_utils.obj_key_mirror cx trust o reason_op
    | (DefT (_, trust, ObjT o), Annot_ObjMapConst (reason_op, target)) ->
      Flow_js_utils.obj_map_const cx trust o reason_op target
    (***********************)
    (* Opaque types (pt 2) *)
    (***********************)
    | (OpaqueT (_, { super_t = Some t; _ }), _) -> elab_t cx t op
    (************************)
    (* Unary minus operator *)
    (************************)
    | (DefT (_, trust, NumT lit), Annot_UnaryMinusT reason_op) ->
      let num =
        match lit with
        | Literal (_, (value, raw)) ->
          let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
          DefT (replace_desc_reason RNumber reason_op, trust, NumT (Literal (None, (value, raw))))
        | AnyLiteral
        | Truthy ->
          t
      in
      num
    | (AnyT _, Annot_UnaryMinusT reason_op) -> AnyT.untyped reason_op
    (********************)
    (* Function Statics *)
    (********************)
    | (DefT (reason, _, FunT (static, _)), _) when object_like_op op ->
      let static = reposition cx (aloc_of_reason reason) static in
      elab_t cx static op
    (*****************)
    (* Class statics *)
    (*****************)
    | (DefT (reason, _, ClassT instance), _) when object_like_op op ->
      let t = get_statics cx reason instance in
      elab_t cx t op
    (*********)
    (* Enums *)
    (*********)
    | ( DefT (enum_reason, trust, EnumObjectT enum),
        Annot_GetPropT (access_reason, use_op, Named (prop_reason, member_name))
      ) ->
      let access = (use_op, access_reason, None, (prop_reason, member_name)) in
      GetPropTKit.on_EnumObjectT cx dummy_trace enum_reason trust enum access
    | (DefT (enum_reason, _, EnumObjectT _), Annot_GetElemT (reason_op, _, elem)) ->
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
    | (ObjProtoT _, Annot_LookupT (reason_op, _, Named (_, x)))
      when Flow_js_utils.is_object_prototype_method x ->
      Flow_js_utils.lookup_builtin_strict cx (OrdinaryName "Object") reason_op
    | (FunProtoT _, Annot_LookupT (reason_op, _, Named (_, x)))
      when Flow_js_utils.is_function_prototype x ->
      Flow_js_utils.lookup_builtin_strict cx (OrdinaryName "Function") reason_op
    | ( (DefT (reason, _, NullT) | ObjProtoT reason | FunProtoT reason),
        Annot_LookupT (reason_op, use_op, (Named (reason_prop, x) as propref))
      ) ->
      let error_message =
        if Reason.is_builtin_reason ALoc.source reason then
          Error_message.EBuiltinLookupFailed
            { reason = reason_prop; name = Some x; potential_generator = None }
        else
          let suggestion = None in
          Error_message.EStrictLookupFailed
            { reason_prop; reason_obj = reason_op; name = Some x; use_op = Some use_op; suggestion }
      in
      Flow_js_utils.add_output cx error_message;
      let p = Field (None, AnyT.error_of_kind UnresolvedName reason_op, Polarity.Neutral) in
      GetPropTKit.perform_read_prop_action cx dummy_trace use_op propref p reason_op
    (****************************************)
    (* Object, function, etc. library calls *)
    (****************************************)
    | (ObjProtoT reason, _) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx reason ~use_desc (OrdinaryName "Object") in
      elab_t cx obj_proto op
    | (FunProtoT reason, _) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx reason ~use_desc (OrdinaryName "Function") in
      elab_t cx fun_proto op
    (*************)
    (* ToStringT *)
    (*************)
    | (DefT (_, _, StrT _), Annot_ToStringT _) -> t
    | (_, Annot_ToStringT reason_op) -> with_trust bogus_trust (StrT.why reason_op)
    (************)
    (* GetPropT *)
    (************)
    | (DefT (reason, _, ArrT (ArrayAT (t, _))), (Annot_GetPropT _ | Annot_LookupT _)) ->
      let arr = get_builtin_typeapp cx reason (OrdinaryName "Array") [t] in
      elab_t cx arr op
    | ( DefT (reason, trust, ArrT (TupleAT (_, ts))),
        Annot_GetPropT (reason_op, _, Named (_, OrdinaryName "length"))
      ) ->
      GetPropTKit.on_array_length cx dummy_trace reason trust ts reason_op
    | ( DefT (reason, _, ArrT ((TupleAT _ | ROArrayAT _) as arrtype)),
        (Annot_GetPropT _ | Annot_LookupT _)
      ) ->
      let t = elemt_of_arrtype arrtype in
      elab_t cx (get_builtin_typeapp cx reason (OrdinaryName "$ReadOnlyArray") [t]) op
    (************************)
    (* Promoting primitives *)
    (************************)
    | (DefT (reason, _, StrT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true (OrdinaryName "String") in
      elab_t cx builtin op
    | (DefT (reason, _, NumT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true (OrdinaryName "Number") in
      elab_t cx builtin op
    | (DefT (reason, _, BoolT _), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true (OrdinaryName "Boolean") in
      elab_t cx builtin op
    | (DefT (reason, _, SymbolT), _) when primitive_promoting_op op ->
      let builtin = get_builtin_type cx reason ~use_desc:true (OrdinaryName "Symbol") in
      elab_t cx builtin op
    | (DefT (lreason, _, MixedT Mixed_function), (Annot_GetPropT _ | Annot_LookupT _)) ->
      elab_t cx (FunProtoT lreason) op
    | (_, _) ->
      let open Error_message in
      let reason_op = reason_of_op op in
      let lower = (reason_of_t t, Flow_js_utils.error_message_kind_of_lower t) in
      let upper = (reason_op, IncompatibleUnclassified (string_of_operation op)) in
      let use_op = use_op_of_operation op in
      Flow_js_utils.add_output cx (EIncompatible { lower; upper; use_op; branches = [] });
      AnyT.error reason_op

  and get_builtin_type cx reason ?(use_desc = false) x =
    let t = Flow_js_utils.lookup_builtin_strict cx x reason in
    mk_instance cx reason ~use_desc ~reason_type:(reason_of_t t) t

  and get_builtin_prop_type cx reason tool =
    let x =
      React.PropType.(
        match tool with
        | ArrayOf -> "React$PropTypes$arrayOf"
        | InstanceOf -> "React$PropTypes$instanceOf"
        | ObjectOf -> "React$PropTypes$objectOf"
        | OneOf -> "React$PropTypes$oneOf"
        | OneOfType -> "React$PropTypes$oneOfType"
        | Shape -> "React$PropTypes$shape"
      )
    in
    get_builtin_type cx reason (OrdinaryName x)

  and specialize cx t use_op reason_op reason_tapp ts =
    elab_t cx t (Annot_SpecializeT (use_op, reason_op, reason_tapp, ts))

  and this_specialize cx reason this t = elab_t cx t (Annot_ThisSpecializeT (reason, this))

  and specialize_class cx c reason_op reason_tapp ts =
    match ts with
    | None -> c
    | Some ts -> specialize cx c unknown_use reason_op reason_tapp (Some ts)

  and mk_type_reference cx reason c =
    let f id = resolve_id cx id (elab_t cx c (Annot_UseT_TypeT reason)) in
    let tvar = mk_lazy_tvar cx reason f in
    AnnotT (reason, tvar, false)

  and mk_instance cx instance_reason ?(use_desc = false) ~reason_type c =
    let source = elab_t cx c (Annot_UseT_TypeT reason_type) in
    AnnotT (instance_reason, source, use_desc)

  and mk_typeapp_instance cx ~use_op ~reason_op ~reason_tapp c ts =
    let t = specialize cx c use_op reason_op reason_tapp (Some ts) in
    mk_instance cx reason_tapp ~reason_type:(reason_of_t c) t

  and get_statics cx reason t = elab_t cx t (Annot_GetStaticsT reason)

  and get_prop cx use_op reason name t =
    elab_t cx t (Annot_GetPropT (reason, use_op, Named (reason, name)))

  and get_elem cx use_op reason ~key t = elab_t cx t (Annot_GetElemT (reason, use_op, key))

  and qualify_type cx use_op reason (reason_name, name) t =
    let open Type in
    let f id =
      let t = elab_t cx t (Annot_GetPropT (reason, use_op, Named (reason_name, name))) in
      resolve_id cx id t
    in
    mk_lazy_tvar cx reason f

  (* Unlike Flow_js, types in this module are 0->1, so there is no need for a
   * mechanism similar to BecomeT of Flow_js. *)
  and mk_typeof_annotation cx ?trace:_ reason t =
    let annot_loc = aloc_of_reason reason in
    let t = reposition cx (aloc_of_reason reason) t in
    AnnotT (opt_annot_reason ~annot_loc reason, t, false)

  and assert_export_is_type cx reason name t =
    let f id =
      let t = elab_t cx t (Annot_AssertExportIsTypeT (reason, Reason.OrdinaryName name)) in
      resolve_id cx id t
    in
    mk_lazy_tvar cx reason f

  and cjs_require cx t reason is_strict = elab_t cx t (Annot_CJSRequireT (reason, is_strict))

  and export_named cx reason kind named t = elab_t cx t (Annot_ExportNamedT (reason, named, kind))

  and cjs_extract_named_exports cx reason local_module t =
    elab_t cx t (Annot_CJSExtractNamedExportsT (reason, local_module))

  and import_typeof cx reason export_name t = elab_t cx t (Annot_ImportTypeofT (reason, export_name))

  and import_default cx reason import_kind export_name module_name is_strict t =
    elab_t cx t (Annot_ImportDefaultT (reason, import_kind, (export_name, module_name), is_strict))

  and import_named cx reason import_kind export_name module_name is_strict t =
    elab_t cx t (Annot_ImportNamedT (reason, import_kind, export_name, module_name, is_strict))

  and import_ns cx reason is_strict t = elab_t cx t (Annot_ImportModuleNsT (reason, is_strict))

  and copy_named_exports cx ~from_ns reason ~module_t =
    elab_t cx from_ns (Annot_CopyNamedExportsT (reason, module_t))

  and copy_type_exports cx ~from_ns reason ~module_t =
    elab_t cx from_ns (Annot_CopyTypeExportsT (reason, module_t))

  and unary_minus cx reason_op t = elab_t cx t (Annot_UnaryMinusT reason_op)

  and unary_not cx reason_op t = elab_t cx t (Annot_NotT reason_op)

  and mixin cx reason t = elab_t cx t (Annot_MixinT reason)

  and obj_rest cx reason xs t = elab_t cx t (Annot_ObjRestT (reason, xs))

  and arr_rest cx _use_op reason_op _i t = error_unsupported_reason cx t reason_op

  and object_kit_concrete =
    let rec widen_obj_type cx ~use_op reason t =
      match t with
      | OpenT (_, id) ->
        let open Constraint in
        begin
          match Context.find_graph cx id with
          | exception Union_find.Tvar_not_found _ ->
            error_internal_reason cx "widen_obj_type" reason
          | Unresolved _
          | Resolved _ ->
            failwith "widen_obj_type unexpected non-FullyResolved tvar"
          | FullyResolved (_, (lazy t)) -> widen_obj_type cx ~use_op reason t
        end
      | UnionT (r, rep) ->
        UnionT
          ( r,
            UnionRep.ident_map
              (fun t ->
                if is_proper_def t then
                  widen_obj_type cx ~use_op reason t
                else
                  t)
              rep
          )
      | t -> t
    in
    let add_output cx msg : unit = Flow_js_utils.add_output cx msg in
    let return _cx _use_op t = t in
    let recurse cx use_op reason resolve_tool tool x =
      object_kit cx use_op reason resolve_tool tool x
    in
    let object_spread options state cx =
      let dict_check _cx _use_op _d1 _d2 = () in
      Slice_utils.object_spread
        ~dict_check
        ~widen_obj_type
        ~add_output
        ~return
        ~recurse
        options
        state
        cx
    in
    let object_rest options state cx =
      let return _ _ _ t = t in
      (* No subtyping checks in annotation inference *)
      let subt_check ~use_op:_ _ _ = () in
      Slice_utils.object_rest ~add_output ~return ~recurse ~subt_check options state cx
    in
    let object_read_only cx _use_op = Slice_utils.object_read_only cx in
    let object_partial cx _use_op = Slice_utils.object_partial cx in
    let next op cx use_op tool reason x =
      Object.(
        match tool with
        | Spread (options, state) -> object_spread options state cx use_op reason x
        | Rest (options, state) -> object_rest options state cx use_op reason x
        | Partial -> object_partial cx use_op reason x
        | ReadOnly -> object_read_only cx use_op reason x
        | ReactConfig _ -> error_internal cx "ReactConfig" op
        | ObjectRep -> error_internal cx "ObjectRep" op
        | ObjectWiden _ -> error_internal cx "ObjectWiden" op
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

  and make_exact cx reason t = elab_t cx t (Annot_MakeExactT reason)

  and obj_test_proto cx reason_op t = elab_t cx t (Annot_ObjTestProtoT reason_op)
end
