(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Name_def
open Type
open Hint_api
open Reason
open Loc_collections
open Utils_js
module Ast = Flow_ast
module EnvMap = Env_api.EnvMap
module EnvSet = Env_api.EnvSet

module type S = sig
  val resolve_component :
    Context.t ->
    (Name_def.def * Name_def.scope_kind * Name_def.class_stack * reason) EnvMap.t ->
    Name_def_ordering.result ->
    unit
end

module Make (Env : Env_sig.S) (Statement : Statement_sig.S with module Env := Env) : S = struct
  module Type_annotation = Statement.Anno
  module Abnormal = Statement.Abnormal

  module TvarResolver = struct
    let resolver =
      object (this)
        inherit [unit] Type_visitor.t

        method! tvar cx pole () r id =
          let module C = Type.Constraint in
          let (root_id, root) = Context.find_root cx id in
          match root.C.constraints with
          | C.FullyResolved _ -> ()
          | _ ->
            let t =
              let no_lowers _ r =
                Flow_js_utils.add_output
                  cx
                  Error_message.(EInternal (aloc_of_reason r, UnconstrainedTvar));
                AnyT.make (AnyError None) r
              in
              Flow_js_utils.merge_tvar ~no_lowers cx r root_id
            in
            let root = C.Root { root with C.constraints = C.FullyResolved (unknown_use, lazy t) } in
            Context.add_tvar cx root_id root;
            this#type_ cx pole () t
      end

    let resolve cx t =
      match Context.env_mode cx with
      | Options.(SSAEnv Enforced) -> resolver#type_ cx Polarity.Positive () t
      | _ -> ()
  end

  (* Hints are not being used to power type checking currently. Instead, we only use their existence
     to determine whether we should emit missing-local-annot errors. The check will already be done
     in statement.ml, so it's safe to always pass down the hint to avoid spurious
     missing-local-annot errors.

     Once contextual typing is done, the existence of hint detection will be moved here and then
     we can replace all hints passed in env_resolution to be Hint_None.
  *)
  let dummy_hint = Hint_Placeholder

  let mk_tparams_map cx tparams_map =
    let { Loc_env.tparams; _ } = Context.environment cx in
    ALocMap.fold
      (fun l _ subst_map ->
        let (name, _, ty) = ALocMap.find l tparams in
        Subst_name.Map.add name ty subst_map)
      tparams_map
      Subst_name.Map.empty

  let expression cx ~hint ?cond exp =
    let cache = Context.node_cache cx in
    let (((_, t), _) as exp) = Statement.expression ~hint ?cond cx exp in
    Node_cache.set_expression cache exp;
    t

  let mk_selector_reason cx loc = function
    | Name_def.Elem n ->
      let key =
        DefT
          (mk_reason RNumber loc, bogus_trust (), NumT (Literal (None, (float n, string_of_int n))))
      in
      (Type.Elem key, mk_reason (RCustom (Utils_js.spf "element %d" n)) loc)
    | Name_def.Prop { prop; prop_loc; has_default } ->
      (Type.Prop (prop, has_default), mk_reason (RProperty (Some (OrdinaryName prop))) prop_loc)
    | Name_def.ArrRest n -> (Type.ArrRest n, mk_reason RArrayPatternRestProp loc)
    | Name_def.ObjRest { used_props; after_computed = _ } ->
      (* TODO: eveyrthing after a computed prop should be optional *)
      (Type.ObjRest used_props, mk_reason RObjectPatternRestProp loc)
    | Name_def.Computed exp ->
      let t = expression cx ~hint:dummy_hint exp in
      (Type.Elem t, mk_reason (RProperty None) loc)
    | Name_def.Default -> (Type.Default, mk_reason RDefaultValue loc)

  let resolve_annotation cx tparams_map anno =
    let cache = Context.node_cache cx in
    let tparams_map = mk_tparams_map cx tparams_map in
    let (t, anno) = Type_annotation.mk_type_available_annotation cx tparams_map anno in
    Node_cache.set_annotation cache anno;
    t

  let rec resolve_binding_partial cx reason loc b =
    let mk_use_op t = Op (AssignVar { var = Some reason; init = TypeUtil.reason_of_t t }) in
    match b with
    | Root (Annotation { tparams_map; optional; default_expression; is_assignment; annot }) ->
      let t = resolve_annotation cx tparams_map annot in
      let t =
        if optional && default_expression = None then
          TypeUtil.optional t
        else
          t
      in
      let use_op =
        if is_assignment then
          mk_use_op t
        else
          unknown_use
      in
      (t, use_op, true)
    | Root (Value exp) ->
      (* TODO: look up the annotation for the variable at loc and pass in *)
      let t = expression cx ~hint:dummy_hint exp in
      let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason exp }) in
      (t, use_op, false)
    | Root (Contextual { reason; hint; default_expression = _ }) ->
      let param_loc = Reason.poly_loc_of_reason reason in
      let () =
        match hint with
        | Hint_api.Hint_None when RequireAnnot.should_require_annot cx ->
          RequireAnnot.add_missing_annotation_error cx reason
        | _ -> ()
      in
      let contextual_typing_enabled = Context.env_mode cx = Options.(SSAEnv Enforced) in
      let t =
        if contextual_typing_enabled then
          let resolve_hint = function
            | AnnotationHint (tparams_locs, anno) -> resolve_annotation cx tparams_locs anno
            | ValueHint exp -> expression cx ~hint:dummy_hint exp
            | ProvidersHint (loc, []) ->
              let env = Context.environment cx in
              New_env.New_env.check_readable cx Env_api.OrdinaryNameLoc loc;
              Base.Option.value_exn (Loc_env.find_ordinary_write env loc)
            | ProvidersHint (l1, l2 :: rest) ->
              let env = Context.environment cx in
              New_env.New_env.check_readable cx Env_api.OrdinaryNameLoc l1;
              New_env.New_env.check_readable cx Env_api.OrdinaryNameLoc l2;
              let t1 = Base.Option.value_exn (Loc_env.find_ordinary_write env l1) in
              let t2 = Base.Option.value_exn (Loc_env.find_ordinary_write env l2) in
              let ts =
                Base.List.map rest ~f:(fun loc ->
                    New_env.New_env.check_readable cx Env_api.OrdinaryNameLoc loc;
                    Base.Option.value_exn (Loc_env.find_ordinary_write env loc)
                )
              in
              UnionT (mk_reason (RCustom "providers") loc, UnionRep.make t1 t2 ts)
          in
          let hint =
            match hint with
            | Hint_t hint_node -> Hint_t (resolve_hint hint_node)
            | Hint_Decomp (ops, hint_node) -> Hint_Decomp (ops, resolve_hint hint_node)
            | Hint_Placeholder -> Hint_Placeholder
            | Hint_None -> Hint_None
          in
          match Type_hint.evaluate_hint cx param_loc hint with
          | None -> Tvar.mk cx reason
          | Some t -> TypeUtil.mod_reason_of_t (Base.Fn.const reason) t
        else
          Tvar.mk cx reason
      in
      (t, mk_use_op t, false)
    | Root Catch ->
      let t = AnyT.annot (mk_reason (RCustom "catch parameter") loc) in
      (t, mk_use_op t, false)
    | Root (For (kind, exp)) ->
      let reason = mk_reason (RCustom "for-in") loc (*TODO: loc should be loc of loop *) in
      let right_t = expression cx ~hint:dummy_hint ~cond:OtherTest exp in
      let t =
        match kind with
        | In ->
          Flow_js.flow cx (right_t, AssertForInRHST reason);
          StrT.at loc |> with_trust bogus_trust
        | Of { await } -> Statement.for_of_elemt cx right_t reason await
      in
      (t, mk_use_op t, false)
    | Select { selector; default = _; binding } ->
      let refined_type =
        match selector with
        | Name_def.Prop { prop; prop_loc; _ } ->
          (* The key is used to generate a reason for read,
             and only the last prop in the chain matters. *)
          let key = (internal_name "_", [Key.Prop prop]) in
          New_env.New_env.get_refinement cx key prop_loc
        | _ -> None
      in
      (match refined_type with
      | Some t ->
        let rec binding_has_annot = function
          | Root (Annotation _) -> true
          | Root _ -> false
          | Select { binding; _ } -> binding_has_annot binding
        in
        (* When we can get a refined value on a destructured property,
           we must be in an assignment position and the type must have been resolved. *)
        (t, mk_use_op t, binding_has_annot binding)
      | None ->
        let (t, use_op, has_anno) = resolve_binding_partial cx reason loc binding in
        let (selector, reason) = mk_selector_reason cx loc selector in
        let kind =
          if has_anno then
            DestructAnnot
          else
            DestructInfer
        in
        ( Tvar.mk_no_wrap_where cx reason (fun tout ->
              Flow_js.flow cx (t, DestructuringT (reason, kind, selector, tout, Reason.mk_id ()))
          ),
          use_op,
          has_anno
        ))

  let resolve_binding cx reason loc binding =
    let (t, use_op, has_annot) = resolve_binding_partial cx reason loc binding in
    let t =
      match (binding, has_annot) with
      (* This is unnecessary if we are directly resolving an annotation. *)
      | (Select _, true) ->
        let rec subtype_default_against_annotation = function
          | Root (Annotation { tparams_map; annot; default_expression; _ }) ->
            let annot_t = resolve_annotation cx tparams_map annot in
            default_expression
            |> Statement.Func_stmt_config.eval_default cx ~annot_t:(Some annot_t)
            (* Func_stmt_config.eval_default will store annot_t as the type of function parameter
               default. We replicate the behavior and cache the node, so that we ensure later check
               on the parameter default will always return annot_t. *)
            |> Base.Option.iter ~f:(Node_cache.set_expression (Context.node_cache cx))
          | Root _ -> ()
          | Select { binding; _ } -> subtype_default_against_annotation binding
        in
        subtype_default_against_annotation binding;
        AnnotT
          ( reason,
            Tvar.mk_where cx reason (fun t' ->
                Flow_js.flow cx (t, BecomeT { reason; t = t'; empty_success = true })
            ),
            false
          )
      | _ -> t
    in
    let default = Name_def.default_of_binding binding in
    Base.Option.iter
      ~f:(fun d ->
        let rec convert = function
          | Name_def.DefaultExpr e -> Default.Expr (expression cx ~hint:dummy_hint e)
          | Name_def.DefaultCons (e, d) -> Default.Cons (expression cx ~hint:dummy_hint e, convert d)
          | Name_def.DefaultSelector (d, s) ->
            let (s, r) = mk_selector_reason cx loc s in
            Default.Selector (r, convert d, s)
        in
        let default = convert d in
        let default_t = Flow_js.mk_default cx reason default in
        let use_op =
          Op
            (AssignVar
               {
                 var = Some reason;
                 init =
                   (match default with
                   | Default.Expr t -> TypeUtil.reason_of_t t
                   | _ -> TypeUtil.reason_of_t t);
               }
            )
        in
        Flow_js.flow cx (default_t, UseT (use_op, t)))
      default;
    (t, use_op)

  let resolve_inferred_function cx id_loc reason function_loc function_ =
    let cache = Context.node_cache cx in
    (* TODO: This is intended to be the general type for the variable in the old environment, needed
       for generic escape detection. We can do generic escape differently in the future and remove
       this when we kill the old env. *)
    let general = Tvar.mk cx reason in
    let ((fun_type, _) as fn) =
      Statement.mk_function
        cx
        ~hint:dummy_hint
        ~needs_this_param:true
        ~general
        reason
        function_loc
        function_
    in
    Flow_js.flow_t cx (fun_type, general);
    Node_cache.set_function cache id_loc fn;
    (fun_type, unknown_use)

  let resolve_annotated_function
      cx reason tparams_map ({ Ast.Function.body; params; _ } as function_) =
    let tparams_map = mk_tparams_map cx tparams_map in
    let (({ Func_class_sig_types.Func_stmt_sig_types.fparams; _ } as func_sig), _) =
      Statement.mk_func_sig
        cx
        ~func_hint:dummy_hint
        ~needs_this_param:true
        tparams_map
        reason
        function_
    in
    let this_t =
      let default =
        if Signature_utils.This_finder.found_this_in_body_or_params body params then
          let loc = aloc_of_reason reason in
          Tvar.mk cx (mk_reason RThis loc)
        else
          Type.implicit_mixed_this reason
      in
      Base.Option.value (Statement.Func_stmt_params.this fparams) ~default
    in
    (Statement.Func_stmt_sig.functiontype cx this_t func_sig, unknown_use)

  let resolve_class cx id_loc reason class_loc class_ =
    let cache = Context.node_cache cx in
    let self = Tvar.mk cx reason in
    let ((class_sig, _) as sig_info) =
      Statement.mk_class_sig cx ~name_loc:id_loc ~class_loc reason self class_
    in
    Node_cache.set_class_sig cache class_loc sig_info;
    let (class_t_internal, class_t) = Statement.Class_stmt_sig.classtype cx class_sig in
    Flow_js.unify cx self class_t_internal;
    (class_t, unknown_use)

  let resolve_op_assign cx ~exp_loc id_reason lhs op rhs =
    let open Ast.Expression in
    match op with
    | Assignment.PlusAssign ->
      (* lhs += rhs *)
      let reason = mk_reason (RCustom "+=") exp_loc in
      let ((_, lhs_t), _) = Statement.assignment_lhs cx lhs in
      let rhs_t = expression cx ~hint:dummy_hint rhs in
      let result_t =
        Statement.plus_assign
          cx
          ~reason
          ~lhs_reason:id_reason
          ~rhs_reason:(mk_expression_reason rhs)
          lhs_t
          rhs_t
      in
      let use_op = Op (AssignVar { var = Some id_reason; init = reason }) in
      (result_t, use_op)
    | Assignment.MinusAssign
    | Assignment.MultAssign
    | Assignment.ExpAssign
    | Assignment.DivAssign
    | Assignment.ModAssign
    | Assignment.LShiftAssign
    | Assignment.RShiftAssign
    | Assignment.RShift3Assign
    | Assignment.BitOrAssign
    | Assignment.BitXorAssign
    | Assignment.BitAndAssign ->
      (* lhs (numop)= rhs *)
      let reason = mk_reason (RCustom "(numop)=") exp_loc in
      let ((_, lhs_t), _) = Statement.assignment_lhs cx lhs in
      let rhs_t = expression cx ~hint:dummy_hint rhs in
      let result_t = Statement.arith_assign cx reason lhs_t rhs_t in
      let use_op = Op (AssignVar { var = Some id_reason; init = reason }) in
      (result_t, use_op)
    | Assignment.AndAssign
    | Assignment.OrAssign
    | Assignment.NullishAssign ->
      let reason = mk_reason (RCustom (Flow_ast_utils.string_of_assignment_operator op)) exp_loc in
      let ((_, lhs_t), _) = Statement.assignment_lhs cx lhs in
      let (((_, rhs_t), _), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () ->
            Statement.expression cx ~hint:dummy_hint rhs (* TODO hint *)
        )
      in
      let rhs_t =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at exp_loc |> with_trust bogus_trust
        | None -> rhs_t
        | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
      in
      let result_t =
        let ub t =
          match op with
          | Assignment.NullishAssign -> NullishCoalesceT (reason, rhs_t, t)
          | Assignment.AndAssign -> AndT (reason, rhs_t, t)
          | Assignment.OrAssign -> OrT (reason, rhs_t, t)
          | _ -> assert_false "Bad conditional guard"
        in
        Tvar.mk_no_wrap_where cx reason (fun t -> Flow_js.flow cx (lhs_t, ub t))
      in
      let use_op = Op (AssignVar { var = Some id_reason; init = reason }) in
      (result_t, use_op)

  let resolve_update cx ~id_loc ~lhs_member ~exp_loc id_reason =
    let reason = mk_reason (RCustom "update") exp_loc in
    let result_t = NumT.at exp_loc |> with_trust literal_trust in
    let use_op =
      match lhs_member with
      | None ->
        let id_t =
          New_env.New_env.ref_entry_exn ~lookup_mode:Env_sig.LookupMode.ForValue cx id_loc id_reason
        in
        Flow_js.flow cx (id_t, AssertArithmeticOperandT reason);
        Op (AssignVar { var = Some id_reason; init = TypeUtil.reason_of_t id_t })
      | Some argument ->
        let arg_val_t = expression cx ~hint:dummy_hint argument in
        Flow_js.flow cx (arg_val_t, AssertArithmeticOperandT reason);
        unknown_use
    in
    (result_t, use_op)

  let resolve_type_alias cx loc alias =
    let cache = Context.node_cache cx in
    let (t, ast) = Statement.type_alias cx loc alias in
    Node_cache.set_alias cache loc (t, ast);
    (t, unknown_use)

  let resolve_opaque_type cx loc opaque =
    let cache = Context.node_cache cx in
    let (t, ast) = Statement.opaque_type cx loc opaque in
    Node_cache.set_opaque cache loc (t, ast);
    (t, unknown_use)

  let resolve_import cx id_loc import_reason import_kind module_name source_loc import =
    let t =
      match import with
      | Name_def.Named { kind; remote; remote_loc; local } ->
        let import_kind = Base.Option.value ~default:import_kind kind in
        Statement.import_named_specifier_type
          cx
          import_reason
          import_kind
          ~source_loc
          ~module_name
          ~remote_name_loc:remote_loc
          ~remote_name:remote
          ~local_name:local
      | Namespace ->
        Statement.import_namespace_specifier_type
          cx
          import_reason
          import_kind
          ~source_loc
          ~module_name
          ~local_loc:id_loc
      | Default local_name ->
        Statement.import_default_specifier_type
          cx
          import_reason
          import_kind
          ~source_loc
          ~module_name
          ~local_loc:id_loc
          ~local_name
    in
    (t, unknown_use)

  let resolve_interface cx loc inter =
    let cache = Context.node_cache cx in
    let (t, ast) = Statement.interface cx loc inter in
    Node_cache.set_interface cache loc (t, ast);
    (t, unknown_use)

  let resolve_declare_class cx loc class_ =
    let cache = Context.node_cache cx in
    let (t, ast) = Statement.declare_class cx loc class_ in
    Node_cache.set_declared_class cache loc (t, ast);
    (t, unknown_use)

  let resolve_enum cx id_loc enum_reason enum_loc enum =
    if Context.enable_enums cx then
      let enum_t = Statement.mk_enum cx ~enum_reason id_loc enum in
      (DefT (enum_reason, literal_trust (), EnumObjectT enum_t), unknown_use)
    else (
      Flow_js.add_output cx (Error_message.EEnumsNotEnabled enum_loc);
      (AnyT.error enum_reason, unknown_use)
    )

  let resolve_type_param cx id_loc =
    let { Loc_env.tparams; _ } = Context.environment cx in
    let (_, _, t) = ALocMap.find id_loc tparams in
    let t = DefT (TypeUtil.reason_of_t t, bogus_trust (), TypeT (TypeParamKind, t)) in
    (t, unknown_use)

  let resolve_this_type_param cx class_loc =
    let env = Context.environment cx in
    New_env.New_env.check_readable cx Env_api.OrdinaryNameLoc class_loc;
    let class_t = Base.Option.value_exn (Loc_env.find_ordinary_write env class_loc) in
    (* class_t will only be resolved after type checking the entire class. *)
    (class_t, unknown_use)

  let resolve_chain_expression cx ~cond exp =
    let cache = Context.node_cache cx in
    let cond =
      match cond with
      | NonConditionalContext -> None
      | OtherConditionalTest -> Some OtherTest
      | SwitchConditionalTest { case_test_reason; switch_discriminant_reason } ->
        Some (SwitchTest { case_test_reason; switch_discriminant_reason })
    in
    (* The is_existence_check parameters is only used for old-env refinements, so it's irrelevant now. *)
    let (t, _, exp, _, _) = Statement.optional_chain ~cond ~is_existence_check:false cx exp in
    Node_cache.set_expression cache exp;
    (t, unknown_use)

  let resolve_generator_next cx reason gen =
    let open TypeUtil in
    match gen with
    | None ->
      ( VoidT.make (replace_desc_reason RUnannotatedNext reason) |> with_trust bogus_trust,
        unknown_use
      )
    | Some { tparams_map; return_annot; async } ->
      let return_t =
        let cache = Context.node_cache cx in
        let tparams_map = mk_tparams_map cx tparams_map in
        let (t, anno) = Type_annotation.mk_type_available_annotation cx tparams_map return_annot in
        Node_cache.set_annotation cache anno;
        t
      in
      let gen_name =
        OrdinaryName
          ( if async then
            "AsyncGenerator"
          else
            "Generator"
          )
      in
      let next_t =
        Tvar.mk_where cx reason (fun next ->
            let t =
              Flow_js.get_builtin_typeapp
                cx
                reason
                gen_name
                [
                  Tvar.mk cx (replace_desc_reason (RCustom "unused yield") reason);
                  Tvar.mk cx (replace_desc_reason (RCustom "unused return") reason);
                  next;
                ]
            in
            let t =
              Flow_js.reposition cx ~desc:(desc_of_t t) (reason_of_t return_t |> aloc_of_reason) t
            in
            Flow_js.flow_t cx (t, return_t)
        )
      in
      (next_t, unknown_use)

  let convert_scope_kind = function
    | Name_def.Ordinary -> Scope.Ordinary
    | Name_def.Ctor -> Scope.Ctor
    | Name_def.Async -> Scope.Async
    | Name_def.AsyncGenerator -> Scope.AsyncGenerator
    | Name_def.Generator -> Scope.Generator
    | Name_def.Module -> Scope.Module
    | Name_def.Predicate -> Scope.Predicate

  let resolve cx (def_kind, id_loc) (def, def_scope_kind, class_stack, def_reason) =
    let env = Context.environment cx in
    Context.set_environment
      cx
      { env with Loc_env.scope_kind = convert_scope_kind def_scope_kind; class_stack };
    let (t, use_op) =
      match def with
      | Binding b -> resolve_binding cx def_reason id_loc b
      | ChainExpression (cond, e) -> resolve_chain_expression cx ~cond e
      | RefiExpression e -> (expression cx ~hint:Hint_None e, unknown_use)
      | Function { function_; synthesizable_from_annotation = false; function_loc; tparams_map = _ }
        ->
        resolve_inferred_function cx id_loc def_reason function_loc function_
      | Function { function_; synthesizable_from_annotation = true; function_loc = _; tparams_map }
        ->
        resolve_annotated_function cx def_reason tparams_map function_
      | Class { class_; class_loc; missing_annotations = _ } ->
        resolve_class cx id_loc def_reason class_loc class_
      | MemberAssign { member_loc = _; member = _; rhs } ->
        (expression cx ~hint:dummy_hint rhs, unknown_use)
      | OpAssign { exp_loc; lhs; op; rhs } -> resolve_op_assign cx ~exp_loc def_reason lhs op rhs
      | Update { exp_loc; lhs_member; op = _ } ->
        resolve_update cx ~id_loc ~exp_loc ~lhs_member def_reason
      | TypeAlias (loc, alias) -> resolve_type_alias cx loc alias
      | OpaqueType (loc, opaque) -> resolve_opaque_type cx loc opaque
      | Import { import_kind; source; source_loc; import } ->
        resolve_import cx id_loc def_reason import_kind source source_loc import
      | Interface (loc, inter) -> resolve_interface cx loc inter
      | DeclaredClass (loc, class_) -> resolve_declare_class cx loc class_
      | Enum (enum_loc, enum) -> resolve_enum cx id_loc def_reason enum_loc enum
      | TypeParam (_, _) -> resolve_type_param cx id_loc
      | ThisTypeParam (_, _) -> resolve_this_type_param cx id_loc
      | GeneratorNext gen -> resolve_generator_next cx def_reason gen
    in
    let update_reason =
      match def with
      | ChainExpression _
      | RefiExpression _ ->
        true
      | _ -> false
    in
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      ( lazy
        [
          Printf.sprintf
            "Setting variable at %s to %s"
            (ALoc.debug_to_string id_loc)
            (Debug_js.dump_t cx t);
        ]
        );
    New_env.New_env.resolve_env_entry ~use_op ~update_reason cx t def_kind id_loc;
    (def_kind, id_loc)

  let entries_of_component graph component =
    let open Name_def_ordering in
    let entries_of_def acc element =
      let (kind, loc) =
        match element with
        | Name_def_ordering.Normal kl
        | Resolvable kl
        | Illegal { loc = kl; _ } ->
          kl
      in
      match EnvMap.find (kind, loc) graph with
      | _ -> EnvSet.add (kind, loc) acc
    in
    match component with
    | Singleton elt -> entries_of_def EnvSet.empty elt
    | ResolvableSCC elts -> Nel.fold_left entries_of_def EnvSet.empty elts
    | IllegalSCC elts ->
      Nel.fold_left (fun acc (elt, _, _) -> entries_of_def acc elt) EnvSet.empty elts

  let init_type_param =
    let rec init_type_param cx graph def_loc =
      let (def, _, _, reason) = EnvMap.find_ordinary def_loc graph in
      let tparam_entry =
        match def with
        | TypeParam (tparams_locs, tparam) ->
          let tparams_map = mk_tparams_map cx graph tparams_locs in
          let ((_, ({ name; _ } as tparam), t) as info) =
            Type_annotation.mk_type_param cx tparams_map tparam
          in
          let cache = Context.node_cache cx in
          Node_cache.set_tparam cache info;
          (name, tparam, t)
        | ThisTypeParam (tparams_locs, class_tparam_loc) ->
          let env = Context.environment cx in
          let class_t = Base.Option.value_exn (Loc_env.find_ordinary_write env def_loc) in
          let (this_param, this_t) =
            let class_tparams =
              Base.Option.map class_tparam_loc ~f:(fun tparams_loc ->
                  ( tparams_loc,
                    tparams_locs
                    |> ALocMap.keys
                    |> Base.List.map ~f:(fun l ->
                           let (_, tparam, _) = get_type_param cx graph l in
                           tparam
                       )
                    |> Nel.of_list_exn
                  )
              )
            in
            Statement.Class_stmt_sig.mk_this class_t cx reason class_tparams
          in
          (Subst_name.Name "this", this_param, this_t)
        | _ ->
          failwith
            (Utils_js.spf
               "tparam_locs contain a non-tparam location: %s"
               (ALoc.debug_to_string ~include_source:true def_loc)
            )
      in
      let ({ Loc_env.tparams; _ } as env) = Context.environment cx in
      Context.set_environment
        cx
        { env with Loc_env.tparams = ALocMap.add def_loc tparam_entry tparams };
      tparam_entry
    and get_type_param cx graph l =
      let { Loc_env.tparams; _ } = Context.environment cx in
      match ALocMap.find_opt l tparams with
      | Some entry -> entry
      | None -> init_type_param cx graph l
    and mk_tparams_map cx graph tparams_map =
      ALocMap.fold
        (fun l _ subst_map ->
          let (name, _, ty) = get_type_param cx graph l in
          Subst_name.Map.add name ty subst_map)
        tparams_map
        Subst_name.Map.empty
    in
    init_type_param

  let resolve_component_type_params cx graph component =
    let open Name_def_ordering in
    let resolve_element = function
      | Name_def_ordering.Normal key
      | Resolvable key
      | Illegal { loc = key; _ } ->
        (match EnvMap.find key graph with
        | (TypeParam _, _, _, _)
        | (ThisTypeParam _, _, _, _) ->
          let (_kind, loc) = key in
          ignore @@ init_type_param cx graph loc
        | _ -> ())
    in
    match component with
    | Singleton elt -> resolve_element elt
    | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts
    | IllegalSCC elts -> Nel.iter (fun (elt, _, _) -> resolve_element elt) elts

  let resolve_component cx graph component =
    resolve_component_type_params cx graph component;
    let open Name_def_ordering in
    let resolve_element = function
      | Name_def_ordering.Normal (kind, loc)
      | Resolvable (kind, loc)
      | Illegal { loc = (kind, loc); _ } ->
        Statement.Abnormal.try_with_abnormal_exn
          ~f:(fun () -> resolve cx (kind, loc) (EnvMap.find (kind, loc) graph))
            (* When there is an unhandled exception, it means that the initialization of the env slot
               won't be completed and will never be written in the new-env, so it's OK to do nothing. *)
          ~on_abnormal_exn:(fun _ -> (kind, loc))
          ()
    in
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy [Utils_js.spf "Resolving component %s" (string_of_component graph component)]);
    begin
      match Context.env_mode cx with
      | Options.(SSAEnv Enforced) ->
        let entries = entries_of_component graph component in
        let ({ Loc_env.readable; _ } as env) = Context.environment cx in
        Context.set_environment
          cx
          { env with Loc_env.readable = EnvSet.union entries readable; under_resolution = entries }
      | _ -> ()
    end;
    let resolve_tvar_in_env_entry (kind, loc) =
      let env = Context.environment cx in
      Loc_env.find_write env kind loc |> Base.Option.iter ~f:(TvarResolver.resolve cx)
    in
    let () =
      match component with
      | Singleton elt -> resolve_element elt |> resolve_tvar_in_env_entry
      | ResolvableSCC elts ->
        Nel.map (fun elt -> resolve_element elt) elts |> Nel.iter resolve_tvar_in_env_entry
      | IllegalSCC elts ->
        Nel.map (fun (elt, _, _) -> resolve_element elt) elts |> Nel.iter resolve_tvar_in_env_entry
    in
    Debug_js.Verbose.print_if_verbose_lazy cx (lazy ["Finished resolving component"])
end
