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

module type S = sig
  val resolve_component :
    Context.t ->
    (Name_def.def * Name_def.scope_kind * Name_def.class_stack * reason) ALocMap.t ->
    Name_def_ordering.result ->
    unit
end

module Make (Env : Env_sig.S) (Statement : Statement_sig.S with module Env := Env) : S = struct
  module Type_annotation = Statement.Anno
  module Abnormal = Statement.Abnormal

  let mk_tparams_map cx tparams_locs =
    let { Loc_env.tparams; _ } = Context.environment cx in
    ALocSet.fold
      (fun l acc ->
        let (name, ty) = ALocMap.find l tparams in
        Subst_name.Map.add name ty acc)
      tparams_locs
      Subst_name.Map.empty

  let expression cx ~hint ?cond exp =
    let cache = Context.node_cache cx in
    let (((_, t), _) as exp) = Statement.expression ~hint ?cond cx exp in
    Node_cache.set_expression cache exp;
    t

  let rec resolve_binding cx reason loc b =
    let mk_use_op t = Op (AssignVar { var = Some reason; init = TypeUtil.reason_of_t t }) in
    let resolve_annotation tparams_locs anno =
      let cache = Context.node_cache cx in
      let tparams_map = mk_tparams_map cx tparams_locs in
      let (t, anno) = Type_annotation.mk_type_available_annotation cx tparams_map anno in
      Node_cache.set_annotation cache anno;
      t
    in
    match b with
    | Root (Annotation { tparams_locs; optional; annot }) ->
      let t = resolve_annotation tparams_locs annot in
      let t =
        if optional then
          TypeUtil.optional t
        else
          t
      in
      (t, mk_use_op t, true)
    | Root (Value exp) ->
      (* TODO: look up the annotation for the variable at loc and pass in *)
      let t = expression cx ~hint:Hint_None exp in
      let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason exp }) in
      (t, use_op, true)
    | Root (Contextual (param_loc, hint)) ->
      let reason = mk_reason (RCustom "contextual variable") loc in
      let t =
        if Context.enable_contextual_typing cx then
          let resolve_hint = function
            | AnnotationHint (tparams_locs, anno) -> resolve_annotation tparams_locs anno
            | ValueHint (exp, []) -> expression cx ~hint:Hint_None exp
            | ValueHint (e1, e2 :: rest) ->
              let t1 = expression cx ~hint:Hint_None e1 in
              let t2 = expression cx ~hint:Hint_None e2 in
              let ts = Base.List.map ~f:(expression cx ~hint:Hint_None) rest in
              UnionT (mk_reason (RCustom "providers") loc, UnionRep.make t1 t2 ts)
          in
          let hint =
            match hint with
            | Hint_t hint_node -> Hint_t (resolve_hint hint_node)
            | Hint_Decomp (ops, hint_node) -> Hint_Decomp (ops, resolve_hint hint_node)
            | Hint_None -> Hint_None
          in
          match Type_hint.evaluate_hint cx param_loc hint with
          | None -> Tvar.mk cx reason
          | Some t -> TypeUtil.mod_reason_of_t (Base.Fn.const reason) t
        else
          Tvar.mk cx reason
      in
      (t, mk_use_op t, Context.enable_contextual_typing cx)
    | Root Catch ->
      let t = AnyT.annot (mk_reason (RCustom "catch parameter") loc) in
      (t, mk_use_op t, true)
    | Root (For (kind, exp)) ->
      let reason = mk_reason (RCustom "for-in") loc (*TODO: loc should be loc of loop *) in
      let right_t = expression cx ~hint:Hint_None ~cond:OtherTest exp in
      let t =
        match kind with
        | In ->
          Flow_js.flow cx (right_t, AssertForInRHST reason);
          StrT.at loc |> with_trust bogus_trust
        | Of { await } -> Statement.for_of_elemt cx right_t reason await
      in
      (t, mk_use_op t, true)
    | Select (sel, b) ->
      let (t, use_op, resolved) = resolve_binding cx reason loc b in
      let (selector, reason) =
        match sel with
        | Name_def.Elem n ->
          let key =
            DefT
              ( mk_reason RNumber loc,
                bogus_trust (),
                NumT (Literal (None, (float n, string_of_int n)))
              )
          in
          (Type.Elem key, mk_reason (RCustom (Utils_js.spf "element %d" n)) loc)
        | Name_def.Prop { prop; prop_loc; has_default } ->
          (Type.Prop (prop, has_default), mk_reason (RProperty (Some (OrdinaryName prop))) prop_loc)
        | Name_def.ArrRest n -> (Type.ArrRest n, mk_reason RArrayPatternRestProp loc)
        | Name_def.ObjRest { used_props; after_computed = _ } ->
          (* TODO: eveyrthing after a computed prop should be optional *)
          (Type.ObjRest used_props, mk_reason RObjectPatternRestProp loc)
        | Name_def.Computed exp ->
          let t = expression cx ~hint:Hint_None exp in
          (Type.Elem t, mk_reason (RProperty None) loc)
        | Name_def.Default _exp ->
          (* TODO: change the way default works to see exp as a source *)
          (Type.Default, mk_reason (RCustom "destructured var") loc)
      in
      ( Tvar.mk_no_wrap_where cx reason (fun tout ->
            Flow_js.flow
              cx
              (t, DestructuringT (reason, DestructInfer, selector, tout, Reason.mk_id ()))
        ),
        use_op,
        resolved
      )

  let resolve_inferred_function cx id_loc reason function_loc function_ =
    let cache = Context.node_cache cx in
    let ((fun_type, _) as fn) =
      (* TODO: This is intended to be the general type for the variable in the old environment, needed
         for generic escape detection. We can do generic escape differently in the future and remove
         this when we kill the old env. *)
      let general = Tvar.mk cx reason in
      Statement.mk_function
        cx
        ~hint:Hint_None
        ~needs_this_param:true
        ~general
        reason
        function_loc
        function_
    in
    Node_cache.set_function cache id_loc fn;
    (fun_type, unknown_use)

  let resolve_annotated_function
      cx reason tparams_locs ({ Ast.Function.body; params; _ } as function_) =
    let tparams_map = mk_tparams_map cx tparams_locs in
    let (({ Func_class_sig_types.Func_stmt_sig_types.fparams; _ } as func_sig), _) =
      Statement.mk_func_sig
        cx
        ~func_hint:Hint_None
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

  let resolve_inferred_class cx id_loc reason class_loc class_ =
    let cache = Context.node_cache cx in
    let ((class_type, _) as class_) =
      (* This is intended to be the general type for the variable in the old environment, needed
         for generic escape detection. We can do generic escape differently in the future and remove
         this when we kill the old env. *)
      let general = Tvar.mk cx reason in
      Statement.mk_class cx class_loc ~name_loc:id_loc ~general reason class_
    in
    Node_cache.set_class cache class_loc class_;
    (class_type, unknown_use)

  let resolve_annotated_class cx id_loc reason class_loc class_ =
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
      let rhs_t = expression cx ~hint:Hint_None rhs in
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
      let rhs_t = expression cx ~hint:Hint_None rhs in
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
            Statement.expression cx ~hint:Hint_None rhs (* TODO hint *)
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
        let arg_val_t = expression cx ~hint:Hint_None argument in
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

  let resolve_type_param cx id_loc tparam =
    let cache = Context.node_cache cx in
    let ((_, { name; _ }, t) as info) =
      Type_annotation.mk_type_param cx Subst_name.Map.empty tparam
    in
    Node_cache.set_tparam cache info;
    let ({ Loc_env.tparams; _ } as env) = Context.environment cx in
    Context.set_environment cx { env with Loc_env.tparams = ALocMap.add id_loc (name, t) tparams };
    let t = DefT (TypeUtil.reason_of_t t, bogus_trust (), TypeT (TypeParamKind, t)) in
    (t, unknown_use)

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
    | Some { tparams; return_annot; async } ->
      let return_t =
        let cache = Context.node_cache cx in
        let tparams_map = mk_tparams_map cx tparams in
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

  let resolve cx id_loc (def, def_scope_kind, class_stack, def_reason) =
    let env = Context.environment cx in
    Context.set_environment
      cx
      { env with Loc_env.scope_kind = convert_scope_kind def_scope_kind; class_stack };
    let (t, use_op, resolved) =
      let as_resolved (t, use_op) = (t, use_op, true) in
      match def with
      | Binding b -> resolve_binding cx def_reason id_loc b
      | ChainExpression (cond, e) -> as_resolved @@ resolve_chain_expression cx ~cond e
      | RefiExpression e -> (expression cx ~hint:Hint_None e, unknown_use, true)
      | Function { function_; fully_annotated = false; function_loc; tparams = _ } ->
        as_resolved @@ resolve_inferred_function cx id_loc def_reason function_loc function_
      | Function { function_; fully_annotated = true; function_loc = _; tparams } ->
        as_resolved @@ resolve_annotated_function cx def_reason tparams function_
      | Class { class_; fully_annotated = false; class_loc } ->
        as_resolved @@ resolve_inferred_class cx id_loc def_reason class_loc class_
      | Class { class_; fully_annotated = true; class_loc } ->
        as_resolved @@ resolve_annotated_class cx id_loc def_reason class_loc class_
      | MemberAssign { member_loc = _; member = _; rhs } ->
        (expression cx ~hint:Hint_None rhs, unknown_use, true)
      | OpAssign { exp_loc; lhs; op; rhs } ->
        as_resolved @@ resolve_op_assign cx ~exp_loc def_reason lhs op rhs
      | Update { exp_loc; lhs_member; op = _ } ->
        as_resolved @@ resolve_update cx ~id_loc ~exp_loc ~lhs_member def_reason
      | TypeAlias (loc, alias) -> as_resolved @@ resolve_type_alias cx loc alias
      | OpaqueType (loc, opaque) -> as_resolved @@ resolve_opaque_type cx loc opaque
      | Import { import_kind; source; source_loc; import } ->
        as_resolved @@ resolve_import cx id_loc def_reason import_kind source source_loc import
      | Interface (loc, inter) -> as_resolved @@ resolve_interface cx loc inter
      | DeclaredClass (loc, class_) -> as_resolved @@ resolve_declare_class cx loc class_
      | Enum (enum_loc, enum) -> as_resolved @@ resolve_enum cx id_loc def_reason enum_loc enum
      | TypeParam param -> as_resolved @@ resolve_type_param cx id_loc param
      | GeneratorNext gen -> as_resolved @@ resolve_generator_next cx def_reason gen
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
    New_env.New_env.resolve_env_entry ~use_op ~resolved ~update_reason cx t id_loc

  let resolve_component cx graph component =
    let open Name_def_ordering in
    let resolve_element = function
      | Name_def_ordering.Normal loc
      | Resolvable loc
      | Illegal { loc; _ } ->
        resolve cx loc (ALocMap.find loc graph)
    in
    match component with
    | Singleton elt -> resolve_element elt
    | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts
    | IllegalSCC elts -> Nel.iter (fun (elt, _, _) -> resolve_element elt) elts
end
