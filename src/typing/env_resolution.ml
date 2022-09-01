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
module Statement = Fix_statement.Statement_
module Anno = Type_annotation.Make (Type_annotation.FlowJS) (Statement)

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
              let id_msg =
                match Context.verbose cx with
                | Some verbose when Debug_js.Verbose.verbose_in_file cx verbose -> Some root_id
                | _ -> None
              in
              Flow_js_utils.add_output
                cx
                Error_message.(EInternal (aloc_of_reason r, UnconstrainedTvar id_msg));
              let desc =
                match desc_of_reason r with
                | RIdentifier (OrdinaryName x) -> RCustom (spf "`%s` (resolved to type `empty`)" x)
                | _ -> REmpty
              in
              EmptyT.make (replace_desc_reason desc r) (bogus_trust ())
            in
            Flow_js_utils.merge_tvar ~no_lowers cx r root_id
          in
          let root = C.Root { root with C.constraints = C.FullyResolved (unknown_use, lazy t) } in
          Context.add_tvar cx root_id root;
          this#type_ cx pole () t
    end

  let resolve cx t =
    match (Context.env_mode cx, Context.current_phase cx <> Context.InitLib) with
    | (Options.LTI, true) -> resolver#type_ cx Polarity.Positive () t
    | _ -> ()

  let resolved_t cx t =
    resolve cx t;
    t
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
      DefT (mk_reason RNumber loc, bogus_trust (), NumT (Literal (None, (float n, string_of_int n))))
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
  let (t, anno) = Anno.mk_type_available_annotation cx tparams_map anno in
  Node_cache.set_annotation cache anno;
  t

let resolve_hint cx loc hint =
  let resolve_hint = function
    | AnnotationHint (tparams_locs, anno) -> resolve_annotation cx tparams_locs anno
    | ValueHint exp -> expression cx ~hint:dummy_hint exp
    | ProvidersHint (loc, []) ->
      let env = Context.environment cx in
      Env.check_readable cx Env_api.OrdinaryNameLoc loc;
      Base.Option.value_exn (Loc_env.find_ordinary_write env loc)
    | ProvidersHint (l1, l2 :: rest) ->
      let env = Context.environment cx in
      Env.check_readable cx Env_api.OrdinaryNameLoc l1;
      Env.check_readable cx Env_api.OrdinaryNameLoc l2;
      let t1 = Base.Option.value_exn (Loc_env.find_ordinary_write env l1) in
      let t2 = Base.Option.value_exn (Loc_env.find_ordinary_write env l2) in
      let ts =
        Base.List.map rest ~f:(fun loc ->
            Env.check_readable cx Env_api.OrdinaryNameLoc loc;
            Base.Option.value_exn (Loc_env.find_ordinary_write env loc)
        )
      in
      UnionT (mk_reason (RCustom "providers") loc, UnionRep.make t1 t2 ts)
  in
  if Context.env_mode cx = Options.LTI then
    match hint with
    | Hint_t hint_node -> Hint_t (resolve_hint hint_node)
    | Hint_Decomp (ops, hint_node) -> Hint_Decomp (ops, resolve_hint hint_node)
    | Hint_Placeholder -> Hint_Placeholder
    | Hint_None -> Hint_None
  else
    match hint with
    | Hint_t _
    | Hint_Decomp _
    | Hint_Placeholder ->
      Hint_Placeholder
    | Hint_None -> Hint_None

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
  | Root (Value { hint; expr }) ->
    let hint = resolve_hint cx loc hint in
    let t = expression cx ~hint expr in
    let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason expr }) in
    (t, use_op, false)
  | Root (EmptyArray { array_providers; arr_loc }) ->
    let env = Context.environment cx in
    let (elem_t, elems, reason) =
      let element_reason = mk_reason Reason.unknown_elem_empty_array_desc loc in
      if Context.array_literal_providers cx && ALocSet.cardinal array_providers > 0 then (
        let ts =
          ALocSet.elements array_providers
          |> Base.List.map ~f:(fun loc ->
                 Env.check_readable cx Env_api.ArrayProviderLoc loc;
                 Env.t_option_value_exn cx loc (Loc_env.find_write env Env_api.ArrayProviderLoc loc)
             )
        in
        let constrain_t =
          Tvar.mk_where cx element_reason (fun tvar ->
              Base.List.iter ~f:(fun t -> Flow_js.flow cx (t, UseT (unknown_use, tvar))) ts
          )
        in
        let elem_t =
          Tvar.mk_where cx element_reason (fun tvar ->
              Flow_js.flow cx (constrain_t, UseT (unknown_use, tvar))
          )
        in
        let use_op =
          let name =
            match desc_of_reason reason with
            | RIdentifier (OrdinaryName x) -> x
            | _ -> "an empty array"
          in
          Frame
            ( ConstrainedAssignment
                {
                  name;
                  declaration = poly_loc_of_reason reason;
                  providers = ALocSet.elements array_providers;
                  array = true;
                },
              unknown_use
            )
        in
        Context.add_constrained_write cx (elem_t, use_op, constrain_t);
        (elem_t, None, reason)
      ) else (
        if Context.array_literal_providers cx then
          Flow_js.add_output cx Error_message.(EEmptyArrayNoProvider { loc });
        (Tvar.mk cx element_reason, Some [], replace_desc_reason REmptyArrayLit reason)
      )
    in
    let t = DefT (reason, bogus_trust (), ArrT (ArrayAT (elem_t, elems))) in
    let cache = Context.node_cache cx in
    let exp =
      ((arr_loc, t), Flow_ast.Expression.(Array { Array.elements = []; comments = None }))
    in
    Node_cache.set_expression cache exp;
    let use_op = Op (AssignVar { var = Some reason; init = mk_reason (RCode "[]") arr_loc }) in
    (t, use_op, false)
  | Root (Contextual { reason; hint; optional; default_expression }) ->
    let param_loc = Reason.poly_loc_of_reason reason in
    let contextual_typing_enabled = Context.env_mode cx = Options.LTI in
    let t =
      if contextual_typing_enabled then
        let hint = resolve_hint cx loc hint in
        match Type_hint.evaluate_hint cx param_loc ~resolver:TvarResolver.resolved_t hint with
        | None -> AnyT.error reason
        | Some t -> TypeUtil.mod_reason_of_t (Base.Fn.const reason) t
      else
        Tvar.mk cx reason
    in
    let () =
      match hint with
      | Hint_api.Hint_None when RequireAnnot.should_require_annot cx ->
        RequireAnnot.add_missing_annotation_error cx ~on_missing:(fun () -> ()) reason
      | _ -> ()
    in
    let t =
      if optional && default_expression = None then
        TypeUtil.optional t
      else
        t
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
        Env.get_refinement cx key prop_loc
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
          Base.Option.iter default_expression ~f:(fun e ->
              let ((default_loc, default_t), default_ast) =
                Statement.expression cx ~hint:(Hint_t annot_t) e
              in
              let use_op =
                Op
                  (AssignVar
                     {
                       var = Some (TypeUtil.reason_of_t annot_t);
                       init = TypeUtil.reason_of_t default_t;
                     }
                  )
              in
              Flow_js.flow cx (default_t, UseT (use_op, annot_t));
              let default_tast = ((default_loc, annot_t), default_ast) in
              (* When there is an annotation available on the parameter, the default expression will
                 be constrained to have the type of the annotation. So we store annot_t as the type
                 of function parameter default to ensure later check on the parameter default will
                 always return annot_t. *)
              Node_cache.set_expression (Context.node_cache cx) default_tast
          )
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

let resolve_inferred_function cx ~hint id_loc reason function_loc function_ =
  let hint = resolve_hint cx (aloc_of_reason reason) hint in
  let cache = Context.node_cache cx in
  (* TODO: This is intended to be the general type for the variable in the old environment, needed
     for generic escape detection. We can do generic escape differently in the future and remove
     this when we kill the old env. *)
  let general = Tvar.mk cx reason in
  let ((fun_type, _) as fn) =
    Statement.mk_function cx ~hint ~needs_this_param:true ~general reason function_loc function_
  in
  Flow_js.flow_t cx (fun_type, general);
  Node_cache.set_function cache id_loc fn;
  (fun_type, unknown_use)

let resolve_annotated_function
    cx ~hint reason tparams_map function_loc ({ Ast.Function.body; params; sig_loc; _ } as function_)
    =
  let hint = resolve_hint cx (aloc_of_reason reason) hint in
  let cache = Context.node_cache cx in
  let tparams_map = mk_tparams_map cx tparams_map in
  let default_this =
    if Signature_utils.This_finder.found_this_in_body_or_params body params then
      let loc = aloc_of_reason reason in
      Tvar.mk cx (mk_reason RThis loc)
    else
      Type.implicit_mixed_this reason
  in
  let ((func_sig, _) as sig_data) =
    Statement.mk_func_sig
      cx
      ~func_hint:hint
      ~required_this_param_type:(Some default_this)
      ~require_return_annot:false
      ~constructor:false
      tparams_map
      reason
      function_
  in
  Node_cache.set_function_sig cache sig_loc sig_data;
  (Statement.Func_stmt_sig.functiontype cx (Some function_loc) default_this func_sig, unknown_use)

let resolve_class cx id_loc reason class_loc class_ =
  let cache = Context.node_cache cx in
  let env = Context.environment cx in
  let self = Base.Option.value_exn (Loc_env.find_write env Env_api.ClassSelfLoc class_loc) in
  let ((class_t, class_t_internal, _, _) as sig_info) =
    Statement.mk_class_sig cx ~name_loc:id_loc ~class_loc reason self class_
  in
  Node_cache.set_class_sig cache class_loc sig_info;
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

let resolve_update cx ~id_loc ~exp_loc id_reason =
  let reason = mk_reason (RCustom "update") exp_loc in
  let result_t = NumT.at exp_loc |> with_trust literal_trust in
  let id_t = Env.ref_entry_exn ~lookup_mode:Env.LookupMode.ForValue cx id_loc id_reason in
  Flow_js.flow cx (id_t, AssertArithmeticOperandT reason);
  let use_op = Op (AssignVar { var = Some id_reason; init = TypeUtil.reason_of_t id_t }) in
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

let resolve_declare_module cx loc module_ =
  let cache = Context.node_cache cx in
  let ({ Loc_env.declare_module_exports_write_loc = old_dme_loc; _ } as env) =
    Context.environment cx
  in
  Context.set_environment cx { env with Loc_env.declare_module_exports_write_loc = loc };
  let ((t, _) as stuff) = Statement.declare_module cx loc module_ in
  Node_cache.set_declared_module cache loc stuff;
  let env = Context.environment cx in
  Context.set_environment cx { env with Loc_env.declare_module_exports_write_loc = old_dme_loc };
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

let resolve_chain_expression cx ~cond exp =
  let cache = Context.node_cache cx in
  let cond =
    match cond with
    | NonConditionalContext -> None
    | OtherConditionalTest -> Some OtherTest
  in
  let (t, _, exp) = Statement.optional_chain ~hint:Hint_None ~cond cx exp in
  Node_cache.set_expression cache exp;
  (t, unknown_use)

let resolve_generator_next cx reason gen =
  let open TypeUtil in
  match gen with
  | None ->
    (VoidT.make (replace_desc_reason RUnannotatedNext reason) |> with_trust bogus_trust, unknown_use)
  | Some { tparams_map; return_annot; async } ->
    let return_t =
      let cache = Context.node_cache cx in
      let tparams_map = mk_tparams_map cx tparams_map in
      let (t, anno) = Anno.mk_type_available_annotation cx tparams_map return_annot in
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

let resolve cx (def_kind, id_loc) (def, def_scope_kind, class_stack, def_reason) =
  let env = Context.environment cx in
  Context.set_environment cx { env with Loc_env.scope_kind = def_scope_kind; class_stack };
  let (t, use_op) =
    match def with
    | Binding b -> resolve_binding cx def_reason id_loc b
    | ChainExpression (cond, e) -> resolve_chain_expression cx ~cond e
    | RefiExpression e -> (expression cx ~hint:Hint_None e, unknown_use)
    | Function
        {
          function_;
          synthesizable_from_annotation = false;
          has_this_def = _;
          function_loc;
          tparams_map = _;
          hint;
        } ->
      resolve_inferred_function cx ~hint id_loc def_reason function_loc function_
    | Function
        {
          function_;
          synthesizable_from_annotation = true;
          has_this_def = _;
          function_loc;
          tparams_map;
          hint;
        } ->
      resolve_annotated_function cx ~hint def_reason tparams_map function_loc function_
    | Class { class_; class_loc; class_implicit_this_tparam = _; this_super_write_locs = _ } ->
      resolve_class cx id_loc def_reason class_loc class_
    | MemberAssign { member_loc = _; member = _; rhs } ->
      (expression cx ~hint:dummy_hint rhs, unknown_use)
    | OpAssign { exp_loc; lhs; op; rhs } -> resolve_op_assign cx ~exp_loc def_reason lhs op rhs
    | Update { exp_loc; op = _ } -> resolve_update cx ~id_loc ~exp_loc def_reason
    | TypeAlias (loc, alias) -> resolve_type_alias cx loc alias
    | OpaqueType (loc, opaque) -> resolve_opaque_type cx loc opaque
    | Import { import_kind; source; source_loc; import } ->
      resolve_import cx id_loc def_reason import_kind source source_loc import
    | Interface (loc, inter) -> resolve_interface cx loc inter
    | DeclaredClass (loc, class_) -> resolve_declare_class cx loc class_
    | Enum (enum_loc, enum) -> resolve_enum cx id_loc def_reason enum_loc enum
    | TypeParam (_, _) -> resolve_type_param cx id_loc
    | GeneratorNext gen -> resolve_generator_next cx def_reason gen
    | DeclaredModule (loc, module_) -> resolve_declare_module cx loc module_
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
  Env.resolve_env_entry ~use_op ~update_reason cx t def_kind id_loc

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
    let acc = EnvSet.add (kind, loc) acc in
    match EnvMap.find (kind, loc) graph with
    | (DeclaredModule (loc, _), _, _, _) -> EnvSet.add (Env_api.DeclareModuleExportsLoc, loc) acc
    | (Class { this_super_write_locs; _ }, _, _, _) -> EnvSet.union this_super_write_locs acc
    | (Function { has_this_def = true; function_loc; _ }, _, _, _) ->
      EnvSet.add (Env_api.FunctionThisLoc, function_loc) acc
    | _ -> acc
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
        let ((_, ({ name; _ } as tparam), t) as info) = Anno.mk_type_param cx tparams_map tparam in
        let cache = Context.node_cache cx in
        Node_cache.set_tparam cache info;
        (name, tparam, t)
      | Class { class_loc; class_implicit_this_tparam = { tparams_map; class_tparams_loc }; _ } ->
        let env = Context.environment cx in
        let class_t =
          Base.Option.value_exn (Loc_env.find_write env Env_api.ClassSelfLoc class_loc)
        in
        let (this_param, this_t) =
          let class_tparams =
            Base.Option.map class_tparams_loc ~f:(fun tparams_loc ->
                ( tparams_loc,
                  tparams_map
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
      | (Class _, _, _, _) ->
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
      Abnormal.try_with_abnormal_exn
        ~f:(fun () -> resolve cx (kind, loc) (EnvMap.find (kind, loc) graph))
          (* When there is an unhandled exception, it means that the initialization of the env slot
             won't be completed and will never be written in the new-env, so it's OK to do nothing. *)
        ~on_abnormal_exn:(fun _ -> ())
        ()
  in
  Debug_js.Verbose.print_if_verbose_lazy
    cx
    (lazy [Utils_js.spf "Resolving component %s" (string_of_component graph component)]);
  let entries_for_resolution =
    match Context.env_mode cx with
    | Options.LTI ->
      let entries = entries_of_component graph component in
      let ({ Loc_env.readable; _ } as env) = Context.environment cx in
      Context.set_environment
        cx
        { env with Loc_env.readable = EnvSet.union entries readable; under_resolution = entries };
      entries
    | _ -> EnvSet.empty
  in
  let () =
    match component with
    | Singleton elt -> resolve_element elt
    | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts
    | IllegalSCC elts -> Nel.iter (fun (elt, _, _) -> resolve_element elt) elts
  in
  let env = Context.environment cx in
  EnvSet.iter
    (fun (kind, loc) ->
      Loc_env.find_write env kind loc |> Base.Option.iter ~f:(TvarResolver.resolve cx))
    entries_for_resolution;
  Debug_js.Verbose.print_if_verbose_lazy cx (lazy ["Finished resolving component"])
