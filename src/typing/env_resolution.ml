(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Name_def
open Type
open Type_hint
open Reason
open Loc_collections
module Ast = Flow_ast

module type S = sig
  val resolve_component :
    Context.t -> (Name_def.def * reason) ALocMap.t -> Name_def_ordering.result -> unit
end

module Make (Env : Env_sig.S) (Statement : Statement_sig.S with module Env := Env) : S = struct
  module Type_annotation = Statement.Anno

  let expression cx ~hint ?cond exp =
    let cache = Context.node_cache cx in
    let (((_, t), _) as exp) = Statement.expression ~hint ?cond cx exp in
    Node_cache.set_expression cache exp;
    t

  let rec resolve_binding cx reason loc b =
    let mk_use_op t = Op (AssignVar { var = Some reason; init = TypeUtil.reason_of_t t }) in
    match b with
    | Root (Annotation anno) ->
      let cache = Context.node_cache cx in
      let (t, anno) = Type_annotation.mk_type_available_annotation cx Subst_name.Map.empty anno in
      Node_cache.set_annotation cache anno;
      (t, mk_use_op t)
    | Root (Value exp) ->
      (* TODO: look up the annotation for the variable at loc and pass in *)
      let t = expression cx ~hint:Hint_None exp in
      let use_op = Op (AssignVar { var = Some reason; init = mk_expression_reason exp }) in
      (t, use_op)
    | Root (Contextual _) ->
      let t = Tvar.mk cx (mk_reason (RCustom "contextual variable") loc) in
      (t, mk_use_op t)
    | Root Catch ->
      let t = AnyT.annot (mk_reason (RCustom "catch parameter") loc) in
      (t, mk_use_op t)
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
      (t, mk_use_op t)
    | Select (sel, b) ->
      let (t, use_op) = resolve_binding cx reason loc b in
      let selector =
        match sel with
        | Name_def.Elem n ->
          let key =
            DefT
              ( mk_reason RNumber loc,
                bogus_trust (),
                NumT (Literal (None, (float n, string_of_int n)))
              )
          in
          Type.Elem key
        | Name_def.Prop { prop; has_default } -> Type.Prop (prop, has_default)
        | Name_def.ArrRest n -> Type.ArrRest n
        | Name_def.ObjRest { used_props; after_computed = _ } ->
          (* TODO: eveyrthing after a computed prop should be optional *)
          Type.ObjRest used_props
        | Name_def.Computed exp ->
          let t = expression cx ~hint:Hint_None exp in
          Type.Elem t
        | Name_def.Default _exp ->
          (* TODO: change the way default works to see exp as a source *)
          Type.Default
      in
      let reason = mk_reason (RCustom "destructured var") loc in
      ( Tvar.mk_no_wrap_where cx reason (fun tout ->
            Flow_js.flow
              cx
              (t, DestructuringT (reason, DestructInfer, selector, tout, Reason.mk_id ()))
        ),
        use_op
      )

  let resolve_inferred_function cx id_loc reason function_ =
    let cache = Context.node_cache cx in
    let ((fun_type, _) as fn) =
      (* TODO: This is intended to be the general type for the variable in the old environment, needed
         for generic escape detection. We can do generic escape differently in the future and remove
         this when we kill the old env. *)
      let general = Tvar.mk cx reason in
      Statement.mk_function cx ~hint:Hint_None ~needs_this_param:true ~general reason function_
    in
    Node_cache.set_function cache id_loc fn;
    (fun_type, unknown_use)

  let resolve_annotated_function cx reason ({ Ast.Function.body; params; _ } as function_) =
    let (({ Func_class_sig_types.Func_stmt_sig_types.fparams; _ } as func_sig), _) =
      Statement.mk_func_sig
        cx
        ~func_hint:Hint_None
        ~needs_this_param:true
        Subst_name.Map.empty
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

  let resolve_op_assign cx ~id_loc ~exp_loc id_reason op rhs =
    let open Ast.Expression in
    match op with
    | Assignment.PlusAssign ->
      (* lhs += rhs *)
      let reason = mk_reason (RCustom "+=") exp_loc in
      let lhs_t =
        New_env.New_env.read_entry_exn ~lookup_mode:Env_sig.LookupMode.ForValue cx id_loc id_reason
      in
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
      let lhs_t =
        New_env.New_env.read_entry_exn ~lookup_mode:Env_sig.LookupMode.ForValue cx id_loc id_reason
      in
      let rhs_t = expression cx ~hint:Hint_None rhs in
      let result_t = Statement.arith_assign cx reason lhs_t rhs_t in
      let use_op = Op (AssignVar { var = Some id_reason; init = reason }) in
      (result_t, use_op)
    | Assignment.NullishAssign
    | Assignment.AndAssign
    | Assignment.OrAssign ->
      (Tvar.mk cx (mk_reason (RCustom "unhandled def") id_loc), unknown_use)

  let resolve_update cx ~id_loc ~exp_loc id_reason =
    let reason = mk_reason (RCustom "update") exp_loc in
    let id_t =
      New_env.New_env.read_entry_exn ~lookup_mode:Env_sig.LookupMode.ForValue cx id_loc id_reason
    in
    Flow_js.flow cx (id_t, AssertArithmeticOperandT reason);
    let t = NumT.at exp_loc |> with_trust literal_trust in
    let use_op = Op (AssignVar { var = Some id_reason; init = TypeUtil.reason_of_t id_t }) in
    (t, use_op)

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

  let resolve_enum cx id_loc enum_reason enum =
    if Context.enable_enums cx then
      let enum_t = Statement.mk_enum cx ~enum_reason id_loc enum in
      (DefT (enum_reason, literal_trust (), EnumObjectT enum_t), unknown_use)
    else (
      Flow_js.add_output cx (Error_message.EEnumsNotEnabled id_loc);
      (AnyT.error enum_reason, unknown_use)
    )

  let resolve cx id_loc (def, def_reason) =
    let (t, use_op) =
      match def with
      | Binding b -> resolve_binding cx def_reason id_loc b
      | Function { function_; fully_annotated = false } ->
        resolve_inferred_function cx id_loc def_reason function_
      | Function { function_; fully_annotated = true } ->
        resolve_annotated_function cx def_reason function_
      | Class { class_; fully_annotated = false; class_loc } ->
        resolve_inferred_class cx id_loc def_reason class_loc class_
      | Class { class_; fully_annotated = true; class_loc } ->
        resolve_annotated_class cx id_loc def_reason class_loc class_
      | OpAssign { exp_loc; op; rhs } -> resolve_op_assign cx ~id_loc ~exp_loc def_reason op rhs
      | Update { exp_loc; op = _ } -> resolve_update cx ~id_loc ~exp_loc def_reason
      | TypeAlias (loc, alias) -> resolve_type_alias cx loc alias
      | OpaqueType (loc, opaque) -> resolve_opaque_type cx loc opaque
      | Import { import_kind; source; source_loc; import } ->
        resolve_import cx id_loc def_reason import_kind source source_loc import
      | Interface (loc, inter) -> resolve_interface cx loc inter
      | DeclaredClass (loc, class_) -> resolve_declare_class cx loc class_
      | Enum enum -> resolve_enum cx id_loc def_reason enum
      | _ -> (Tvar.mk cx (mk_reason (RCustom "unhandled def") id_loc), unknown_use)
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
    New_env.New_env.resolve_env_entry ~use_op cx t id_loc

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
