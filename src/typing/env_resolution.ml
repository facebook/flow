(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Name_def
open Type
open Reason
open FlowSymbol
open Loc_collections
open Utils_js
open Type_operation_utils
module Ast = Flow_ast
module EnvMap = Env_api.EnvMap
module Statement = Fix_statement.Statement_
module Anno = Type_annotation.Make (Type_annotation_cons_gen.FlowJS) (Statement)

let mk_tparams_map cx tparams_map =
  let { Loc_env.tparams; _ } = Context.environment cx in
  ALocMap.fold
    (fun l _ subst_map ->
      let (name, _, ty) = ALocMap.find l tparams in
      Subst_name.Map.add name ty subst_map)
    tparams_map
    Subst_name.Map.empty

let try_cache : 'l. check:(unit -> 'l) -> cache:('l -> unit) -> Context.t -> 'l =
 fun ~check ~cache cx ->
  if Context.typing_mode cx <> Context.CheckingMode then begin
    let original_errors = Context.errors cx in
    Context.reset_errors cx Flow_error.ErrorSet.empty;
    let (produced_placeholders, e) = Context.run_in_synthesis_mode cx check in
    let can_cache =
      (* If we didn't introduce new placeholders and synthesis doesn't introduce new errors,
         we can cache the result *)
      (not produced_placeholders) && Flow_error.ErrorSet.is_empty (Context.errors cx)
    in
    Context.reset_errors cx original_errors;
    if can_cache then cache e;
    e
  end else
    let e = check () in
    cache e;
    e

let expression cx ?cond exp =
  let cache = Context.node_cache cx in
  let ((_, t), _) =
    try_cache
      cx
      ~check:(fun () -> Statement.expression ?cond cx exp)
      ~cache:(Node_cache.set_expression cache)
  in
  t

let make_hooklike cx t =
  if Context.hook_compatibility cx then
    match t with
    | DefT (_, TypeT _) -> t
    | _ ->
      Flow_js.mk_possibly_evaluated_destructor
        cx
        unknown_use
        (TypeUtil.reason_of_t t)
        t
        MakeHooklike
        (Eval.generate_id ())
  else
    t

let resolve_annotation cx tparams_map ?(react_deep_read_only = None) anno =
  let cache = Context.node_cache cx in
  let tparams_map = mk_tparams_map cx tparams_map in
  let (t, anno) = Anno.mk_type_available_annotation cx tparams_map anno in
  let t =
    match react_deep_read_only with
    | Some ((_, kind) as param_loc) ->
      let enabled =
        match kind with
        | HookArg
        | Props ->
          Context.react_rule_enabled cx Options.DeepReadOnlyProps
        | HookReturn -> Context.react_rule_enabled cx Options.DeepReadOnlyHookReturns
        | DebugAnnot
        | ImmutableAnnot ->
          true
      in
      if enabled then
        Flow_js.mk_possibly_evaluated_destructor
          cx
          unknown_use
          (TypeUtil.reason_of_t t)
          t
          (ReactDRO param_loc)
          (Eval.generate_id ())
      else
        t
    | _ -> t
  in
  if Context.typing_mode cx = Context.CheckingMode then Node_cache.set_annotation cache anno;
  t

let rec synthesizable_expression cx ?cond exp =
  let open Ast.Expression in
  match exp with
  | (loc, Identifier (_, name)) -> Statement.identifier cx name loc
  | (loc, StringLiteral lit) -> Statement.string_literal cx ~singleton:false loc lit
  | (loc, BooleanLiteral lit) -> Statement.boolean_literal ~singleton:false loc lit
  | (loc, NullLiteral _) -> Statement.null_literal loc
  | (loc, NumberLiteral lit) -> Statement.number_literal ~singleton:false loc lit
  | (loc, BigIntLiteral lit) -> Statement.bigint_literal ~singleton:false loc lit
  | (loc, RegExpLiteral _) -> Statement.regexp_literal cx loc
  | (loc, ModuleRefLiteral lit) ->
    let (t, _lit) = Statement.module_ref_literal cx loc lit in
    t
  | (_, AsExpression { AsExpression.annot; _ })
  | (_, TypeCast { TypeCast.annot; _ }) ->
    resolve_annotation cx ALocMap.empty annot
  | ( loc,
      Ast.Expression.Member
        {
          Ast.Expression.Member._object;
          property =
            Ast.Expression.Member.PropertyIdentifier (ploc, { Ast.Identifier.name; comments = _ });
          comments = _;
        }
    ) ->
    let t = synthesizable_expression cx ?cond _object in
    let tout =
      match Refinement.get ~allow_optional:false cx exp loc with
      | Some t -> t
      | None ->
        let expr_reason = mk_expression_reason exp in
        let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) ploc in
        let use_op = Op (GetProperty expr_reason) in
        Statement.get_prop
          ~use_op (* TODO(jmbrown) This feels incorrect *)
          ~hint:(Type_env.get_hint cx loc)
          ~cond:None
          cx
          expr_reason
          t
          (prop_reason, name)
    in
    tout
  | _ -> expression cx ?cond exp

let mk_selector_reason_has_default cx loc = function
  | Name_def.Elem { index = n; has_default } ->
    let key = DefT (mk_reason RNumber loc, NumT (Literal (None, (float n, string_of_int n)))) in
    (Type.Elem key, mk_reason (RArrayNthElement n) loc, has_default)
  | Name_def.Prop { prop; prop_loc; has_default } ->
    ( Type.Prop (prop, has_default),
      mk_reason (RProperty (Some (OrdinaryName prop))) prop_loc,
      has_default
    )
  | Name_def.ArrRest n -> (Type.ArrRest n, mk_reason RArrayPatternRestProp loc, false)
  | Name_def.ObjRest { used_props; after_computed = _ } ->
    (* TODO: eveyrthing after a computed prop should be optional *)
    (Type.ObjRest used_props, mk_reason RObjectPatternRestProp loc, false)
  | Name_def.Computed { expression = exp; has_default } ->
    let t = expression cx exp in
    (Type.Elem t, mk_reason (RProperty None) loc, has_default)
  | Name_def.Default -> (Type.Default, mk_reason RDefaultValue loc, false)

let synthesize_expression_for_instantiation cx e =
  let cache = Context.node_cache cx in
  try_cache cx ~check:(fun () -> Statement.expression cx e) ~cache:(Node_cache.set_expression cache)

let synthesize_jsx_children_for_instantiation cx children =
  let cache = Context.node_cache cx in
  try_cache
    cx
    ~check:(fun () -> Statement.collapse_children cx children)
    ~cache:(Node_cache.set_jsx_children cache)

let synth_arg_list cx (_loc, { Ast.Expression.ArgList.arguments; comments = _ }) =
  Base.List.map
    arguments
    ~f:
      (let open Ast.Expression in
      function
      | Expression e ->
        let ((loc, t), _) = synthesize_expression_for_instantiation cx e in
        (loc, Arg t)
      | Spread (_, { SpreadElement.argument = e; comments = _ }) ->
        let ((loc, t), _) = synthesize_expression_for_instantiation cx e in
        (loc, SpreadArg t)
      )

let resolve_hint cx loc hint =
  let rec resolve_hint_node = function
    | AnnotationHint (tparams_locs, anno) -> resolve_annotation cx tparams_locs anno
    | ValueHint exp -> expression cx exp
    | ProvidersHint (loc, []) -> Type_env.checked_find_loc_env_write cx Env_api.OrdinaryNameLoc loc
    | ProvidersHint (l1, l2 :: rest) ->
      let t1 = Type_env.checked_find_loc_env_write cx Env_api.OrdinaryNameLoc l1 in
      let t2 = Type_env.checked_find_loc_env_write cx Env_api.OrdinaryNameLoc l2 in
      let ts =
        Base.List.map rest ~f:(fun loc ->
            Type_env.checked_find_loc_env_write cx Env_api.OrdinaryNameLoc loc
        )
      in
      UnionT (mk_reason (RCustom "providers") loc, UnionRep.make t1 t2 ts)
    | WriteLocHint (kind, loc) -> Type_env.checked_find_loc_env_write cx kind loc
    | StringLiteralType name ->
      DefT (mk_reason (RIdentifier (OrdinaryName name)) loc, SingletonStrT (OrdinaryName name))
    | BuiltinType name ->
      let reason = mk_reason (RType (OrdinaryName name)) loc in
      Flow_js.get_builtin_type cx reason name
    | AnyErrorHint reason -> AnyT.error reason
    | ComposedArrayPatternHint (loc, elements) ->
      let reason = mk_reason RDestructuring loc in
      let elem_spread_list =
        Base.List.map elements ~f:(function
            | ArrayElementPatternHint h ->
              let t = resolve_hint_node h in
              let reason = mk_reason RArrayElement loc in
              UnresolvedArg (TypeUtil.mk_tuple_element reason t, None)
            | ArrayRestElementPatternHint h -> UnresolvedSpreadArg (resolve_hint_node h)
            )
      in
      Tvar.mk_where cx reason (fun tout ->
          let reason_op = reason in
          let element_reason =
            replace_desc_reason (Reason.RInferredUnionElemArray { instantiable = false }) reason_op
          in
          let elem_t = Tvar.mk cx element_reason in
          Flow_js.resolve_spread_list
            cx
            ~use_op:unknown_use
            ~reason_op
            elem_spread_list
            (ResolveSpreadsToArrayLiteral { id = Reason.mk_id (); as_const = false; elem_t; tout })
      )
    | ComposedObjectPatternHint (loc, properties) ->
      let acc =
        Base.List.fold properties ~init:(Statement.ObjectExpressionAcc.empty ()) ~f:(fun acc -> function
          | ObjectPropPatternHint (n, l, h) ->
            Statement.ObjectExpressionAcc.add_prop
              (Properties.add_field
                 (OrdinaryName n)
                 Polarity.Neutral
                 ~key_loc:(Some l)
                 (resolve_hint_node h)
              )
              acc
          | ObjectSpreadPropPatternHint h ->
            Statement.ObjectExpressionAcc.add_spread (resolve_hint_node h) acc
        )
      in
      let reason = mk_reason RDestructuring loc in
      Statement.ObjectExpressionAcc.mk_object_from_spread_acc
        cx
        acc
        reason
        ~as_const:false
        ~frozen:false
        ~default_proto:(ObjProtoT reason)
  in
  let map_base_hint = resolve_hint_node in
  let map_targs = Statement.convert_call_targs_opt' cx in
  let map_arg_list arg_list =
    let cache_ref = Context.hint_map_arglist_cache cx in
    let (l, _) = arg_list in
    match ALocMap.find_opt l !cache_ref with
    | Some result -> result
    | None ->
      let result = synth_arg_list cx arg_list in
      cache_ref := ALocMap.add l result !cache_ref;
      result
  in
  let map_jsx reason name (props, children) =
    let cache = Context.hint_map_jsx_cache cx in
    let key =
      ( reason,
        name,
        Base.List.map props ~f:(function
            | Ast.JSX.Opening.Attribute (l, _)
            | Ast.JSX.Opening.SpreadAttribute (l, _)
            -> l
            ),
        fst children
      )
    in
    match Hashtbl.find_opt cache key with
    | Some result -> result
    | None ->
      let original_errors = Context.errors cx in
      Context.reset_errors cx Flow_error.ErrorSet.empty;
      let props =
        lazy
          (let (props, _, _, _) =
             Statement.jsx_mk_props
               cx
               reason
               ~check_expression:synthesize_expression_for_instantiation
               ~collapse_children:synthesize_jsx_children_for_instantiation
               name
               props
               children
           in
           props
          )
      in
      Context.reset_errors cx original_errors;
      Hashtbl.add cache key props;
      props
  in
  Hint.map hint ~map_base_hint ~map_targs ~map_arg_list ~map_jsx

let resolve_hints cx loc = Base.List.map ~f:(resolve_hint cx loc)

let lazily_resolve_hints cx loc hints =
  let has_hint = not @@ Base.List.is_empty hints in
  let lazy_hint reason = resolve_hints cx loc hints |> Type_hint.evaluate_hints cx reason in
  (has_hint, lazy_hint)

let resolve_pred_func cx (ex, callee, targs, arguments) =
  lazy
    (let (loc, _) = callee in
     let use_op =
       Op
         (FunCall
            {
              op = Reason.mk_expression_reason ex;
              fn = Reason.mk_expression_reason callee;
              args = Reason.mk_initial_arguments_reason arguments;
              local = true;
            }
         )
     in
     (* [callee] might be a member access expression. Since we are explicitly unbinding it from
      * the call, make sure we don't raise a method-unbinding error. *)
     let callee = Context.with_allowed_method_unbinding cx loc (fun () -> expression cx callee) in
     let targs = Statement.convert_call_targs_opt' cx targs in
     let (_, { Ast.Expression.ArgList.arguments; comments = _ }) = arguments in
     let argts = Base.List.map arguments ~f:(fun e -> fst (Statement.expression_or_spread cx e)) in
     (use_op, fst ex, callee, targs, argts)
    )

let resolve_annotated_function
    cx ~bind_this ~statics ~hook_like reason tparams_map function_loc function_ =
  let { Ast.Function.sig_loc; effect; _ } = function_ in
  let cache = Context.node_cache cx in
  let tparams_map = mk_tparams_map cx tparams_map in
  let default_this = Flow_js_utils.default_this_type cx ~needs_this_param:bind_this function_ in
  let ((func_sig, _) as sig_data) =
    Statement.mk_func_sig
      cx
      ~require_return_annot:false
      ~constructor:false
      ~getset:false
      ~statics
      tparams_map
      reason
      function_
  in
  Node_cache.set_function_sig cache sig_loc sig_data;
  let t =
    Statement.Func_stmt_sig.functiontype
      cx
      ~arrow:(not bind_this)
      (Some function_loc)
      default_this
      func_sig
  in
  if effect <> Ast.Function.Hook && hook_like then
    make_hooklike cx t
  else
    t

let resolve_annotated_component cx scope_kind reason tparams_map component_loc component =
  if not (Context.component_syntax cx) then begin
    Flow_js_utils.add_output
      cx
      (Error_message.EUnsupportedSyntax
         (component_loc, Flow_intermediate_error_types.ComponentSyntax)
      );
    AnyT.at (AnyError None) component_loc
  end else begin
    if scope_kind = ComponentBody then begin
      Flow_js_utils.add_output cx Error_message.(ENestedComponent reason)
    end;
    let tparams_map = mk_tparams_map cx tparams_map in
    let { Ast.Statement.ComponentDeclaration.sig_loc; _ } = component in
    let ((component_sig, _) as sig_data) =
      Statement.mk_component_sig cx tparams_map reason component
    in
    let cache = Context.node_cache cx in
    Node_cache.set_component_sig cache sig_loc sig_data;
    Statement.Component_declaration_sig.component_type cx component_loc component_sig
  end

let rec binding_has_annot = function
  | Root (Annotation _) -> true
  | Hooklike b
  | Select { parent = (_, b); _ } ->
    binding_has_annot b
  | _ -> false

let rec resolve_binding cx reason loc b =
  match b with
  | Root
      (Annotation
        {
          tparams_map;
          optional;
          has_default_expression;
          param_loc;
          annot;
          react_deep_read_only;
          concrete = _;
        }
        ) ->
    let t =
      resolve_annotation
        cx
        tparams_map
        ~react_deep_read_only:
          (match (param_loc, react_deep_read_only) with
          | (Some param_loc, Some Comp) -> Some (param_loc, Props)
          | (Some param_loc, Some Hook) -> Some (param_loc, HookArg)
          | _ -> None)
        annot
    in
    Base.Option.iter param_loc ~f:(Type_env.bind_function_param cx t);
    if optional && not has_default_expression then
      TypeUtil.optional t
    else
      t
  | Root (Value { hints = _; expr }) -> expression cx expr
  | Root (ObjectValue { obj_loc = loc; obj; synthesizable = ObjectSynthesizable _ }) ->
    let open Ast.Expression.Object in
    let resolve_prop ~bind_this ~prop_loc ~fn_loc fn =
      let reason = func_reason ~async:false ~generator:false prop_loc in
      resolve_annotated_function
        cx
        ~bind_this
        ~hook_like:false
        ~statics:SMap.empty
        reason
        ALocMap.empty
        fn_loc
        fn
    in

    let rec mk_obj obj_loc { properties; _ } =
      let rec mk_expression (loc, expr) =
        match expr with
        | Ast.Expression.StringLiteral _
        | Ast.Expression.NumberLiteral _
        | Ast.Expression.NullLiteral _
        | Ast.Expression.BooleanLiteral _
        | Ast.Expression.BigIntLiteral _
        | Ast.Expression.RegExpLiteral _
        | Ast.Expression.ModuleRefLiteral _
        | Ast.Expression.Identifier _
        | Ast.Expression.TypeCast _
        | Ast.Expression.AsConstExpression _
        | Ast.Expression.AsExpression _
        | Ast.Expression.Member _ ->
          synthesizable_expression cx (loc, expr)
        | Ast.Expression.Function fn
        | Ast.Expression.ArrowFunction fn ->
          let { Ast.Function.sig_loc; _ } = fn in
          let bind_this =
            match expr with
            | Ast.Expression.Function _ -> true
            | _ -> false
          in
          resolve_prop ~bind_this ~prop_loc:sig_loc ~fn_loc:loc fn
        | Ast.Expression.Object obj -> mk_obj loc obj
        | Ast.Expression.Array { Ast.Expression.Array.elements = []; _ } ->
          let (_, elem_t) = Statement.empty_array cx loc in
          DefT
            (reason, ArrT (ArrayAT { elem_t; tuple_view = Some empty_tuple_view; react_dro = None }))
        | Ast.Expression.Array { Ast.Expression.Array.elements; _ } ->
          (* TODO merge code with statement.ml implementation *)
          let array_elements cx undef_loc =
            let open Ast.Expression.Array in
            Base.List.map ~f:(fun e ->
                match e with
                | Expression e ->
                  let t = mk_expression e in
                  let (loc, _) = e in
                  let reason = mk_reason RArrayElement loc in
                  UnresolvedArg (TypeUtil.mk_tuple_element reason t, None)
                | Hole hole_loc ->
                  let t = EmptyT.at undef_loc in
                  let reason = mk_reason RArrayElement hole_loc in
                  UnresolvedArg (TypeUtil.mk_tuple_element reason t, None)
                | Spread (_, { Ast.Expression.SpreadElement.argument; comments = _ }) ->
                  let t = synthesizable_expression cx argument in
                  UnresolvedSpreadArg t
            )
          in
          let reason = mk_reason RArrayLit loc in
          let elem_spread_list = array_elements cx loc elements in
          Tvar.mk_where cx reason (fun tout ->
              let reason_op = reason in
              let element_reason =
                replace_desc_reason
                  (Reason.RInferredUnionElemArray { instantiable = false })
                  reason_op
              in
              let elem_t = Tvar.mk cx element_reason in
              let resolve_to =
                ResolveSpreadsToArrayLiteral { id = mk_id (); as_const = false; elem_t; tout }
              in
              Flow_js.resolve_spread_list
                cx
                ~use_op:unknown_use
                ~reason_op
                elem_spread_list
                resolve_to
          )
        | Ast.Expression.Assignment _
        | Ast.Expression.Binary _
        | Ast.Expression.Call _
        | Ast.Expression.Class _
        | Ast.Expression.Conditional _
        | Ast.Expression.Import _
        | Ast.Expression.JSXElement _
        | Ast.Expression.JSXFragment _
        | Ast.Expression.Logical _
        | Ast.Expression.MetaProperty _
        | Ast.Expression.New _
        | Ast.Expression.OptionalCall _
        | Ast.Expression.OptionalMember _
        | Ast.Expression.Sequence _
        | Ast.Expression.Super _
        | Ast.Expression.TaggedTemplate _
        | Ast.Expression.TemplateLiteral _
        | Ast.Expression.This _
        | Ast.Expression.TSSatisfies _
        | Ast.Expression.Unary _
        | Ast.Expression.Update _
        | Ast.Expression.Yield _ ->
          failwith "Object not synthesizable"
      in
      let reason = mk_reason RObjectLit obj_loc in
      let obj_proto = ObjProtoT reason in
      let acc =
        Base.List.fold properties ~init:(Statement.ObjectExpressionAcc.empty ()) ~f:(fun acc prop ->
            match prop with
            | SpreadProperty
                (_, { SpreadProperty.argument = (_, Ast.Expression.Identifier _) as exp; _ }) ->
              let spread = synthesizable_expression cx exp in
              Statement.ObjectExpressionAcc.add_spread spread acc
            | Property
                ( prop_loc,
                  Property.Method
                    {
                      key =
                        ( Property.Identifier (name_loc, { Ast.Identifier.name; comments = _ })
                        | Property.StringLiteral (name_loc, { Ast.StringLiteral.value = name; _ }) );
                      value = (fn_loc, fn);
                    }
                ) ->
              let t = resolve_prop ~bind_this:false ~prop_loc ~fn_loc fn in
              Statement.ObjectExpressionAcc.add_prop
                (Properties.add_method (OrdinaryName name) (Some name_loc) t)
                acc
            | Property
                ( _,
                  Property.Init
                    {
                      key =
                        ( Property.Identifier (name_loc, { Ast.Identifier.name; comments = _ })
                        | Property.StringLiteral (name_loc, { Ast.StringLiteral.value = name; _ }) );
                      value;
                      _;
                    }
                ) ->
              let t = mk_expression value in
              Statement.ObjectExpressionAcc.add_prop
                (Properties.add_field
                   (OrdinaryName name)
                   Polarity.Neutral
                   ~key_loc:(Some name_loc)
                   t
                )
                acc
            | _ -> failwith "Object not synthesizable"
        )
      in
      Statement.ObjectExpressionAcc.mk_object_from_spread_acc
        cx
        acc
        reason
        ~as_const:false
        ~frozen:false
        ~default_proto:obj_proto
    in
    mk_obj loc obj
  | Root (ObjectValue { obj_loc; obj; _ }) ->
    let expr = (obj_loc, Ast.Expression.Object obj) in
    expression cx expr
  | Root
      (FunctionValue
        {
          hints = _;
          synthesizable_from_annotation = FunctionSynthesizable | FunctionPredicateSynthesizable _;
          function_loc;
          function_;
          statics;
          arrow;
          tparams_map;
        }
        ) ->
    let cache = Context.node_cache cx in
    let tparams_map = mk_tparams_map cx tparams_map in
    let { Ast.Function.sig_loc; async; generator; _ } = function_ in
    let reason_fun =
      func_reason
        ~async
        ~generator
        ( if arrow then
          function_loc
        else
          sig_loc
        )
    in
    let default_this = Flow_js_utils.default_this_type cx ~needs_this_param:(not arrow) function_ in
    let ((func_sig, _) as sig_data) =
      Statement.mk_func_sig
        cx
        ~require_return_annot:false
        ~constructor:false
        ~getset:false
        ~statics
        tparams_map
        reason_fun
        function_
    in
    let t =
      Statement.Func_stmt_sig.functiontype cx ~arrow (Some function_loc) default_this func_sig
    in
    Node_cache.set_function_sig cache sig_loc sig_data;
    t
  | Root
      (FunctionValue
        {
          hints = _;
          synthesizable_from_annotation = _;
          function_loc;
          function_;
          statics;
          arrow;
          tparams_map = _;
        }
        ) ->
    let { Ast.Function.id; async; generator; sig_loc; _ } = function_ in
    let reason_fun =
      func_reason
        ~async
        ~generator
        ( if arrow then
          function_loc
        else
          sig_loc
        )
    in
    let func =
      if arrow then
        Statement.mk_arrow cx ~statics reason_fun function_
      else
        Statement.mk_function cx ~needs_this_param:true ~statics reason_fun function_loc function_
    in
    let (func_type, func_ast) = func in
    let cache = Context.node_cache cx in
    (match id with
    | Some (id_loc, _) -> Node_cache.set_function cache id_loc func
    | None -> Node_cache.set_function cache function_loc func);
    let expr =
      ( (function_loc, func_type),
        if arrow then
          Ast.Expression.ArrowFunction func_ast
        else
          Ast.Expression.Function func_ast
      )
    in
    Node_cache.set_expression cache expr;
    func_type
  | Root (EmptyArray { array_providers; arr_loc }) ->
    let (elem_t, tuple_view, reason) =
      if ALocSet.cardinal array_providers > 0 then
        let ts =
          ALocSet.elements array_providers
          |> Base.List.map ~f:(Type_env.checked_find_loc_env_write cx Env_api.ArrayProviderLoc)
        in
        let elem_t =
          Tvar.mk_where cx (mk_reason REmptyArrayElement loc) (fun tvar ->
              Base.List.iter ~f:(fun t -> Flow_js.flow cx (t, UseT (unknown_use, tvar))) ts
          )
        in
        (elem_t, None, reason)
      else
        let elem_t = EmptyT.make (mk_reason REmptyArrayElement loc) in
        Flow_js_utils.add_output cx Error_message.(EEmptyArrayNoProvider { loc });
        (elem_t, Some empty_tuple_view, replace_desc_reason REmptyArrayLit reason)
    in
    let t = DefT (reason, ArrT (ArrayAT { elem_t; tuple_view; react_dro = None })) in
    let cache = Context.node_cache cx in
    let exp =
      ((arr_loc, t), Flow_ast.Expression.(Array { Array.elements = []; comments = None }))
    in
    Node_cache.set_expression cache exp;
    t
  | Root (Contextual { reason; hints; optional; default_expression }) ->
    let param_loc = Reason.loc_of_reason reason in
    let t =
      let (has_hint, lazy_hint) = lazily_resolve_hints cx loc hints in
      match lazy_hint reason with
      | HintAvailable (t, _) ->
        let t =
          if Option.is_some default_expression then
            Tvar.mk_no_wrap_where cx reason (fun tout ->
                Flow_js.flow cx (t, PredicateT (NotP VoidP, tout))
            )
          else
            t
        in
        TypeUtil.mod_reason_of_t (Base.Fn.const reason) t
      | NoHint
      | DecompositionError
      | EncounteredPlaceholder ->
        if has_hint then
          Flow_js_utils.add_output
            cx
            (Error_message.EMissingLocalAnnotation
               { reason; hint_available = true; from_generic_function = false }
            );
        AnyT (reason, AnyError (Some MissingAnnotation))
    in
    let () =
      match hints with
      | [] ->
        Flow_js_utils.add_output
          cx
          (Error_message.EMissingLocalAnnotation
             { reason; hint_available = false; from_generic_function = false }
          )
      | _ -> ()
    in
    Type_env.bind_function_param cx t param_loc;
    if optional && default_expression = None then
      TypeUtil.optional t
    else
      t
  | Root (UnannotatedParameter reason) ->
    let t = AnyT (reason, AnyError (Some MissingAnnotation)) in
    Type_env.bind_function_param cx t (loc_of_reason reason);
    Flow_js_utils.add_output
      cx
      (Error_message.EMissingLocalAnnotation
         { reason; hint_available = false; from_generic_function = false }
      );
    t
  | Root CatchUnannotated ->
    if Context.use_mixed_in_catch_variables cx then
      MixedT.at loc
    else
      AnyT (mk_reason RAnyImplicit loc, AnyError (Some MissingAnnotation))
  | Root (For (kind, exp)) ->
    let reason = mk_reason (RCustom "for-in") loc (*TODO: loc should be loc of loop *) in
    let right_t = expression cx ~cond:OtherTest exp in
    (match kind with
    | In ->
      Flow_js.flow
        cx
        (right_t, RunTypeAssertion { reason; type_assertion_kind = TypeAssertionForInRHS });
      StrT.at loc
    | Of { await } -> Statement.for_of_elemt cx right_t reason await)
  | Hooklike binding ->
    let t = resolve_binding cx reason loc binding in
    make_hooklike cx t
  | Select { selector; parent = (parent_loc, binding) } ->
    let refined_type =
      match selector with
      | Name_def.Prop { prop; prop_loc; _ } ->
        (* The key is used to generate a reason for read,
           and only the last prop in the chain matters. *)
        let key = (internal_name "_", [Key.Prop prop]) in
        Type_env.get_refinement cx key prop_loc
      | _ -> None
    in
    (match refined_type with
    | Some t ->
      (* When we can get a refined value on a destructured property,
         we must be in an assignment position and the type must have been resolved. *)
      t
    | None ->
      let t = Type_env.checked_find_loc_env_write cx Env_api.PatternLoc parent_loc in
      let has_anno = binding_has_annot binding in
      let (selector, reason, has_default) = mk_selector_reason_has_default cx loc selector in
      let kind =
        if has_anno then
          DestructAnnot
        else
          DestructInfer
      in
      let t =
        Flow_js_utils.map_on_resolved_type cx reason t (fun t ->
            Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
                Flow_js.flow cx (t, DestructuringT (reason, kind, selector, tout, Reason.mk_id ()))
            )
        )
      in
      if has_default then
        let (selector, reason, _) = mk_selector_reason_has_default cx loc Name_def.Default in
        Flow_js_utils.map_on_resolved_type cx reason t (fun t ->
            Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
                Flow_js.flow cx (t, DestructuringT (reason, kind, selector, tout, Reason.mk_id ()))
            )
        )
      else
        t)

let resolve_inferred_function cx ~statics ~needs_this_param id_loc reason function_loc function_ =
  let cache = Context.node_cache cx in
  let ((fun_type, _) as fn) =
    Statement.mk_function cx ~needs_this_param ~statics reason function_loc function_
  in
  Node_cache.set_function cache id_loc fn;
  if
    function_.Ast.Function.effect <> Ast.Function.Hook
    && Base.Option.is_some (Flow_ast_utils.hook_function function_)
  then
    make_hooklike cx fun_type
  else
    fun_type

let resolve_class cx id_loc reason class_loc class_ =
  let cache = Context.node_cache cx in
  let ((class_t, class_t_internal, _, _) as sig_info) =
    Statement.mk_class_sig cx ~name_loc:id_loc ~class_loc reason class_
  in
  Node_cache.set_class_sig cache class_loc sig_info;
  Type_env.bind_class_self_type cx class_t_internal class_loc;
  class_t

let resolve_op_assign cx ~exp_loc id_reason lhs op rhs =
  let open Ast.Expression in
  let reason = mk_reason (RCustom (Flow_ast_utils.string_of_assignment_operator op)) exp_loc in
  match op with
  | Assignment.PlusAssign
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
    (* lhs (op)= rhs *)
    let ((_, lhs_t), _) = Statement.assignment_lhs cx lhs in
    let rhs_t = expression cx rhs in
    Statement.arith_assign
      cx
      ~reason
      ~lhs_reason:id_reason
      ~rhs_reason:(mk_expression_reason rhs)
      lhs_t
      rhs_t
      (ArithKind.arith_kind_of_assignment_operator op)
  | Assignment.AndAssign
  | Assignment.OrAssign
  | Assignment.NullishAssign ->
    let ((_, lhs_t), _) = Statement.assignment_lhs cx lhs in
    let (((_, rhs_t), _), right_abnormal) =
      Abnormal.catch_expr_control_flow_exception (fun () -> Statement.expression cx rhs)
    in
    let rhs_t =
      match right_abnormal with
      | Some Abnormal.Throw -> EmptyT.at exp_loc
      | None -> rhs_t
    in
    let ub t =
      match op with
      | Assignment.NullishAssign -> NullishCoalesceT (reason, rhs_t, t)
      | Assignment.AndAssign -> AndT (reason, rhs_t, t)
      | Assignment.OrAssign -> OrT (reason, rhs_t, t)
      | _ -> assert_false "Bad conditional guard"
    in
    Tvar.mk_no_wrap_where cx reason (fun t -> Flow_js.flow cx (lhs_t, ub t))

let resolve_update cx ~id_loc ~exp_loc id_reason =
  let reason = mk_reason (RCustom "update") exp_loc in
  let id_t = Type_env.ref_entry_exn ~lookup_mode:Type_env.LookupMode.ForValue cx id_loc id_reason in
  Tvar.mk_where cx reason (fun result_t ->
      Flow_js.flow cx (id_t, UnaryArithT { reason; result_t; kind = UnaryArithKind.Update })
  )

let resolve_type_alias cx loc alias =
  let cache = Context.node_cache cx in
  let (t, ast) = Statement.type_alias cx loc alias in
  Node_cache.set_alias cache loc (t, ast);
  t

let resolve_opaque_type cx loc opaque =
  let cache = Context.node_cache cx in
  let (t, ast) = Statement.opaque_type cx loc opaque in
  Node_cache.set_opaque cache loc (t, ast);
  t

let resolve_import cx id_loc import_reason import_kind module_name source_loc import =
  let source_module_t =
    let import_kind_for_untyped_import_validation =
      match import_kind with
      | Ast.Statement.ImportDeclaration.ImportType -> Some Type.ImportType
      | Ast.Statement.ImportDeclaration.ImportTypeof -> Some Type.ImportTypeof
      | Ast.Statement.ImportDeclaration.ImportValue -> Some Type.ImportValue
    in
    Import_export.get_module_t
      cx
      ~import_kind_for_untyped_import_validation
      (source_loc, module_name)
  in
  match import with
  | Name_def.Named { kind; remote; local } ->
    let import_kind = Base.Option.value ~default:import_kind kind in
    let (_, t) =
      Import_export.import_named_specifier_type
        cx
        import_reason
        import_kind
        ~module_name
        ~source_module_t
        ~remote_name:remote
        ~local_name:local
    in
    if Flow_ast_utils.hook_name local then
      make_hooklike cx t
    else
      t
  | Namespace name ->
    let t =
      Import_export.import_namespace_specifier_type
        cx
        import_reason
        import_kind
        ~module_name
        ~namespace_symbol:(mk_namespace_symbol ~name ~def_loc:id_loc)
        ~source_module_t
        ~local_loc:id_loc
    in
    if Flow_ast_utils.hook_name name then
      make_hooklike cx t
    else
      t
  | Default local_name ->
    let (_, t) =
      Import_export.import_default_specifier_type
        cx
        import_reason
        import_kind
        ~module_name
        ~source_module_t
        ~local_name
    in
    if Flow_ast_utils.hook_name local_name then
      make_hooklike cx t
    else
      t

let resolve_interface cx loc inter =
  let cache = Context.node_cache cx in
  let (t, ast) = Statement.interface cx loc inter in
  Node_cache.set_interface cache loc (t, ast);
  t

let resolve_declare_class cx loc class_ =
  let cache = Context.node_cache cx in
  let (t, ast) = Statement.declare_class cx loc class_ in
  Node_cache.set_declared_class cache loc (t, ast);
  t

let resolve_declare_component cx loc component =
  let cache = Context.node_cache cx in
  let (t, ast) = Statement.declare_component cx loc component in
  Node_cache.set_declared_component cache loc (t, ast);
  t

let resolve_declare_namespace cx loc ns =
  let cache = Context.node_cache cx in
  let ((t, _) as ast) = Statement.declare_namespace cx loc ns in
  Node_cache.set_declared_namespace cache loc ast;
  t

let resolve_enum cx id_loc enum_reason enum_loc name enum =
  if Context.enable_enums cx then
    let enum_info = ConcreteEnum (Statement.mk_enum cx ~enum_reason id_loc name enum) in
    mk_enum_object_type enum_reason enum_info
  else (
    Flow_js_utils.add_output cx (Error_message.EEnumsNotEnabled enum_loc);
    AnyT.error enum_reason
  )

let resolve_type_param cx id_loc =
  let { Loc_env.tparams; _ } = Context.environment cx in
  let (_, _, t) = ALocMap.find id_loc tparams in
  DefT (TypeUtil.reason_of_t t, TypeT (TypeParamKind, t))

let resolve_chain_expression cx ~cond exp =
  let cache = Context.node_cache cx in
  let cond =
    match cond with
    | NonConditionalContext -> None
    | OtherConditionalTest -> Some OtherTest
  in
  let (t, _, exp) = Statement.optional_chain ~cond cx exp in
  Node_cache.set_expression cache exp;
  t

let resolve_write_expression cx ~cond exp =
  let cond =
    match cond with
    | NonConditionalContext -> None
    | OtherConditionalTest -> Some OtherTest
  in
  synthesizable_expression cx ?cond exp

let resolve_generator_next cx reason gen =
  let open TypeUtil in
  match gen with
  | None -> VoidT.make (replace_desc_reason RUnannotatedNext reason)
  | Some { tparams_map; return_annot; async } ->
    let return_t =
      let cache = Context.node_cache cx in
      let tparams_map = mk_tparams_map cx tparams_map in
      let (t, anno) = Anno.mk_type_available_annotation cx tparams_map return_annot in
      Node_cache.set_annotation cache anno;
      t
    in
    let gen_name =
      if async then
        "AsyncGenerator"
      else
        "Generator"
    in
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
        let t = Flow_js.reposition cx (reason_of_t return_t |> loc_of_reason) t in
        Flow_js.flow_t cx (t, return_t)
    )

let resolve cx (def_kind, id_loc) (def, def_scope_kind, class_stack, def_reason) =
  let env = Context.environment cx in
  Context.set_environment cx { env with Loc_env.scope_kind = def_scope_kind; class_stack };
  let t =
    match def with
    | Binding b -> resolve_binding cx def_reason id_loc b
    | ExpressionDef { cond_context = cond; expr; chain = true; hints = _ } ->
      resolve_chain_expression cx ~cond expr
    | ExpressionDef { cond_context = cond; expr; chain = false; hints = _ } ->
      resolve_write_expression cx ~cond expr
    | Component { component; component_loc; tparams_map } ->
      resolve_annotated_component cx def_scope_kind def_reason tparams_map component_loc component
    | Function
        {
          function_;
          synthesizable_from_annotation = FunctionSynthesizable | FunctionPredicateSynthesizable _;
          arrow;
          has_this_def = _;
          function_loc;
          tparams_map;
          statics;
          hints = _;
        } ->
      let hook_like = Base.Option.is_some (Flow_ast_utils.hook_function function_) in
      resolve_annotated_function
        cx
        ~bind_this:(not arrow)
        ~statics
        ~hook_like
        def_reason
        tparams_map
        function_loc
        function_
    | Function
        {
          function_;
          synthesizable_from_annotation = _;
          arrow;
          has_this_def = _;
          function_loc;
          tparams_map = _;
          statics;
          hints = _;
        } ->
      resolve_inferred_function
        cx
        ~statics
        ~needs_this_param:(not arrow)
        id_loc
        def_reason
        function_loc
        function_
    | Class { class_; class_loc; this_super_write_locs = _ } ->
      resolve_class cx id_loc def_reason class_loc class_
    | MemberAssign { member_loc = _; member = _; rhs } -> expression cx rhs
    | OpAssign { exp_loc; lhs; op; rhs } -> resolve_op_assign cx ~exp_loc def_reason lhs op rhs
    | Update { exp_loc; op = _ } -> resolve_update cx ~id_loc ~exp_loc def_reason
    | TypeAlias (loc, alias) -> resolve_type_alias cx loc alias
    | OpaqueType (loc, opaque) -> resolve_opaque_type cx loc opaque
    | Import { import_kind; source; source_loc; import } ->
      resolve_import cx id_loc def_reason import_kind source source_loc import
    | Interface (loc, inter) -> resolve_interface cx loc inter
    | DeclaredClass (loc, class_) -> resolve_declare_class cx loc class_
    | DeclaredComponent (loc, comp) -> resolve_declare_component cx loc comp
    | Enum (enum_loc, name, enum) -> resolve_enum cx id_loc def_reason enum_loc name enum
    | TypeParam _ -> resolve_type_param cx id_loc
    | GeneratorNext gen -> resolve_generator_next cx def_reason gen
    | DeclaredNamespace (loc, ns) -> resolve_declare_namespace cx loc ns
    | MissingThisAnnot -> AnyT.at (AnyError None) id_loc
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
  Type_env.resolve_env_entry cx t def_kind id_loc

let entries_of_def graph (kind, loc) =
  let open Name_def_ordering in
  let acc = EnvSet.singleton (kind, loc) in
  let rec add_from_bindings acc = function
    | Root (UnannotatedParameter r) -> EnvSet.add (Env_api.FunctionParamLoc, loc_of_reason r) acc
    | Root (Annotation { param_loc = Some l; _ }) -> EnvSet.add (Env_api.FunctionParamLoc, l) acc
    | Root (Contextual { reason; _ }) ->
      let l = Reason.loc_of_reason reason in
      EnvSet.add (Env_api.FunctionParamLoc, l) acc
    | Root
        (FunctionValue
          {
            function_loc;
            arrow = false;
            function_ = { Ast.Function.params = (_, { Ast.Function.Params.this_ = None; _ }); _ };
            _;
          }
          ) ->
      EnvSet.add (Env_api.FunctionThisLoc, function_loc) acc
    | Root (ObjectValue { synthesizable = ObjectSynthesizable { this_write_locs }; _ }) ->
      EnvSet.union this_write_locs acc
    | Hooklike bind -> add_from_bindings acc bind
    | Root _ -> acc
    | Select _ -> acc
  in
  match EnvMap.find (kind, loc) graph with
  | (Binding b, _, _, _) -> add_from_bindings acc b
  | (Class { this_super_write_locs; _ }, _, _, _) -> EnvSet.union this_super_write_locs acc
  | ( Function
        {
          has_this_def = true;
          function_loc;
          function_ = { Ast.Function.params = (_, { Ast.Function.Params.this_ = None; _ }); _ };
          _;
        },
      _,
      _,
      _
    ) ->
    EnvSet.add (Env_api.FunctionThisLoc, function_loc) acc
  | _ -> acc

let entries_of_component graph component =
  let open Name_def_ordering in
  let entries_of_elt element =
    let kl =
      match element with
      | Name_def_ordering.Normal kl
      | Resolvable kl
      | Illegal { payload = kl; _ } ->
        kl
    in
    entries_of_def graph kl
  in

  match component with
  | Singleton elt -> entries_of_elt elt
  | ResolvableSCC elts ->
    Nel.fold_left (fun acc def -> EnvSet.union acc (entries_of_elt def)) EnvSet.empty elts
  | IllegalSCC elts ->
    Nel.fold_left
      (fun acc ({ payload = elt; _ }, _) -> EnvSet.union acc (entries_of_elt elt))
      EnvSet.empty
      elts

let init_type_param =
  let rec init_type_param cx graph def_loc =
    let (def, _, _, reason) = EnvMap.find_ordinary def_loc graph in
    let tparam_entry =
      match def with
      | TypeParam { tparams_map = tparams_locs; from_infer_type; tparam } ->
        let tparams_map = mk_tparams_map cx graph tparams_locs in
        let ((_, ({ name; _ } as tparam), t) as info) =
          Anno.mk_type_param cx tparams_map ~from_infer_type tparam
        in
        let cache = Context.node_cache cx in
        Node_cache.set_tparam cache info;
        (name, tparam, t)
      | Class { class_loc; _ } ->
        let self = Type_env.read_class_self_type cx class_loc in
        let (this_param, this_t) = Statement.Class_stmt_sig.mk_this ~self cx reason in
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
  let resolve_illegal loc def =
    match def with
    | ( TypeParam
          {
            tparam =
              ( _,
                {
                  Ast.Type.TypeParam.name =
                    (name_loc, { Ast.Identifier.name = str_name; comments = _ });
                  _;
                }
              );
            _;
          },
        _,
        _,
        _
      ) ->
      let name = Subst_name.Name str_name in
      let reason = mk_annot_reason (RType (OrdinaryName str_name)) name_loc in
      let tparam =
        {
          reason;
          name;
          bound = DefT (reason, MixedT Mixed_everything);
          polarity = Polarity.Neutral;
          default = None;
          is_this = false;
        }
      in
      let ({ Loc_env.tparams; _ } as env) = Context.environment cx in
      Context.set_environment
        cx
        {
          env with
          Loc_env.tparams = ALocMap.add loc (name, tparam, AnyT.at (AnyError None) loc) tparams;
        }
    | (Class _, _, _, _) ->
      let name = Subst_name.Name "this" in
      let reason = mk_annot_reason RThis loc in
      let tparam =
        {
          reason;
          name;
          bound = DefT (reason, MixedT Mixed_everything);
          polarity = Polarity.Neutral;
          default = None;
          is_this = true;
        }
      in
      let ({ Loc_env.tparams; _ } as env) = Context.environment cx in
      Context.set_environment
        cx
        {
          env with
          Loc_env.tparams = ALocMap.add loc (name, tparam, AnyT.at (AnyError None) loc) tparams;
        }
    | _ -> ()
  in
  let resolve_element = function
    | Illegal { payload = key; _ } ->
      let (_kind, loc) = key in
      resolve_illegal loc (EnvMap.find key graph)
    | Name_def_ordering.Normal key
    | Resolvable key ->
      (match EnvMap.find key graph with
      | (TypeParam _, _, _, _)
      | (Class _, _, _, _) ->
        let (_kind, loc) = key in
        ignore @@ init_type_param cx graph loc
      | _ -> ())
  in
  match component with
  | IllegalSCC elts ->
    Nel.iter
      (fun ( {
               payload =
                 Illegal { payload; _ } | Resolvable payload | Name_def_ordering.Normal payload;
               _;
             },
             _
           ) -> resolve_illegal (snd payload) (EnvMap.find payload graph))
      elts
  | Singleton elt -> resolve_element elt
  | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts

let resolve_component cx graph component =
  let open Name_def_ordering in
  Context.constraint_cache cx := FlowSet.empty;
  let resolve_illegal entries =
    EnvSet.iter
      (fun (kind, loc) -> Type_env.resolve_env_entry cx (AnyT.at (AnyError None) loc) kind loc)
      entries
  in
  let resolve_element = function
    | Illegal { payload; _ } -> resolve_illegal (entries_of_def graph payload)
    | Name_def_ordering.Normal (kind, loc)
    | Resolvable (kind, loc) ->
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
  let log_slow_to_check ~f =
    match Context.slow_to_check_logging cx with
    | { Slow_to_check_logging.slow_components_logging_threshold = Some threshold; _ } ->
      let start_time = Unix.gettimeofday () in
      let result = f () in
      let end_time = Unix.gettimeofday () in
      let run_time = end_time -. start_time in
      if run_time > threshold then
        Hh_logger.info
          "[%d] Slow CHECK component %s (%f seconds)"
          (Sys_utils.get_pretty_pid ())
          (string_of_component graph component)
          run_time;
      result
    | _ -> f ()
  in
  let f () =
    let entries_for_resolution =
      let entries = entries_of_component graph component in
      Type_env.make_env_entries_under_resolution cx entries;
      entries
    in
    resolve_component_type_params cx graph component;
    let () =
      match component with
      | IllegalSCC _ -> resolve_illegal entries_for_resolution
      | Singleton elt -> resolve_element elt
      | ResolvableSCC elts -> Nel.iter (fun elt -> resolve_element elt) elts
    in
    Debug_js.Verbose.print_if_verbose_lazy cx (lazy ["Finished resolving component"]);
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy ["Forcing all lazy tvars after resolving component"]);
    let env = Context.environment cx in
    EnvSet.iter
      (fun (kind, loc) ->
        Loc_env.find_write env kind loc
        |> Base.Option.iter ~f:(fun (Loc_env.TypeEntry { t; state = _ }) ->
               match t with
               | OpenT (_, id) ->
                 (match Context.find_constraints cx id with
                 | (root_id, Type.Constraint.FullyResolved s) ->
                   Context.add_post_component_tvar_forcing_state cx root_id s
                 | _ -> ())
               | _ -> ()
           ))
      entries_for_resolution;
    Context.post_component_tvar_forcing_states cx
    |> Base.List.iter ~f:(fun s -> ignore @@ Context.force_fully_resolved_tvar cx s);
    Debug_js.Verbose.print_if_verbose_lazy
      cx
      (lazy ["Forced all lazy tvars after resolving component"])
  in
  log_slow_to_check ~f
