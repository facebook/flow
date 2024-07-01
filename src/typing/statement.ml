(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Tast_utils = Typed_ast_utils

(* This module contains the traversal functions which set up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. *)

module Flow = Flow_js
open Utils_js
open Reason
open FlowSymbol
open Type
open TypeUtil
open Func_class_sig_types
open Type_operation_utils
module Eq_test = Eq_test.Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)

module Make
    (Destructuring : Destructuring_sig.S)
    (Func_stmt_config : Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types)
    (Component_declaration_config : Component_params_intf.Config
                                      with module Types := Component_sig_types
                                                           .DeclarationParamConfig)
    (Statement : Statement_sig.S) : Statement_sig.S = struct
  module Anno = Type_annotation.Make (Type_annotation_cons_gen.FlowJS) (Statement)
  module Class_type_sig = Anno.Class_type_sig
  module Func_stmt_config = Func_stmt_config
  module Component_declaration_config = Component_declaration_config
  open Type_env.LookupMode

  (*************)
  (* Utilities *)
  (*************)

  module ChainingConf = struct
    type ('a, 'b) t = {
      refinement_action: ('a -> Type.t -> Type.t -> Type.t) option;
      refine: unit -> Type.t option;
      subexpressions: unit -> 'a * 'b;
      get_result: 'a -> Reason.t -> Type.t -> Type.t;
      get_opt_use: 'a -> Reason.t -> Type.opt_use_t;
      get_reason: Type.t -> Reason.t;
    }
  end

  type class_member_kind =
    | Class_Member_Field
    | Class_Member_Getter
    | Class_Member_GetterSetter
    | Class_Member_Method
    | Class_Member_Setter

  type seen_names = {
    static_names: class_member_kind SMap.t;
    instance_names: class_member_kind SMap.t;
  }

  let empty_seen_names = { static_names = SMap.empty; instance_names = SMap.empty }

  module ObjectExpressionAcc = struct
    type element =
      | Spread of Type.t
      | Slice of { slice_pmap: Type.Properties.t }

    type t = {
      obj_pmap: Type.Properties.t;
      tail: element list;
      proto: Type.t option;
      obj_key_autocomplete: bool;
    }

    let empty _ =
      { obj_pmap = NameUtils.Map.empty; tail = []; proto = None; obj_key_autocomplete = false }

    let empty_slice = Slice { slice_pmap = NameUtils.Map.empty }

    let head_slice { obj_pmap; _ } =
      if NameUtils.Map.is_empty obj_pmap then
        None
      else
        Some (Slice { slice_pmap = obj_pmap })

    let add_prop f acc = { acc with obj_pmap = f acc.obj_pmap }

    let add_proto p acc = { acc with proto = Some p }

    let add_spread t acc =
      let tail =
        match head_slice acc with
        | None -> acc.tail
        | Some slice -> slice :: acc.tail
      in
      { acc with obj_pmap = NameUtils.Map.empty; tail = Spread t :: tail }

    let set_obj_key_autocomplete acc = { acc with obj_key_autocomplete = true }

    let obj_key_autocomplete acc = acc.obj_key_autocomplete

    let elements_rev acc =
      match head_slice acc with
      | Some slice -> (slice, acc.tail)
      | None ->
        (match acc.tail with
        | [] -> (empty_slice, [])
        | x :: xs -> (x, xs))

    let proto { proto; _ } = proto

    let mk_object_from_spread_acc cx acc reason ~as_const ~frozen ~default_proto =
      match elements_rev acc with
      | (Slice { slice_pmap }, []) ->
        let proto = Base.Option.value ~default:default_proto (proto acc) in
        let obj_t =
          Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen ~props:slice_pmap proto
        in
        if obj_key_autocomplete acc then
          let get_autocomplete_t () =
            Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tvar ->
                Flow_js.flow_t cx (obj_t, tvar)
            )
          in
          let (_, lazy_hint) = Type_env.get_hint cx (Reason.loc_of_reason reason) in
          lazy_hint reason |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:get_autocomplete_t
        else
          obj_t
      | os ->
        let (t, ts, head_slice) =
          let (t, ts) = os in
          (* We don't need to do this recursively because every pair of slices must be separated
           * by a spread *)
          match (t, ts) with
          | (Spread t, ts) ->
            let ts =
              Base.List.map
                ~f:(function
                  | Spread t -> Object.Spread.Type t
                  | Slice { slice_pmap } ->
                    Object.Spread.Slice
                      {
                        Object.Spread.reason;
                        prop_map = slice_pmap;
                        dict = None;
                        generics = Generic.spread_empty;
                        reachable_targs = [];
                      })
                ts
            in
            (t, ts, None)
          | (Slice { slice_pmap = prop_map }, Spread t :: ts) ->
            let head_slice =
              {
                Type.Object.Spread.reason;
                prop_map;
                dict = None;
                generics = Generic.spread_empty;
                reachable_targs = [];
              }
            in
            let ts =
              Base.List.map
                ~f:(function
                  | Spread t -> Object.Spread.Type t
                  | Slice { slice_pmap } ->
                    Object.Spread.Slice
                      {
                        Object.Spread.reason;
                        prop_map = slice_pmap;
                        dict = None;
                        generics = Generic.spread_empty;
                        reachable_targs = [];
                      })
                ts
            in
            (t, ts, Some head_slice)
          | _ -> failwith "Invariant Violation: spread list has two slices in a row"
        in
        let target = Object.Spread.Value { make_seal = Obj_type.mk_seal ~frozen ~as_const } in
        let tool = Object.Resolve Object.Next in
        let state =
          {
            Object.Spread.todo_rev = ts;
            acc =
              Base.Option.value_map
                ~f:(fun x -> [Object.Spread.InlineSlice x])
                ~default:[]
                head_slice;
            spread_id = Reason.mk_id ();
            union_reason = None;
            curr_resolve_idx = 0;
          }
        in
        let tout =
          Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
              let use_op = Op (ObjectSpread { op = reason }) in
              Flow.flow
                cx
                (t, ObjKitT (use_op, reason, tool, Type.Object.Spread (target, state), tout))
          )
        in
        if obj_key_autocomplete acc then
          let (_, lazy_hint) = Type_env.get_hint cx (Reason.loc_of_reason reason) in
          lazy_hint reason |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:(fun () -> tout)
        else
          tout
  end

  let mk_ident ~comments name = { Ast.Identifier.name; comments }

  let snd_fst ((_, x), _) = x

  let translate_identifer_or_literal_key t =
    let module P = Ast.Expression.Object.Property in
    function
    | P.Identifier (loc, name) -> P.Identifier ((loc, t), name)
    | P.StringLiteral (loc, lit) -> P.StringLiteral ((loc, t), lit)
    | P.NumberLiteral (loc, lit) -> P.NumberLiteral ((loc, t), lit)
    | P.BigIntLiteral _
    | P.PrivateName _
    | P.Computed _ ->
      assert_false "precondition not met"

  let name_of_identifier_or_literal_key key =
    let module P = Ast.Expression.Object.Property in
    match key with
    | P.Identifier (_, { Ast.Identifier.name; _ })
    | P.StringLiteral (_, { Ast.StringLiteral.value = name; _ }) ->
      Ok name
    | P.NumberLiteral (loc, { Ast.NumberLiteral.value; _ }) ->
      if Js_number.is_float_safe_integer value then
        let name = Dtoa.ecma_string_of_float value in
        Ok name
      else
        Error
          (Error_message.EUnsupportedKeyInObject
             {
               loc;
               obj_kind = `Literal;
               key_error_kind = Flow_intermediate_error_types.InvalidObjKey.kind_of_num_value value;
             }
          )
    | P.BigIntLiteral (loc, _)
    | P.PrivateName (loc, _)
    | P.Computed (loc, _) ->
      Error
        (Error_message.EUnsupportedKeyInObject
           {
             loc;
             obj_kind = `Literal;
             key_error_kind = Flow_intermediate_error_types.InvalidObjKey.Other;
           }
        )

  let convert_call_targs =
    let open Ast.Expression.CallTypeArg in
    let rec loop ts tasts cx tparams_map = function
      | [] -> (List.rev ts, List.rev tasts)
      | ast :: asts -> begin
        match ast with
        | Explicit ast ->
          let (((_, t), _) as tast) = Anno.convert cx tparams_map ast in
          loop (ExplicitArg t :: ts) (Explicit tast :: tasts) cx tparams_map asts
        | Implicit (loc, impl) ->
          let reason = mk_reason RImplicitInstantiation loc in
          let id = Tvar.mk_no_wrap cx reason in
          loop
            (ImplicitArg (reason, id) :: ts)
            (Implicit ((loc, OpenT (reason, id)), impl) :: tasts)
            cx
            tparams_map
            asts
      end
    in
    fun cx tparams_map call_targs ->
      let open Ast.Expression.CallTypeArgs in
      let { arguments; comments } = call_targs in
      let (ts, tasts) = loop [] [] cx tparams_map arguments in
      (ts, { arguments = tasts; comments })

  let convert_call_targs_opt cx = function
    | None -> (None, None)
    | Some (loc, args) ->
      let (targts, targs_ast) = convert_call_targs cx Subst_name.Map.empty args in
      (Some targts, Some (loc, targs_ast))

  let convert_call_targs_opt' cx = function
    | None -> None
    | Some (_, args) ->
      let (targts, _) = convert_call_targs cx Subst_name.Map.empty args in
      Some targts

  module ALoc_this_finder = This_finder.Make (Loc_collections.ALocSet)

  let error_on_this_uses_in_object_methods cx =
    let open Ast in
    let open Expression in
    Base.List.iter ~f:(function
        | Object.Property (prop_loc, Object.Property.Method { key; value = (_, func); _ })
        | Object.Property (prop_loc, Object.Property.Get { key; value = (_, func); _ })
        | Object.Property (prop_loc, Object.Property.Set { key; value = (_, func); _ }) ->
          let finder = new ALoc_this_finder.finder in
          finder#eval (finder#function_ prop_loc) func
          |> Loc_collections.ALocSet.iter (fun loc ->
                 let reason =
                   match key with
                   | Object.Property.Identifier (_, { Identifier.name; _ })
                   | Object.Property.PrivateName (_, { PrivateName.name; _ })
                   | Object.Property.StringLiteral (_, { StringLiteral.raw = name; _ }) ->
                     mk_reason (RMethod (Some name)) prop_loc
                   | _ -> mk_reason (RMethod None) prop_loc
                 in
                 Flow_js.add_output cx (Error_message.EObjectThisReference (loc, reason))
             )
        | _ -> ()
        )

  let error_on_this_uses_in_components cx { Ast.Statement.ComponentDeclaration.sig_loc; body; _ } =
    let finder = new ALoc_this_finder.finder in
    finder#eval finder#component_body body
    |> Loc_collections.ALocSet.iter (fun this_loc ->
           Flow_js.add_output
             cx
             (Error_message.EComponentThisReference { component_loc = sig_loc; this_loc })
       )

  (* Given the expression of a statement expression, returns a list of child
     expressions which are _potentially_ unhandled promises. At this point,
     we don't know if they are actually of type Promise. We will determine that
     later, but we don't need to even check that if we can tell that the
     expression is being handled ("used") syntactically here. *)
  let rec syntactically_unhandled_promises ((_, expr_ast) as expr) =
    let open Flow_ast.Expression in
    match expr_ast with
    | Assignment _
    (* Call to `catch` or `finally` with one argument *)
    | Call
        {
          Call.callee =
            ( _,
              Member
                {
                  Member.property =
                    Member.PropertyIdentifier
                      (_, { Flow_ast.Identifier.name = "catch" | "finally"; _ });
                  _;
                }
            );
          arguments = (_, { ArgList.arguments = _ :: _; _ });
          _;
        }
    | OptionalCall
        {
          OptionalCall.call =
            {
              Call.callee =
                ( _,
                  OptionalMember
                    {
                      OptionalMember.member =
                        {
                          Member.property =
                            Member.PropertyIdentifier
                              (_, { Flow_ast.Identifier.name = "catch" | "finally"; _ });
                          _;
                        };
                      _;
                    }
                );
              arguments = (_, { ArgList.arguments = _ :: _; _ });
              _;
            };
          _;
        }
    (* Call to `then` with two arguments *)
    | Call
        {
          Call.callee =
            ( _,
              Member
                {
                  Member.property =
                    Member.PropertyIdentifier (_, { Flow_ast.Identifier.name = "then"; _ });
                  _;
                }
            );
          arguments = (_, { ArgList.arguments = _ :: _ :: _; _ });
          _;
        }
    | OptionalCall
        {
          OptionalCall.call =
            {
              Call.callee =
                ( _,
                  OptionalMember
                    {
                      OptionalMember.member =
                        {
                          Member.property =
                            Member.PropertyIdentifier (_, { Flow_ast.Identifier.name = "then"; _ });
                          _;
                        };
                      _;
                    }
                );
              arguments = (_, { ArgList.arguments = _ :: _ :: _; _ });
              _;
            };
          _;
        } ->
      []
    (* Recurse into logical operands for expressions like `condition && somePromise();` *)
    | Logical { Logical.left; right; _ } ->
      Base.List.unordered_append
        (syntactically_unhandled_promises left)
        (syntactically_unhandled_promises right)
    (* Recurse into conditional operands for expressions like `b ? x : somePromise();` *)
    | Conditional { Conditional.consequent; alternate; _ } ->
      Base.List.unordered_append
        (syntactically_unhandled_promises consequent)
        (syntactically_unhandled_promises alternate)
    | _ -> [expr]

  module Func_stmt_params =
    Func_params.Make (Func_stmt_config_types.Types) (Func_stmt_config) (Func_stmt_params_types)
  module Func_stmt_sig =
    Func_sig.Make (Statement) (Func_stmt_config_types.Types) (Func_stmt_config) (Func_stmt_params)
      (Func_stmt_sig_types)
  module Class_stmt_sig =
    Class_sig.Make (Type_annotation_cons_gen.FlowJS) (Func_stmt_config_types.Types)
      (Func_stmt_config)
      (Func_stmt_params)
      (Func_stmt_sig)
      (Class_stmt_sig_types)
  module Component_declaration_params =
    Component_params.Make
      (Component_sig_types.DeclarationParamConfig)
      (Component_declaration_config)
      (Component_sig_types.Component_declaration_params_types)
  module Component_declaration_body =
    Component_sig.Component_declaration_body (Statement) (Component_sig_types.DeclarationBodyConfig)
  module Component_declaration_sig =
    Component_sig.Make (Component_sig_types.DeclarationParamConfig) (Component_declaration_config)
      (Component_declaration_params)
      (Component_sig_types.DeclarationBodyConfig)
      (Component_declaration_body)
      (Component_sig_types.Component_declaration_sig_types)

  (* In positions where an annotation may be present or an annotation can be pushed down,
   * we should prefer the annotation over the pushed-down annotation. *)
  let mk_inference_target_with_annots ~has_hint annot_or_inferred =
    match (annot_or_inferred, has_hint) with
    | (Annotated _, _) -> annot_or_inferred
    | (_, true) -> Annotated (type_t_of_annotated_or_inferred annot_or_inferred)
    | _ -> annot_or_inferred

  (******************)
  (* Constraint gen *)
  (******************)

  (* We assume that constructor functions return void
      and constructions return objects.
      TODO: This assumption does not always hold.
      If construction functions return non-void values (e.g., functions),
      then those values are returned by constructions.
  *)
  let new_call cx loc reason ~use_op class_ targs args =
    let specialized_ctor = Context.new_specialized_callee cx in
    let t =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          Flow.flow
            cx
            ( class_,
              ConstructorT
                {
                  use_op;
                  reason;
                  targs;
                  args;
                  tout;
                  return_hint = Type_env.get_hint cx loc;
                  specialized_ctor = Some specialized_ctor;
                }
            )
      )
    in
    let ctor_t =
      Flow_js_utils.CalleeRecorder.type_for_tast_opt reason specialized_ctor
      |> Base.Option.value ~default:class_
    in
    (t, ctor_t)

  let func_call_opt_use
      cx loc reason ~use_op ?(call_strict_arity = true) targts argts specialized_callee =
    let opt_app =
      mk_opt_functioncalltype reason targts argts call_strict_arity specialized_callee
    in
    let return_hint = Type_env.get_hint cx loc in
    OptCallT { use_op; reason; opt_funcalltype = opt_app; return_hint }

  let func_call cx loc reason ~use_op ?(call_strict_arity = true) func_t targts argts t_callee =
    let opt_use =
      func_call_opt_use cx loc reason ~use_op ~call_strict_arity targts argts t_callee
    in
    Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
        Flow.flow cx (func_t, apply_opt_use opt_use t)
    )

  let method_call_opt_use
      cx
      opt_state
      ~voided_out
      reason
      ~use_op
      ~private_
      ?(call_strict_arity = true)
      prop_loc
      (expr, name)
      chain_loc
      targts
      argts
      specialized_callee =
    let (expr_loc, _) = expr in
    let prop_name = OrdinaryName name in
    let reason_prop = mk_reason (RProperty (Some prop_name)) prop_loc in
    let reason_expr = mk_reason (RProperty (Some prop_name)) expr_loc in
    let opt_methodcalltype = mk_opt_methodcalltype targts argts call_strict_arity in
    let propref = mk_named_prop ~reason:reason_prop prop_name in
    let action =
      match opt_state with
      | NewChain ->
        let exp_reason = mk_reason ROptionalChain chain_loc in
        OptChainM
          {
            exp_reason;
            lhs_reason = mk_expression_reason expr;
            opt_methodcalltype;
            voided_out;
            return_hint = Type.hint_unavailable;
            specialized_callee;
          }
      | _ ->
        OptCallM
          { opt_methodcalltype; return_hint = Type_env.get_hint cx chain_loc; specialized_callee }
    in
    if private_ then
      let class_entries = Type_env.get_class_entries cx in
      OptPrivateMethodT (use_op, reason, reason_expr, name, class_entries, false, action)
    else
      OptMethodT (use_op, reason, reason_expr, propref, action)

  (* returns (type of method itself, type returned from method) *)
  let method_call
      cx reason ~use_op ?(call_strict_arity = true) prop_loc (expr, obj_t, name) targts argts =
    let (expr_loc, _) = expr in
    match Refinement.get ~allow_optional:true cx expr (loc_of_reason reason) with
    | Some f ->
      (* note: the current state of affairs is that we understand
         member expressions as having refined types, rather than
         understanding receiver objects as carrying refined properties.
         generalizing this properly is a todo, and will deliver goodness.
         meanwhile, here we must hijack the property selection normally
         performed by the flow algorithm itself. *)
      ( f,
        Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
            let app =
              mk_boundfunctioncalltype
                ~call_kind:RegularCallKind
                obj_t
                targts
                argts
                t
                ~call_strict_arity
            in
            Flow.flow
              cx
              ( f,
                CallT
                  {
                    use_op;
                    reason;
                    call_action = Funcalltype app;
                    return_hint = Type.hint_unavailable;
                  }
              )
        )
      )
    | None ->
      let name = OrdinaryName name in
      let reason_prop = mk_reason (RProperty (Some name)) prop_loc in
      let specialized_callee = Context.new_specialized_callee cx in
      let out =
        Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
            let reason_expr = mk_reason (RProperty (Some name)) expr_loc in
            let methodcalltype =
              mk_methodcalltype targts argts t ~meth_strict_arity:call_strict_arity
            in
            let propref = mk_named_prop ~reason:reason_prop name in
            Flow.flow
              cx
              ( obj_t,
                MethodT
                  ( use_op,
                    reason,
                    reason_expr,
                    propref,
                    CallM
                      {
                        methodcalltype;
                        return_hint = Type.hint_unavailable;
                        specialized_callee = Some specialized_callee;
                      }
                  )
              )
        )
      in
      let prop_t = Flow_js_utils.CalleeRecorder.type_for_tast reason_prop specialized_callee in
      (prop_t, out)

  let elem_call_opt_use
      opt_state
      ~voided_out
      ~use_op
      ~reason_call
      ~reason_lookup
      ~reason_expr
      ~reason_chain
      targts
      argts
      elem_t
      specialized_callee =
    let opt_methodcalltype = mk_opt_methodcalltype targts argts true in
    let action =
      match opt_state with
      | NewChain ->
        OptChainM
          {
            exp_reason = reason_chain;
            lhs_reason = reason_expr;
            opt_methodcalltype;
            voided_out;
            return_hint = Type.hint_unavailable;
            specialized_callee;
          }
      | _ ->
        OptCallM { opt_methodcalltype; return_hint = Type.hint_unavailable; specialized_callee }
    in
    OptCallElemT (use_op, reason_call, reason_lookup, elem_t, action)

  (**********)
  (* Values *)
  (**********)

  let identifier_ cx name loc =
    let get_checking_mode_type () =
      let t = Type_env.var_ref ~lookup_mode:ForValue cx (OrdinaryName name) loc in
      (* We want to make sure that the reason description for the type we return
       * is always `RIdentifier name`. *)
      match (desc_of_t t, t) with
      | (RIdentifier name', _) when OrdinaryName name = name' -> t
      | (_, OpenT _) ->
        (* If this is an `OpenT` we can change its reason description directly. *)
        mod_reason_of_t (replace_desc_new_reason (RIdentifier (OrdinaryName name))) t
      (* If this is not an `OpenT` then create a new type variable with our
       * desired reason and unify it with our type. This adds a level of
       * indirection so that we don't modify the underlying reason of our type. *)
      | _ ->
        let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (Flow.unify cx t)
    in
    if Type_inference_hooks_js.dispatch_id_hook cx name loc then
      let reason = mk_reason RAutocompleteToken loc in
      let (_, lazy_hint) = Type_env.get_hint cx loc in
      lazy_hint reason |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:(fun () -> EmptyT.at loc)
    else
      get_checking_mode_type ()

  let identifier cx { Ast.Identifier.name; comments = _ } loc =
    let t = identifier_ cx name loc in
    t

  let string_literal_value cx ~singleton loc value =
    if Type_inference_hooks_js.dispatch_literal_hook cx loc then
      let (_, lazy_hint) = Type_env.get_hint cx loc in
      let hint = lazy_hint (mk_reason RString loc) in
      let error () = EmptyT.at loc in
      Type_hint.with_hint_result hint ~ok:Base.Fn.id ~error
    else if singleton then
      let reason = mk_annot_reason (RStringLit (OrdinaryName value)) loc in
      DefT (reason, SingletonStrT (OrdinaryName value))
    else
      (* It's too expensive to track literal information for large strings.*)
      let max_literal_length = Context.max_literal_length cx in
      if max_literal_length = 0 || String.length value <= max_literal_length then
        let reason = mk_annot_reason RString loc in
        DefT (reason, StrT (Literal (None, OrdinaryName value)))
      else
        let reason = mk_annot_reason (RLongStringLit max_literal_length) loc in
        DefT (reason, StrT AnyLiteral)

  let string_literal cx ~singleton loc { Ast.StringLiteral.value; _ } =
    string_literal_value cx ~singleton loc value

  let boolean_literal ~singleton loc { Ast.BooleanLiteral.value; _ } =
    if singleton then
      let reason = mk_annot_reason (RBooleanLit value) loc in
      DefT (reason, SingletonBoolT value)
    else
      let reason = mk_annot_reason RBoolean loc in
      DefT (reason, BoolT (Some value))

  let null_literal loc = NullT.at loc

  let number_literal ~singleton loc { Ast.NumberLiteral.value; raw; _ } =
    if singleton then
      let reason = mk_annot_reason (RNumberLit raw) loc in
      DefT (reason, SingletonNumT (value, raw))
    else
      let reason = mk_annot_reason RNumber loc in
      DefT (reason, NumT (Literal (None, (value, raw))))

  let bigint_literal ~singleton loc { Ast.BigIntLiteral.value; raw; _ } =
    if singleton then
      let reason = mk_annot_reason (RBigIntLit raw) loc in
      DefT (reason, SingletonBigIntT (value, raw))
    else
      let reason = mk_annot_reason RBigInt loc in
      DefT (reason, BigIntT (Literal (None, (value, raw))))

  let regexp_literal cx loc =
    let reason = mk_annot_reason RRegExp loc in
    Flow.get_builtin_type cx reason "RegExp"

  let module_ref_literal cx loc lit =
    let { Ast.ModuleRefLiteral.value; require_out; prefix_len; legacy_interop; _ } = lit in
    let mref = Base.String.drop_prefix value prefix_len in
    let module_t =
      Import_export.get_module_t
        cx
        ~import_kind_for_untyped_import_validation:(Some ImportValue)
        (loc, mref)
    in
    let require_t =
      Import_export.cjs_require_type
        cx
        (mk_reason (RModule mref) loc)
        ~namespace_symbol:(mk_module_symbol ~name:mref ~def_loc:loc)
        ~legacy_interop
        module_t
    in
    let reason = mk_reason (RCustom "module reference") loc in
    let t = Flow.get_builtin_typeapp cx reason "$Flow$ModuleRef" [require_t] in
    (t, { lit with Ast.ModuleRefLiteral.require_out = (require_out, require_t) })

  let check_const_assertion cx (loc, e) =
    let open Ast.Expression in
    if
      match e with
      | StringLiteral _
      | BooleanLiteral _
      | NumberLiteral _
      | BigIntLiteral _
      | RegExpLiteral _
      | Array _
      | Object _ ->
        false
      | _ -> true
    then
      Flow.add_output
        cx
        (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.AsConstOnNonLiteral))

  (*********)
  (* Types *)
  (*********)

  let opaque_type
      cx
      loc
      {
        Ast.Statement.OpaqueType.id = (name_loc, ({ Ast.Identifier.name; comments = _ } as id));
        tparams;
        impltype;
        supertype;
        comments;
      } =
    let cache = Context.node_cache cx in
    match Node_cache.get_opaque cache loc with
    | Some info ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Opaque type cache hit at %s" (ALoc.debug_to_string loc)]);
      info
    | None ->
      let r = DescFormat.type_reason (OrdinaryName name) name_loc in
      let (tparams, tparams_map, tparams_ast) = Anno.mk_type_param_declarations cx tparams in
      let (underlying_t, impltype_ast) = Anno.convert_opt cx tparams_map impltype in
      let (super_t, supertype_ast) = Anno.convert_opt cx tparams_map supertype in
      begin
        match tparams with
        | None -> ()
        | Some (_, tps) ->
          (* TODO: use tparams_map *)
          let tparams =
            Nel.fold_left (fun acc tp -> Subst_name.Map.add tp.name tp acc) Subst_name.Map.empty tps
          in
          Base.Option.iter
            underlying_t
            ~f:(Context.add_post_inference_polarity_check cx tparams Polarity.Positive);
          Base.Option.iter
            super_t
            ~f:(Context.add_post_inference_polarity_check cx tparams Polarity.Positive)
      end;
      let opaque_type_args =
        Base.List.map
          ~f:(fun { name; reason; polarity; _ } ->
            let t = Subst_name.Map.find name tparams_map in
            (name, reason, t, polarity))
          (TypeParams.to_list tparams)
      in
      let opaque_id = Context.make_aloc_id cx name_loc in
      let opaquetype = { underlying_t; super_t; opaque_id; opaque_type_args; opaque_name = name } in
      let t = OpaqueT (mk_reason (ROpaqueType name) name_loc, opaquetype) in
      let type_ =
        poly_type_of_tparams (Type.Poly.generate_id ()) tparams (DefT (r, TypeT (OpaqueKind, t)))
      in
      let () =
        Flow.(
          match (underlying_t, super_t) with
          | (Some l, Some u) -> flow_t cx (l, u)
          | _ -> ()
        )
      in

      let opaque_type_ast =
        {
          Ast.Statement.OpaqueType.id = ((name_loc, type_), id);
          tparams = tparams_ast;
          impltype = impltype_ast;
          supertype = supertype_ast;
          comments;
        }
      in
      (type_, opaque_type_ast)

  (*****************)
  (* Import/Export *)
  (*****************)

  let export_specifiers cx source export_kind =
    let open Ast.Statement in
    let module E = ExportNamedDeclaration in
    let lookup_mode =
      match export_kind with
      | Ast.Statement.ExportValue -> ForValue
      | Ast.Statement.ExportType -> ForType
    in
    (* [declare] export [type] {foo [as bar]}; *)
    let export_ref loc local_name =
      let t = Type_env.var_ref ~lookup_mode cx local_name loc in
      match export_kind with
      | Ast.Statement.ExportType ->
        let reason = mk_reason (RType local_name) loc in
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
            Flow.flow cx (t, AssertExportIsTypeT (reason, local_name, tout))
        )
      | Ast.Statement.ExportValue -> t
    in
    (* [declare] export [type] {foo [as bar]} from 'module' *)
    let export_from source_ns_t loc local_name =
      let reason = mk_reason (RIdentifier local_name) loc in
      Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
          let use_t =
            match export_kind with
            | Ast.Statement.ExportType ->
              GetTypeFromNamespaceT
                { use_op = unknown_use; reason; prop_ref = (reason, local_name); tout }
            | Ast.Statement.ExportValue ->
              GetPropT
                {
                  use_op = unknown_use;
                  reason;
                  id = None;
                  from_annot = false;
                  propref = mk_named_prop ~reason local_name;
                  tout;
                  hint = hint_unavailable;
                }
          in
          Flow.flow cx (source_ns_t, use_t)
      )
    in
    let export_specifier export (loc, { E.ExportSpecifier.local; exported }) =
      let (local_loc, ({ Ast.Identifier.name = local_name; comments = _ } as local_id)) = local in
      let local_name = OrdinaryName local_name in
      let reconstruct_remote =
        match exported with
        | None -> Fun.const None
        | Some (remote_loc, remote_id) -> (fun t -> Some ((remote_loc, t), remote_id))
      in
      let t = export local_loc local_name in
      ( loc,
        { E.ExportSpecifier.local = ((local_loc, t), local_id); exported = reconstruct_remote t }
      )
    in
    function
    (* [declare] export [type] {foo [as bar]} [from ...]; *)
    | E.ExportSpecifiers specifiers ->
      let export =
        match source with
        | Some ((source_loc, module_t), { Ast.StringLiteral.value = module_name; _ }) ->
          let source_ns_t =
            let reason = mk_reason (RModule module_name) source_loc in
            let namespace_symbol = mk_module_symbol ~name:module_name ~def_loc:source_loc in
            Import_export.get_module_namespace_type cx reason ~namespace_symbol module_t
          in
          export_from source_ns_t
        | None -> export_ref
      in
      let specifiers = Base.List.map ~f:(export_specifier export) specifiers in
      E.ExportSpecifiers specifiers
    (* [declare] export [type] * as id from "source"; *)
    | E.ExportBatchSpecifier (specifier_loc, Some (id_loc, ({ Ast.Identifier.name; _ } as id))) ->
      let ((_, module_t), _) = Base.Option.value_exn source in
      let reason = mk_reason (RIdentifier (OrdinaryName name)) id_loc in
      let ns_t =
        Import_export.get_module_namespace_type
          cx
          reason
          ~namespace_symbol:(mk_constant_symbol ~name ~def_loc:id_loc)
          module_t
      in
      E.ExportBatchSpecifier (specifier_loc, Some ((id_loc, ns_t), id))
    (* [declare] export [type] * from "source"; *)
    | E.ExportBatchSpecifier (specifier_loc, None) -> E.ExportBatchSpecifier (specifier_loc, None)

  let hook_check cx effect (loc, { Ast.Identifier.name; _ }) =
    if effect = Ast.Function.Hook && not (Flow_ast_utils.hook_name name) then
      Flow.add_output cx Error_message.(EHookNaming loc)

  (************)
  (* Visitors *)
  (************)

  (***************************************************************
   * local inference pass: visit AST statement list, calling
   * flow to check types/create graphs for merge-time checking
   ***************************************************************)

  let rec statement cx ((loc, _) as stmt) =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_statement node_cache loc with
    | Some node ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Statement cache hit at %s" (ALoc.debug_to_string loc)]);
      node
    | None -> statement_ cx stmt

  and statement_ cx : (ALoc.t, ALoc.t) Ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t
      =
    let open Ast.Statement in
    let variables cx decls =
      VariableDeclaration.(
        let { declarations; kind; comments } = decls in
        let declarations =
          Base.List.map
            ~f:(fun (loc, { Declarator.id; init }) ->
              let (id, init) = variable cx kind id init in
              (loc, { Declarator.id; init }))
            declarations
        in
        { declarations; kind; comments }
      )
    in
    let catch_clause cx catch_clause =
      let { Try.CatchClause.param; body = (b_loc, b); comments } = catch_clause in
      let open Ast.Pattern in
      match param with
      | Some p ->
        (match p with
        | (loc, Identifier { Identifier.name = (name_loc, id); annot; optional }) ->
          let (t, ast_annot) =
            match annot with
            | Ast.Type.Missing mloc ->
              let t =
                if Context.use_mixed_in_catch_variables cx then
                  MixedT.at loc
                else
                  AnyT.why CatchAny (mk_reason RAnyImplicit loc)
              in
              (t, Ast.Type.Missing (mloc, t))
            | Ast.Type.Available ((_, (_, (Ast.Type.Any _ | Ast.Type.Mixed _))) as annot) ->
              (* Not relevant with our limited accepted annotations. *)
              let tparams_map = Subst_name.Map.empty in
              let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
              (t, Ast.Type.Available ast_annot)
            | Ast.Type.Available (_, (loc, _)) ->
              Flow.add_output cx (Error_message.EInvalidCatchParameterAnnotation loc);
              ( AnyT.why CatchAny (mk_reason RAnyImplicit loc),
                Tast_utils.error_mapper#type_annotation_hint annot
              )
          in
          let body = statement_list cx b.Block.body in
          {
            Try.CatchClause.param =
              Some
                ( (loc, t),
                  Ast.Pattern.Identifier
                    {
                      Ast.Pattern.Identifier.name = ((name_loc, t), id);
                      annot = ast_annot;
                      optional;
                    }
                );
            body = (b_loc, { Block.body; comments = b.Block.comments });
            comments;
          }
        | (loc, _) ->
          Flow.add_output
            cx
            (Error_message.EUnsupportedSyntax
               (loc, Flow_intermediate_error_types.CatchParameterDeclaration)
            );
          Tast_utils.error_mapper#catch_clause catch_clause)
      | None ->
        let body = statement_list cx b.Block.body in
        {
          Try.CatchClause.param = None;
          body = (b_loc, { Block.body; comments = b.Block.comments });
          comments;
        }
    in
    let function_ ~is_declared_function loc func =
      match func with
      | { Ast.Function.id = None; _ } -> failwith "unexpected anonymous function statement"
      | { Ast.Function.id = Some id; _ } ->
        let { Ast.Function.sig_loc; async; generator; _ } = func in
        let (name_loc, { Ast.Identifier.name; comments = _ }) = id in
        let reason = func_reason ~async ~generator sig_loc in
        let tast_fun_type =
          Type_env.get_var_declared_type
            ~lookup_mode:Type_env.LookupMode.ForValue
            ~is_declared_function
            cx
            (OrdinaryName name)
            name_loc
        in
        let (fn_type, func_ast) = mk_function_declaration cx ~tast_fun_type reason loc func in
        (fn_type, id, (loc, FunctionDeclaration func_ast))
    in
    let declare_function cx loc f =
      match declare_function_to_function_declaration cx loc f with
      | Some (FunctionDeclaration func, reconstruct_ast) ->
        let (_, _, node) = function_ ~is_declared_function:true loc func in
        reconstruct_ast node
      | _ ->
        (* error case *)
        let { DeclareFunction.id = (id_loc, id_name); annot; predicate; comments } = f in
        let effect =
          match annot with
          | (_, (_, Ast.Type.Function { Ast.Type.Function.effect; _ })) -> effect
          | _ -> Ast.Function.Arbitrary
        in
        hook_check cx effect (id_loc, id_name);
        let (_, annot_ast) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
        let t =
          Type_env.get_var_declared_type
            ~lookup_mode:Type_env.LookupMode.ForValue
            ~is_declared_function:true
            cx
            (OrdinaryName id_name.Ast.Identifier.name)
            id_loc
        in
        let predicate = Base.Option.map ~f:Tast_utils.error_mapper#predicate predicate in
        { DeclareFunction.id = ((id_loc, t), id_name); annot = annot_ast; predicate; comments }
    in
    function
    | (_, Empty _) as stmt -> stmt
    | (loc, Block { Block.body; comments }) ->
      let body = statement_list cx body in
      (loc, Block { Block.body; comments })
    | (loc, Expression { Expression.expression = e; directive; comments }) ->
      let expr = expression cx e in
      Base.List.iter (syntactically_unhandled_promises expr) ~f:(fun ((_, expr_t), _) ->
          Context.mark_maybe_unused_promise cx loc expr_t ~async:(Type_env.in_async_scope cx)
      );
      (loc, Expression { Expression.expression = expr; directive; comments })
    | (loc, If { If.test; consequent; alternate; comments }) ->
      let test_ast = condition ~cond:OtherTest cx test in
      let then_ast = statement cx consequent in
      let else_ast =
        Base.Option.map alternate ~f:(fun (loc, { If.Alternate.body; comments }) ->
            (loc, { If.Alternate.body = statement cx body; comments })
        )
      in
      (loc, If { If.test = test_ast; consequent = then_ast; alternate = else_ast; comments })
    | (loc, Labeled { Labeled.label; body; comments }) ->
      (loc, Labeled { Labeled.label; body = statement cx body; comments })
    | (loc, Break { Break.label; comments }) -> (loc, Break { Break.label; comments })
    | (loc, Continue { Continue.label; comments }) -> (loc, Continue { Continue.label; comments })
    | (loc, With _) as s ->
      Flow.add_output
        cx
        (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.WithStatement));
      Tast_utils.error_mapper#statement s
    | (loc, DeclareTypeAlias alias) ->
      let (_, type_alias_ast) = type_alias cx loc alias in
      (loc, DeclareTypeAlias type_alias_ast)
    | (loc, TypeAlias alias) ->
      let (_, type_alias_ast) = type_alias cx loc alias in
      (loc, TypeAlias type_alias_ast)
    | (loc, DeclareOpaqueType otype) ->
      let (_, opaque_type_ast) = opaque_type cx loc otype in
      (loc, DeclareOpaqueType opaque_type_ast)
    | (loc, OpaqueType otype) ->
      let (_, opaque_type_ast) = opaque_type cx loc otype in
      (loc, OpaqueType opaque_type_ast)
    | (switch_loc, Switch { Switch.discriminant; cases; comments; exhaustive_out }) ->
      let discriminant_ast = expression cx discriminant in
      let exhaustive_check_incomplete_out =
        Tvar.mk cx (mk_reason (RCustom "exhaustive check incomplete out") switch_loc)
      in
      let (cases_ast_rev, has_default) =
        cases
        |> Base.List.fold_left
             ~init:([], false)
             ~f:(fun (cases_ast, has_default) (loc, { Switch.Case.test; consequent; comments }) ->
               let test_ast =
                 match test with
                 | None -> None
                 | Some expr ->
                   let open Ast.Expression in
                   let fake_discriminant =
                     match discriminant with
                     | (mem_loc, Member ({ Member._object = (_, x) as _object; _ } as mem))
                       when Base.Option.is_some (Refinement.key ~allow_optional:true _object) ->
                       (mem_loc, Member { mem with Member._object = (loc, x) })
                     | _ -> discriminant
                   in
                   let fake =
                     ( loc,
                       Binary
                         {
                           Binary.operator = Binary.StrictEqual;
                           left = fake_discriminant;
                           right = expr;
                           comments = None;
                         }
                     )
                   in
                   let (_, fake_ast) =
                     condition
                       cx
                       ~cond:
                         (SwitchTest
                            { case_test_loc = fst expr; switch_discriminant_loc = fst discriminant }
                         )
                       fake
                   in
                   let expr_ast =
                     match fake_ast with
                     | Ast.Expression.(Binary { Binary.right; _ }) -> right
                     | _ -> assert false
                   in
                   Some expr_ast
               in
               let consequent_ast = statement_list cx consequent in
               ( (loc, { Switch.Case.test = test_ast; consequent = consequent_ast; comments })
                 :: cases_ast,
                 has_default || Base.Option.is_none test
               )
           )
      in
      let cases_ast = List.rev cases_ast_rev in

      let enum_exhaustive_check = enum_exhaustive_check_of_switch_cases cases_ast in
      let ((_, discriminant_t), _) = discriminant_ast in
      let discriminant_after_check =
        if not has_default then
          let refinement_key = Refinement.key ~allow_optional:true discriminant in
          Type_env.discriminant_after_negated_cases cx switch_loc refinement_key
        else
          None
      in
      Flow.flow
        cx
        ( discriminant_t,
          EnumExhaustiveCheckT
            {
              reason = reason_of_t discriminant_t;
              check = enum_exhaustive_check;
              incomplete_out = exhaustive_check_incomplete_out;
              discriminant_after_check;
            }
        );
      (* We need to fully resolve all types attached to AST,
         because the post inference pass might inspect them. *)
      Tvar_resolver.resolve cx exhaustive_check_incomplete_out;
      ( switch_loc,
        Switch
          {
            Switch.discriminant = discriminant_ast;
            cases = cases_ast;
            comments;
            exhaustive_out = (exhaustive_out, exhaustive_check_incomplete_out);
          }
      )
    | (loc, Return { Return.argument; comments; return_out }) ->
      let (t, argument_ast) =
        match argument with
        | None -> (VoidT.at loc, None)
        | Some expr ->
          let (((_, t), _) as ast) = expression cx expr in
          (t, Some ast)
      in
      (loc, Return { Return.argument = argument_ast; comments; return_out = (return_out, t) })
    | (loc, Throw { Throw.argument; comments }) ->
      (loc, Throw { Throw.argument = expression cx argument; comments })
    | (loc, Try { Try.block = (b_loc, b); handler; finalizer; comments }) ->
      let try_block_ast = statement_list cx b.Block.body in
      let catch_ast = Base.Option.map handler ~f:(fun (h_loc, h) -> (h_loc, catch_clause cx h)) in
      let finally_ast =
        Base.Option.map finalizer ~f:(fun (f_loc, { Block.body; comments }) ->
            (f_loc, { Block.body = statement_list cx body; comments })
        )
      in
      ( loc,
        Try
          {
            Try.block = (b_loc, { Block.body = try_block_ast; comments = b.Block.comments });
            handler = catch_ast;
            finalizer = finally_ast;
            comments;
          }
      )
    | (loc, While { While.test; body; comments }) ->
      let test_ast = condition ~cond:OtherTest cx test in
      let body_ast = statement cx body in
      (loc, While { While.test = test_ast; body = body_ast; comments })
    | (loc, DoWhile { DoWhile.body; test; comments }) ->
      let body_ast = statement cx body in
      let test_ast = condition ~cond:OtherTest cx test in
      (loc, DoWhile { DoWhile.body = body_ast; test = test_ast; comments })
    | (loc, For { For.init; test; update; body; comments }) ->
      let init_ast =
        match init with
        | None -> None
        | Some (For.InitDeclaration (decl_loc, decl)) ->
          Some (For.InitDeclaration (decl_loc, variables cx decl))
        | Some (For.InitExpression expr) -> Some (For.InitExpression (expression cx expr))
      in

      let test_ast =
        match test with
        | None -> None
        | Some expr ->
          let expr_ast = condition ~cond:OtherTest cx expr in
          Some expr_ast
      in
      let body_ast = statement cx body in
      let update_ast = Base.Option.map ~f:(expression cx) update in
      ( loc,
        For { For.init = init_ast; test = test_ast; update = update_ast; body = body_ast; comments }
      )
    | (loc, ForIn { ForIn.left; right; body; each; comments }) ->
      let reason = mk_reason (RCustom "for-in") loc in
      let eval_right () =
        let (((_, _), _) as right_ast) = condition ~cond:OtherTest cx right in
        right_ast
      in
      let (left_ast, right_ast) =
        match left with
        | ForIn.LeftDeclaration
            ( decl_loc,
              {
                VariableDeclaration.kind;
                declarations = [(vdecl_loc, { VariableDeclaration.Declarator.id; init = None })];
                comments;
              }
            ) ->
          let right_ast = eval_right () in
          let (id_ast, _) = variable cx kind id None ~if_uninitialized:StrT.at in
          ( ForIn.LeftDeclaration
              ( decl_loc,
                {
                  VariableDeclaration.kind;
                  declarations =
                    [(vdecl_loc, { VariableDeclaration.Declarator.id = id_ast; init = None })];
                  comments;
                }
              ),
            right_ast
          )
        | ForIn.LeftPattern
            ( pat_loc,
              Ast.Pattern.Identifier
                {
                  Ast.Pattern.Identifier.name =
                    (name_loc, ({ Ast.Identifier.name = name_str; comments = _ } as id));
                  optional;
                  annot;
                }
            ) ->
          let right_ast = eval_right () in
          let t = StrT.at pat_loc in
          let use_op =
            Op
              (AssignVar
                 {
                   var = Some (mk_reason (RIdentifier (OrdinaryName name_str)) pat_loc);
                   init = reason_of_t t;
                 }
              )
          in
          Type_env.set_var cx ~use_op name_str t pat_loc;
          ( ForIn.LeftPattern
              ( (pat_loc, t),
                Ast.Pattern.Identifier
                  {
                    Ast.Pattern.Identifier.name = ((name_loc, t), id);
                    annot =
                      (match annot with
                      | Ast.Type.Available _ ->
                        Tast_utils.unchecked_mapper#type_annotation_hint annot
                      | Ast.Type.Missing loc -> Ast.Type.Missing (loc, t));
                    optional;
                  }
              ),
            right_ast
          )
        | _ ->
          let right_ast = eval_right () in
          Flow.add_output cx Error_message.(EInternal (loc, ForInLHS));
          (Tast_utils.error_mapper#for_in_statement_lhs left, right_ast)
      in
      let ((_, right_t), _) = right_ast in
      Flow.flow cx (right_t, AssertForInRHST reason);

      let body_ast = statement cx body in
      (loc, ForIn { ForIn.left = left_ast; right = right_ast; body = body_ast; each; comments })
    | (loc, ForOf { ForOf.left; right; body; await; comments }) ->
      let reason_desc =
        match left with
        | ForOf.LeftDeclaration
            ( _,
              {
                VariableDeclaration.declarations =
                  [
                    ( _,
                      {
                        VariableDeclaration.Declarator.id =
                          ( _,
                            Ast.Pattern.Identifier
                              {
                                Ast.Pattern.Identifier.name =
                                  (_, { Ast.Identifier.name; comments = _ });
                                _;
                              }
                          );
                        _;
                      }
                    );
                  ];
                _;
              }
            ) ->
          RIdentifier (OrdinaryName name)
        | ForOf.LeftPattern
            ( _,
              Ast.Pattern.Identifier
                { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; comments = _ }); _ }
            ) ->
          RIdentifier (OrdinaryName name)
        | _ -> RCustom "for-of element"
      in
      let reason = mk_reason reason_desc loc in
      let eval_right () =
        let (((_, t), _) as right_ast) = condition ~cond:OtherTest cx right in
        let elem_t = for_of_elemt cx t reason await in

        (* null/undefined are NOT allowed *)
        (Flow.reposition cx (loc_of_t t) elem_t, right_ast)
      in
      let (left_ast, right_ast) =
        match left with
        | ForOf.LeftDeclaration
            ( decl_loc,
              {
                VariableDeclaration.kind;
                declarations = [(vdecl_loc, { VariableDeclaration.Declarator.id; init = None })];
                comments;
              }
            ) ->
          let (elem_t, right_ast) = eval_right () in
          let (id_ast, _) = variable cx kind id None ~if_uninitialized:(fun _ -> elem_t) in
          ( ForOf.LeftDeclaration
              ( decl_loc,
                {
                  VariableDeclaration.kind;
                  declarations =
                    [(vdecl_loc, { VariableDeclaration.Declarator.id = id_ast; init = None })];
                  comments;
                }
              ),
            right_ast
          )
        | ForOf.LeftPattern
            ( pat_loc,
              Ast.Pattern.Identifier
                {
                  Ast.Pattern.Identifier.name =
                    (name_loc, ({ Ast.Identifier.name = name_str; comments = _ } as id));
                  optional;
                  annot;
                }
            ) ->
          let (elem_t, right_ast) = eval_right () in
          let use_op =
            Op
              (AssignVar
                 {
                   var = Some (mk_reason (RIdentifier (OrdinaryName name_str)) pat_loc);
                   init = reason_of_t elem_t;
                 }
              )
          in
          Type_env.set_var cx ~use_op name_str elem_t pat_loc;
          ( ForOf.LeftPattern
              ( (pat_loc, elem_t),
                Ast.Pattern.Identifier
                  {
                    Ast.Pattern.Identifier.name = ((name_loc, elem_t), id);
                    annot =
                      (match annot with
                      | Ast.Type.Available annot ->
                        Ast.Type.Available (Tast_utils.error_mapper#type_annotation annot)
                      | Ast.Type.Missing loc -> Ast.Type.Missing (loc, elem_t));
                    optional;
                  }
              ),
            right_ast
          )
        | _ ->
          let (_, right_ast) = eval_right () in
          Flow.add_output cx Error_message.(EInternal (loc, ForOfLHS));
          (Tast_utils.error_mapper#for_of_statement_lhs left, right_ast)
      in
      let body_ast = statement cx body in
      (loc, ForOf { ForOf.left = left_ast; right = right_ast; body = body_ast; await; comments })
    | (_, Debugger _) as stmt -> stmt
    | (loc, FunctionDeclaration func) ->
      let (_, _, node) = function_ ~is_declared_function:false loc func in
      node
    | (loc, ComponentDeclaration component) when Context.component_syntax cx ->
      error_on_this_uses_in_components cx component;
      let { ComponentDeclaration.id = (name_loc, { Ast.Identifier.name; _ }); _ } = component in
      if name <> String.capitalize_ascii name then
        Flow_js_utils.add_output cx Error_message.(EComponentCase name_loc);
      let reason = mk_reason (RComponent (OrdinaryName name)) loc in
      let (component_sig, reconstruct_component) =
        mk_component_sig cx Subst_name.Map.empty reason component
      in
      let general = Type_env.read_declared_type cx reason name_loc in
      let (params_ast, body_ast) = Component_declaration_sig.toplevels cx component_sig in
      (loc, ComponentDeclaration (reconstruct_component params_ast body_ast general))
    | (loc, ComponentDeclaration comp) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.ComponentSyntax));
      (loc, ComponentDeclaration (Tast_utils.error_mapper#component_declaration comp))
    | (loc, EnumDeclaration enum) ->
      let enum_ast = enum_declaration cx loc enum in
      (loc, EnumDeclaration enum_ast)
    | (loc, DeclareVariable decl) ->
      let decl_ast = declare_variable cx decl in
      (loc, DeclareVariable decl_ast)
    | (loc, DeclareFunction decl) ->
      let decl_ast = declare_function cx loc decl in
      (loc, DeclareFunction decl_ast)
    | (loc, VariableDeclaration decl) -> (loc, VariableDeclaration (variables cx decl))
    | (_, ClassDeclaration { Ast.Class.id = None; _ }) ->
      failwith "unexpected anonymous class declaration"
    | (class_loc, ClassDeclaration ({ Ast.Class.id = Some id; _ } as c)) ->
      let (name_loc, { Ast.Identifier.name; comments = _ }) = id in
      let name = OrdinaryName name in
      let reason = DescFormat.instance_reason name name_loc in
      let tast_class_type = Type_env.read_declared_type cx reason name_loc in
      (* ClassDeclarations are statements, so we will never have an annotation to push down here *)
      let (class_t, c_ast) = mk_class cx class_loc ~name_loc ~tast_class_type reason c in
      let use_op =
        Op
          (AssignVar
             { var = Some (mk_reason (RIdentifier name) name_loc); init = reason_of_t class_t }
          )
      in
      Type_env.init_implicit_let cx ~use_op class_t name_loc;
      (class_loc, ClassDeclaration c_ast)
    | ( loc,
        DeclareClass
          ( { Ast.Statement.DeclareClass.id = (name_loc, { Ast.Identifier.name; comments = _ }); _ }
          as decl
          )
      ) ->
      let (t, decl_ast) = declare_class cx loc decl in
      let use_op =
        Op
          (AssignVar
             { var = Some (mk_reason (RIdentifier (OrdinaryName name)) loc); init = reason_of_t t }
          )
      in
      Type_env.init_var cx ~use_op t name_loc;
      (loc, DeclareClass decl_ast)
    | ( loc,
        DeclareComponent
          ( {
              Ast.Statement.DeclareComponent.id = (name_loc, { Ast.Identifier.name; comments = _ });
              _;
            } as decl
          )
      ) ->
      if name <> String.capitalize_ascii name then
        Flow_js_utils.add_output cx Error_message.(EComponentCase name_loc);
      let (t, decl_ast) = declare_component cx loc decl in
      let use_op =
        Op
          (AssignVar
             { var = Some (mk_reason (RIdentifier (OrdinaryName name)) loc); init = reason_of_t t }
          )
      in
      Type_env.init_var cx ~use_op t name_loc;
      (loc, DeclareComponent decl_ast)
    | (loc, DeclareEnum enum) ->
      let decl_ast = enum_declaration cx loc enum in
      (loc, DeclareEnum decl_ast)
    | (loc, DeclareInterface decl) ->
      let (_, decl_ast) = interface cx loc decl in
      (loc, DeclareInterface decl_ast)
    | (loc, InterfaceDeclaration decl) ->
      let (_, decl_ast) = interface cx loc decl in
      (loc, InterfaceDeclaration decl_ast)
    | (loc, DeclareModule module_) ->
      let (_, decl_ast) = declare_module cx module_ in
      (loc, DeclareModule decl_ast)
    | (loc, DeclareNamespace namespace) ->
      let (_, decl_ast) = declare_namespace cx loc namespace in
      (loc, DeclareNamespace decl_ast)
    | (loc, DeclareExportDeclaration decl) ->
      let module D = DeclareExportDeclaration in
      let { D.default = _; declaration; specifiers; source; comments = _ } = decl in
      let declaration =
        let f = function
          | D.Variable (loc, v) ->
            let v_ast = declare_variable cx v in
            D.Variable (loc, v_ast)
          | D.Function (loc, f) ->
            let f_ast = declare_function cx loc f in
            D.Function (loc, f_ast)
          | D.Class (loc, c) ->
            let (_, c_ast) = declare_class cx loc c in
            D.Class (loc, c_ast)
          | D.Component (loc, c) ->
            let (_, c_ast) = declare_component cx loc c in
            D.Component (loc, c_ast)
          | D.DefaultType (loc, t) ->
            let t_ast = Anno.convert cx Subst_name.Map.empty (loc, t) in
            D.DefaultType t_ast
          | D.NamedType (loc, t) ->
            let (_, type_alias_ast) = type_alias cx loc t in
            if Type_env.in_toplevel_scope cx then
              Flow_js_utils.add_output cx (Error_message.EUnnecessaryDeclareTypeOnlyExport loc);
            D.NamedType (loc, type_alias_ast)
          | D.NamedOpaqueType (loc, t) ->
            let (_, opaque_type_ast) = opaque_type cx loc t in
            D.NamedOpaqueType (loc, opaque_type_ast)
          | D.Interface (loc, i) ->
            let (_, i_ast) = interface cx loc i in
            if Type_env.in_toplevel_scope cx then
              Flow_js_utils.add_output cx (Error_message.EUnnecessaryDeclareTypeOnlyExport loc);
            D.Interface (loc, i_ast)
          | D.Enum (loc, enum) ->
            let enum_ast = enum_declaration cx loc enum in
            D.Enum (loc, enum_ast)
        in
        Option.map f declaration
      in
      let source =
        match source with
        | None -> None
        | Some (source_loc, ({ Ast.StringLiteral.value = module_name; _ } as source_literal)) ->
          let source_module_t =
            Import_export.get_module_t
              cx
              ~import_kind_for_untyped_import_validation:(Some ImportValue)
              (source_loc, module_name)
          in
          Some ((source_loc, source_module_t), source_literal)
      in
      let specifiers =
        let export_kind = Ast.Statement.ExportValue in
        Option.map (export_specifiers cx source export_kind) specifiers
      in
      (loc, DeclareExportDeclaration { decl with D.declaration; specifiers; source })
    | (loc, DeclareModuleExports { Ast.Statement.DeclareModuleExports.annot = (t_loc, t); comments })
      ->
      let (((_, t), _) as t_ast) = Anno.convert cx Subst_name.Map.empty t in
      let reason =
        let filename = Context.file cx in
        mk_reason
          (RModule (File_key.to_string filename))
          (Loc.{ none with source = Some filename } |> ALoc.of_loc)
      in
      Flow.flow_t cx (t, Type.Unsoundness.exports_any reason);
      ( loc,
        DeclareModuleExports { Ast.Statement.DeclareModuleExports.annot = (t_loc, t_ast); comments }
      )
    | ( loc,
        ExportNamedDeclaration
          ( { ExportNamedDeclaration.declaration; specifiers; source; export_kind; comments = _ } as
          export_decl
          )
      ) ->
      let declaration =
        match declaration with
        | None -> None
        | Some (loc, stmt) ->
          let stmt' = statement cx (loc, stmt) in
          Some stmt'
      in
      let source =
        match source with
        | None -> None
        | Some (source_loc, ({ Ast.StringLiteral.value = module_name; _ } as source_literal)) ->
          let (perform_platform_validation, import_kind_for_untyped_import_validation) =
            match export_kind with
            | Ast.Statement.ExportType -> (false, Some ImportType)
            | Ast.Statement.ExportValue -> (true, Some ImportValue)
          in
          let source_module_t =
            Import_export.get_module_t
              cx
              ~import_kind_for_untyped_import_validation
              (source_loc, module_name)
              ~perform_platform_validation
          in
          Some ((source_loc, source_module_t), source_literal)
      in
      let specifiers = Option.map (export_specifiers cx source export_kind) specifiers in
      ( loc,
        ExportNamedDeclaration
          { export_decl with ExportNamedDeclaration.declaration; specifiers; source }
      )
    | (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration; comments }) ->
      let module D = ExportDefaultDeclaration in
      let (t, declaration) =
        match declaration with
        | D.Declaration (loc, stmt) ->
          let (t, stmt) =
            match stmt with
            | FunctionDeclaration ({ Ast.Function.id = None; _ } as fn) ->
              let { Ast.Function.sig_loc; async; generator; _ } = fn in
              let reason = func_reason ~async ~generator sig_loc in
              let (t, fn) = mk_function_declaration cx reason loc fn in
              (t, (loc, FunctionDeclaration fn))
            | ClassDeclaration ({ Ast.Class.id = None; _ } as c) ->
              let reason = DescFormat.instance_reason (internal_name "*default*") loc in
              let (t, c) = mk_class cx loc ~name_loc:loc reason c in
              (t, (loc, ClassDeclaration c))
            | FunctionDeclaration { Ast.Function.id = Some id; _ }
            | ClassDeclaration { Ast.Class.id = Some id; _ }
            | EnumDeclaration { EnumDeclaration.id; _ }
            | ComponentDeclaration { ComponentDeclaration.id; _ } ->
              let stmt = statement cx (loc, stmt) in
              let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
              let t =
                Type_env.get_var_declared_type ~lookup_mode:ForValue cx (OrdinaryName name) id_loc
              in
              (t, stmt)
            | _ -> failwith "unexpected default export declaration"
          in
          (t, D.Declaration stmt)
        | D.Expression expr ->
          let (((_, t), _) as expr) = expression cx expr in
          (t, D.Expression expr)
      in
      let default = (default, t) in
      (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration; comments })
    | (import_loc, ImportDeclaration import_decl) ->
      let { ImportDeclaration.source; specifiers; default; import_kind; comments } = import_decl in
      let (source_loc, ({ Ast.StringLiteral.value = module_name; _ } as source_literal)) = source in
      let (perform_platform_validation, import_kind_for_untyped_import_validation) =
        match import_kind with
        | ImportDeclaration.ImportType -> (false, Some ImportType)
        | ImportDeclaration.ImportTypeof -> (false, Some ImportTypeof)
        | ImportDeclaration.ImportValue -> (true, Some ImportValue)
      in
      let source_module_t =
        Import_export.get_module_t
          cx
          ~import_kind_for_untyped_import_validation
          (source_loc, module_name)
          ~perform_platform_validation
      in
      let source_ast = ((source_loc, source_module_t), source_literal) in

      let specifiers_ast =
        match specifiers with
        | Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers) ->
          let named_specifiers_ast =
            named_specifiers
            |> Base.List.map ~f:(function
                   | {
                       Ast.Statement.ImportDeclaration.local;
                       remote;
                       remote_name_def_loc = _;
                       kind;
                     }
                   ->
                   let ( remote_name_loc,
                         ({ Ast.Identifier.name = remote_name; comments = _ } as rmt)
                       ) =
                     remote
                   in
                   let (_, { Ast.Identifier.name = local_name; comments = _ }) =
                     Base.Option.value ~default:remote local
                   in
                   let import_reason =
                     mk_reason (RNamedImportedType (module_name, local_name)) (fst remote)
                   in
                   let import_kind = Base.Option.value ~default:import_kind kind in
                   let (remote_name_def_loc, imported_t) =
                     Import_export.import_named_specifier_type
                       cx
                       import_reason
                       import_kind
                       ~module_name
                       ~source_module_t
                       ~remote_name
                       ~local_name
                   in
                   let remote_ast = ((remote_name_loc, imported_t), rmt) in
                   let local_ast =
                     Base.Option.map local ~f:(fun (local_loc, local_id) ->
                         let { Ast.Identifier.name = local_name; comments } = local_id in
                         ((local_loc, imported_t), mk_ident ~comments local_name)
                     )
                   in
                   {
                     Ast.Statement.ImportDeclaration.local = local_ast;
                     remote = remote_ast;
                     remote_name_def_loc;
                     kind;
                   }
                   )
          in
          Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers_ast)
        | Some
            (ImportDeclaration.ImportNamespaceSpecifier
              ( loc_with_star,
                (local_loc, ({ Flow_ast.Identifier.name = local_name; _ } as local_id))
              )
              ) ->
          let namespace_specifier_ast =
            let import_reason =
              let import_reason_desc =
                match import_kind with
                | ImportDeclaration.ImportType -> RImportStarType local_name
                | ImportDeclaration.ImportTypeof -> RImportStarTypeOf local_name
                | ImportDeclaration.ImportValue -> RImportStar local_name
              in
              mk_reason import_reason_desc local_loc
            in
            let t =
              Import_export.import_namespace_specifier_type
                cx
                import_reason
                import_kind
                ~module_name
                ~namespace_symbol:(mk_namespace_symbol ~name:local_name ~def_loc:local_loc)
                ~source_module_t
                ~local_loc
            in
            ((local_loc, t), local_id)
          in
          Some (ImportDeclaration.ImportNamespaceSpecifier (loc_with_star, namespace_specifier_ast))
        | None -> None
      in
      let default_ast =
        match default with
        | Some
            {
              ImportDeclaration.identifier =
                (loc, ({ Ast.Identifier.name = local_name; comments = _ } as id));
              remote_default_name_def_loc = _;
            } ->
          let import_reason = mk_reason (RDefaultImportedType (local_name, module_name)) loc in
          let (remote_default_name_def_loc, imported_t) =
            Import_export.import_default_specifier_type
              cx
              import_reason
              import_kind
              ~module_name
              ~source_module_t
              ~local_name
          in
          Some
            { ImportDeclaration.identifier = ((loc, imported_t), id); remote_default_name_def_loc }
        | None -> None
      in

      ( import_loc,
        ImportDeclaration
          {
            ImportDeclaration.source = source_ast;
            specifiers = specifiers_ast;
            default = default_ast;
            import_kind;
            comments;
          }
      )

  and statement_list cx stmts =
    Base.List.map stmts ~f:(fun stmt ->
        let (stmt, _) = Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx stmt) in
        stmt
    )

  and for_of_elemt cx right_t reason await =
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun elem_t ->
        let loc = loc_of_reason reason in
        (* Second and third args here are never relevant to the loop, but they should be as
           general as possible to allow iterating over arbitrary generators *)
        let targs = [elem_t; MixedT.why reason; EmptyT.why reason] in
        let (async, iterable_reason) =
          if await then
            (true, mk_reason (RCustom "async iteration expected on AsyncIterable") loc)
          else
            (false, mk_reason (RCustom "iteration expected on Iterable") loc)
        in
        Flow.flow
          cx
          (right_t, AssertIterableT { use_op = unknown_use; reason = iterable_reason; async; targs })
    )

  and type_alias
      cx
      loc
      {
        Ast.Statement.TypeAlias.id = (name_loc, ({ Ast.Identifier.name; comments = _ } as id));
        tparams;
        right;
        comments;
      } =
    let cache = Context.node_cache cx in
    match Node_cache.get_alias cache loc with
    | Some info ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Alias cache hit at %s" (ALoc.debug_to_string loc)]);
      info
    | None ->
      let r = DescFormat.type_reason (OrdinaryName name) name_loc in
      let (tparams, tparams_map, tparams_ast) = Anno.mk_type_param_declarations cx tparams in
      let (((_, t), _) as right_ast) = Anno.convert cx tparams_map right in
      let t =
        mod_reason_of_t (update_desc_reason (fun desc -> RTypeAlias (name, Some name_loc, desc))) t
      in
      let type_ =
        poly_type_of_tparams (Type.Poly.generate_id ()) tparams (DefT (r, TypeT (TypeAliasKind, t)))
      in
      begin
        match tparams with
        | None -> ()
        | Some (_, tps) ->
          (* TODO: use tparams_map *)
          let tparams =
            Nel.fold_left (fun acc tp -> Subst_name.Map.add tp.name tp acc) Subst_name.Map.empty tps
          in
          Context.add_post_inference_polarity_check cx tparams Polarity.Positive t
      end;

      let type_alias_ast =
        {
          Ast.Statement.TypeAlias.id = ((name_loc, type_), id);
          tparams = tparams_ast;
          right = right_ast;
          comments;
        }
      in
      (type_, type_alias_ast)

  and interface cx loc decl =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_interface node_cache loc with
    | Some node ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Interface cache hit at %s" (ALoc.debug_to_string loc)]);
      node
    | None ->
      let { Ast.Statement.Interface.id = (name_loc, { Ast.Identifier.name; comments = _ }); _ } =
        decl
      in
      let desc = RType (OrdinaryName name) in
      let reason = mk_reason desc name_loc in
      let (t, iface_sig, decl_ast) = Anno.mk_interface_sig cx loc reason decl in
      Class_type_sig.check_signature_compatibility cx (mk_reason desc loc) iface_sig;
      (t, decl_ast)

  and declare_variable cx decl =
    let { Ast.Statement.DeclareVariable.id = (id_loc, id); annot; kind; comments } = decl in
    let (t, annot_ast) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
    { Ast.Statement.DeclareVariable.id = ((id_loc, t), id); annot = annot_ast; kind; comments }

  and declare_class cx loc decl =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_declared_class node_cache loc with
    | Some node ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Declared class cache hit at %s" (ALoc.debug_to_string loc)]);
      node
    | None ->
      let { Ast.Statement.DeclareClass.id = (name_loc, { Ast.Identifier.name; comments = _ }); _ } =
        decl
      in
      let desc = RType (OrdinaryName name) in
      let reason = mk_reason desc name_loc in
      let (t, class_sig, decl_ast) = Anno.mk_declare_class_sig cx loc name reason decl in
      Class_type_sig.check_signature_compatibility cx (mk_reason desc loc) class_sig;
      (t, decl_ast)

  and declare_component cx loc decl =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_declared_component node_cache loc with
    | Some node ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Declared component cache hit at %s" (ALoc.debug_to_string loc)]);
      node
    | None ->
      let (t, decl_ast) = Anno.mk_declare_component_sig cx loc decl in
      (t, decl_ast)

  and declare_module cx { Ast.Statement.DeclareModule.id; body; comments } =
    let open Ast.Statement in
    let (id_loc, name) =
      match id with
      | DeclareModule.Identifier (id_loc, { Ast.Identifier.name = value; comments = _ })
      | DeclareModule.Literal (id_loc, { Ast.StringLiteral.value; _ }) ->
        (id_loc, value)
    in
    if not (File_key.is_lib_file (Context.file cx) && Type_env.in_global_scope cx) then
      Flow_js_utils.add_output
        cx
        (Error_message.EUnsupportedSyntax
           ( id_loc,
             Flow_intermediate_error_types.(
               ContextDependentUnsupportedStatement NonLibdefToplevelDeclareModule
             )
           )
        );
    let (body_loc, { Ast.Statement.Block.body = elements; comments = elements_comments }) = body in
    let prev_scope_kind = Type_env.set_scope_kind cx Name_def.DeclareModule in

    let elements_ast = statement_list cx elements in
    Base.List.iter elements_ast ~f:(fun (loc, stmt) ->
        match
          Flow_ast_utils.acceptable_statement_in_declaration_context
            ~in_declare_namespace:false
            stmt
        with
        | Ok () -> ()
        | Error kind ->
          Flow_js_utils.add_output
            cx
            (Error_message.EUnsupportedSyntax
               ( loc,
                 Flow_intermediate_error_types.(
                   ContextDependentUnsupportedStatement (UnsupportedStatementInDeclareModule kind)
                 )
               )
            )
    );
    let reason = mk_reason (RModule name) id_loc in
    let module_t =
      ModuleT
        {
          module_reason = reason;
          module_export_types =
            {
              value_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
              type_exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
              cjs_export = None;
              has_every_named_export = false;
            };
          module_is_strict = Context.is_strict cx;
          module_available_platforms = Context.available_platforms cx;
        }
    in
    let ast =
      {
        DeclareModule.id =
          begin
            match id with
            | DeclareModule.Identifier (id_loc, id) ->
              DeclareModule.Identifier ((id_loc, module_t), id)
            | DeclareModule.Literal (id_loc, lit) -> DeclareModule.Literal ((id_loc, module_t), lit)
          end;
        body = (body_loc, { Block.body = elements_ast; comments = elements_comments });
        comments;
      }
    in
    ignore @@ Type_env.set_scope_kind cx prev_scope_kind;

    (module_t, ast)

  and declare_namespace
      cx
      loc
      {
        Ast.Statement.DeclareNamespace.id;
        body = (body_loc, { Ast.Statement.Block.body; comments = body_comments });
        comments;
      } =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_declared_namespace node_cache loc with
    | Some x -> x
    | None ->
      let prev_scope_kind = Type_env.set_scope_kind cx Name_def.DeclareModule in
      let body_statements = statement_list cx body in
      ignore @@ Type_env.set_scope_kind cx prev_scope_kind;
      let body =
        (body_loc, { Ast.Statement.Block.body = body_statements; comments = body_comments })
      in
      let (t, id) =
        let (name_loc, { Ast.Identifier.name; comments }) = id in
        let reason = mk_reason (RNamespace name) name_loc in
        let namespace_symbol = mk_namespace_symbol ~name ~def_loc:name_loc in
        let t =
          Module_info_analyzer.analyze_declare_namespace cx namespace_symbol reason body_statements
        in
        if not (File_key.is_lib_file (Context.file cx) || Context.namespaces cx) then
          Flow_js_utils.add_output
            cx
            (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.DeclareNamespace));
        (t, ((name_loc, t), { Ast.Identifier.name; comments }))
      in
      (t, { Ast.Statement.DeclareNamespace.id; body; comments })

  and object_prop cx ~as_const ~frozen acc prop =
    let open Ast.Expression.Object in
    match prop with
    (* named prop *)
    | Property
        ( prop_loc,
          Property.Init
            {
              key =
                ( Property.Identifier (loc, _)
                | Property.StringLiteral (loc, _)
                | Property.NumberLiteral (loc, _)
                | Property.BigIntLiteral (loc, _) ) as key;
              value = v;
              shorthand;
            }
        ) ->
      (match name_of_identifier_or_literal_key key with
      | Error err ->
        Flow.add_output cx err;
        (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
      | Ok name ->
        let (acc, key, value) =
          if Type_inference_hooks_js.dispatch_obj_prop_decl_hook cx name loc then
            let t = Unsoundness.at InferenceHooks loc in
            let key = translate_identifer_or_literal_key t key in
            (* don't add `name` to `acc` because `name` is the autocomplete token *)
            let acc = ObjectExpressionAcc.set_obj_key_autocomplete acc in
            let (((_, _t), _) as value) = expression cx ~as_const ~frozen v in
            (acc, key, value)
          else
            let (((_, t), _) as value) = expression cx ~as_const ~frozen v in
            let key = translate_identifer_or_literal_key t key in
            let acc =
              ObjectExpressionAcc.add_prop
                (Properties.add_field
                   (OrdinaryName name)
                   (Polarity.object_literal_polarity (as_const || frozen))
                   ~key_loc:(Some loc)
                   t
                )
                acc
            in
            (acc, key, value)
        in
        (acc, Property (prop_loc, Property.Init { key; value; shorthand })))
    (* named method *)
    | Property
        ( prop_loc,
          Property.Method
            {
              key =
                ( Property.Identifier (loc, _)
                | Property.StringLiteral (loc, _)
                | Property.NumberLiteral (loc, _)
                | Property.BigIntLiteral (loc, _) ) as key;
              value = (fn_loc, func);
            }
        ) ->
      (match name_of_identifier_or_literal_key key with
      | Error err ->
        Flow.add_output cx err;
        (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
      | Ok name ->
        let reason = func_reason ~async:false ~generator:false prop_loc in
        let (t, func) = mk_function_expression cx ~needs_this_param:false reason fn_loc func in
        ( ObjectExpressionAcc.add_prop (Properties.add_method (OrdinaryName name) (Some loc) t) acc,
          Property
            ( prop_loc,
              Property.Method
                { key = translate_identifer_or_literal_key t key; value = (fn_loc, func) }
            )
        ))
    (* We enable some unsafe support for getters and setters. The main unsafe bit
     *  is that we don't properly havok refinements when getter and setter methods
     *  are called. *)
    (* unsafe getter property *)
    | Property
        ( loc,
          Property.Get
            {
              key =
                ( Property.Identifier (id_loc, _)
                | Property.StringLiteral (id_loc, _)
                | Property.NumberLiteral (id_loc, _)
                | Property.BigIntLiteral (id_loc, _) ) as key;
              value = (vloc, func);
              comments;
            }
        ) ->
      Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
      (match name_of_identifier_or_literal_key key with
      | Error err ->
        Flow.add_output cx err;
        (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
      | Ok name ->
        let reason = func_reason ~async:false ~generator:false vloc in
        let (function_type, func) =
          mk_function_expression cx ~needs_this_param:false reason vloc func
        in
        let return_t = Type.extract_getter_type function_type in
        ( ObjectExpressionAcc.add_prop
            (Properties.add_getter (OrdinaryName name) (Some id_loc) return_t)
            acc,
          Property
            ( loc,
              Property.Get
                {
                  key = translate_identifer_or_literal_key return_t key;
                  value = (vloc, func);
                  comments;
                }
            )
        ))
    (* unsafe setter property *)
    | Property
        ( loc,
          Property.Set
            {
              key =
                ( Property.Identifier (id_loc, _)
                | Property.StringLiteral (id_loc, _)
                | Property.NumberLiteral (id_loc, _)
                | Property.BigIntLiteral (id_loc, _) ) as key;
              value = (vloc, func);
              comments;
            }
        ) ->
      Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
      (match name_of_identifier_or_literal_key key with
      | Error err ->
        Flow.add_output cx err;
        (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
      | Ok name ->
        let reason = func_reason ~async:false ~generator:false vloc in
        let (function_type, func) =
          mk_function_expression cx ~needs_this_param:false reason vloc func
        in
        let param_t = Type.extract_setter_type function_type in
        ( ObjectExpressionAcc.add_prop
            (Properties.add_setter (OrdinaryName name) (Some id_loc) param_t)
            acc,
          Property
            ( loc,
              Property.Set
                {
                  key = translate_identifer_or_literal_key param_t key;
                  value = (vloc, func);
                  comments;
                }
            )
        ))
    (* computed getters and setters aren't supported yet regardless of the
       `enable_getters_and_setters` config option *)
    | Property (loc, Property.Get { key = Property.Computed _; _ })
    | Property (loc, Property.Set { key = Property.Computed _; _ }) ->
      Flow.add_output
        cx
        (Error_message.EUnsupportedSyntax
           (loc, Flow_intermediate_error_types.ObjectPropertyComputedGetSet)
        );
      (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
    (* computed LHS silently ignored for now *)
    | Property (_, Property.Init { key = Property.Computed _; _ })
    | Property (_, Property.Method { key = Property.Computed _; _ }) ->
      (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
    (* spread prop *)
    | SpreadProperty _ -> (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
    | Property (_, Property.Init { key = Property.PrivateName _; _ })
    | Property (_, Property.Method { key = Property.PrivateName _; _ })
    | Property (_, Property.Get { key = Property.PrivateName _; _ })
    | Property (_, Property.Set { key = Property.PrivateName _; _ }) ->
      failwith "Internal Error: Non-private field with private name"

  and prop_map_of_object cx props =
    let (acc, rev_prop_asts) =
      List.fold_left
        (fun (map, rev_prop_asts) prop ->
          let (map, prop) = object_prop cx ~as_const:false ~frozen:false map prop in
          (map, prop :: rev_prop_asts))
        (ObjectExpressionAcc.empty (), [])
        props
    in
    (acc.ObjectExpressionAcc.obj_pmap, List.rev rev_prop_asts)

  and create_obj_with_computed_prop cx key_loc keys ~reason ~reason_key ~reason_obj ~as_const value
      =
    let single_key key =
      match Flow_js_utils.propref_for_elem_t key with
      | Computed elem_t ->
        let check =
          WriteComputedObjPropCheckT
            { reason; reason_key = Some reason_key; value_t = value; err_on_str_key = None }
        in
        Flow.flow cx (elem_t, check);
        (* No properties are added in this case. *)
        Obj_type.mk_frozen_exact_empty cx reason_obj
      | Named { name; reason; _ } ->
        let prop =
          Field
            {
              preferred_def_locs = None;
              key_loc = Some key_loc;
              type_ = value;
              polarity = Polarity.object_literal_polarity as_const;
            }
        in
        let props = NameUtils.Map.singleton name prop in
        let proto = NullT.make reason in
        Obj_type.mk_with_proto ~obj_kind:Exact cx reason_obj ~props proto
    in
    match keys with
    | [] -> DefT (reason_obj, EmptyT)
    | [key] -> single_key key
    | _ ->
      Flow_js_utils.add_output cx (Error_message.EComputedPropertyWithUnion reason);
      AnyT.error reason

  and object_ cx reason ~frozen ~as_const props =
    let open Ast.Expression.Object in
    (* Use the same reason for proto and the ObjT so we can walk the proto chain
       and use the root proto reason to build an error. *)
    let obj_proto = ObjProtoT reason in
    let mk_computed k key value =
      let (key_loc, _e) = k in
      let keys = Flow.possible_concrete_types_for_computed_props cx reason key in
      let reason = reason_of_t key in
      let reason_key = Reason.mk_expression_reason k in
      let reason_obj = reason in
      create_obj_with_computed_prop cx key_loc keys ~reason ~reason_key ~reason_obj ~as_const value
    in
    let (acc, rev_prop_asts) =
      List.fold_left
        (fun (acc, rev_prop_asts) -> function
          | SpreadProperty (prop_loc, { SpreadProperty.argument; comments }) ->
            let (((_, spread), _) as argument) = expression cx ~as_const argument in
            ( ObjectExpressionAcc.add_spread spread acc,
              SpreadProperty (prop_loc, { SpreadProperty.argument; comments }) :: rev_prop_asts
            )
          | Property
              ( prop_loc,
                Property.Init
                  {
                    key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k; comments });
                    value = v;
                    shorthand;
                  }
              ) ->
            let (((_, kt), _) as k') = expression cx k in
            let (((_, vt), _) as v') = expression cx ~as_const v in
            let computed = mk_computed k kt vt in
            ( ObjectExpressionAcc.add_spread computed acc,
              Property
                ( prop_loc,
                  Property.Init
                    {
                      key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k'; comments });
                      value = v';
                      shorthand;
                    }
                )
              :: rev_prop_asts
            )
          | Property
              ( prop_loc,
                Property.Method
                  {
                    key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k; comments });
                    value = (fn_loc, fn);
                  }
              ) ->
            let (((_, kt), _) as k') = expression cx k in
            let ((_, vt), v') = expression cx (fn_loc, Ast.Expression.Function fn) in
            let fn =
              match v' with
              | Ast.Expression.Function fn -> fn
              | _ -> assert false
            in
            let computed = mk_computed k kt vt in
            ( ObjectExpressionAcc.add_spread computed acc,
              Property
                ( prop_loc,
                  Property.Method
                    {
                      key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k'; comments });
                      value = (fn_loc, fn);
                    }
                )
              :: rev_prop_asts
            )
          | Property
              ( prop_loc,
                Property.Init
                  {
                    key =
                      ( Property.Identifier (_, { Ast.Identifier.name = "__proto__"; comments = _ })
                      | Property.StringLiteral (_, { Ast.StringLiteral.value = "__proto__"; _ }) )
                      as key;
                    value = v;
                    shorthand = false;
                  }
              ) ->
            let reason = mk_reason RPrototype (fst v) in
            let (((_, vt), _) as v) = expression cx ~as_const v in
            let t =
              Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
                  Flow.flow cx (vt, ObjTestProtoT (reason, t))
              )
            in
            ( ObjectExpressionAcc.add_proto t acc,
              Property
                ( prop_loc,
                  Property.Init
                    {
                      key = translate_identifer_or_literal_key vt key;
                      value = v;
                      shorthand = false;
                    }
                )
              :: rev_prop_asts
            )
          | prop ->
            let (acc, prop) = object_prop cx ~as_const ~frozen acc prop in
            (acc, prop :: rev_prop_asts))
        (ObjectExpressionAcc.empty (), [])
        props
    in
    let t =
      ObjectExpressionAcc.mk_object_from_spread_acc
        cx
        acc
        reason
        ~as_const
        ~frozen
        ~default_proto:obj_proto
    in
    (t, List.rev rev_prop_asts)

  and variable cx kind ?if_uninitialized id init =
    let init_var =
      match kind with
      | Ast.Variable.Const -> Type_env.init_const
      | Ast.Variable.Let -> Type_env.init_let
      | Ast.Variable.Var -> Type_env.init_var
    in
    let annot = Destructuring.type_of_pattern id in
    let has_anno =
      match annot with
      | Ast.Type.Missing _ -> false
      | Ast.Type.Available _ -> true
    in
    let id_reason =
      match id with
      | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
        let (id_loc, { Ast.Identifier.name; _ }) = name in
        mk_reason (RIdentifier (OrdinaryName name)) id_loc
      | (ploc, _) -> mk_reason RDestructuring ploc
    in
    (* Identifiers do not need to be initialized at the declaration site as long
     * as they are definitely initialized before use. Destructuring patterns must
     * be initialized, since their declaration involves some operation on the
     * right hand side, like a property access. *)
    let (init_opt, init_ast) =
      match (id, init, if_uninitialized) with
      | ((_, Ast.Pattern.Identifier _), None, None) -> (None, None)
      | (_, Some expr, _) ->
        let (((_, t), _) as init_ast) = expression cx expr in
        let r = mk_expression_reason expr in
        (Some (t, r), Some init_ast)
      | ((ploc, _), None, Some f) ->
        let t = f ploc in
        let r = reason_of_t t in
        (Some (t, r), None)
      | ((ploc, _), None, None) ->
        let t = VoidT.at ploc in
        let r = reason_of_t t in
        (Some (t, r), None)
    in

    let id_ast =
      match id with
      | (ploc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot = _; optional }) ->
        let (id_loc, { Ast.Identifier.name; comments }) = name in
        let (annot_t, annot_ast) =
          match annot with
          | Ast.Type.Missing loc ->
            (* When there is no annotation, we still need to populate `annot_t` when we can.
               In this case, we unify it with the type of the env entry binding, if there is one *)
            let t =
              if Base.Option.is_some init_ast || Base.Option.is_some if_uninitialized then
                Type_env.read_declared_type cx id_reason id_loc
              else
                EmptyT.at id_loc
            in
            (t, Ast.Type.Missing (loc, t))
          | Ast.Type.Available annot ->
            let (t, ast_annot) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
            (t, Ast.Type.Available ast_annot)
        in
        begin
          match init_opt with
          | Some (init_t, init_reason) ->
            let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
            init_var cx ~use_op init_t id_loc
          | None -> ()
        end;
        let ast_t = Type_env.constraining_type ~default:annot_t cx name id_loc in
        ( (ploc, ast_t),
          Ast.Pattern.Identifier
            {
              Ast.Pattern.Identifier.name = ((id_loc, ast_t), { Ast.Identifier.name; comments });
              annot = annot_ast;
              optional;
            }
        )
      | _ ->
        let annot_t =
          match annot with
          | Ast.Type.Missing _ ->
            (match init_opt with
            | Some (init_t, _) -> init_t
            | None -> EmptyT.why id_reason)
          | Ast.Type.Available annot ->
            let (annot_t, _) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
            Base.Option.iter init_opt ~f:(fun (init_t, init_reason) ->
                let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
                Flow.flow cx (init_t, UseT (use_op, annot_t))
            );
            annot_t
        in
        let init =
          Destructuring.empty
            ?init
            annot_t
            ~annot:
              (match annot with
              | Ast.Type.Missing _ -> false
              | Ast.Type.Available _ -> true)
        in
        Destructuring.pattern cx init id ~f:(fun ~use_op ~name_loc name default t ->
            let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in

            (* If this is a variable declaration without a type annotation
               constraining writes, we need the type of the identifier to be the
               general type of the variable in order to detect if a generic escapes
               into it.

               If there is an annotation, the specific and the general will be
               unified. *)
            let id_node_type =
              if has_anno then
                t
              else (
                init_var cx ~use_op t name_loc;
                Type_env.constraining_type
                  ~default:(Type_env.get_var_declared_type cx (OrdinaryName name) name_loc)
                  cx
                  name
                  name_loc
              )
            in
            Base.Option.iter default ~f:(fun d ->
                let default_t = Flow.mk_default cx reason d in
                Flow.flow cx (default_t, UseT (use_op, t))
            );
            id_node_type
        )
    in
    (id_ast, init_ast)

  and expression_or_spread cx =
    let open Ast.Expression in
    function
    | Expression e ->
      let (((_, t), _) as e') = expression cx e in
      (Arg t, Expression e')
    | Spread (loc, { SpreadElement.argument; comments }) ->
      let (((_, t), _) as e') = expression cx argument in
      (SpreadArg t, Spread (loc, { SpreadElement.argument = e'; comments }))

  and array_elements cx ~as_const undef_loc =
    let open Ast.Expression.Array in
    Base.Fn.compose
      List.split
      (Base.List.map ~f:(fun e ->
           match e with
           | Expression e ->
             let (((loc, t), _) as e) = expression cx ~as_const e in
             let reason = mk_reason RArrayElement loc in
             (UnresolvedArg (mk_tuple_element reason t, None), Expression e)
           | Hole loc ->
             let t = EmptyT.at undef_loc in
             let reason = mk_reason RArrayElement loc in
             (UnresolvedArg (mk_tuple_element reason t, None), Hole loc)
           | Spread (loc, { Ast.Expression.SpreadElement.argument; comments }) ->
             let (((_, t), _) as argument) = expression cx argument in
             ( UnresolvedSpreadArg t,
               Spread (loc, { Ast.Expression.SpreadElement.argument; comments })
             )
       )
      )

  and empty_array cx loc =
    let reason = mk_reason REmptyArrayLit loc in
    let element_reason = mk_reason REmptyArrayElement loc in
    let (has_hint, lazy_hint) = Type_env.get_hint cx loc in
    (* empty array, analogous to object with implicit properties *)
    let elemt =
      if not has_hint then
        EmptyT.make (mk_reason REmptyArrayElement loc)
      else
        match lazy_hint element_reason with
        | HintAvailable (hint, _) -> hint
        | DecompositionError ->
          (* A hint is available, but cannot be used to provide a type for this
           * array element. In this case, the element type of the array is likely
           * useless (does not escape the annotation), so we can use `empty` as
           * its type. *)
          EmptyT.make (mk_reason REmptyArrayElement loc)
        | NoHint
        | EncounteredPlaceholder ->
          (* If there is no hint then raise an error. The EncounteredPlaceholder case
           * corresponds to code like `const set = new Set([]);`. This case will
           * raise a [missing-empty-array-annot] error on `[]`. *)
          Flow.add_output cx (Error_message.EEmptyArrayNoProvider { loc });
          AnyT.at Untyped loc
    in
    (reason, elemt)

  (* can raise Abnormal.(Exn (_, _))
   * annot should become a Type.t option when we have the ability to
   * inspect annotations and recurse into them *)
  and expression ?cond ?(as_const = false) ?(frozen = false) cx (loc, e) =
    let log_slow_to_check ~f =
      match Context.slow_to_check_logging cx with
      | { Slow_to_check_logging.slow_expressions_logging_threshold = Some threshold; _ } ->
        let start_time = Unix.gettimeofday () in
        let result = f () in
        let end_time = Unix.gettimeofday () in
        let run_time = end_time -. start_time in
        if run_time > threshold then
          Hh_logger.info
            "[%d] Slow CHECK expression at %s (%f seconds)"
            (Sys_utils.get_pretty_pid ())
            (ALoc.debug_to_string ~include_source:true loc)
            run_time;
        result
      | _ -> f ()
    in
    let f () =
      let node_cache = Context.node_cache cx in
      let (((_, t), _) as res) =
        match Node_cache.get_expression node_cache loc with
        | Some node ->
          Debug_js.Verbose.print_if_verbose_lazy
            cx
            (lazy [spf "Expression cache hit at %s" (ALoc.debug_to_string loc)]);
          node
        | None ->
          let res = expression_ ~cond ~as_const ~frozen cx loc e in
          if not (Context.typing_mode cx <> Context.CheckingMode) then begin
            let cache = Context.constraint_cache cx in
            cache := FlowSet.empty;
            Node_cache.set_expression node_cache res
          end;
          res
      in
      (* We need to fully resolve all types attached to AST,
         because the post inference pass might inspect them. *)
      (match res with
      | (_, Ast.Expression.OptionalCall { Ast.Expression.OptionalCall.filtered_out = (_, t); _ })
      | (_, Ast.Expression.OptionalMember { Ast.Expression.OptionalMember.filtered_out = (_, t); _ })
      | (_, Ast.Expression.Yield { Ast.Expression.Yield.result_out = (_, t); _ }) ->
        Tvar_resolver.resolve cx t
      | _ -> ());
      Tvar_resolver.resolve cx t;
      res
    in
    log_slow_to_check ~f

  and this_ cx loc this =
    let open Ast.Expression in
    match Refinement.get ~allow_optional:true cx (loc, This this) loc with
    | Some t -> t
    | None -> Type_env.var_ref cx (internal_name "this") loc

  and super_ cx loc = Type_env.var_ref cx (internal_name "super") loc

  and expression_ ~cond ~as_const ~frozen cx loc e : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t =
    let ex = (loc, e) in
    let open Ast.Expression in
    match e with
    | StringLiteral lit ->
      let t = string_literal cx ~singleton:(as_const || frozen) loc lit in
      ((loc, t), StringLiteral lit)
    | BooleanLiteral lit ->
      let t = boolean_literal ~singleton:(as_const || frozen) loc lit in
      ((loc, t), BooleanLiteral lit)
    | NullLiteral lit ->
      let t = null_literal loc in
      ((loc, t), NullLiteral lit)
    | NumberLiteral lit ->
      let t = number_literal ~singleton:(as_const || frozen) loc lit in
      ((loc, t), NumberLiteral lit)
    | BigIntLiteral lit ->
      let t = bigint_literal ~singleton:(as_const || frozen) loc lit in
      ((loc, t), BigIntLiteral lit)
    | RegExpLiteral lit ->
      let t = regexp_literal cx loc in
      ((loc, t), RegExpLiteral lit)
    | ModuleRefLiteral lit ->
      let (t, lit) = module_ref_literal cx loc lit in
      ((loc, t), ModuleRefLiteral lit)
    (* Treat the identifier `undefined` as an annotation for error reporting
     * purposes. Like we do with other literals. Otherwise we end up pointing to
     * `void` in `core.js`. While possible to re-declare `undefined`, it is
     * unlikely. The tradeoff is worth it. *)
    | Identifier (id_loc, ({ Ast.Identifier.name = "undefined"; comments = _ } as name))
      when Type_env.is_global_var cx id_loc ->
      let t = VoidT.make (mk_reason RVoid loc) in
      ((loc, t), Identifier ((id_loc, t), name))
    | Identifier (id_loc, name) ->
      let t = identifier cx name loc in
      ((loc, t), Identifier ((id_loc, t), name))
    | This this ->
      let t = this_ cx loc this in
      ((loc, t), This this)
    | Super s -> ((loc, identifier cx (mk_ident ~comments:None "super") loc), Super s)
    | Unary u ->
      let (t, u) = unary cx ~cond loc u in
      ((loc, t), Unary u)
    | Update u ->
      let (t, u) = update cx loc u in
      ((loc, t), Update u)
    | Binary b ->
      let (t, b) = binary cx loc ~cond b in
      ((loc, t), Binary b)
    | Logical l ->
      let (t, l) = logical cx loc ~cond l in
      ((loc, t), Logical l)
    | TypeCast ({ TypeCast.expression = e; annot; comments } as cast) ->
      let casting_syntax = Context.casting_syntax cx in
      let open Options.CastingSyntax in
      (match casting_syntax with
      | Colon
      | Both ->
        let (t, annot') = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
        let (((_, infer_t), _) as e') = expression cx e in
        let use_op = Op (Cast { lower = mk_expression_reason e; upper = reason_of_t t }) in
        Flow.flow cx (infer_t, TypeCastT (use_op, t));
        ((loc, t), TypeCast { TypeCast.expression = e'; annot = annot'; comments })
      | As ->
        Flow_js_utils.add_output
          cx
          (Error_message.EInvalidTypeCastSyntax { loc; enabled_casting_syntax = casting_syntax });
        let t = AnyT.at (AnyError None) loc in
        ((loc, t), TypeCast (Tast_utils.error_mapper#type_cast cast)))
    | AsExpression ({ AsExpression.expression = e; annot; comments } as cast) ->
      let casting_syntax = Context.casting_syntax cx in
      let open Options.CastingSyntax in
      (match casting_syntax with
      | As
      | Both ->
        let (t, annot') = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
        let (((_, infer_t), _) as e') = expression cx e in
        let use_op = Op (Cast { lower = mk_expression_reason e; upper = reason_of_t t }) in
        Flow.flow cx (infer_t, TypeCastT (use_op, t));
        ((loc, t), AsExpression { AsExpression.expression = e'; annot = annot'; comments })
      | Colon ->
        Flow_js_utils.add_output
          cx
          (Error_message.EInvalidTypeCastSyntax { loc; enabled_casting_syntax = casting_syntax });
        let t = AnyT.at (AnyError None) loc in
        ((loc, t), AsExpression (Tast_utils.error_mapper#as_expression cast)))
    | AsConstExpression ({ AsConstExpression.expression = e; comments } as cast) ->
      if Context.enable_as_const cx then (
        check_const_assertion cx e;
        let (((_, t), _) as e) = expression cx ~as_const:true e in
        ((loc, t), AsConstExpression { AsConstExpression.expression = e; comments })
      ) else
        let kind = Error_message.TSAsConst (Context.casting_syntax cx) in
        Flow_js_utils.add_output cx (Error_message.ETSSyntax { kind; loc });
        let t = AnyT.at (AnyError None) loc in
        ((loc, t), AsConstExpression (Tast_utils.error_mapper#as_const_expression cast))
    | TSSatisfies cast ->
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax
           { kind = Error_message.TSSatisfiesType (Context.casting_syntax cx); loc }
        );
      let t = AnyT.at (AnyError None) loc in
      ((loc, t), TSSatisfies (Tast_utils.error_mapper#ts_satisfies cast))
    | Member _ -> subscript ~cond cx ex
    | OptionalMember _ -> subscript ~cond cx ex
    | Object { Object.properties; comments } ->
      error_on_this_uses_in_object_methods cx properties;
      let reason = Reason.mk_obj_lit_reason ~as_const ~frozen:false loc in
      let (t, properties) = object_ ~frozen:false ~as_const cx reason properties in
      ((loc, t), Object { Object.properties; comments })
    | Array { Array.elements; comments } ->
      (match elements with
      | [] when as_const ->
        (* Special case `[] as const` *)
        let reason = mk_reason RConstArrayLit loc in
        let elem_t = EmptyT.make (mk_reason REmptyArrayElement loc) in
        let arrtype =
          TupleAT { elem_t; elements = []; react_dro = None; arity = (0, 0); inexact = false }
        in
        ((loc, DefT (reason, ArrT arrtype)), Array { Array.elements = []; comments })
      | [] when Context.typing_mode cx <> Context.CheckingMode ->
        let reason = mk_reason REmptyArrayLit loc in
        let element_reason = mk_reason REmptyArrayElement loc in
        let elem_t = Context.mk_placeholder cx element_reason in
        let arrtype = ArrayAT { elem_t; tuple_view = Some empty_tuple_view; react_dro = None } in
        ((loc, DefT (reason, ArrT arrtype)), Array { Array.elements = []; comments })
      | [] ->
        let (reason, elem_t) = empty_array cx loc in
        let arrtype = ArrayAT { elem_t; tuple_view = Some empty_tuple_view; react_dro = None } in
        ((loc, DefT (reason, ArrT arrtype)), Array { Array.elements = []; comments })
      | elems ->
        let reason =
          if as_const then
            mk_reason RConstArrayLit loc
          else
            mk_reason RArrayLit loc
        in
        let (elem_spread_list, elements) = array_elements cx ~as_const loc elems in
        ( ( loc,
            Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
                let reason_op = reason in
                let element_reason =
                  replace_desc_reason
                    (Reason.RInferredUnionElemArray { instantiable = false })
                    reason_op
                in
                let elem_t = Tvar.mk cx element_reason in
                let resolve_to =
                  ResolveSpreadsToArrayLiteral { id = mk_id (); as_const; elem_t; tout }
                in
                Flow.resolve_spread_list
                  cx
                  ~use_op:unknown_use
                  ~reason_op
                  elem_spread_list
                  resolve_to
            )
          ),
          Array { Array.elements; comments }
        ))
    | New
        {
          New.callee =
            ( callee_loc,
              Identifier (id_loc, ({ Ast.Identifier.name = "Function"; comments = _ } as name))
            );
          targs;
          arguments;
          comments;
        } ->
      let targts_opt =
        Base.Option.map targs ~f:(fun (targts_loc, args) ->
            (targts_loc, convert_call_targs cx Subst_name.Map.empty args)
        )
      in
      let (argts, arges) =
        match arguments with
        | Some arguments ->
          let (argts, arges) = arg_list cx arguments in
          (argts, Some arges)
        | None -> ([], None)
      in
      let id_t = identifier cx name callee_loc in
      let callee_annot = (callee_loc, id_t) in
      (match targts_opt with
      | None ->
        List.iter
          (function
            | Arg t
            | SpreadArg t ->
              Flow.flow_t cx (t, StrT.at loc))
          argts;
        let reason = mk_reason (RCustom "new Function(..)") loc in
        let proto = ObjProtoT reason in
        ( ( loc,
            DefT
              ( reason,
                FunT
                  ( dummy_static reason,
                    mk_functiontype
                      reason
                      []
                      ~rest_param:None
                      ~def_reason:reason
                      ~params_names:[]
                      ~predicate:None
                      proto
                  )
              )
          ),
          New
            {
              New.callee = (callee_annot, Identifier ((id_loc, id_t), name));
              targs = None;
              arguments = arges;
              comments;
            }
        )
      | Some (targts_loc, targts) ->
        Flow.add_output
          cx
          Error_message.(
            ECallTypeArity
              {
                call_loc = loc;
                is_new = true;
                reason_arity = Reason.(locationless_reason (RType (OrdinaryName "Function")));
                expected_arity = 0;
              }
          );
        ( (loc, AnyT.at (AnyError None) loc),
          New
            {
              New.callee = (callee_annot, Identifier ((id_loc, id_t), name));
              targs = Some (targts_loc, snd targts);
              arguments = arges;
              comments;
            }
        ))
    | New
        {
          New.callee =
            ( callee_loc,
              Identifier (id_loc, ({ Ast.Identifier.name = "Array" as n; comments = _ } as name))
            );
          targs;
          arguments;
          comments;
        } ->
      let targts =
        Base.Option.map targs ~f:(fun (loc, args) ->
            (loc, convert_call_targs cx Subst_name.Map.empty args)
        )
      in
      let (argts, args) =
        match arguments with
        | Some arguments ->
          let (argts, args) = arg_list cx arguments in
          (argts, Some args)
        | None -> ([], None)
      in
      let result =
        match (targts, argts) with
        | ( Some (loc, ([t], ({ CallTypeArgs.arguments = [_]; comments = _ } as call_targs))),
            [Arg argt]
          ) ->
          Ok (Some (loc, call_targs, t), argt)
        | (None, [Arg argt]) -> Ok (None, argt)
        | (None, _) -> Error (Error_message.EUseArrayLiteral loc)
        | (Some _, _) ->
          Error
            Error_message.(
              ECallTypeArity
                {
                  call_loc = loc;
                  is_new = true;
                  reason_arity = Reason.(locationless_reason (RType (OrdinaryName n)));
                  expected_arity = 1;
                }
            )
      in
      (match result with
      | Ok (targ_t, arg_t) ->
        let reason = mk_reason (RCustom "new Array(..)") loc in
        let length_reason = replace_desc_reason (RCustom "array length") reason in
        Flow.flow_t cx (arg_t, DefT (length_reason, NumT AnyLiteral));
        let (targ_ts, targs_ast) =
          match targ_t with
          | Some (loc, ast, ExplicitArg t) -> (Some [ExplicitArg t], Some (loc, ast))
          | Some (_, _, ImplicitArg _)
          | None ->
            (None, None)
        in
        let id_t = identifier cx name callee_loc in
        let reason_call = mk_reason (RConstructorCall (desc_of_t id_t)) loc in
        let use_op =
          Op
            (FunCall
               { op = reason; fn = reason_of_t id_t; args = [reason_of_t arg_t]; local = true }
            )
        in
        let (t, ctor_t) = new_call cx loc reason_call ~use_op id_t targ_ts [Arg arg_t] in
        Context.set_ctor_callee cx loc ctor_t;
        ( (loc, t),
          New
            {
              New.callee = ((callee_loc, id_t), Identifier ((id_loc, id_t), name));
              targs = targs_ast;
              arguments = args;
              comments;
            }
        )
      | Error err ->
        Flow.add_output cx err;
        Tast_utils.error_mapper#expression ex)
    | New { New.callee; targs; arguments; comments } ->
      let (((_, class_), _) as callee_ast) = expression cx callee in
      let (targts, targs_ast) = convert_call_targs_opt cx targs in
      let (argts, args_reasons, arguments_ast) =
        match arguments with
        | Some arguments ->
          let (argst, arguments_ast) = arg_list cx arguments in
          let args_reasons = mk_initial_arguments_reason arguments in
          (argst, args_reasons, Some arguments_ast)
        | None -> ([], [], None)
      in
      let reason = mk_reason (RConstructorCall (desc_of_t class_)) loc in
      let use_op =
        Op
          (FunCall
             {
               op = mk_expression_reason ex;
               fn = mk_expression_reason callee;
               args = args_reasons;
               local = true;
             }
          )
      in
      let (t, ctor_t) = new_call cx loc reason ~use_op class_ targts argts in
      Context.set_ctor_callee cx loc ctor_t;
      ( (loc, t),
        New { New.callee = callee_ast; targs = targs_ast; arguments = arguments_ast; comments }
      )
    | Call _ -> subscript ~cond cx ex
    | OptionalCall _ -> subscript ~cond cx ex
    | Conditional { Conditional.test; consequent; alternate; comments } ->
      let reason = mk_reason RConditional loc in
      let test = condition ~cond:OtherTest cx test in
      let ((((_, t1), _) as consequent), then_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx consequent)
      in
      let ((((_, t2), _) as alternate), else_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx alternate)
      in
      let combined_type =
        match (then_abnormal, else_abnormal) with
        | (Some Abnormal.Throw, None) -> t2
        | (None, Some Abnormal.Throw) -> t1
        | (Some Abnormal.Throw, Some Abnormal.Throw) -> EmptyT.at loc
        (* Both sides threw--see below for where we re-raise *)
        | (None, None) -> UnionT (reason, UnionRep.make t1 t2 [])
        (* NOTE: In general it is dangerous to express the least upper bound of
           some types as a union: it might pin down the least upper bound
           prematurely (before all the types have been inferred), and when the
           union appears as an upper bound, it might lead to speculative matching.

           However, here a union is safe, because this union is guaranteed to only
           appear as a lower bound.

           In such "covariant" positions, avoiding unnecessary indirection via
           tvars is a good thing, because it improves precision. In particular, it
           enables more types to be fully resolvable, which improves results of
           speculative matching.

           It should be possible to do this more broadly and systematically. For
           example, results of operations on annotations (like property gets on
           objects, calls on functions) are often represented as unresolved tvars,
           where they could be pinned down to resolved types.
        *)
      in

      (* TODO call loc_of_predicate on some pred?
         t1 is wrong but hopefully close *)
      let ast =
        ((loc, combined_type), Conditional { Conditional.test; consequent; alternate; comments })
      in
      (* handle control flow in cases where we've thrown from both sides *)
      begin
        match (then_abnormal, else_abnormal) with
        | (Some then_exn, Some else_exn) when then_exn = else_exn ->
          Abnormal.throw_expr_control_flow_exception loc ast
        | _ -> ast
      end
    | Assignment { Assignment.operator; left; right; comments } ->
      let (t, left, right) = assignment cx loc (left, operator, right) in
      ((loc, t), Assignment { Assignment.operator; left; right; comments })
    | Sequence { Sequence.expressions; comments } ->
      let expressions = Base.List.map ~f:(expression cx) expressions in
      (* t = last element of ts. The parser guarantees sequence expressions are nonempty. *)
      let t = List.(expressions |> map snd_fst |> rev |> hd) in
      ((loc, t), Sequence { Sequence.expressions; comments })
    | Function func ->
      let { Ast.Function.id; predicate; sig_loc; generator; async; _ } = func in
      (match predicate with
      | Some (_, { Ast.Type.Predicate.kind = Ast.Type.Predicate.Inferred; comments = _ }) ->
        Flow.add_output
          cx
          (Error_message.EUnsupportedSyntax
             (loc, Flow_intermediate_error_types.PredicateDeclarationWithoutExpression)
          )
      | _ -> ());
      let reason = func_reason ~async ~generator sig_loc in
      let (t, func) =
        match id with
        | None -> mk_function_expression cx reason ~needs_this_param:true loc func
        | Some _ ->
          let prev_scope_kind = Type_env.set_scope_kind cx Name_def.Ordinary in
          let (t, func) = mk_function_expression cx reason ~needs_this_param:true loc func in
          ignore @@ Type_env.set_scope_kind cx prev_scope_kind;
          (t, func)
      in
      ((loc, t), Function func)
    | ArrowFunction func ->
      let reason = Ast.Function.(func_reason ~async:func.async ~generator:func.generator loc) in
      let (t, f) = mk_arrow cx ~statics:SMap.empty reason func in
      ((loc, t), ArrowFunction f)
    (*
     * GraphQL literals, e.g.:
     * graphql`fragment Foo {}`
     *)
    | TaggedTemplate
        {
          TaggedTemplate.tag = (_, Identifier (_, { Ast.Identifier.name = "graphql"; _ })) as tag;
          quasi;
          comments;
        }
      when Context.enable_relay_integration cx ->
      let module_prefix = Context.relay_integration_module_prefix cx in
      let t =
        match Graphql.extract_module_name ~module_prefix quasi with
        | Ok module_name ->
          let module_t =
            Import_export.get_module_t
              cx
              ~import_kind_for_untyped_import_validation:(Some ImportValue)
              (loc, module_name)
          in
          if Context.relay_integration_esmodules cx then
            let import_reason = mk_reason (RDefaultImportedType (module_name, module_name)) loc in
            Import_export.import_default_specifier_type
              cx
              import_reason
              Ast.Statement.ImportDeclaration.ImportValue
              ~module_name
              ~source_module_t:module_t
              ~local_name:module_name
            |> snd
          else
            Import_export.cjs_require_type
              cx
              (mk_reason (RModule module_name) loc)
              ~namespace_symbol:(mk_module_symbol ~name:module_name ~def_loc:loc)
              ~legacy_interop:false
              module_t
        | Error err ->
          Flow.add_output cx (Error_message.EInvalidGraphQL (loc, err));
          let reason = mk_reason RAnyImplicit loc in
          AnyT.error reason
      in
      let tag_ast = expression cx tag in
      let quasi_ast =
        let (quasi_loc, { TemplateLiteral.quasis; expressions; comments }) = quasi in
        let expressions = Base.List.map ~f:(expression cx) expressions in
        (quasi_loc, { TemplateLiteral.quasis; expressions; comments })
      in
      ((loc, t), TaggedTemplate { TaggedTemplate.tag = tag_ast; quasi = quasi_ast; comments })
    | TaggedTemplate
        {
          TaggedTemplate.tag;
          (* TODO: walk quasis? *)
          quasi = (quasi_loc, { TemplateLiteral.quasis; expressions; comments = quasi_comments });
          comments = tagged_template_comments;
        } ->
      let expressions = Base.List.map ~f:(expression cx) expressions in
      let (((_, t), _) as tag_ast) = expression cx tag in
      let reason = mk_reason (RCustom "encaps tag") loc in
      let reason_array = replace_desc_reason RArray reason in
      let ret = (reason, Tvar.mk_no_wrap cx reason) in
      (* tag`a${b}c${d}` -> tag(['a', 'c'], b, d) *)
      let call_t =
        let args =
          let quasi_t = Flow.get_builtin_type cx reason_array "TaggedTemplateLiteralArray" in
          let exprs_t = Base.List.map ~f:(fun ((_, t), _) -> Arg t) expressions in
          Arg quasi_t :: exprs_t
        in
        let ft = mk_functioncalltype ~call_kind:RegularCallKind reason None args ret in
        let use_op =
          Op
            (FunCall
               {
                 op = mk_expression_reason ex;
                 fn = mk_expression_reason tag;
                 args = [];
                 local = true;
               }
            )
        in
        CallT
          { use_op; reason; call_action = Funcalltype ft; return_hint = Type_env.get_hint cx loc }
      in
      Flow.flow cx (t, call_t);

      ( (loc, OpenT ret),
        TaggedTemplate
          {
            TaggedTemplate.tag = tag_ast;
            quasi = (quasi_loc, { TemplateLiteral.quasis; expressions; comments = quasi_comments });
            comments = tagged_template_comments;
          }
      )
    | TemplateLiteral { TemplateLiteral.quasis; expressions; comments } ->
      let (t, expressions) =
        match quasis with
        | [head] ->
          let ( elem_loc,
                { TemplateLiteral.Element.value = { TemplateLiteral.Element.cooked; _ }; _ }
              ) =
            head
          in
          let t = string_literal_value cx ~singleton:false elem_loc cooked in
          (t, [])
        | _ ->
          let t_out = StrT.at loc in
          let expressions =
            Base.List.map
              ~f:(fun expr ->
                let (((_, t), _) as e) = expression cx expr in
                Flow.flow
                  cx
                  ( t,
                    UseT
                      ( Op
                          (Coercion { from = mk_expression_reason expr; target = reason_of_t t_out }),
                        t_out
                      )
                  );
                e)
              expressions
          in
          (t_out, expressions)
      in
      ((loc, t), TemplateLiteral { TemplateLiteral.quasis; expressions; comments })
    | JSXElement e ->
      let (t, e) = jsx cx loc e in
      ((loc, t), JSXElement e)
    | JSXFragment f ->
      let (t, f) = jsx_fragment cx loc f in
      ((loc, t), JSXFragment f)
    | Class c ->
      let class_loc = loc in
      let (name_loc, name) =
        match Ast.Class.(c.id) with
        | Some (name_loc, { Ast.Identifier.name; comments = _ }) -> (name_loc, name)
        | None -> (class_loc, "<<anonymous class>>")
      in
      let reason = mk_reason (RIdentifier (OrdinaryName name)) name_loc in
      (match c.Ast.Class.id with
      | Some _ ->
        let (class_t, c) = mk_class cx class_loc ~name_loc reason c in
        (* mk_class above ensures that the function name in the inline declaration
           has the same type as its references inside the class.
           However, in the new env, we need to perform a bind of the class declaration type to the
           name to ensure that the environment knows the type of both the declaration and usages. *)
        let () =
          let name = OrdinaryName name in
          let use_op =
            Op
              (AssignVar
                 { var = Some (mk_reason (RIdentifier name) name_loc); init = reason_of_t class_t }
              )
          in
          Type_env.init_implicit_let cx ~use_op class_t name_loc
        in
        ((class_loc, class_t), Class c)
      | None ->
        let (class_t, c) = mk_class cx class_loc ~name_loc reason c in
        ((class_loc, class_t), Class c))
    | Yield { Yield.argument; delegate = false; comments; result_out } ->
      let (t, argument_ast) =
        match argument with
        | Some expr ->
          let (((_, t), _) as expr) = expression cx expr in
          (t, Some expr)
        | None -> (VoidT.at loc, None)
      in
      ( (loc, Type_env.get_next cx loc),
        Yield
          {
            Yield.argument = argument_ast;
            delegate = false;
            comments;
            result_out = (result_out, t);
          }
      )
    | Yield { Yield.argument; delegate = true; comments; result_out } ->
      let reason = mk_reason (RCustom "yield* delegate") loc in
      let next = Type_env.get_next cx loc in
      let (t, argument_ast) =
        match argument with
        | Some expr ->
          let (((_, t), _) as expr) = expression cx expr in
          (t, Some expr)
        | None -> assert_false "delegate yield without argument"
      in
      let ret_reason =
        update_desc_reason
          (fun desc -> RCustom (spf "return of child generator in %s" (string_of_desc desc)))
          reason
      in
      let ret = Tvar.mk cx ret_reason in
      let yield = Tvar.mk cx reason in
      (* widen yield with the element type of the delegated-to iterable *)
      let targs = [yield; ret; next] in
      let (async, iterable_reason) =
        if Type_env.in_async_scope cx then
          (true, mk_reason (RCustom "async iteration expected on AsyncIterable") loc)
        else
          (false, mk_reason (RCustom "iteration expected on Iterable") loc)
      in
      let use_op =
        Op
          (GeneratorYield
             {
               value =
                 (match argument with
                 | Some expr -> mk_expression_reason expr
                 | None -> reason_of_t t);
             }
          )
      in
      Flow.flow cx (t, AssertIterableT { use_op; reason = iterable_reason; async; targs });

      ( (loc, ret),
        Yield
          {
            Yield.argument = argument_ast;
            delegate = true;
            comments;
            result_out = (result_out, yield);
          }
      )
    | MetaProperty
        {
          MetaProperty.meta = (_, { Ast.Identifier.name = "new"; _ }) as meta;
          property = (_, { Ast.Identifier.name = "target"; _ }) as property;
          comments;
        } ->
      let t = MixedT.at loc in
      ((loc, t), MetaProperty { MetaProperty.meta; property; comments })
    | MetaProperty
        {
          MetaProperty.meta = (_, { Ast.Identifier.name = "import"; _ }) as meta;
          property = (_, { Ast.Identifier.name = "meta"; _ }) as property;
          comments;
        } ->
      let reason = mk_reason (RCustom "import.meta") loc in
      let t = Flow.get_builtin_type cx reason "Import$Meta" in
      ((loc, t), MetaProperty { MetaProperty.meta; property; comments })
    | MetaProperty _ ->
      Flow.add_output
        cx
        (Error_message.EUnsupportedSyntax (loc, Flow_intermediate_error_types.MetaPropertyExpression)
        );
      Tast_utils.error_mapper#expression ex
    | Import { Import.argument = (source_loc, argument); comments } ->
      let t module_name =
        let ns_t =
          let reason = mk_reason (RModule module_name) loc in
          Import_export.get_module_t
            cx
            (source_loc, module_name)
            ~perform_platform_validation:true
            ~import_kind_for_untyped_import_validation:(Some ImportValue)
          |> Import_export.get_module_namespace_type
               cx
               reason
               ~namespace_symbol:(mk_module_symbol ~name:module_name ~def_loc:loc)
        in
        let reason = mk_annot_reason RAsyncImport loc in
        Flow.get_builtin_typeapp cx reason "Promise" [ns_t]
      in
      (match argument with
      | Ast.Expression.StringLiteral ({ Ast.StringLiteral.value = module_name; _ } as lit) ->
        let t = t module_name in
        ( (loc, t),
          Import { Import.argument = ((source_loc, t), Ast.Expression.StringLiteral lit); comments }
        )
      | TemplateLiteral ({ TemplateLiteral.quasis = [quasi]; expressions = []; _ } as lit) ->
        let ( _,
              {
                TemplateLiteral.Element.value = { TemplateLiteral.Element.cooked = module_name; _ };
                _;
              }
            ) =
          quasi
        in
        let t = t module_name in
        ((loc, t), Import { Import.argument = ((source_loc, t), TemplateLiteral lit); comments })
      | _ ->
        let ignore_non_literals = Context.should_ignore_non_literal_requires cx in
        if not ignore_non_literals then (
          Flow.add_output
            cx
            (Error_message.EUnsupportedSyntax
               (loc, Flow_intermediate_error_types.ImportDynamicArgument)
            );
          Tast_utils.error_mapper#expression ex
        ) else
          Tast_utils.unchecked_mapper#expression ex)

  (* Handles operations that may traverse optional chains

     Returns a tuple:
       * type of expression if no optional chains short-circuited,
       * a list of void types of all possible short-circuitings,
       * typed AST of expression, where the type is the combination of
         short-circuiting and non short-circuiting (i.e. representing the actual
         range of possible types of the expression)
  *)
  and optional_chain ~cond cx ((loc, e) as ex) =
    let open Ast.Expression in
    let normalize_voided_out t =
      let ts = Flow.possible_concrete_types_for_inspection cx (reason_of_t t) t in
      Base.List.iter ts ~f:(Tvar_resolver.resolve cx);
      ts
    in
    let factor_out_optional (_, e) =
      let (opt_state, filtered_out_loc, e') =
        match e with
        | OptionalCall { OptionalCall.call; optional; filtered_out } ->
          let opt_state =
            if optional then
              NewChain
            else
              ContinueChain
          in
          (opt_state, filtered_out, Call call)
        | OptionalMember { OptionalMember.member; optional; filtered_out } ->
          let opt_state =
            if optional then
              NewChain
            else
              ContinueChain
          in
          (opt_state, filtered_out, Member member)
        | _ -> (NonOptional, loc, e)
      in
      let call_ast ~filtered_out:ty ~sig_help call =
        let { Call.callee = ((loc, _), _); _ } = call in
        Context.set_signature_help_callee cx loc sig_help;
        match opt_state with
        | NewChain ->
          OptionalCall { OptionalCall.call; optional = true; filtered_out = (filtered_out_loc, ty) }
        | ContinueChain ->
          OptionalCall
            { OptionalCall.call; optional = false; filtered_out = (filtered_out_loc, ty) }
        | NonOptional -> Call call
      in
      let member_ast member ty =
        match opt_state with
        | NewChain ->
          OptionalMember
            { OptionalMember.member; optional = true; filtered_out = (filtered_out_loc, ty) }
        | ContinueChain ->
          OptionalMember
            { OptionalMember.member; optional = false; filtered_out = (filtered_out_loc, ty) }
        | NonOptional -> Member member
      in
      (e', opt_state, call_ast, member_ast)
    in
    let try_non_chain cx loc e ~call_ast ~member_ast =
      (* Special cases where optional chaining doesn't occur *)
      match e with
      | Call
          {
            Call.callee =
              ( callee_loc,
                Identifier (id_loc, ({ Ast.Identifier.name = "require"; comments = _ } as name))
              );
            targs;
            arguments;
            comments;
          }
        when not (Type_env.local_scope_entry_exists cx id_loc) ->
        let targs =
          Base.Option.map targs ~f:(fun (args_loc, args) ->
              (args_loc, snd (convert_call_targs cx Subst_name.Map.empty args))
          )
        in
        let (lhs_t, arguments) =
          match (targs, arguments) with
          | ( None,
              ( args_loc,
                {
                  ArgList.arguments =
                    [
                      Expression
                        ( ( source_loc,
                            Ast.Expression.StringLiteral
                              { Ast.StringLiteral.value = module_name; _ }
                          ) as lit_exp
                        );
                    ];
                  comments;
                }
              )
            ) ->
            let t =
              Import_export.get_module_t
                cx
                (source_loc, module_name)
                ~perform_platform_validation:true
                ~import_kind_for_untyped_import_validation:(Some ImportValue)
              |> Import_export.cjs_require_type
                   cx
                   (mk_reason (RModule module_name) loc)
                   ~namespace_symbol:(mk_module_symbol ~name:module_name ~def_loc:loc)
                   ~legacy_interop:false
            in
            (t, (args_loc, { ArgList.arguments = [Expression (expression cx lit_exp)]; comments }))
          | ( None,
              ( args_loc,
                {
                  ArgList.arguments =
                    [
                      Expression
                        ( ( source_loc,
                            TemplateLiteral
                              {
                                TemplateLiteral.quasis =
                                  [
                                    ( _,
                                      {
                                        TemplateLiteral.Element.value =
                                          { TemplateLiteral.Element.cooked = module_name; _ };
                                        _;
                                      }
                                    );
                                  ];
                                expressions = [];
                                comments = _;
                              }
                          ) as lit_exp
                        );
                    ];
                  comments;
                }
              )
            ) ->
            let t =
              Import_export.get_module_t
                cx
                (source_loc, module_name)
                ~perform_platform_validation:true
                ~import_kind_for_untyped_import_validation:(Some ImportValue)
              |> Import_export.cjs_require_type
                   cx
                   (mk_reason (RModule module_name) loc)
                   ~namespace_symbol:(mk_module_symbol ~name:module_name ~def_loc:loc)
                   ~legacy_interop:false
            in
            (t, (args_loc, { ArgList.arguments = [Expression (expression cx lit_exp)]; comments }))
          | (Some _, arguments) ->
            ignore (arg_list cx arguments);
            Flow.add_output
              cx
              Error_message.(
                ECallTypeArity
                  {
                    call_loc = loc;
                    is_new = false;
                    reason_arity = Reason.(locationless_reason (RFunction RNormal));
                    expected_arity = 0;
                  }
              );
            (AnyT.at (AnyError None) loc, Tast_utils.error_mapper#arg_list arguments)
          | (None, arguments) ->
            ignore (arg_list cx arguments);
            let ignore_non_literals = Context.should_ignore_non_literal_requires cx in
            if not ignore_non_literals then
              Flow.add_output
                cx
                (Error_message.EUnsupportedSyntax
                   (loc, Flow_intermediate_error_types.RequireDynamicArgument)
                );
            (AnyT.at (AnyError None) loc, Tast_utils.error_mapper#arg_list arguments)
        in
        let id_t = MixedT.at callee_loc in
        Some
          ( (loc, lhs_t),
            call_ast
              ~filtered_out:lhs_t
              ~sig_help:id_t
              {
                Call.callee = ((callee_loc, id_t), Identifier ((id_loc, id_t), name));
                targs;
                arguments;
                comments;
              }
          )
      | Call
          {
            Call.callee =
              ( callee_loc,
                Member
                  {
                    Member._object =
                      (_, Identifier (_, { Ast.Identifier.name = "Object"; comments = _ })) as obj;
                    property =
                      Member.PropertyIdentifier
                        (prop_loc, ({ Ast.Identifier.name; comments = _ } as id));
                    comments = member_comments;
                  }
              ) as expr;
            targs;
            arguments;
            comments;
          } ->
        let (((_, obj_t), _) as obj_ast) = expression cx obj in
        let (lhs_t, targs, arguments) =
          static_method_call_Object cx loc callee_loc prop_loc expr obj_t name targs arguments
        in
        Some
          ( (loc, lhs_t),
            let t = MixedT.at callee_loc in
            call_ast
              ~filtered_out:lhs_t
              ~sig_help:t
              {
                Call.callee (* TODO(vijayramamurthy): what is the type of `Object.name` ? *) =
                  ( (callee_loc, t),
                    Member
                      {
                        Member._object = obj_ast;
                        property = Member.PropertyIdentifier ((prop_loc, t), id);
                        comments = member_comments;
                      }
                  );
                targs;
                arguments;
                comments;
              }
          )
      | Call
          {
            Call.callee =
              ( callee_loc,
                Member
                  {
                    Member._object = (super_loc, Super super);
                    property =
                      Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments = _ } as id));
                    comments = member_comments;
                  }
              ) as callee;
            targs;
            arguments;
            comments;
          } ->
        let reason = mk_reason (RMethodCall (Some name)) loc in
        let name = OrdinaryName name in
        let reason_lookup = mk_reason (RProperty (Some name)) callee_loc in
        let reason_prop = mk_reason (RProperty (Some name)) ploc in
        let super_t = super_ cx super_loc in
        let meth_generic_this = Tvar.mk cx reason in
        let (targts, targs) = convert_call_targs_opt cx targs in
        let (argts, arguments_ast) = arg_list cx arguments in
        let specialized_callee = Context.new_specialized_callee cx in
        let lhs_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
              let methodcalltype = mk_methodcalltype ~meth_generic_this targts argts t in
              let use_op =
                Op
                  (FunCallMethod
                     {
                       op = mk_expression_reason ex;
                       fn = mk_expression_reason callee;
                       prop = reason_prop;
                       args = mk_initial_arguments_reason arguments;
                       local = true;
                     }
                  )
              in
              Flow.flow
                cx
                ( super_t,
                  MethodT
                    ( use_op,
                      reason,
                      reason_lookup,
                      mk_named_prop ~reason:reason_prop name,
                      CallM
                        {
                          methodcalltype;
                          return_hint = Type_env.get_hint cx loc;
                          specialized_callee = Some specialized_callee;
                        }
                    )
                )
          )
        in
        let prop_t = Flow_js_utils.CalleeRecorder.type_for_tast reason_lookup specialized_callee in
        Some
          ( (loc, lhs_t),
            call_ast
              ~filtered_out:lhs_t
              ~sig_help:prop_t
              {
                Call.callee =
                  ( (callee_loc, prop_t),
                    Member
                      {
                        Member._object = ((super_loc, super_t), Super super);
                        property = Member.PropertyIdentifier ((ploc, prop_t), id);
                        comments = member_comments;
                      }
                  );
                targs;
                arguments = arguments_ast;
                comments;
              }
          )
      | Call { Call.callee = (super_loc, Super super) as callee; targs; arguments; comments } ->
        let (targts, targs) = convert_call_targs_opt cx targs in
        let reason = mk_reason (RFunctionCall RSuper) loc in
        let super_t = super_ cx super_loc in
        let (argts, arguments_ast) = arg_list cx arguments in

        let super_reason = reason_of_t super_t in
        let lhs_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
              let methodcalltype = mk_methodcalltype targts argts t in
              let propref = mk_named_prop ~reason:super_reason (OrdinaryName "constructor") in
              let use_op =
                Op
                  (FunCall
                     {
                       op = mk_expression_reason ex;
                       fn = mk_expression_reason callee;
                       args = mk_initial_arguments_reason arguments;
                       local = true;
                     }
                  )
              in
              Flow.flow
                cx
                ( super_t,
                  MethodT
                    ( use_op,
                      reason,
                      super_reason,
                      propref,
                      CallM
                        {
                          methodcalltype;
                          return_hint = Type.hint_unavailable;
                          specialized_callee = None;
                        }
                    )
                )
          )
        in
        Some
          ( (loc, lhs_t),
            call_ast
              ~filtered_out:lhs_t
              ~sig_help:super_t
              {
                Call.callee = ((super_loc, super_t), Super super);
                targs;
                arguments = arguments_ast;
                comments;
              }
          )
      (******************************************)
      (* See ~/www/static_upstream/core/ *)
      | Call { Call.callee; targs; arguments; comments }
        when Flow_ast_utils.is_call_to_invariant callee ->
        (* TODO: require *)
        let (((_, callee_t), _) as callee) = expression cx callee in
        let targs =
          Base.Option.map targs ~f:(fun (loc, args) ->
              (loc, snd (convert_call_targs cx Subst_name.Map.empty args))
          )
        in
        (* NOTE: if an invariant expression throws abnormal control flow, the
           entire statement it was in is reconstructed in the typed AST as an
           expression statement containing just the invariant call. This should
           be ok for the most part since this is the most common way to call
           invariant. It's worth experimenting with whether people use invariant
           in other ways, and if not, restricting it to this pattern. *)
        let (t, arguments) =
          match (targs, arguments) with
          | (None, (args_loc, { ArgList.arguments = []; comments = args_comments })) ->
            let t = EmptyT.at loc in
            (* invariant() is treated like a throw *)
            ( t,
              Abnormal.throw_expr_control_flow_exception
                loc
                ( (loc, t),
                  Ast.Expression.Call
                    {
                      Call.callee;
                      targs;
                      arguments = (args_loc, { ArgList.arguments = []; comments = args_comments });
                      comments;
                    }
                )
            )
          | ( None,
              ( args_loc,
                {
                  ArgList.arguments =
                    Expression
                      ( (_, Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = false; _ })
                      as lit_exp
                      )
                    :: arguments;
                  comments = args_comments;
                }
              )
            ) ->
            (* invariant(false, ...) is treated like a throw *)
            let arguments =
              Base.List.map ~f:(Base.Fn.compose snd (expression_or_spread cx)) arguments
            in
            let lit_exp = expression cx lit_exp in
            let t = EmptyT.at loc in
            ( t,
              Abnormal.throw_expr_control_flow_exception
                loc
                ( (loc, t),
                  Ast.Expression.Call
                    {
                      Call.callee;
                      targs;
                      arguments =
                        ( args_loc,
                          {
                            ArgList.arguments = Expression lit_exp :: arguments;
                            comments = args_comments;
                          }
                        );
                      comments;
                    }
                )
            )
          | ( None,
              ( args_loc,
                { ArgList.arguments = Expression cond :: arguments; comments = args_comments }
              )
            ) ->
            let arguments =
              Base.List.map ~f:(Base.Fn.compose snd (expression_or_spread cx)) arguments
            in
            let (((_, cond_t), _) as cond) = condition ~cond:OtherTest cx cond in
            let reason = mk_reason (RFunctionCall (desc_of_t callee_t)) loc in
            Flow.flow cx (cond_t, InvariantT reason);
            let t = VoidT.at loc in
            ( t,
              ( args_loc,
                { ArgList.arguments = Expression cond :: arguments; comments = args_comments }
              )
            )
          | (_, (_, { ArgList.arguments = Spread _ :: _; comments = _ })) ->
            ignore (arg_list cx arguments);
            Flow.add_output
              cx
              (Error_message.EUnsupportedSyntax
                 (loc, Flow_intermediate_error_types.InvariantSpreadArgument)
              );
            let t = AnyT.at (AnyError None) loc in
            (t, Tast_utils.error_mapper#arg_list arguments)
          | (Some _, arguments) ->
            ignore (arg_list cx arguments);
            Flow.add_output
              cx
              Error_message.(
                ECallTypeArity
                  {
                    call_loc = loc;
                    is_new = false;
                    reason_arity = Reason.(locationless_reason (RFunction RNormal));
                    expected_arity = 0;
                  }
              );
            let t = AnyT.at (AnyError None) loc in
            (t, Tast_utils.error_mapper#arg_list arguments)
        in
        Some
          ( (loc, t),
            call_ast ~filtered_out:t ~sig_help:callee_t { Call.callee; targs; arguments; comments }
          )
      | Member
          {
            Member._object = (super_loc, Super super);
            property =
              Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments = _ } as id));
            comments;
          } ->
        let super_t = super_ cx super_loc in
        let expr_reason = mk_reason (RProperty (Some (OrdinaryName name))) loc in
        let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) ploc in
        let lhs_t =
          match Refinement.get ~allow_optional:true cx (loc, e) loc with
          | Some t -> t
          | None ->
            let use_op = Op (GetProperty (mk_expression_reason ex)) in
            get_prop
              ~use_op
              ~cond
              ~hint:(Type_env.get_hint cx loc)
              cx
              expr_reason
              super_t
              (prop_reason, name)
        in
        let property = Member.PropertyIdentifier ((ploc, lhs_t), id) in
        let ast =
          ( (loc, lhs_t),
            member_ast
              { Member._object = ((super_loc, super_t), Super super); property; comments }
              lhs_t
          )
        in
        Some ast
      | Call { Call.callee; targs = _; arguments; comments = _ }
        when Context.enable_jest_integration cx ->
        (match Flow_ast_utils.get_call_to_jest_module_mocking_fn callee arguments with
        | Some (jest_loc, source_loc, module_name)
          when not (Type_env.local_scope_entry_exists cx jest_loc) ->
          ignore
          @@ Import_export.get_module_t
               cx
               (source_loc, module_name)
               ~perform_platform_validation:false
               ~import_kind_for_untyped_import_validation:None
        | _ -> ());
        None
      | _ -> None
    in
    let (e', opt_state, call_ast, member_ast) = factor_out_optional ex in

    (*
     When traversing an optional chain, we need to track the "successful" types
     (if all optional chain operators in the sequence filtered out null/void),
     the nullish results if any, from the possibility of the optional chain
     short-circuiting (there may be multiple sources of null, from multiple
     chain operators in the chain) and the "actual"/final type of the overall
     expression, which can be seen as a union of the successful type and all
     possible nullish failure types.

     The optional_chain function therefore returns a 5-tuple:
       * T1: the type of the expression modulo optional chaining--i.e., the
         type in the case where any optional chain tests succeed,
       * T2: a list of types representing the union of all optional chain
         *failures*, if they may exist
       * exp: the typed AST expression, where the type of the node is the
         "actual" type of the expression, including both chain failures and
         chain successes.

     So, if `a: ?{b?: {c: number}}`, and the checked expression is `a?.b?.c`,
       then the output would be (T1, T2, T3, exp), where:
       * T1 = number
       * T2 = void, both from `a: ?{...}` and from `a: {b? : {...}}`
       * exp = ast for `a?.b?.c` with type T1 U T2

    Below are several helper functions for setting up this tuple in the
    presence of chaining.
  *)
    let join_optional_branches voided filtered =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx (reason_of_t filtered) (fun t ->
          Flow.flow_t cx (filtered, t);
          Base.List.iter voided ~f:(fun void -> Flow.flow_t cx (void, t))
      )
    in
    let noop _ = None in
    let handle_new_chain conf lhs_reason loc (chain_t, voided_t, object_ast) =
      let { ChainingConf.subexpressions; get_reason; get_opt_use; _ } = conf in
      (* We've encountered an optional chaining operator.
         We need to flow the "success" type of obj_ into a OptionalChainT
         type, which will "filter out" VoidT and NullT from the type of
         obj_ and flow them into `voided_out`, and then flow any non-void
         type into a use_t created by applying an opt_use_t (representing the
         operation that will occur on the upper bound) to a new "output" tvar.

         This might not be the first optional chain operator in the chain, so
         we need to take chain_t, which is equivalent to T1 above and
         represents the result if any previous operator succeeded--this is the
         type that we want to flow into the OptionalChainT, because if the
         previous operator failed we wouldn't reach this point in the chain in
         the first place. We also take voided_t, equivalent to T2 above and
         representing any previous chain short-circuits, and it will
         contribute to the failure/short-circuit output of this function,
         `voided_out`.

         Method calls need a little bit of extra support, because MethodT
         acts as both a lookup and a call. Suppose `a: ?{b?: () => number}`
         and `a?.b?.().` We need to generate a funcalltype for the call to
         () => number, and funcalltypes include the receiver ("this") of the
         call. However, we don't want the receiver to be the type of `a`,
         ?{b?: () => number}, because before calling the method, we've
         already filtered out the nullish case on `a`. The receiver instead
         should be {b?: () => number} (not optional). The bind_t parameter is
         (if present) the receiver of the method call, and is included in the
         OptionalChainT; see the rules in flow_js for how it's used, but
         essentially the successfully filtered receiver of the function call
         is flowed into it, and it is used as the `this`-parameter of the
         calltype that the method call will flow into.
      *)
      let (subexpression_types, subexpression_asts) = subexpressions () in
      let reason = get_reason chain_t in
      let chain_reason = mk_reason ROptionalChain loc in
      let mem_tvar = (reason, Tvar.mk_no_wrap cx reason) in
      let voided_out =
        Tvar.mk_where cx reason (fun t ->
            Base.List.iter ~f:(fun voided_t -> Flow.flow_t cx (voided_t, t)) voided_t
        )
      in
      let opt_use = get_opt_use subexpression_types reason in
      Flow.flow
        cx
        ( chain_t,
          OptionalChainT
            {
              reason = chain_reason;
              lhs_reason;
              t_out = apply_opt_use opt_use mem_tvar;
              voided_out;
            }
        );
      let mem_t = OpenT mem_tvar in
      let voided_out = normalize_voided_out voided_out in
      let lhs_t =
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
            Flow.flow_t cx (mem_t, t);
            Base.List.iter voided_out ~f:(fun out -> Flow.flow_t cx (out, t))
        )
      in
      Tvar_resolver.resolve cx mem_t;
      (mem_t, voided_out, lhs_t, chain_t, object_ast, subexpression_asts)
    in
    let handle_continue_chain conf (chain_t, voided_t, object_ast) =
      let { ChainingConf.refine; refinement_action; subexpressions; get_result; get_reason; _ } =
        conf
      in
      (* We're looking at a non-optional call or member access, but one where
         deeper in the chain there was an optional chaining operator. We don't
         need to do anything special locally, but we do need to remember that
         we might have short-circuited before getting here--that's the
         voided_t parameter. We'll flow that type into the type of the overall
         expression to account for that possibility.
      *)
      let (subexpression_types, subexpression_asts) = subexpressions () in
      let reason = get_reason chain_t in
      let res_t =
        match refine () with
        | Some refi ->
          Base.Option.value_map
            ~f:(fun refinement_action -> refinement_action subexpression_types chain_t refi)
            ~default:refi
            refinement_action
        | None -> get_result subexpression_types reason chain_t
      in
      let lhs_t = join_optional_branches voided_t res_t in
      (res_t, voided_t, lhs_t, chain_t, object_ast, subexpression_asts)
    in
    let handle_chaining conf opt obj_ loc =
      let { ChainingConf.refinement_action; refine; subexpressions; get_result; get_reason; _ } =
        conf
      in
      match opt with
      | NonOptional ->
        (* Proceeding as normal: no need to worry about optionality, so T2 from
           above is None. We don't need to consider optional short-circuiting, so
           we can call expression_ rather than optional_chain. *)
        let (((_, obj_t), _) as object_ast) = expression cx obj_ in
        let (subexpression_types, subexpression_asts) = subexpressions () in
        let reason = get_reason obj_t in
        let lhs_t =
          match refine () with
          | Some refi ->
            Base.Option.value_map
              ~f:(fun refinement_action -> refinement_action subexpression_types obj_t refi)
              ~default:refi
              refinement_action
          | None -> get_result subexpression_types reason obj_t
        in
        (lhs_t, [], lhs_t, obj_t, object_ast, subexpression_asts)
      | NewChain ->
        let lhs_reason = mk_expression_reason obj_ in
        let ((filtered_t, voided_t, object_ast) as object_data) =
          optional_chain ~cond:None cx obj_
        in
        begin
          match refine () with
          | Some t ->
            Context.mark_optional_chain cx loc lhs_reason ~useful:false;
            let (subexpression_types, subexpression_asts) = subexpressions () in
            let tout =
              Base.Option.value_map
                ~f:(fun refinement_action -> refinement_action subexpression_types filtered_t t)
                ~default:t
                refinement_action
            in
            ( tout,
              voided_t,
              join_optional_branches voided_t tout,
              filtered_t,
              object_ast,
              subexpression_asts
            )
          | None -> handle_new_chain conf lhs_reason loc object_data
        end
      | ContinueChain -> handle_continue_chain conf (optional_chain ~cond:None cx obj_)
    in
    let specialize_callee callee specialized_callee =
      let (Specialized_callee { finalized; _ }) = specialized_callee in
      if Base.List.is_empty finalized then
        callee
      else
        (* If the type of the callee has been specialized (due to implicit
         * instantiation or overload resolution) then use that type. *)
        let ((_, t_init), _) = callee in
        let t = union_of_ts (reason_of_t t_init) finalized in
        Flow_ast_utils.push_toplevel_type t callee
    in
    match try_non_chain cx loc e' ~call_ast ~member_ast with
    | Some (((_, lhs_t), _) as res) ->
      (* Nothing to do with respect to optional chaining, because we're in a
         case where chaining isn't allowed. *)
      (lhs_t, [], res)
    | None ->
      let (e', method_receiver_and_state) =
        (* If we're looking at a call, look "one level deeper" to see if the
         * next element of the chain is an member access, in which case we're
         * looking at an optional method call and we need to process both
         * "levels" at once.  Similar to the call to factor_out_optional above,
         * we then factor out the optionality of the member lookup component of
         * the method call. However, we can skip this if the callee is optional
         * and the call is non-optional--this means that the callee is in
         * parentheses, so we can treat it as a regular GetProp followed by a
         * regular Call instead of using the special method call machinery. Such
         * a case would look like this:
         *
         *     callee
         *    vvvvvvvvv
         *   (obj?.meth)()
         *    ^^^
         *     member._object
         *)
        match (e', opt_state) with
        | ( Call
              ( {
                  Call.callee =
                    (callee_loc, OptionalMember { OptionalMember.member; optional; filtered_out })
                    as orig_receiver;
                  targs = _;
                  arguments = _;
                  comments = _;
                } as call
              ),
            (NewChain | ContinueChain)
          ) ->
          let receiver_ast member ty =
            OptionalMember { OptionalMember.member; optional; filtered_out = (filtered_out, ty) }
          in
          let member_opt =
            if optional then
              (* In this case:
               *
               *   callee
               *  vvvvvvvvv
               *  obj?.meth() (or obj?.meth?.())
               *  ^^^
               *   member._object
               *
               * There may or may not be other links in the chain earlier than obj, and the call
               * to meth() may be optional itself (e.g. obj?.meth?.()) -- this has already been
               * factored out.
               *)
              NewChain
            else
              (* In this case:
               *
               *             callee
               *            vvvvvvvv
               * other_obj?.obj.meth() (or other_obj?.obj.meth?.())
               *            ^^^
               *             member._object
               *)
              ContinueChain
          in
          ( Call { call with Call.callee = (callee_loc, Member member) },
            Some (member_opt, member, receiver_ast, orig_receiver)
          )
        | ( Call
              {
                Call.callee = (_, Member member) as orig_receiver;
                targs = _;
                arguments = _;
                comments = _;
              },
            _
          ) ->
          (e', Some (NonOptional, member, (fun member _ -> Member member), orig_receiver))
        | _ -> (e', None)
      in
      (match (e', method_receiver_and_state) with
      (* e1[e2] *)
      | (Member { Member._object; property = Member.PropertyExpression index; comments }, _) ->
        let reason = mk_reason (RProperty None) loc in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        (* Only create an id if the property expression is a literal, which are
           treated like named props. *)
        let id =
          match index with
          | (_, StringLiteral _)
          | (_, NumberLiteral _) ->
            Some (mk_id ())
          | _ -> None
        in
        let get_opt_use tind _ = OptGetElemT (use_op, reason, id, false (* annot *), tind) in
        let get_mem_t tind reason obj_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
              let use = apply_opt_use (get_opt_use tind reason) t in
              Flow.flow cx (obj_t, use)
          )
        in
        let eval_index () =
          let (((_, tind), _) as index) = expression cx index in
          (tind, index)
        in
        let conf =
          {
            ChainingConf.refinement_action = None;
            refine = (fun () -> Refinement.get ~allow_optional:true cx (loc, e) loc);
            subexpressions = eval_index;
            get_result = get_mem_t;
            get_opt_use;
            get_reason = Fun.const reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, index) =
          handle_chaining conf opt_state _object loc
        in
        ( filtered_out,
          voided_out,
          ( (loc, lhs_t),
            member_ast
              { Member._object = object_ast; property = Member.PropertyExpression index; comments }
              filtered_out
          )
        )
      (* e.l *)
      | ( Member
            {
              Member._object;
              property =
                Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments = _ } as id));
              comments;
            },
          _
        ) ->
        let expr_reason = mk_expression_reason ex in
        let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) ploc in
        let use_op = Op (GetProperty expr_reason) in
        let opt_use =
          get_prop_opt_use
            ~cond
            expr_reason
            ~use_op
            ~hint:(Type_env.get_hint cx loc)
            (prop_reason, name)
        in
        let get_mem_t () _ obj_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx expr_reason (fun t ->
              let use = apply_opt_use opt_use t in
              Flow.flow cx (obj_t, use)
          )
        in
        let conf =
          {
            ChainingConf.refinement_action = None;
            subexpressions = (fun () -> ((), ()));
            get_result = get_mem_t;
            refine = (fun () -> Refinement.get ~allow_optional:true cx (loc, e) loc);
            get_opt_use = (fun _ _ -> opt_use);
            get_reason = Fun.const expr_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
          handle_chaining conf opt_state _object loc
        in
        let property = Member.PropertyIdentifier ((ploc, lhs_t), id) in
        ( filtered_out,
          voided_out,
          ((loc, lhs_t), member_ast { Member._object = object_ast; property; comments } filtered_out)
        )
      (* e.#l *)
      | ( Member
            {
              Member._object;
              property =
                Member.PropertyPrivateName (_, { Ast.PrivateName.name; comments = _ }) as property;
              comments;
            },
          _
        ) ->
        let expr_reason = mk_reason (RPrivateProperty name) loc in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        let opt_use = get_private_field_opt_use cx expr_reason ~use_op name in
        let get_mem_t () _ obj_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx expr_reason (fun t ->
              let use = apply_opt_use opt_use t in
              Flow.flow cx (obj_t, use)
          )
        in
        let conf =
          {
            ChainingConf.refinement_action = None;
            subexpressions = (fun () -> ((), ()));
            get_result = get_mem_t;
            refine = (fun () -> Refinement.get ~allow_optional:true cx (loc, e) loc);
            get_opt_use = (fun _ _ -> opt_use);
            get_reason = Fun.const expr_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
          handle_chaining conf opt_state _object loc
        in
        ( filtered_out,
          voided_out,
          ((loc, lhs_t), member_ast { Member._object = object_ast; property; comments } filtered_out)
        )
      (* Method calls: e.l(), e.#l(), and e1[e2]() *)
      | ( Call { Call.callee = (lookup_loc, callee_expr) as callee; targs; arguments; comments },
          Some
            ( member_opt,
              { Member._object; property; comments = member_comments },
              receiver_ast,
              orig_receiver
            )
        ) ->
        let (targts, targs) = convert_call_targs_opt cx targs in
        let expr_reason = mk_expression_reason ex in
        let specialized_callee = Context.new_specialized_callee cx in
        let ( filtered_out,
              lookup_voided_out,
              call_voided_out,
              member_lhs_t,
              prop_t,
              obj_filtered_out,
              object_ast,
              property,
              argument_asts,
              reason_lookup
            ) =
          match property with
          | Member.PropertyPrivateName (prop_loc, { Ast.PrivateName.name; comments = _ })
          | Member.PropertyIdentifier (prop_loc, { Ast.Identifier.name; comments = _ }) ->
            let reason_call = mk_reason (RMethodCall (Some name)) loc in
            let reason_prop = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
            let use_op =
              Op
                (FunCallMethod
                   {
                     op = expr_reason;
                     fn = mk_expression_reason orig_receiver;
                     prop = reason_prop;
                     args = mk_initial_arguments_reason arguments;
                     local = true;
                   }
                )
            in
            let prop_t = Tvar.mk cx reason_prop in
            let call_voided_out = Tvar.mk cx reason_call in
            let private_ =
              match property with
              | Member.PropertyExpression _ ->
                Utils_js.assert_false "unexpected property expression"
              | Member.PropertyPrivateName _ -> true
              | Member.PropertyIdentifier _ -> false
            in
            let get_opt_use argts _ =
              method_call_opt_use
                cx
                opt_state
                ~voided_out:call_voided_out
                reason_call
                ~use_op
                ~private_
                prop_loc
                (callee, name)
                loc
                targts
                argts
                (Some specialized_callee)
            in
            let handle_refined_callee argts obj_t f =
              Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason_call (fun t ->
                  let app =
                    mk_boundfunctioncalltype
                      ~call_kind:RegularCallKind
                      obj_t
                      targts
                      argts
                      t
                      ~call_strict_arity:true
                  in
                  Flow.unify cx f prop_t;
                  let call_t =
                    match opt_state with
                    | NewChain ->
                      let chain_reason = mk_reason ROptionalChain loc in
                      let lhs_reason = mk_expression_reason callee in
                      OptionalChainT
                        {
                          reason = chain_reason;
                          lhs_reason;
                          t_out =
                            CallT
                              {
                                use_op;
                                reason = reason_call;
                                call_action = Funcalltype app;
                                return_hint = Type_env.get_hint cx loc;
                              };
                          voided_out = OpenT t;
                        }
                    | _ ->
                      CallT
                        {
                          use_op;
                          reason = reason_call;
                          call_action = Funcalltype app;
                          return_hint = Type_env.get_hint cx loc;
                        }
                  in
                  Flow.flow cx (f, call_t)
              )
            in
            let get_mem_t argts reason obj_t =
              Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason_call (fun t ->
                  let use = apply_opt_use (get_opt_use argts reason) t in
                  Flow.flow cx (obj_t, use)
              )
            in
            let eval_args () = arg_list cx arguments in
            let conf =
              {
                ChainingConf.subexpressions = eval_args;
                get_result = get_mem_t;
                get_opt_use;
                refine =
                  (fun () ->
                    Refinement.get ~allow_optional:true cx (lookup_loc, callee_expr) lookup_loc);
                refinement_action = Some handle_refined_callee;
                get_reason = Fun.const expr_reason;
              }
            in
            let ( filtered_out,
                  lookup_voided_out,
                  member_lhs_t,
                  obj_filtered_out,
                  object_ast,
                  argument_asts
                ) =
              handle_chaining conf member_opt _object lookup_loc
            in
            let prop_ast =
              match property with
              | Member.PropertyExpression _ ->
                Utils_js.assert_false "unexpected property expression"
              | Member.PropertyPrivateName (_, id) -> Member.PropertyPrivateName (prop_loc, id)
              | Member.PropertyIdentifier (_, id) ->
                Member.PropertyIdentifier ((prop_loc, prop_t), id)
            in
            ( filtered_out,
              lookup_voided_out,
              call_voided_out,
              member_lhs_t,
              prop_t,
              obj_filtered_out,
              object_ast,
              prop_ast,
              argument_asts,
              reason_prop
            )
          | Member.PropertyExpression expr ->
            let reason_call = mk_reason (RMethodCall None) loc in
            let reason_lookup = mk_reason (RProperty None) lookup_loc in
            let use_op =
              Op
                (FunCallMethod
                   {
                     op = expr_reason;
                     fn = mk_expression_reason orig_receiver;
                     prop = mk_expression_reason expr;
                     args = mk_initial_arguments_reason arguments;
                     local = true;
                   }
                )
            in
            let call_voided_out = Tvar.mk cx expr_reason in
            let prop_t = Tvar.mk cx reason_lookup in
            let get_opt_use (argts, elem_t) _ =
              elem_call_opt_use
                opt_state
                ~voided_out:call_voided_out
                ~use_op
                ~reason_call
                ~reason_lookup
                ~reason_expr:expr_reason
                ~reason_chain:(mk_reason ROptionalChain loc)
                targts
                argts
                elem_t
                (Some specialized_callee)
            in
            let get_mem_t arg_and_elem_ts reason obj_t =
              Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason_call (fun t ->
                  let use = apply_opt_use (get_opt_use arg_and_elem_ts reason) t in
                  Flow.flow cx (obj_t, use)
              )
            in
            let eval_args_and_expr () =
              let (((_, elem_t), _) as expr) = expression cx expr in
              let (argts, arguments_ast) = arg_list cx arguments in
              ((argts, elem_t), (arguments_ast, expr))
            in
            let conf =
              {
                ChainingConf.refinement_action = None;
                subexpressions = eval_args_and_expr;
                get_result = get_mem_t;
                get_opt_use;
                refine = noop;
                get_reason = Fun.const expr_reason;
              }
            in
            let ( filtered_out,
                  lookup_voided_out,
                  member_lhs_t,
                  obj_filtered_out,
                  object_ast,
                  (argument_asts, expr_ast)
                ) =
              handle_chaining conf member_opt _object lookup_loc
            in
            ( filtered_out,
              lookup_voided_out,
              call_voided_out,
              member_lhs_t,
              prop_t,
              obj_filtered_out,
              object_ast,
              Member.PropertyExpression expr_ast,
              argument_asts,
              reason_lookup
            )
        in
        let voided_out =
          join_optional_branches lookup_voided_out call_voided_out |> normalize_voided_out
        in
        let lhs_t =
          Tvar_resolver.mk_tvar_and_fully_resolve_where cx (reason_of_t member_lhs_t) (fun t ->
              Flow.flow_t cx (member_lhs_t, t);
              Base.List.iter voided_out ~f:(fun out -> Flow.flow_t cx (out, t))
          )
        in
        let callee =
          ( (lookup_loc, prop_t),
            receiver_ast
              { Member._object = object_ast; property; comments = member_comments }
              obj_filtered_out
          )
        in
        let callee = specialize_callee callee specialized_callee in
        let sig_help =
          Flow_js_utils.CalleeRecorder.type_for_sig_help reason_lookup specialized_callee
        in
        let call =
          ( (loc, lhs_t),
            call_ast
              ~filtered_out
              ~sig_help
              { Call.callee; targs; arguments = argument_asts; comments }
          )
        in
        (filtered_out, voided_out, call)
      (* e1(e2...) *)
      | (Call { Call.callee; targs; arguments; comments }, None) ->
        let (targts, targs) = convert_call_targs_opt cx targs in
        let use_op =
          Op
            (FunCall
               {
                 op = mk_expression_reason ex;
                 fn = mk_expression_reason callee;
                 args = mk_initial_arguments_reason arguments;
                 local = true;
               }
            )
        in
        let spec_callee = Context.new_specialized_callee cx in
        let get_opt_use argts reason =
          func_call_opt_use cx loc reason ~use_op targts argts (Some spec_callee)
        in
        let get_reason lhs_t = mk_reason (RFunctionCall (desc_of_t lhs_t)) loc in
        let get_result argts reason f =
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
              let use = apply_opt_use (get_opt_use argts reason) t in
              Flow.flow cx (f, use)
          )
        in
        let eval_args () = arg_list cx arguments in
        let conf =
          {
            ChainingConf.refinement_action = None;
            subexpressions = eval_args;
            refine = noop;
            get_result;
            get_opt_use;
            get_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, argument_asts) =
          handle_chaining conf opt_state callee loc
        in
        let reason_callee = mk_expression_reason callee in
        let sig_help = Flow_js_utils.CalleeRecorder.type_for_sig_help reason_callee spec_callee in
        let exp callee =
          let callee = specialize_callee callee spec_callee in
          call_ast
            ~filtered_out
            ~sig_help
            { Call.callee; targs; arguments = argument_asts; comments }
        in
        (filtered_out, voided_out, ((loc, lhs_t), exp object_ast))
      | _ ->
        let (((_, t), _) as res) = expression ?cond cx ex in
        (t, [], res))

  and arg_list cx (args_loc, { Ast.Expression.ArgList.arguments; comments }) =
    let (argts, arg_asts) = arguments |> Base.List.map ~f:(expression_or_spread cx) |> List.split in
    (argts, (args_loc, { Ast.Expression.ArgList.arguments = arg_asts; comments }))

  and subscript ~cond cx ex =
    let (_, _, ast) = optional_chain ~cond cx ex in
    ast

  (* traverse a unary expression, return result type *)
  and unary cx ~cond loc =
    let open Ast.Expression.Unary in
    function
    | { operator = Not; argument; comments } ->
      let (((_, arg), _) as argument) = expression cx ?cond argument in
      let reason = mk_reason (RUnaryOperator ("not", desc_of_t arg)) loc in
      let tout =
        match cond with
        | Some _ -> BoolT.at loc
        | None ->
          Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
              Flow.flow cx (arg, NotT (reason, t))
          )
      in
      (tout, { operator = Not; argument; comments })
    | { operator = Plus; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      let reason = mk_reason (desc_of_t argt) loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun result_t ->
            Flow.flow cx (argt, UnaryArithT { reason; result_t; kind = UnaryArithKind.Plus })
        ),
        { operator = Plus; argument; comments }
      )
    | { operator = Minus; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      ( begin
          match argt with
          | DefT (reason, NumT (Literal (sense, (value, raw)))) ->
            (* special case for negative number literals, to avoid creating an unnecessary tvar. not
               having a tvar allows other special cases that match concrete lower bounds to proceed
               (notably, Object.freeze upgrades literal props to singleton types, and a tvar would
               make a negative number not look like a literal.) *)
            let annot_loc = loc in
            let reason = annot_reason ~annot_loc @@ repos_reason annot_loc reason in
            let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
            DefT (reason, NumT (Literal (sense, (value, raw))))
          | arg ->
            let reason = mk_reason (desc_of_t arg) loc in
            Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun result_t ->
                Flow.flow cx (arg, UnaryArithT { reason; result_t; kind = UnaryArithKind.Minus })
            )
        end,
        { operator = Minus; argument; comments }
      )
    | { operator = BitNot; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      let reason = mk_reason (desc_of_t argt) loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun result_t ->
            Flow.flow cx (argt, UnaryArithT { reason; result_t; kind = UnaryArithKind.BitNot })
        ),
        { operator = BitNot; argument; comments }
      )
    | { operator = Typeof; argument; comments } ->
      let argument = expression cx argument in
      (StrT.at loc, { operator = Typeof; argument; comments })
    | { operator = Void; argument; comments } ->
      let argument = expression cx argument in
      (VoidT.at loc, { operator = Void; argument; comments })
    | { operator = Ast.Expression.Unary.Delete; argument; comments } ->
      let argument = delete cx loc argument in
      (BoolT.at loc, { operator = Ast.Expression.Unary.Delete; argument; comments })
    | { operator = Await; argument; comments } ->
      let reason = mk_reason (RCustom "await") loc in
      let (((_, arg), _) as argument_ast) = expression cx argument in
      ( Type_operation_utils.Promise.await cx reason arg,
        { operator = Await; argument = argument_ast; comments }
      )

  (* numeric pre/post inc/dec *)
  and update cx loc expr =
    let open Ast.Expression.Update in
    let reason = mk_reason (RCustom "update") loc in
    let { argument; _ } = expr in
    let (((_, arg_t), _) as arg_ast) = expression cx argument in
    let result_t =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun result_t ->
          Flow.flow cx (arg_t, UnaryArithT { reason; result_t; kind = UnaryArithKind.Update })
      )
    in
    let arg_ast =
      match argument with
      | (_, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ })) ->
        (* enforce state-based guards for binding update, e.g., const *)
        let use_op =
          Op
            (AssignVar
               {
                 var = Some (mk_reason (RIdentifier (OrdinaryName name)) id_loc);
                 init = reason_of_t result_t;
               }
            )
        in
        Type_env.set_var cx ~use_op name result_t id_loc;
        arg_ast
      | (lhs_loc, Ast.Expression.Member mem) ->
        (* Updating involves both reading and writing. We need to model both of these, and ensuring
         * an arithmetic operand should use the read type, which is affected by refinements. *)
        let make_op ~lhs ~prop = Op (UpdateProperty { lhs; prop }) in
        let lhs_prop_reason = mk_expression_reason argument in
        let reconstruct_ast mem _ = Ast.Expression.Member mem in
        let arg_update_ast =
          assign_member
            cx
            ~make_op
            ~t:result_t
            ~lhs_loc
            ~reconstruct_ast
            ~lhs_prop_reason
            ~mode:Assign
            mem
        in
        arg_update_ast
      | _ -> arg_ast
    in
    (result_t, { expr with argument = arg_ast })

  (* Returns a function that type check LHS or RHS of eq_test under correct conditional context. *)
  and visit_eq_test cx ~cond loc left right =
    let check ~cond = expression cx ?cond in
    match cond with
    | None -> check ~cond:None
    | Some c ->
      let reconstruct_ast expr_under_cond e =
        if e == expr_under_cond then
          check ~cond:(Some OtherTest) e
        else
          check ~cond:None e
      in
      Eq_test.visit_eq_test
      (* Strict and sense don't influence whether we should propagate cond context. *)
        ~sense:false
        ~strict:false
        ~on_type_of_test:(fun _ expr _value _ _ -> reconstruct_ast expr)
        ~on_literal_test:(fun ~strict:_ ~sense:_ _ expr _ _value -> reconstruct_ast expr)
        ~on_null_test:(fun ~sense:_ ~strict:_ _ expr _value -> reconstruct_ast expr)
        ~on_void_test:(fun ~sense:_ ~strict:_ ~check_for_bound_undefined:_ _ expr _value ->
          reconstruct_ast expr)
        ~on_member_eq_other:(fun expr _value -> reconstruct_ast expr)
        ~on_other_eq_member:(fun _value expr -> reconstruct_ast expr)
        ~on_other_eq_test:(fun _ _ -> check ~cond:None)
        ~is_switch_cond_context:
          (match c with
          | SwitchTest _ -> true
          | OtherTest -> false)
        loc
        left
        right

  and matching_prop_check cx left right =
    let open Ast.Expression in
    let left =
      match left with
      | (annot, OptionalMember { OptionalMember.member; _ }) -> (annot, Member member)
      | _ -> left
    in
    match left with
    | ( _,
        Member
          {
            Member._object = ((_, obj_t), _);
            property =
              ( Member.PropertyIdentifier (_, { Ast.Identifier.name = pname; _ })
              | Member.PropertyExpression (_, StringLiteral { Ast.StringLiteral.value = pname; _ })
                );
            _;
          }
      ) ->
      let ((_, other_t), _) = right in
      Context.add_matching_props cx (pname, other_t, obj_t)
    | _ -> ()

  (* traverse a binary expression, return result type *)
  and binary cx loc ~cond { Ast.Expression.Binary.operator; left; right; comments } =
    let open Ast.Expression.Binary in
    match operator with
    | Equal
    | NotEqual ->
      let reconstruct_ast = visit_eq_test cx ~cond loc left right in
      let (((_, t1), _) as left) = reconstruct_ast left in
      let (((_, t2), _) as right) = reconstruct_ast right in
      let desc =
        RBinaryOperator
          ( Flow_ast_utils.string_of_binary_operator operator,
            desc_of_reason (reason_of_t t1),
            desc_of_reason (reason_of_t t2)
          )
      in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, EqT { reason; flip = false; arg = t2 });
      (BoolT.at loc, { operator; left; right; comments })
    | In ->
      let (loc1, _) = left in
      let (loc2, _) = right in
      let (((_, t1), _) as left) = expression cx left in
      let (((_, t2), _) as right) = expression cx right in
      let reason_lhs = mk_reason (RCustom "LHS of `in` operator") loc1 in
      let reason_rhs = mk_reason (RCustom "RHS of `in` operator") loc2 in
      Flow.flow cx (t1, AssertBinaryInLHST reason_lhs);
      Flow.flow cx (t2, AssertBinaryInRHST reason_rhs);
      (BoolT.at loc, { operator; left; right; comments })
    | StrictEqual
    | StrictNotEqual ->
      let reconstruct_ast = visit_eq_test cx ~cond loc left right in
      let (((_, t1), _) as left) = reconstruct_ast left in
      let (((_, t2), _) as right) = reconstruct_ast right in
      Base.Option.iter
        ~f:(fun _ ->
          matching_prop_check cx left right;
          (* If this is a switch statement only consider the case where the object
           * access in the discriminant. *)
          match cond with
          | Some (SwitchTest _) -> ()
          | _ -> matching_prop_check cx right left)
        cond;
      let desc =
        RBinaryOperator
          ( Flow_ast_utils.string_of_binary_operator operator,
            desc_of_reason (reason_of_t t1),
            desc_of_reason (reason_of_t t2)
          )
      in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, StrictEqT { reason; cond_context = cond; flip = false; arg = t2 });
      (BoolT.at loc, { operator; left; right; comments })
    | Instanceof ->
      let left = expression cx left in
      let (((right_loc, right_t), _) as right) = expression cx right in
      let reason_rhs = mk_reason (RCustom "RHS of `instanceof` operator") right_loc in
      Flow.flow cx (right_t, AssertInstanceofRHST reason_rhs);
      (BoolT.at loc, { operator; left; right; comments })
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual ->
      let (((_, t1), _) as left) = expression cx left in
      let (((_, t2), _) as right) = expression cx right in
      let desc =
        RBinaryOperator
          ( Flow_ast_utils.string_of_binary_operator operator,
            desc_of_reason (reason_of_t t1),
            desc_of_reason (reason_of_t t2)
          )
      in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, ComparatorT { reason; flip = false; arg = t2 });
      (BoolT.at loc, { operator; left; right; comments })
    | Plus
    | LShift
    | RShift
    | RShift3
    | Minus
    | Mult
    | Exp
    | Div
    | Mod
    | BitOr
    | Xor
    | BitAnd ->
      let (((_, t1), _) as left_ast) = expression cx left in
      let (((_, t2), _) as right_ast) = expression cx right in
      let desc =
        RBinaryOperator
          ("arithmetic operation", desc_of_reason (reason_of_t t1), desc_of_reason (reason_of_t t2))
      in
      let reason = mk_reason desc loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
            let use_op =
              Op
                (Arith
                   {
                     op = reason;
                     left = mk_expression_reason left;
                     right = mk_expression_reason right;
                   }
                )
            in
            Flow.flow
              cx
              ( t1,
                ArithT
                  {
                    use_op;
                    reason;
                    flip = false;
                    rhs_t = t2;
                    result_t = t;
                    kind = ArithKind.arith_kind_of_binary_operator operator;
                  }
              )
        ),
        { operator; left = left_ast; right = right_ast; comments }
      )

  and logical cx loc ~cond { Ast.Expression.Logical.operator; left; right; comments } =
    let open Ast.Expression.Logical in
    (* With logical operators the LHS is always evaluated. So if the LHS throws, the whole
     * expression throws. To model this we do not catch abnormal exceptions on the LHS.
     * As such, we only analyze the RHS expression if the LHS does not throw.
     * If the LHS does not throw, and the RHS does throw, then we cannot say that the
     * entire expression throws, because we only evaluate the RHS depending on the value of the LHS.
     * Thus, we catch abnormal control flow exceptions on the RHS and do not rethrow them.
     *)
    match operator with
    | Or ->
      let () = check_default_pattern cx left right in
      let (((_, t1), _) as left) = condition ~cond:OtherTest cx left in
      let ((((_, t2), _) as right), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx ?cond right)
      in
      let t2 =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at loc
        | None -> t2
      in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
            Flow.flow cx (t1, OrT (reason, t2, t))
        ),
        { operator = Or; left; right; comments }
      )
    | And ->
      let (((_, t1), _) as left) = condition ~cond:OtherTest cx left in
      let ((((_, t2), _) as right), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx ?cond right)
      in
      let t2 =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at loc
        | None -> t2
      in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
            Flow.flow cx (t1, AndT (reason, t2, t))
        ),
        { operator = And; left; right; comments }
      )
    | NullishCoalesce ->
      let (((_, t1), _) as left) = expression cx left in
      let ((((_, t2), _) as right), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx right)
      in
      let t2 =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at loc
        | None -> t2
      in
      let reason = mk_reason (RLogical ("??", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
            Flow.flow cx (t1, NullishCoalesceT (reason, t2, t))
        ),
        { operator = NullishCoalesce; left; right; comments }
      )

  and assignment_lhs cx patt =
    match patt with
    | ( pat_loc,
        Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (loc, name); optional; annot }
      ) ->
      let t = identifier cx name loc in
      ( (pat_loc, t),
        Ast.Pattern.Identifier
          {
            Ast.Pattern.Identifier.name = ((loc, t), name);
            annot =
              (match annot with
              | Ast.Type.Available annot ->
                Ast.Type.Available (Tast_utils.error_mapper#type_annotation annot)
              | Ast.Type.Missing hint -> Ast.Type.Missing (hint, AnyT.locationless Untyped));
            optional;
          }
      )
    | (loc, Ast.Pattern.Expression ((_, Ast.Expression.Member _) as m)) ->
      let (((_, t), _) as m) = expression cx m in
      ((loc, t), Ast.Pattern.Expression m)
    (* TODO: object, array and non-member expression patterns are invalid
       (should be a parse error but isn't yet) *)
    | (lhs_loc, Ast.Pattern.Object _)
    | (lhs_loc, Ast.Pattern.Array _)
    | (lhs_loc, Ast.Pattern.Expression _) ->
      Flow.add_output cx (Error_message.EInvalidLHSInAssignment lhs_loc);
      Tast_utils.error_mapper#pattern patt

  (* write a type t into a member.
     - the `optional` parameter should be set to NewChain when the member access
       is optional (a?.b) and should be ContinueChain when it is not itself
       optional but is part of an optional chain (a?.b.c). *)
  and assign_member
      cx ?(optional = NonOptional) ~make_op ~t ~lhs_loc ~lhs_prop_reason ~reconstruct_ast ~mode lhs
      =
    let open Ast.Expression in
    let maybe_chain lhs_reason use_t =
      match (optional, mode) with
      | (NewChain, Delete) ->
        let reason = mk_reason ROptionalChain lhs_loc in

        (* When deleting an optional chain, we only really care about the case
           where the object type is non-nullable. The specification is:

             delete a?.b
              is equivalent to
             a == null ? true : delete a.b
           So if a is null, no work has to be done. Hence, the nullable output
           for the optional chain is mixed.
        *)
        let mixed = MixedT.at lhs_loc in
        OptionalChainT { reason; lhs_reason; t_out = use_t; voided_out = mixed }
      | _ -> use_t
    in
    let typecheck_object obj =
      (* If we're deleting a member expression, it's allowed to be an optional chain, and we
         need to respect short-circuiting, which means the type that's flowed into the
         SetPropT (or similar) upper bound must be the "filtered," non-nullish type.
         However, syntactically `a?.x = e` is banned, so if this is an assignment expression,
         we should just use `expression` to evaluate the object. It still might contain
         an optional chain, but if so the chain is in parentheses (like `(a?.b).x = e`),
         which means that the type that flows into SetPropT should include the nullish
         case.
      *)
      match (optional, mode) with
      | ((NewChain | ContinueChain), Delete) ->
        let (o, _, _object) = optional_chain ~cond:None cx obj in
        (o, _object)
      | _ ->
        let (((_, o), _) as _object) = expression cx obj in
        (o, _object)
    in
    match lhs with
    (* super.name = e *)
    | {
     Member._object = (super_loc, Super super);
     property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments = _ } as id));
     comments;
    } ->
      let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
      let prop_name = OrdinaryName name in
      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let super_t = super_ cx super_loc in
      let prop_t =
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx prop_reason (fun prop_t ->
            let use_op =
              make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) prop_loc)
            in
            Flow.flow
              cx
              ( super_t,
                SetPropT
                  ( use_op,
                    reason,
                    mk_named_prop ~reason:prop_reason prop_name,
                    mode,
                    Normal,
                    t,
                    Some prop_t
                  )
              )
        )
      in
      let property = Member.PropertyIdentifier ((prop_loc, prop_t), id) in
      ( (lhs_loc, prop_t),
        reconstruct_ast
          { Member._object = ((super_loc, super_t), Super super); property; comments }
          prop_t
      )
    (* _object.#name = e *)
    | {
     Member._object;
     property =
       Member.PropertyPrivateName (prop_loc, { Ast.PrivateName.name; comments = _ }) as property;
     comments;
    } ->
      let lhs_reason = mk_expression_reason _object in
      let (o, _object) = typecheck_object _object in
      let wr_ctx =
        match (_object, Type_env.var_scope_kind cx) with
        | ((_, This _), Name_def.Ctor) -> ThisInCtor
        | _ -> Normal
      in
      let prop_t =
        let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
        (* flow type to object property itself *)
        let class_entries = Type_env.get_class_entries cx in
        let prop_reason = mk_reason (RPrivateProperty name) prop_loc in
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx prop_reason (fun prop_t ->
            let use_op =
              make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) prop_loc)
            in
            let upper =
              maybe_chain
                lhs_reason
                (SetPrivatePropT
                   (use_op, reason, name, mode, class_entries, false, wr_ctx, t, Some prop_t)
                )
            in
            Flow.flow cx (o, upper)
        )
      in
      ((lhs_loc, prop_t), reconstruct_ast { Member._object; property; comments } prop_t)
    (* _object.name = e *)
    | {
     Member._object;
     property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments = _ } as id));
     comments;
    } ->
      let wr_ctx =
        match (_object, Type_env.var_scope_kind cx) with
        | ((_, This _), Name_def.Ctor) -> ThisInCtor
        | _ -> Normal
      in
      let lhs_reason = mk_expression_reason _object in
      let (o, _object) = typecheck_object _object in
      let prop_t =
        let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
        let prop_name = OrdinaryName name in
        let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
        (* flow type to object property itself *)
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx prop_reason (fun prop_t ->
            let use_op =
              make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) prop_loc)
            in
            let upper =
              maybe_chain
                lhs_reason
                (SetPropT
                   ( use_op,
                     reason,
                     mk_named_prop ~reason:prop_reason prop_name,
                     mode,
                     wr_ctx,
                     t,
                     Some prop_t
                   )
                )
            in
            Flow.flow cx (o, upper)
        )
      in
      let lhs_t =
        match (_object, name) with
        | ( ( _,
              Ast.Expression.Identifier
                ((id_loc, _), { Ast.Identifier.name = "module"; comments = _ })
            ),
            "exports"
          )
          when not (Type_env.local_scope_entry_exists cx id_loc) ->
          (* module.exports has type `any` in theory, but shouldnt be treated as uncovered *)
          t
        | _ -> prop_t
      in
      let property = Member.PropertyIdentifier ((prop_loc, lhs_t), id) in
      ((lhs_loc, lhs_t), reconstruct_ast { Member._object; property; comments } prop_t)
    (* _object[index] = e *)
    | { Member._object; property = Member.PropertyExpression ((iloc, _) as index); comments } ->
      let reason = mk_reason (RPropertyAssignment None) lhs_loc in
      let lhs_reason = mk_expression_reason _object in
      let (o, _object) = typecheck_object _object in
      let (((_, i), _) as index) = expression cx index in
      let use_op = make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) iloc) in
      let upper = maybe_chain lhs_reason (SetElemT (use_op, reason, i, mode, t, None)) in
      Flow.flow cx (o, upper);

      (* types involved in the assignment itself are computed
         in pre-havoc environment. it's the assignment itself
         which clears refis *)
      ( (lhs_loc, t),
        reconstruct_ast { Member._object; property = Member.PropertyExpression index; comments } t
      )

  (* traverse simple assignment expressions (`lhs = rhs`) *)
  and simple_assignment cx _loc lhs rhs =
    let (((_, t), _) as typed_rhs) = expression cx rhs in
    let lhs =
      match lhs with
      | (lhs_loc, Ast.Pattern.Expression (pat_loc, Ast.Expression.Member mem)) ->
        let lhs_prop_reason = mk_pattern_reason lhs in
        let make_op ~lhs ~prop = Op (SetProperty { lhs; prop; value = mk_expression_reason rhs }) in
        let reconstruct_ast mem _ = Ast.Expression.Member mem in
        let ((lhs_loc, t), lhs) =
          assign_member cx ~make_op ~t ~lhs_loc ~lhs_prop_reason ~reconstruct_ast ~mode:Assign mem
        in
        ((lhs_loc, t), Ast.Pattern.Expression ((pat_loc, t), lhs))
      (* other r structures are handled as destructuring assignments *)
      | _ -> Destructuring.assignment cx t rhs lhs
    in
    (t, lhs, typed_rhs)

  and arith_assign cx ~reason ~lhs_reason ~rhs_reason lhs_t rhs_t kind =
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun result_t ->
        let use_op = Op (Arith { op = reason; left = lhs_reason; right = rhs_reason }) in
        Flow.flow cx (lhs_t, ArithT { use_op; reason; flip = false; rhs_t; result_t; kind })
    )

  (* traverse assignment expressions with operators (`lhs += rhs`, `lhs *= rhs`, etc) *)
  and op_assignment cx loc lhs op rhs =
    let open Ast.Expression in
    let reason = mk_reason (RCustom (Flow_ast_utils.string_of_assignment_operator op)) loc in
    let rhs_reason = mk_expression_reason rhs in
    let update_env result_t =
      match lhs with
      | ( _,
          Ast.Pattern.Identifier
            { Ast.Pattern.Identifier.name = (id_loc, { Ast.Identifier.name; comments = _ }); _ }
        ) ->
        let use_op =
          Op
            (AssignVar
               {
                 var = Some (mk_reason (RIdentifier (OrdinaryName name)) id_loc);
                 init = rhs_reason;
               }
            )
        in
        Type_env.set_var cx ~use_op name result_t id_loc
      | (lhs_loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
        let lhs_prop_reason = mk_pattern_reason lhs in
        let make_op ~lhs ~prop = Op (UpdateProperty { lhs; prop }) in
        let reconstruct_ast mem _ = Ast.Expression.Member mem in
        ignore
        @@ assign_member
             cx
             ~make_op
             ~t:result_t
             ~lhs_loc
             ~lhs_prop_reason
             ~reconstruct_ast
             ~mode:Assign
             mem
      | _ -> ()
    in
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
      let (((_, lhs_t), _) as lhs_ast) = assignment_lhs cx lhs in
      let (((_, rhs_t), _) as rhs_ast) = expression cx rhs in
      let result_t =
        arith_assign
          cx
          ~reason
          ~lhs_reason:(mk_pattern_reason lhs)
          ~rhs_reason
          lhs_t
          rhs_t
          (ArithKind.arith_kind_of_assignment_operator op)
      in
      (* enforce state-based guards for binding update, e.g., const *)
      let () = update_env result_t in
      (lhs_t, lhs_ast, rhs_ast)
    | Assignment.NullishAssign
    | Assignment.AndAssign
    | Assignment.OrAssign ->
      let (((_, lhs_t), _) as lhs_pattern_ast) = assignment_lhs cx lhs in
      let left_expr =
        match lhs with
        | (lhs_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
          Some (lhs_loc, Ast.Expression.Identifier name)
        | (lhs_loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
          Some (lhs_loc, Ast.Expression.Member mem)
        | _ -> None
      in
      (match left_expr with
      | None ->
        ( AnyT.error reason,
          lhs_pattern_ast,
          (fun () -> expression cx rhs) |> Abnormal.catch_expr_control_flow_exception |> fst
        )
      | Some left_expr ->
        (match op with
        | Assignment.NullishAssign ->
          let ((((_, rhs_t), _) as rhs_ast), right_abnormal) =
            Abnormal.catch_expr_control_flow_exception (fun () -> expression cx rhs)
          in
          let rhs_t =
            match right_abnormal with
            | Some Abnormal.Throw -> EmptyT.at loc
            | None -> rhs_t
          in
          let result_t =
            Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
                Flow.flow cx (lhs_t, NullishCoalesceT (reason, rhs_t, t))
            )
          in
          let () = update_env result_t in
          (lhs_t, lhs_pattern_ast, rhs_ast)
        | Assignment.AndAssign ->
          let ((_, lhs_t), _) = condition ~cond:OtherTest cx left_expr in
          let ((((_, rhs_t), _) as rhs_ast), right_abnormal) =
            Abnormal.catch_expr_control_flow_exception (fun () -> expression cx rhs)
          in
          let rhs_t =
            match right_abnormal with
            | Some Abnormal.Throw -> EmptyT.at loc
            | None -> rhs_t
          in
          let result_t =
            Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
                Flow.flow cx (lhs_t, AndT (reason, rhs_t, t))
            )
          in
          let () = update_env result_t in
          (lhs_t, lhs_pattern_ast, rhs_ast)
        | Assignment.OrAssign ->
          let () = check_default_pattern cx left_expr rhs in
          let ((_, lhs_t), _) = condition ~cond:OtherTest cx left_expr in
          let ((((_, rhs_t), _) as rhs_ast), right_abnormal) =
            Abnormal.catch_expr_control_flow_exception (fun () -> expression cx rhs)
          in
          let rhs_t =
            match right_abnormal with
            | Some Abnormal.Throw -> EmptyT.at loc
            | None -> rhs_t
          in
          let result_t =
            Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
                Flow.flow cx (lhs_t, OrT (reason, rhs_t, t))
            )
          in
          let () = update_env result_t in
          (lhs_t, lhs_pattern_ast, rhs_ast)
        | _ -> assert_false "Unexpected operator"))

  (* traverse assignment expressions *)
  and assignment cx loc (lhs, op, rhs) =
    match op with
    | None -> simple_assignment cx loc lhs rhs
    | Some op -> op_assignment cx loc lhs op rhs

  (* delete variables and properties *)
  and delete cx loc target =
    let open Ast.Expression in
    let void = VoidT.at loc in
    let (lhs_loc, targ_exp) = target in
    match targ_exp with
    | Member mem ->
      let lhs_prop_reason = mk_expression_reason target in
      let make_op ~lhs ~prop = Op (DeleteProperty { lhs; prop }) in
      let reconstruct_ast mem _ = Member mem in
      assign_member
        cx
        ~make_op
        ~t:void
        ~lhs_loc
        ~lhs_prop_reason
        ~reconstruct_ast
        ~mode:Type.Delete
        mem
    | OptionalMember { OptionalMember.member = mem; optional; filtered_out } ->
      let lhs_prop_reason = mk_expression_reason target in
      let make_op ~lhs ~prop = Op (DeleteProperty { lhs; prop }) in
      let reconstruct_ast mem ty =
        OptionalMember { OptionalMember.member = mem; optional; filtered_out = (filtered_out, ty) }
      in
      let opt_state =
        if optional then
          NewChain
        else
          ContinueChain
      in
      assign_member
        cx
        ~optional:opt_state
        ~make_op
        ~t:void
        ~lhs_loc
        ~lhs_prop_reason
        ~reconstruct_ast
        ~mode:Type.Delete
        mem
    | Identifier (loc, { Ast.Identifier.name; _ }) ->
      let use_op = Op (DeleteVar { var = mk_expression_reason target }) in
      Type_env.set_var cx ~use_op name void loc;
      expression cx target
    | _ ->
      let (((_, t), _) as target) = expression cx target in
      Flow.add_output cx Error_message.(ECannotDelete (loc, reason_of_t t));
      target

  and collapse_children cx (children_loc, children) :
      Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list) =
    let cache = Context.node_cache cx in
    match Node_cache.get_jsx_children cache children_loc with
    | Some result -> result
    | None ->
      let (unresolved_params, children') =
        children
        |> Base.List.fold ~init:([], []) ~f:(fun (unres_params, children) child ->
               let (unres_param_opt, child) = jsx_body cx child in
               ( Base.Option.value_map unres_param_opt ~default:unres_params ~f:(fun x ->
                     x :: unres_params
                 ),
                 child :: children
               )
           )
        |> map_pair List.rev List.rev
      in
      (unresolved_params, (children_loc, children'))

  and jsx cx expr_loc e : Type.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.element =
    let open Ast.JSX in
    let { opening_element; children; closing_element; comments } = e in
    let (children_loc, _) = children in
    let locs =
      let (open_, _) = opening_element in
      match closing_element with
      | Some _ -> (expr_loc, open_, children_loc)
      | _ -> (open_, open_, open_)
    in
    let (t, opening_element, children, closing_element) =
      jsx_title cx opening_element children closing_element locs
    in
    Tvar_resolver.resolve cx t;
    (t, { opening_element; children; closing_element; comments })

  and jsx_fragment cx expr_loc fragment : Type.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.fragment =
    let open Ast.JSX in
    let { frag_opening_element; frag_children; frag_closing_element; frag_comments } = fragment in
    let (loc_children, _) = frag_children in
    (* TODO: we could make it configurable like the jsx pragma, with the @jsxFrag directive.
     * See https://babeljs.io/docs/babel-plugin-transform-react-jsx#fragments *)
    let fragment_t =
      match Context.react_runtime cx with
      | Options.ReactRuntimeAutomatic ->
        let reason = mk_reason (RIdentifier (OrdinaryName "Fragment")) expr_loc in
        Flow.get_builtin_type cx reason "React$FragmentType"
      | Options.ReactRuntimeClassic ->
        let reason = mk_reason (RIdentifier (OrdinaryName "React.Fragment")) expr_loc in
        let react = Type_env.var_ref ~lookup_mode:ForValue cx (OrdinaryName "React") expr_loc in
        let use_op = Op (GetProperty reason) in
        get_prop ~cond:None cx reason ~use_op ~hint:hint_unavailable react (reason, "Fragment")
    in
    let (unresolved_params, frag_children) = collapse_children cx frag_children in
    let props =
      match react_jsx_normalize_children_prop cx loc_children unresolved_params with
      | None -> NullT.at expr_loc
      | Some fragment_children_prop ->
        let reason_props = mk_reason RReactProps loc_children in
        let props =
          NameUtils.Map.singleton
            (OrdinaryName "children")
            (Type.Field
               {
                 preferred_def_locs = None;
                 key_loc = None;
                 type_ = fragment_children_prop;
                 polarity = Polarity.Neutral;
               }
            )
        in
        Obj_type.mk_with_proto
          cx
          reason_props
          ~obj_kind:Exact
          ~frozen:false
          ~props
          (ObjProtoT reason_props)
    in
    let (t, _) =
      react_jsx_desugar
        cx
        ~loc_element:expr_loc
        ~loc_children
        "React.Fragment"
        fragment_t
        None
        props
    in
    Tvar_resolver.resolve cx t;
    (t, { frag_opening_element; frag_children; frag_closing_element; frag_comments })

  and jsx_title cx opening_element children closing_element locs =
    let open Ast.JSX in
    let (loc_element, loc_opening, loc_children) = locs in
    let (loc, { Opening.name; targs; attributes; self_closing }) = opening_element in
    let targs_with_tast_opt =
      Base.Option.map targs ~f:(fun (targts_loc, args) ->
          let (targs, targs_tast) = convert_call_targs cx Subst_name.Map.empty args in
          (targs, (targts_loc, targs_tast))
      )
    in
    let targs_opt = Base.Option.map ~f:fst targs_with_tast_opt in
    let targs_tast_opt = Base.Option.map ~f:snd targs_with_tast_opt in
    let facebook_fbs = Context.facebook_fbs cx in
    let facebook_fbt = Context.facebook_fbt cx in
    let jsx_mode = Context.jsx cx in
    let (t, name, attributes, children) =
      match (name, jsx_mode, (facebook_fbs, facebook_fbt)) with
      | ( Identifier (loc_id, ({ Identifier.name = "fbs" as name; comments = _ } as id)),
          _,
          (Some custom_jsx_type, _)
        )
      | ( Identifier (loc_id, ({ Identifier.name = "fbt" as name; comments = _ } as id)),
          _,
          (_, Some custom_jsx_type)
        ) ->
        let fbt_reason = mk_reason RFbt loc_element in
        let t = Flow.get_builtin_type cx fbt_reason custom_jsx_type in
        (* TODO check attribute types against an fbt API *)
        let (_, attributes, _, children) =
          jsx_mk_props
            cx
            fbt_reason
            ~check_expression:(Statement.expression ?cond:None ?as_const:None ?frozen:None)
            ~collapse_children
            name
            attributes
            children
        in
        let name = Identifier ((loc_id, t), id) in
        (t, name, attributes, children)
      | (Identifier (loc, { Identifier.name; comments }), _, _) ->
        if Type_inference_hooks_js.dispatch_id_hook cx name loc then
          let t = Unsoundness.at InferenceHooks loc_element in
          let name = Identifier ((loc, t), { Identifier.name; comments }) in
          let attributes =
            Base.List.map ~f:Tast_utils.error_mapper#jsx_opening_attribute attributes
          in
          let (_, children) = collapse_children cx children in
          (t, name, attributes, children)
        else
          let reason =
            match jsx_mode with
            | Options.Jsx_react ->
              mk_reason
                (RReactElement
                   { name_opt = Some (OrdinaryName name); from_component_syntax = false }
                )
                loc_element
            | Options.Jsx_pragma _ -> mk_reason (RJSXElement (Some name)) loc_element
          in
          let c =
            if name = String.capitalize_ascii name then
              identifier cx (mk_ident ~comments:None name) loc
            else begin
              Type_env.intrinsic_ref cx (OrdinaryName name) loc
              |> Base.Option.iter ~f:(fun (t, def_loc) ->
                     Flow.flow
                       cx
                       ( t,
                         AssertNonComponentLikeT
                           (def_loc, mk_reason (RIdentifier (OrdinaryName name)) loc)
                       )
                 );
              let strt = SingletonStrT (OrdinaryName name) in
              DefT (mk_reason (RIdentifier (OrdinaryName name)) loc, strt)
            end
          in
          let (o, attributes', unresolved_params, children) =
            jsx_mk_props
              cx
              reason
              ~check_expression:(Statement.expression ?cond:None ?as_const:None)
              ~collapse_children
              name
              attributes
              children
          in
          let (t, c_opt) =
            match Context.jsx cx with
            | Options.Jsx_react ->
              let (loc_element, _loc_opening, loc_children) = locs in
              react_jsx_desugar cx name ~loc_element ~loc_children c targs_opt o
            | Options.Jsx_pragma (raw_jsx_expr, jsx_expr) ->
              non_react_jsx_desugar
                cx
                ~raw_jsx_expr
                ~jsx_expr
                ~loc_element
                ~loc_opening
                c
                targs_opt
                o
                attributes
                unresolved_params
          in
          let c = Base.Option.value c_opt ~default:c in
          let name = Identifier ((loc, c), { Identifier.name; comments }) in
          (t, name, attributes', children)
      | (MemberExpression member, Options.Jsx_react, _) ->
        let name = jsx_title_member_to_string member in
        let el =
          RReactElement { name_opt = Some (OrdinaryName name); from_component_syntax = false }
        in
        let reason = mk_reason el loc_element in
        let m_expr = jsx_title_member_to_expression member in
        let ((m_loc, t), m_expr') = expression cx m_expr in
        let c = mod_reason_of_t (replace_desc_reason (RIdentifier (OrdinaryName name))) t in
        let (o, attributes', _unresolved_params, children) =
          jsx_mk_props
            cx
            reason
            ~check_expression:(Statement.expression ?cond:None ?as_const:None)
            ~collapse_children
            name
            attributes
            children
        in
        let (t, _) = react_jsx_desugar cx ~loc_element ~loc_children name c targs_opt o in
        let member' =
          match expression_to_jsx_title_member m_loc m_expr' with
          | Some member -> member
          | None -> Tast_utils.error_mapper#jsx_member_expression member
        in
        (t, MemberExpression member', attributes', children)
      | (MemberExpression member, Options.Jsx_pragma _, _) ->
        let t = Unsoundness.at InferenceHooks loc_element in
        let name' = Tast_utils.error_mapper#jsx_element_name name in
        let el_name = jsx_title_member_to_string member in
        let reason = mk_reason (RJSXElement (Some el_name)) loc_element in
        let (_o, attributes', _, children) =
          jsx_mk_props
            cx
            reason
            ~check_expression:(Statement.expression ?cond:None ?as_const:None)
            ~collapse_children
            el_name
            attributes
            children
        in
        (t, name', attributes', children)
      | (NamespacedName namespace, _, _) ->
        (* TODO? covers namespaced names as element names *)
        let t = Unsoundness.at InferenceHooks loc_element in
        let name' = Tast_utils.error_mapper#jsx_element_name name in
        let el_name = jsx_title_namespaced_name_to_string namespace in
        let reason = mk_reason (RJSXElement (Some el_name)) loc_element in
        let (_o, attributes', _, children) =
          jsx_mk_props
            cx
            reason
            ~check_expression:(Statement.expression ?cond:None ?as_const:None)
            ~collapse_children
            el_name
            attributes
            children
        in
        (t, name', attributes', children)
    in
    let closing_element =
      match closing_element with
      | Some (c_loc, { Closing.name = cname }) ->
        Some (c_loc, { Closing.name = jsx_match_closing_element name cname })
      | None -> None
    in
    ( t,
      (loc, { Opening.name; targs = targs_tast_opt; self_closing; attributes }),
      children,
      closing_element
    )

  and jsx_match_closing_element =
    let match_identifiers o_id c_id =
      let ((_, t), _) = o_id in
      let (loc, name) = c_id in
      ((loc, t), name)
    in
    let rec match_member_expressions o_mexp c_mexp =
      let open Ast.JSX.MemberExpression in
      let (_, { _object = o_obj; property = o_prop }) = o_mexp in
      let (loc, { _object = c_obj; property = c_prop }) = c_mexp in
      let _object = match_objects o_obj c_obj in
      let property = match_identifiers o_prop c_prop in
      (loc, { _object; property })
    and match_objects o_obj c_obj =
      match (o_obj, c_obj) with
      | (Ast.JSX.MemberExpression.Identifier o_id, Ast.JSX.MemberExpression.Identifier c_id) ->
        Ast.JSX.MemberExpression.Identifier (match_identifiers o_id c_id)
      | ( Ast.JSX.MemberExpression.MemberExpression o_exp,
          Ast.JSX.MemberExpression.MemberExpression c_exp
        ) ->
        Ast.JSX.MemberExpression.MemberExpression (match_member_expressions o_exp c_exp)
      | (_, _) -> Tast_utils.error_mapper#jsx_member_expression_object c_obj
    in
    let match_namespaced_names o_id c_id =
      let (_, { Ast.JSX.NamespacedName.namespace = o_ns; name = o_name }) = o_id in
      let (loc, { Ast.JSX.NamespacedName.namespace = c_ns; name = c_name }) = c_id in
      let namespace = match_identifiers o_ns c_ns in
      let name = match_identifiers o_name c_name in
      (loc, { Ast.JSX.NamespacedName.namespace; name })
    in
    (* Transfer open types to close types *)
    fun o_name c_name ->
      let open Ast.JSX in
      match (o_name, c_name) with
      | (Identifier o_id, Identifier c_id) -> Identifier (match_identifiers o_id c_id)
      | (NamespacedName o_nname, NamespacedName c_nname) ->
        NamespacedName (match_namespaced_names o_nname c_nname)
      | (MemberExpression o_mexp, MemberExpression c_mexp) ->
        MemberExpression (match_member_expressions o_mexp c_mexp)
      | (_, _) -> Tast_utils.error_mapper#jsx_element_name c_name

  and jsx_mk_props cx reason ~check_expression ~collapse_children name attributes children =
    let open Ast.JSX in
    let is_react = Context.jsx cx = Options.Jsx_react in
    let reason_props =
      replace_desc_reason
        ( if is_react then
          RReactProps
        else
          RJSXElementProps name
        )
        reason
    in
    (* Use the same reason for proto and the ObjT so we can walk the proto chain
       and use the root proto reason to build an error. *)
    let proto = ObjProtoT reason_props in
    let (acc, atts) =
      List.fold_left
        (fun (acc, atts) att ->
          match att with
          (* All attributes with a non-namespaced name that are not a react ignored
           * attribute. *)
          | Opening.Attribute
              ( attr_loc,
                {
                  Attribute.name =
                    Attribute.Identifier (id_loc, { Identifier.name = aname; comments = acomments });
                  value;
                }
              ) ->
            (* Get the type for the attribute's value. *)
            let (atype, value) =
              match value with
              (* <element name="literal" /> *)
              | Some (Attribute.StringLiteral (loc, lit)) ->
                let t = string_literal cx ~singleton:false loc lit in
                (t, Some (Attribute.StringLiteral ((loc, t), lit)))
              (* <element name={expression} /> *)
              | Some
                  (Attribute.ExpressionContainer
                    ( ec_loc,
                      {
                        ExpressionContainer.expression = ExpressionContainer.Expression (loc, e);
                        comments;
                      }
                    )
                    ) ->
                let (((_, t), _) as e) = check_expression cx (loc, e) in
                ( t,
                  Some
                    (Attribute.ExpressionContainer
                       ( (ec_loc, t),
                         {
                           ExpressionContainer.expression = ExpressionContainer.Expression e;
                           comments;
                         }
                       )
                    )
                )
              (* <element name={} /> *)
              | Some (Attribute.ExpressionContainer _ as ec) ->
                let t = EmptyT.at attr_loc in
                (t, Some (Tast_utils.unchecked_mapper#jsx_attribute_value ec))
              (* <element name /> *)
              | None -> (DefT (mk_reason RBoolean attr_loc, BoolT (Some true)), None)
            in
            let acc =
              if Type_inference_hooks_js.dispatch_jsx_hook cx aname id_loc then
                (* don't add `aname` to the prop map because it is the autocomplete token *)
                acc
              else
                ObjectExpressionAcc.add_prop
                  (Properties.add_field
                     (OrdinaryName aname)
                     Polarity.Neutral
                     ~key_loc:(Some id_loc)
                     atype
                  )
                  acc
            in
            let att =
              Opening.Attribute
                ( attr_loc,
                  {
                    Attribute.name =
                      Attribute.Identifier
                        ((id_loc, atype), { Identifier.name = aname; comments = acomments });
                    value;
                  }
                )
            in
            (acc, att :: atts)
          (* Do nothing for namespaced attributes or ignored React attributes. *)
          | Opening.Attribute _ ->
            (* TODO: attributes with namespaced names *)
            (acc, atts)
          (* <element {...spread} /> *)
          | Opening.SpreadAttribute (spread_loc, { SpreadAttribute.argument; comments }) ->
            let (((_, spread), _) as argument) = check_expression cx argument in
            let acc = ObjectExpressionAcc.add_spread spread acc in
            let att =
              Opening.SpreadAttribute (spread_loc, { SpreadAttribute.argument; comments })
            in
            (acc, att :: atts))
        (ObjectExpressionAcc.empty (), [])
        attributes
    in
    let attributes = List.rev atts in
    let (unresolved_params, ((loc_children, _) as children)) = collapse_children cx children in
    let acc =
      match unresolved_params with
      | [] -> acc
      (* We add children to the React.createElement() call for React. Not to the
       * props as other JSX users may support. *)
      | _ when is_react ->
        (match react_jsx_normalize_children_prop cx loc_children unresolved_params with
        | None -> acc
        | Some children_prop ->
          ObjectExpressionAcc.add_prop
            (Properties.add_field
               (OrdinaryName "children")
               Polarity.Neutral
               ~key_loc:None
               children_prop
            )
            acc)
      | _ ->
        let arr =
          Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
              let reason_op = reason in
              let element_reason =
                replace_desc_reason
                  (Reason.RInferredUnionElemArray { instantiable = false })
                  reason_op
              in
              let elem_t = Tvar.mk cx element_reason in
              Flow.resolve_spread_list
                cx
                ~use_op:unknown_use
                ~reason_op:reason
                unresolved_params
                (ResolveSpreadsToArrayLiteral { id = mk_id (); as_const = false; elem_t; tout })
          )
        in
        ObjectExpressionAcc.add_prop
          (Properties.add_field (OrdinaryName "children") Polarity.Neutral ~key_loc:None arr)
          acc
    in
    let t =
      ObjectExpressionAcc.mk_object_from_spread_acc
        cx
        acc
        reason_props
        ~as_const:false
        ~frozen:false
        ~default_proto:proto
    in
    (t, attributes, unresolved_params, children)

  and react_jsx_normalize_children_prop cx loc_children children =
    children
    |> Base.List.map ~f:(function
           | UnresolvedArg (TupleElement { t; _ }, _) -> t
           | UnresolvedSpreadArg a ->
             Flow.add_output
               cx
               (Error_message.EUnsupportedSyntax
                  (loc_children, Flow_intermediate_error_types.SpreadArgument)
               );
             reason_of_t a |> AnyT.error
           )
    |> TypeUtil.normalize_jsx_children_prop loc_children

  and react_jsx_desugar cx name ~loc_element ~loc_children component_t targs_opt props =
    let return_hint = Type_env.get_hint cx loc_element in
    let reason =
      mk_reason
        (RReactElement { name_opt = Some (OrdinaryName name); from_component_syntax = false })
        loc_element
    in
    let (tout, instantiated_component, use_op) =
      let reason_jsx = mk_reason (RFunction RNormal) loc_element in
      let reason_c = reason_of_t component_t in
      let use_op =
        Op
          (ReactCreateElementCall { op = reason_jsx; component = reason_c; children = loc_children })
      in
      let tout = OpenT (reason, Tvar.mk_no_wrap cx reason) in
      let specialized_component = Context.new_specialized_callee cx in
      Flow.flow
        cx
        ( component_t,
          ReactKitT
            ( use_op,
              reason,
              React.CreateElement
                {
                  component = component_t;
                  jsx_props = props;
                  tout;
                  targs = targs_opt;
                  return_hint;
                  record_monomorphized_result = false;
                  inferred_targs = None;
                  specialized_component = Some specialized_component;
                }
            )
        );
      let specialized_component_t =
        Flow_js_utils.CalleeRecorder.type_for_tast_opt reason_c specialized_component
      in
      (tout, specialized_component_t, use_op)
    in
    (match Context.react_runtime cx with
    | Options.ReactRuntimeAutomatic ->
      (* TODO(jmbrown): Model jsx more faithfully. children are now passed in as part of the props
       * object. See https://github.com/reactjs/rfcs/blob/createlement-rfc/text/0000-create-element-changes.md
       * for more details. *)
      ()
    | Options.ReactRuntimeClassic ->
      (* Under classic jsx, we trust but verify:
       * - We first unconditionally call the right createElement (already done above)
       * - Then we validate that we are calling the right one. By modeling React$CreateElement
       *   as an opaque type bounded by the real definition, we can reliable check it. *)
      (* Validate that we are actually calling the right React.createElement *)
      let react_t = Type_env.var_ref ~lookup_mode:ForValue cx (OrdinaryName "React") loc_element in
      let create_element_t =
        get_prop
          ~cond:None
          cx
          reason
          ~use_op
          ~hint:hint_unavailable
          react_t
          (mk_reason (RProperty (Some (OrdinaryName "createElement"))) loc_element, "createElement")
      in
      if
        not
          (Speculation_flow.is_flow_successful
             cx
             reason
             create_element_t
             (UseT
                (unknown_use, Flow.get_builtin_type cx reason ~use_desc:false "React$CreateElement")
             )
          )
      then
        Flow_js_utils.add_output
          cx
          (Error_message.EInvalidReactCreateElement
             { create_element_loc = loc_element; invalid_react = reason_of_t react_t }
          ));
    (tout, instantiated_component)

  and non_react_jsx_desugar
      cx
      ~raw_jsx_expr
      ~jsx_expr
      ~loc_element
      ~loc_opening
      component_t
      targs_opt
      props
      attributes
      children =
    let reason = mk_reason (RJSXFunctionCall raw_jsx_expr) loc_element in
    (* A JSX element with no attributes should pass in null as the second
     * arg *)
    let props =
      match attributes with
      | [] -> NullT.at loc_opening
      | _ -> props
    in
    let argts =
      [Arg component_t; Arg props]
      @ Base.List.map
          ~f:(function
            | UnresolvedArg (TupleElement { t; _ }, _) -> Arg t
            | UnresolvedSpreadArg c -> SpreadArg c)
          children
    in
    let use_op = Op (JSXCreateElement { op = reason; component = reason_of_t component_t }) in
    let open Ast.Expression in
    let t =
      match jsx_expr with
      | ( _,
          Member
            {
              Member._object;
              property = Member.PropertyIdentifier (prop_loc, { Ast.Identifier.name; comments = _ });
              _;
            }
        ) ->
        let ot = jsx_pragma_expression cx raw_jsx_expr (fst _object) _object in
        snd
          (method_call
             cx
             reason
             ~use_op
             ~call_strict_arity:false
             prop_loc
             (jsx_expr, ot, name)
             targs_opt
             argts
          )
      | _ ->
        let f = jsx_pragma_expression cx raw_jsx_expr loc_element jsx_expr in
        func_call cx loc_element reason ~use_op ~call_strict_arity:false f targs_opt argts None
    in
    (t, None)

  (* The @jsx pragma specifies a left hand side expression EXPR such that
   *
   * <Foo />
   *
   * is transformed into
   *
   * EXPR(Foo, props, child1, child2, etc)
   *
   * This means we need to process EXPR. However, EXPR is not inline in the code,
   * it's up in a comment at the top of the file. This means if we run into an
   * error, we're going to point at the comment at the top.
   *
   * We can cover almost all the cases by just explicitly handling identifiers,
   * since the common error is that the identifier is not in scope.
   *)
  and jsx_pragma_expression cx raw_jsx_expr loc =
    let open Ast.Expression in
    function
    | (_, Identifier (_, { Ast.Identifier.name; comments = _ })) ->
      let desc = RJSXIdentifier (raw_jsx_expr, name) in
      Type_env.var_ref ~lookup_mode:ForValue cx (OrdinaryName name) loc ~desc
    | expr ->
      (* Oh well, we tried *)
      let ((_, t), _) = expression cx expr in
      t

  and jsx_body cx (loc, child) =
    let open Ast.JSX in
    match child with
    | Element e ->
      let (t, e) = jsx cx loc e in
      let reason = mk_reason RJSXChild loc in
      (Some (UnresolvedArg (mk_tuple_element reason t, None)), ((loc, t), Element e))
    | Fragment f ->
      let (t, f) = jsx_fragment cx loc f in
      let reason = mk_reason RJSXChild loc in
      (Some (UnresolvedArg (mk_tuple_element reason t, None)), ((loc, t), Fragment f))
    | ExpressionContainer ec ->
      ExpressionContainer.(
        let { expression = ex; ExpressionContainer.comments } = ec in
        let (unresolved_param, ex, t) =
          match ex with
          | Expression e ->
            let (((loc, t), _) as e) = expression cx e in
            let reason = mk_reason RJSXChild loc in
            (Some (UnresolvedArg (mk_tuple_element reason t, None)), Expression e, t)
          | EmptyExpression -> (None, EmptyExpression, AnyT.at Untyped loc)
        in
        ( unresolved_param,
          ((loc, t), ExpressionContainer { expression = ex; ExpressionContainer.comments })
        )
      )
    | SpreadChild { SpreadChild.expression = expr; comments } ->
      let (((_, t), _) as e) = expression cx expr in
      ( Some (UnresolvedSpreadArg t),
        ((loc, t), SpreadChild { SpreadChild.expression = e; comments })
      )
    | Text { Text.value; raw } ->
      let unresolved_param_opt =
        match jsx_trim_text loc value with
        | Some c ->
          let reason = mk_reason RJSXChild loc in
          Some (UnresolvedArg (mk_tuple_element reason c, None))
        | None -> None
      in
      (unresolved_param_opt, ((loc, AnyT.at Untyped loc), Text { Text.value; raw }))

  and jsx_trim_text loc value =
    match Utils_jsx.trim_jsx_text (ALoc.to_loc_exn loc) value with
    | Some (loc, trimmed) ->
      Some
        (DefT
           ( mk_reason RJSXText (loc |> ALoc.of_loc),
             StrT (Type.Literal (None, OrdinaryName trimmed))
           )
        )
    | None -> None

  and jsx_title_member_to_string (_, member) =
    let open Ast.JSX.MemberExpression in
    let (_, { Ast.JSX.Identifier.name; comments = _ }) = member.property in
    match member._object with
    | MemberExpression member -> jsx_title_member_to_string member ^ "." ^ name
    | Identifier (_, { Ast.JSX.Identifier.name = obj; comments = _ }) -> obj ^ "." ^ name

  and jsx_title_namespaced_name_to_string namespaced_name =
    let (_, { Ast.JSX.NamespacedName.namespace = (_, namespace); name = (_, name) }) =
      namespaced_name
    in
    namespace.Ast.JSX.Identifier.name ^ name.Ast.JSX.Identifier.name

  and jsx_title_member_to_expression member =
    let (mloc, member) = member in
    let _object =
      let open Ast.JSX.MemberExpression in
      match member._object with
      | MemberExpression member -> jsx_title_member_to_expression member
      | Identifier (loc, { Ast.JSX.Identifier.name = "this"; comments }) ->
        (loc, Ast.Expression.This { Ast.Expression.This.comments })
      | Identifier (loc, { Ast.JSX.Identifier.name; comments }) ->
        (loc, Ast.Expression.Identifier (loc, mk_ident ~comments name))
    in
    let property =
      let open Ast.JSX.MemberExpression in
      let (loc, { Ast.JSX.Identifier.name; comments }) = member.property in
      (loc, mk_ident ~comments name)
    in
    let open Ast.Expression.Member in
    ( mloc,
      Ast.Expression.Member { _object; property = PropertyIdentifier property; comments = None }
    )

  (* reverses jsx_title_member_to_expression *)
  and expression_to_jsx_title_member loc member =
    match member with
    | Ast.Expression.Member.(
        Ast.Expression.Member
          {
            _object = ((mloc, tobj), obj_expr);
            property = PropertyIdentifier (pannot, { Ast.Identifier.name; comments });
            comments = _;
          }) ->
      let _object =
        match obj_expr with
        | Ast.Expression.This { Ast.Expression.This.comments } ->
          Some
            (Ast.JSX.MemberExpression.Identifier
               ((mloc, tobj), { Ast.JSX.Identifier.name = "this"; comments })
            )
        | Ast.Expression.Identifier ((id_loc, t), { Ast.Identifier.name; comments }) ->
          Some
            (Ast.JSX.MemberExpression.Identifier ((id_loc, t), { Ast.JSX.Identifier.name; comments })
            )
        | _ ->
          expression_to_jsx_title_member mloc obj_expr
          |> Base.Option.map ~f:(fun e -> Ast.JSX.MemberExpression.MemberExpression e)
      in
      let property = (pannot, { Ast.JSX.Identifier.name; comments }) in
      Base.Option.map _object ~f:(fun _object ->
          (loc, Ast.JSX.MemberExpression.{ _object; property })
      )
    | _ -> None

  (* Conditional expressions are checked like expressions, except that property
     accesses are provisionally allowed even when such properties do not exist.
     This accommodates the common JavaScript idiom of testing for the existence
     of a property before using that property. *)
  and condition cx ~cond e : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t = expression ~cond cx e

  and get_private_field_opt_use cx reason ~use_op name =
    let class_entries = Type_env.get_class_entries cx in
    OptGetPrivatePropT (use_op, reason, name, class_entries, false)

  (* Property lookups become non-strict when processing conditional expressions
     (see above).

     TODO: It should be possible to factor the processing of LHS / reference
     expressions out of `expression`, somewhat like what assignment_lhs does. That
     would make everything involving Refinement be in the same place.
  *)
  and get_prop_opt_use ~cond reason ~use_op ~hint (prop_reason, name) =
    let id = mk_id () in
    let prop_name = OrdinaryName name in
    if Base.Option.is_some cond then
      OptTestPropT (use_op, reason, id, mk_named_prop ~reason:prop_reason prop_name, hint)
    else
      OptGetPropT
        {
          use_op;
          reason;
          id = Some id;
          propref = mk_named_prop ~reason:prop_reason prop_name;
          hint;
        }

  and get_prop ~cond cx reason ~use_op ~hint tobj (prop_reason, name) =
    let opt_use = get_prop_opt_use ~cond reason ~use_op ~hint (prop_reason, name) in
    Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun t ->
        let get_prop_u = apply_opt_use opt_use t in
        Flow.flow cx (tobj, get_prop_u)
    )

  and static_method_call_Object cx loc callee_loc prop_loc expr obj_t m targs args =
    let open Ast.Expression in
    let reason = mk_reason (RCustom (spf "`Object.%s`" m)) loc in
    let use_op =
      Op
        (FunCallMethod
           {
             op = reason;
             fn = mk_reason (RMethod (Some m)) callee_loc;
             prop = mk_reason (RProperty (Some (OrdinaryName m))) prop_loc;
             args = mk_initial_arguments_reason args;
             local = true;
           }
        )
    in
    let get_keys ~arr_reason obj_t =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx arr_reason (fun tvar ->
          let keys_reason =
            update_desc_reason
              (fun desc -> RCustom (spf "element of %s" (string_of_desc desc)))
              reason
          in
          Flow.flow cx (obj_t, GetKeysT (keys_reason, UseT (use_op, tvar)))
      )
    in
    let get_values ~arr_reason obj_t =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx arr_reason (fun tvar ->
          Flow.flow cx (obj_t, GetDictValuesT (reason, UseT (use_op, tvar)))
      )
    in
    match (m, targs, args) with
    | ("create", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let (((_, e_t), _) as e_ast) = expression cx e in
      let proto =
        let reason = mk_reason RPrototype (fst e) in
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
            Flow.flow cx (e_t, ObjTestProtoT (reason, t))
        )
      in
      ( Obj_type.mk_with_proto cx reason ~obj_kind:Exact proto,
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ( "create",
        None,
        ( args_loc,
          {
            ArgList.arguments =
              [
                Expression e;
                Expression (obj_loc, Object { Object.properties; comments = obj_comments });
              ];
            comments;
          }
        )
      ) ->
      error_on_this_uses_in_object_methods cx properties;
      let (((_, e_t), _) as e_ast) = expression cx e in
      let proto =
        let reason = mk_reason RPrototype (fst e) in
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
            Flow.flow cx (e_t, ObjTestProtoT (reason, t))
        )
      in
      let (pmap, properties) = prop_map_of_object cx properties in
      let propdesc_type = Flow_js_utils.lookup_builtin_type cx "PropertyDescriptor" reason in
      let props =
        NameUtils.Map.fold
          (fun x p acc ->
            let loc = Property.read_loc p in
            match Property.read_t p with
            | None ->
              (* Since the properties object must be a literal, and literal objects
                 can only ever contain neutral fields, this should not happen. *)
              Flow.add_output
                cx
                Error_message.(EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead));
              acc
            | Some spec ->
              let reason =
                update_desc_reason
                  (fun desc ->
                    RCustom (spf ".%s of %s" (display_string_of_name x) (string_of_desc desc)))
                  reason
              in
              let t =
                Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tvar ->
                    let loc = loc_of_reason reason in
                    let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [tvar] in
                    Flow.flow cx (spec, UseT (use_op, propdesc))
                )
              in
              let p =
                Field
                  {
                    preferred_def_locs = None;
                    key_loc = loc;
                    type_ = t;
                    polarity = Polarity.Neutral;
                  }
              in
              NameUtils.Map.add x p acc)
          pmap
          NameUtils.Map.empty
      in
      ( Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~props proto,
        None,
        ( args_loc,
          {
            ArgList.arguments =
              [
                Expression e_ast;
                (* TODO(vijayramamurthy) construct object type *)
                Expression
                  ( (obj_loc, AnyT.at Untyped obj_loc),
                    Object { Object.properties; comments = obj_comments }
                  );
              ];
            comments;
          }
        )
      )
    | ( ("getOwnPropertyNames" | "keys"),
        None,
        (args_loc, { ArgList.arguments = [Expression e]; comments })
      ) ->
      let arr_reason = mk_reason RArrayType loc in
      let (((_, o), _) as e_ast) = expression cx e in
      let keys_t = get_keys ~arr_reason o in
      ( DefT (arr_reason, ArrT (ArrayAT { elem_t = keys_t; tuple_view = None; react_dro = None })),
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ("values", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let arr_reason = mk_reason RArrayType loc in
      let (((_, o), _) as e_ast) = expression cx e in
      ( DefT
          ( arr_reason,
            ArrT (ArrayAT { elem_t = get_values ~arr_reason o; tuple_view = None; react_dro = None })
          ),
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ("entries", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let arr_reason = mk_reason RArrayType loc in
      let (((_, o), _) as e_ast) = expression cx e in
      let keys_t = get_keys ~arr_reason o in
      let values_t = get_values ~arr_reason o in
      let elem_reason = mk_reason (RTupleElement { name = None }) loc in
      let elem_t = UnionT (elem_reason, UnionRep.make keys_t values_t []) in
      let entry_t =
        DefT
          ( mk_reason RTupleType loc,
            ArrT
              (TupleAT
                 {
                   elem_t;
                   react_dro = None;
                   elements =
                     [
                       mk_tuple_element ~name:"key" elem_reason keys_t;
                       mk_tuple_element ~name:"value" elem_reason values_t;
                     ];
                   arity = (2, 2);
                   inexact = false;
                 }
              )
          )
      in
      ( DefT (arr_reason, ArrT (ArrayAT { elem_t = entry_t; tuple_view = None; react_dro = None })),
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ( "defineProperty",
        ( None
        | Some
            (_, { CallTypeArgs.arguments = [Ast.Expression.CallTypeArg.Explicit _]; comments = _ })
          ),
        ( args_loc,
          {
            ArgList.arguments =
              [
                Expression e;
                Expression
                  ((ploc, Ast.Expression.StringLiteral { Ast.StringLiteral.value = x; _ }) as key);
                Expression config;
              ];
            comments;
          }
        )
      ) ->
      let (ty, targs) =
        match targs with
        | None -> (Tvar.mk cx reason, None)
        | Some
            ( targs_loc,
              { CallTypeArgs.arguments = [Ast.Expression.CallTypeArg.Explicit targ]; comments }
            ) ->
          let (((_, ty), _) as targ) = Anno.convert cx Subst_name.Map.empty targ in
          ( ty,
            Some
              ( targs_loc,
                { CallTypeArgs.arguments = [Ast.Expression.CallTypeArg.Explicit targ]; comments }
              )
          )
        | _ -> assert_false "unexpected type argument to Object.defineProperty, match guard failed"
      in
      let loc = loc_of_reason reason in
      let propdesc_type = Flow_js_utils.lookup_builtin_type cx "PropertyDescriptor" reason in
      let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [ty] in
      let (((_, o), _) as e_ast) = expression cx e in
      let key_ast = expression cx key in
      let (((_, spec), _) as config_ast) = expression cx config in
      let prop_name = OrdinaryName x in
      let prop_reason = mk_reason (RProperty (Some prop_name)) ploc in
      Flow.flow cx (spec, UseT (use_op, propdesc));
      let prop_t = Tvar.mk cx prop_reason in
      Flow.flow
        cx
        ( o,
          SetPropT
            ( use_op,
              reason,
              mk_named_prop ~reason:prop_reason prop_name,
              Assign,
              Normal,
              ty,
              Some prop_t
            )
        );
      ( o,
        targs,
        ( args_loc,
          {
            ArgList.arguments = [Expression e_ast; Expression key_ast; Expression config_ast];
            comments;
          }
        )
      )
    | ( "defineProperties",
        None,
        ( args_loc,
          {
            ArgList.arguments =
              [
                Expression e;
                Expression (obj_loc, Object { Object.properties; comments = obj_comments });
              ];
            comments;
          }
        )
      ) ->
      error_on_this_uses_in_object_methods cx properties;
      let (((_, o), _) as e_ast) = expression cx e in
      let (pmap, properties) = prop_map_of_object cx properties in
      let propdesc_type = Flow_js_utils.lookup_builtin_type cx "PropertyDescriptor" reason in
      pmap
      |> NameUtils.Map.iter (fun x p ->
             match Property.read_t p with
             | None ->
               (* Since the properties object must be a literal, and literal objects
                  can only ever contain neutral fields, this should not happen. *)
               Flow.add_output
                 cx
                 Error_message.(EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead))
             | Some spec ->
               let reason =
                 update_desc_reason
                   (fun desc ->
                     RCustom (spf ".%s of %s" (display_string_of_name x) (string_of_desc desc)))
                   reason
               in
               let tvar = Tvar.mk cx reason in
               let loc = loc_of_reason reason in
               let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [tvar] in
               Flow.flow cx (spec, UseT (use_op, propdesc));
               Flow.flow
                 cx
                 (o, SetPropT (use_op, reason, mk_named_prop ~reason x, Assign, Normal, tvar, None))
         );
      ( o,
        None,
        ( args_loc,
          {
            ArgList.arguments =
              [
                Expression e_ast;
                (* TODO(vijayramamurthy) construct object type *)
                Expression
                  ( (obj_loc, AnyT.at Untyped obj_loc),
                    Object { Object.properties; comments = obj_comments }
                  );
              ];
            comments;
          }
        )
      )
    (* Freezing an object literal is supported since there's no way it could
       have been mutated elsewhere *)
    | ( "freeze",
        ((None | Some (_, { CallTypeArgs.arguments = [_]; comments = _ })) as targs),
        (args_loc, { ArgList.arguments = [Expression (arg_loc, Object o)]; comments })
      ) ->
      let targs =
        Base.Option.map
          ~f:(fun (loc, targs) -> (loc, convert_call_targs cx Subst_name.Map.empty targs))
          targs
      in
      let (((_, arg_t), _) as e_ast) =
        let { Object.properties; comments } = o in
        error_on_this_uses_in_object_methods cx properties;
        let reason = mk_reason (RFrozen RObjectLit) arg_loc in
        let (t, properties) = object_ ~frozen:true ~as_const:false cx reason properties in
        ((arg_loc, t), Object { Object.properties; comments })
      in
      let reason = mk_reason (RMethodCall (Some m)) loc in
      ( snd
          (method_call
             cx
             reason
             prop_loc
             ~use_op
             (expr, obj_t, m)
             (Base.Option.map ~f:(snd %> fst) targs)
             [Arg arg_t]
          ),
        Base.Option.map ~f:(fun (loc, targs) -> (loc, snd targs)) targs,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ( ( "create" | "getOwnPropertyNames" | "keys" | "defineProperty" | "defineProperties"
        | "freeze" ),
        Some (targs_loc, targs),
        _
      ) ->
      let targs = snd (convert_call_targs cx Subst_name.Map.empty targs) in
      let (_argts, args) = arg_list cx args in
      let arity =
        if m = "freeze" || m = "defineProperty" then
          1
        else
          0
      in
      Flow.add_output
        cx
        Error_message.(
          ECallTypeArity
            {
              call_loc = loc;
              is_new = false;
              reason_arity = Reason.(locationless_reason (RFunction RNormal));
              expected_arity = arity;
            }
        );
      (AnyT.at (AnyError None) loc, Some (targs_loc, targs), args)
    (* TODO *)
    | _ ->
      let (targts, targ_asts) = convert_call_targs_opt cx targs in
      let (argts, arg_asts) = arg_list cx args in
      let reason = mk_reason (RMethodCall (Some m)) loc in
      let use_op =
        Op
          (FunCallMethod
             {
               op = reason;
               fn = mk_reason (RMethod (Some m)) callee_loc;
               prop = mk_reason (RProperty (Some (OrdinaryName m))) prop_loc;
               args = mk_initial_arguments_reason args;
               local = true;
             }
          )
      in
      ( snd (method_call cx reason ~use_op prop_loc (expr, obj_t, m) targts argts),
        targ_asts,
        arg_asts
      )

  and mk_class cx class_loc ~name_loc ?tast_class_type reason c =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_class node_cache class_loc with
    | Some x ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Class cache hit at %s" (ALoc.debug_to_string (loc_of_reason reason))]);
      x
    | None ->
      let def_reason = repos_reason class_loc reason in
      let (class_t, _, class_sig, class_ast_f) = mk_class_sig cx ~name_loc ~class_loc reason c in

      let public_property_map =
        Class_stmt_sig.fields_to_prop_map cx
        @@ Class_stmt_sig.public_fields_of_signature ~static:false class_sig
      in
      let private_property_map =
        Class_stmt_sig.fields_to_prop_map cx
        @@ Class_stmt_sig.private_fields_of_signature ~static:false class_sig
      in
      Class_stmt_sig.check_signature_compatibility cx def_reason class_sig;
      Class_stmt_sig.toplevels cx class_sig;

      let class_body = Ast.Class.((snd c.body).Body.body) in
      Context.add_voidable_check
        cx
        {
          Context.public_property_map;
          private_property_map;
          errors = Property_assignment.eval_property_assignment class_body;
        };
      let tast_class_type = Base.Option.value tast_class_type ~default:class_t in
      (class_t, class_ast_f tast_class_type)

  (* Process a class definition, returning a (polymorphic) class type. A class
     type is a wrapper around an instance type, which contains types of instance
     members, a pointer to the super instance type, and a container for types of
     static members. The static members can be thought of as instance members of a
     "metaclass": thus, the static type is itself implemented as an instance
     type. *)
  and mk_class_sig =
    let open Class_stmt_sig_types in
    (* Given information about a field, returns:
       - Class_sig.field representation of this field
       - typed AST of the field's type annotation
       - a function which will return a typed AST of the field's initializer expression.
         Function should only be called after Class_sig.toplevels has been called on a
         Class_sig.t containing this field, as that is when the initializer expression
         gets checked.
    *)
    let mk_field cx tparams_map reason annot init =
      let unconditionally_required_annot annot =
        match annot with
        | Ast.Type.Missing loc ->
          Flow.add_output
            cx
            (Error_message.EMissingLocalAnnotation
               { reason; hint_available = false; from_generic_function = false }
            );
          let t = AnyT.make (AnyError (Some MissingAnnotation)) reason in
          (t, Ast.Type.Missing (loc, t))
        | Ast.Type.Available annot ->
          let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
          (t, Ast.Type.Available ast_annot)
      in
      match init with
      | Ast.Class.Property.Declared ->
        let (annot_t, ast_annot) = unconditionally_required_annot annot in
        (Annot annot_t, annot_t, ast_annot, Fun.const Ast.Class.Property.Declared)
      | Ast.Class.Property.Uninitialized ->
        let (annot_t, ast_annot) = unconditionally_required_annot annot in
        (Annot annot_t, annot_t, ast_annot, Fun.const Ast.Class.Property.Uninitialized)
      | Ast.Class.Property.Initialized expr ->
        (* TODO(pvekris) expr could be an `as const` *)
        let (annot_t, annot_ast) =
          match (expr, annot) with
          | ((loc, Ast.Expression.StringLiteral lit), Ast.Type.Missing annot_loc) ->
            let t = string_literal cx ~singleton:false loc lit in
            (t, Ast.Type.Missing (annot_loc, t))
          | ((loc, Ast.Expression.BooleanLiteral lit), Ast.Type.Missing annot_loc) ->
            let t = boolean_literal ~singleton:false loc lit in
            (t, Ast.Type.Missing (annot_loc, t))
          | ((loc, Ast.Expression.NumberLiteral lit), Ast.Type.Missing annot_loc) ->
            let t = number_literal ~singleton:false loc lit in
            (t, Ast.Type.Missing (annot_loc, t))
          | ((loc, Ast.Expression.BigIntLiteral lit), Ast.Type.Missing annot_loc) ->
            let t = bigint_literal loc ~singleton:false lit in
            (t, Ast.Type.Missing (annot_loc, t))
          | ((loc, Ast.Expression.RegExpLiteral _), Ast.Type.Missing annot_loc) ->
            let t = regexp_literal cx loc in
            (t, Ast.Type.Missing (annot_loc, t))
          | ( (_, Ast.Expression.(ArrowFunction function_ | Function function_)),
              Ast.Type.Missing annot_loc
            ) ->
            let { Ast.Function.sig_loc; _ } = function_ in
            let cache = Context.node_cache cx in
            let (this_t, arrow, function_loc_opt) =
              match expr with
              | (_, Ast.Expression.ArrowFunction _) ->
                (dummy_this (loc_of_reason reason), true, None)
              | (loc, _) ->
                let this_t = Flow_js_utils.default_this_type cx ~needs_this_param:true function_ in
                (this_t, false, Some loc)
            in
            let ((func_sig, _) as sig_data) =
              mk_func_sig
                cx
                ~constructor:false
                ~getset:false
                ~require_return_annot:true
                ~statics:SMap.empty
                tparams_map
                reason
                function_
            in
            if Context.typing_mode cx = Context.CheckingMode then
              Node_cache.set_function_sig cache sig_loc sig_data;
            let t =
              Statement.Func_stmt_sig.functiontype cx ~arrow function_loc_opt this_t func_sig
            in
            (t, Ast.Type.Missing (annot_loc, t))
          | (_, Ast.Type.Missing annot_loc) ->
            Flow.add_output
              cx
              (Error_message.EMissingLocalAnnotation
                 {
                   reason = repos_reason annot_loc reason;
                   hint_available = false;
                   from_generic_function = false;
                 }
              );
            let t = AnyT.make (AnyError (Some MissingAnnotation)) reason in
            (t, Ast.Type.Missing (annot_loc, t))
          | (_, Ast.Type.Available annot) ->
            let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
            (t, Ast.Type.Available ast_annot)
        in
        let value_ref : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option ref = ref None in
        let (annot_loc, annot_or_inferred) =
          match annot with
          | Ast.Type.Missing loc -> (loc, Inferred annot_t)
          | Ast.Type.Available (loc, _) -> (loc, Annotated annot_t)
        in
        ( Infer
            ( Func_stmt_sig.field_initializer reason expr annot_loc annot_or_inferred,
              (fun (_, _, value_opt) -> value_ref := Some (Base.Option.value_exn value_opt))
            ),
          annot_t,
          annot_ast,
          fun () ->
            Ast.Class.Property.Initialized
              (Base.Option.value !value_ref ~default:(Tast_utils.error_mapper#expression expr))
        )
    in
    let mk_method cx ~constructor ~getset =
      mk_func_sig
        cx
        ~require_return_annot:(not constructor)
        ~constructor
        ~getset
        ~statics:SMap.empty
    in
    let mk_extends cx tparams_map = function
      | None -> (Implicit { null = false }, (fun () -> None))
      | Some (loc, { Ast.Class.Extends.expr; targs; comments }) ->
        let (c, expr) =
          let open Ast.Expression in
          let rec super_expr (loc, expr) =
            match expr with
            | Identifier (id_loc, ({ Ast.Identifier.name; comments = _ } as id)) ->
              let t = Type_env.sig_var_ref cx (OrdinaryName name) id_loc in
              (t, (fun () -> ((loc, t), Identifier ((id_loc, t), id))))
            | Member
                {
                  Member._object;
                  property =
                    Member.PropertyIdentifier (ploc, ({ Ast.Identifier.name; comments = _ } as id));
                  comments;
                } ->
              let (t, _object_f) = super_expr _object in
              let expr_reason = mk_expression_reason (loc, expr) in
              let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) ploc in
              let use_op = Op (GetProperty expr_reason) in
              let tout =
                Type_annotation_cons_gen.FlowJS.get_prop
                  cx
                  use_op
                  ~op_reason:expr_reason
                  prop_reason
                  (OrdinaryName name)
                  t
              in
              ( tout,
                fun () ->
                  ( (loc, tout),
                    Member
                      {
                        Member._object = _object_f ();
                        property = Member.PropertyIdentifier ((ploc, tout), id);
                        comments;
                      }
                  )
              )
            | AsExpression ({ AsExpression.expression = expr; annot; comments } as cast) ->
              let casting_syntax = Context.casting_syntax cx in
              let open Options.CastingSyntax in
              (match casting_syntax with
              | As
              | Both ->
                let (t, annot') = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
                ( t,
                  fun () ->
                    let (((_, infer_t), _) as e') = expression cx expr in
                    let use_op =
                      Op (Cast { lower = mk_expression_reason expr; upper = reason_of_t t })
                    in
                    Flow.flow cx (infer_t, TypeCastT (use_op, t));
                    ( (loc, t),
                      AsExpression { AsExpression.expression = e'; annot = annot'; comments }
                    )
                )
              | Colon ->
                Flow_js_utils.add_output
                  cx
                  (Error_message.EInvalidTypeCastSyntax
                     { loc; enabled_casting_syntax = casting_syntax }
                  );
                let t = AnyT.at (AnyError None) loc in
                ( t,
                  (fun () -> ((loc, t), AsExpression (Tast_utils.error_mapper#as_expression cast)))
                ))
            | TypeCast ({ TypeCast.expression = expr; annot; comments } as cast) ->
              let casting_syntax = Context.casting_syntax cx in
              let open Options.CastingSyntax in
              (match casting_syntax with
              | Colon
              | Both ->
                let (t, annot') = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
                ( t,
                  fun () ->
                    let (((_, infer_t), _) as e') = expression cx expr in
                    let use_op =
                      Op (Cast { lower = mk_expression_reason expr; upper = reason_of_t t })
                    in
                    Flow.flow cx (infer_t, TypeCastT (use_op, t));
                    ((loc, t), TypeCast { TypeCast.expression = e'; annot = annot'; comments })
                )
              | As ->
                Flow_js_utils.add_output
                  cx
                  (Error_message.EInvalidTypeCastSyntax
                     { loc; enabled_casting_syntax = casting_syntax }
                  );
                let t = AnyT.at (AnyError None) loc in
                (t, (fun () -> ((loc, t), TypeCast (Tast_utils.error_mapper#type_cast cast)))))
            | _ ->
              Flow.add_output cx Error_message.(EInvalidExtends (mk_expression_reason (loc, expr)));
              (AnyT.at (AnyError None) loc, (fun () -> expression cx (loc, expr)))
          in
          super_expr expr
        in
        let (t, targs) = Anno.mk_super cx tparams_map loc c targs in
        (Explicit t, (fun () -> Some (loc, { Ast.Class.Extends.expr = expr (); targs; comments })))
    in
    let mk_class_sig_with_self cx ~name_loc ~class_loc reason self cls =
      let node_cache = Context.node_cache cx in
      match Node_cache.get_class_sig node_cache class_loc with
      | Some x ->
        Debug_js.Verbose.print_if_verbose_lazy
          cx
          (lazy [spf "Class sig cache hit at %s" (ALoc.debug_to_string (loc_of_reason reason))]);
        x
      | None ->
        let {
          Ast.Class.id;
          body = (body_loc, { Ast.Class.Body.body = elements; comments = body_comments });
          tparams;
          extends;
          implements;
          class_decorators;
          comments;
        } =
          cls
        in
        let class_decorators_ast =
          Base.List.map ~f:Tast_utils.error_mapper#class_decorator class_decorators
        in
        let (tparams, tparams_map, tparams_ast) = Anno.mk_type_param_declarations cx tparams in
        let (this_tparam, this_t) = Class_stmt_sig.mk_this ~self cx reason in
        let tparams_map_with_this =
          Subst_name.Map.add (Subst_name.Name "this") this_t tparams_map
        in
        let class_name = Base.Option.map id ~f:(fun (_, { Ast.Identifier.name; _ }) -> name) in
        let (class_sig, extends_ast_f, implements_ast) =
          let id = Context.make_aloc_id cx name_loc in
          let (extends, extends_ast_f) = mk_extends cx tparams_map extends in
          let (implements, implements_ast) =
            match implements with
            | None -> ([], None)
            | Some (implements_loc, { Ast.Class.Implements.interfaces; comments }) ->
              let (implements, interfaces_ast) =
                interfaces
                |> Base.List.map ~f:(fun (loc, i) ->
                       let {
                         Ast.Class.Implements.Interface.id =
                           (id_loc, ({ Ast.Identifier.name; comments = _ } as id));
                         targs;
                       } =
                         i
                       in
                       let c = Type_env.get_var ~lookup_mode:ForType cx name id_loc in
                       let (typeapp, targs) =
                         match targs with
                         | None -> ((loc, c, None), None)
                         | Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
                           let (ts, targs_ast) = Anno.convert_list cx tparams_map targs in
                           ( (loc, c, Some ts),
                             Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs_ast; comments })
                           )
                       in
                       ( typeapp,
                         (loc, { Ast.Class.Implements.Interface.id = ((id_loc, c), id); targs })
                       )
                   )
                |> List.split
              in
              ( implements,
                Some (implements_loc, { Ast.Class.Implements.interfaces = interfaces_ast; comments })
              )
          in
          let super =
            Class { Class_stmt_sig_types.extends; mixins = []; implements; this_t; this_tparam }
          in
          ( Class_stmt_sig.empty id class_name class_loc reason tparams tparams_map super,
            extends_ast_f,
            implements_ast
          )
        in
        (* In case there is no constructor, pick up a default one. *)
        let class_sig =
          if extends <> None then
            (* Subclass default constructors are technically of the form (...args) =>
               { super(...args) }, but we can approximate that using flow's existing
               inheritance machinery. *)
            (* TODO: Does this distinction matter for the type checker? *)
            class_sig
          else
            let reason = replace_desc_reason RDefaultConstructor reason in
            Class_stmt_sig.add_default_constructor reason class_sig
        in
        (* All classes have a static "name" property. *)
        let class_sig = Class_stmt_sig.add_name_field class_sig in

        let check_duplicate_name public_seen_names member_loc name ~static ~private_ kind =
          if private_ then
            (* duplicate private names are a parser error - so we don't need to check them *)
            public_seen_names
          else
            let names_map =
              if static then
                public_seen_names.static_names
              else
                public_seen_names.instance_names
            in
            let names_map' =
              match SMap.find_opt name names_map with
              | Some seen ->
                (match (kind, seen) with
                | (Class_Member_Getter, Class_Member_Setter)
                | (Class_Member_Setter, Class_Member_Getter) ->
                  (* One getter and one setter are allowed as long as it's not used as a field
                     We use the special type here to indicate we've seen both a getter and a
                     setter for the name so that future getters/setters can have an error raised. *)
                  SMap.add name Class_Member_GetterSetter names_map
                | _ ->
                  Flow.add_output
                    cx
                    Error_message.(EDuplicateClassMember { loc = member_loc; name; static });
                  names_map)
              | None -> SMap.add name kind names_map
            in
            if static then
              { public_seen_names with static_names = names_map' }
            else
              { public_seen_names with instance_names = names_map' }
        in

        (* NOTE: We used to mine field declarations from field assignments in a
           constructor as a convenience, but it was not worth it: often, all that did
           was exchange a complaint about a missing field for a complaint about a
           missing annotation. Moreover, it caused fields declared in the super class
           to be redeclared if they were assigned in the constructor. So we don't do
           it. In the future, we could do it again, but only for private fields. *)

        (* NOTE: field initializer expressions and method bodies don't get checked
           until Class_sig.toplevels is called on class_sig. For this reason rather
           than returning a typed AST, we'll return a function which returns a typed
           AST, and this function shouldn't be called until after Class_sig.toplevels
           has been called.

           If a field/method ever gets shadowed later in the class, then its
           initializer/body (respectively) will not get checked, and the corresponding
           nodes of the typed AST will be filled in with error nodes.
        *)
        let (class_sig, rev_elements, _) =
          List.fold_left
            (let open Ast.Class in
            fun (c, rev_elements, public_seen_names) ->
              let add_method_sig_and_element
                  ~method_loc
                  ~name
                  ~id_loc
                  ~func_loc
                  ~func
                  ~kind
                  ~private_
                  ~static
                  ~decorators
                  ~comments
                  ~get_typed_method_key =
                let decorators =
                  Base.List.map ~f:Tast_utils.error_mapper#class_decorator decorators
                in
                (match kind with
                | Method.Get
                | Method.Set ->
                  Flow_js.add_output cx (Error_message.EUnsafeGettersSetters method_loc)
                | _ -> ());
                let reason =
                  Ast.Function.(func_reason ~async:func.async ~generator:func.generator method_loc)
                in
                let (method_sig, reconstruct_func) =
                  mk_method
                    cx
                    ~constructor:(kind = Method.Constructor)
                    ~getset:(kind = Method.Get || kind = Method.Set)
                    tparams_map_with_this
                    reason
                    func
                in
                (* The body of a class method doesn't get checked until Class_sig.toplevels
                   is called on the class sig (in this case c). The order of how the methods
                   were arranged in the class is lost by the time this happens, so rather
                   than attempting to return a list of method bodies from the Class_sig.toplevels
                   function, we have it place the function bodies into a list via side effects.
                   We use a similar approach for method types *)
                let params_ref : (ALoc.t, ALoc.t * Type.t) Ast.Function.Params.t option ref =
                  ref None
                in
                let body_ref : (ALoc.t, ALoc.t * Type.t) Ast.Function.body option ref = ref None in
                let set_asts (params_opt, body_opt, _) =
                  params_ref := Some (Base.Option.value_exn params_opt);
                  body_ref := Some (Base.Option.value_exn body_opt)
                in
                let func_t_ref : Type.t option ref = ref None in
                let set_type t = func_t_ref := Some t in
                let get_element () =
                  let params =
                    Base.Option.value
                      !params_ref
                      ~default:(Tast_utils.error_mapper#function_params func.Ast.Function.params)
                  in
                  let body =
                    Base.Option.value
                      !body_ref
                      ~default:(Tast_utils.error_mapper#function_body_any func.Ast.Function.body)
                  in
                  let func_t = Base.Option.value !func_t_ref ~default:(EmptyT.at id_loc) in
                  let func = reconstruct_func params body func_t in
                  Body.Method
                    ( (method_loc, func_t),
                      {
                        Method.key = get_typed_method_key func_t;
                        value = (func_loc, func);
                        kind;
                        static;
                        decorators;
                        comments;
                      }
                    )
                in
                let (add, class_member_kind) =
                  match kind with
                  | Method.Constructor ->
                    let add =
                      Class_stmt_sig.add_constructor ~id_loc:(Some id_loc) ~set_asts ~set_type
                    in
                    (add, None)
                  | Method.Method ->
                    let add =
                      if private_ then
                        Class_stmt_sig.add_private_method
                          ~static
                          name
                          ~id_loc
                          ~this_write_loc:(Some func_loc)
                          ~set_asts
                          ~set_type
                      else
                        Class_stmt_sig.add_method
                          ~static
                          name
                          ~id_loc
                          ~this_write_loc:(Some func_loc)
                          ~set_asts
                          ~set_type
                    in
                    (add, Some Class_Member_Method)
                  | Method.Get ->
                    let add =
                      Class_stmt_sig.add_getter
                        ~static
                        name
                        ~id_loc
                        ~this_write_loc:(Some func_loc)
                        ~set_asts
                        ~set_type
                    in
                    (add, Some Class_Member_Getter)
                  | Method.Set ->
                    let add =
                      Class_stmt_sig.add_setter
                        ~static
                        name
                        ~id_loc
                        ~this_write_loc:(Some func_loc)
                        ~set_asts
                        ~set_type
                    in
                    (add, Some Class_Member_Setter)
                in
                let public_seen_names' =
                  match class_member_kind with
                  | Some k -> check_duplicate_name public_seen_names id_loc name ~static ~private_ k
                  | None -> public_seen_names
                in
                (add ~func_sig:method_sig c, get_element :: rev_elements, public_seen_names')
              in
              function
              (* instance and static methods *)
              | Body.Property (_, { Property.key = Ast.Expression.Object.Property.PrivateName _; _ })
                ->
                failwith "Internal Error: Found non-private field with private name"
              | Body.Method
                  ( method_loc,
                    {
                      Method.key =
                        Ast.Expression.Object.Property.PrivateName
                          (id_loc, ({ Ast.PrivateName.name; comments = _ } as id));
                      value = (func_loc, func);
                      kind;
                      static;
                      decorators;
                      comments;
                    }
                  ) ->
                add_method_sig_and_element
                  ~method_loc
                  ~name
                  ~id_loc
                  ~func_loc
                  ~func
                  ~kind
                  ~private_:true
                  ~static
                  ~decorators
                  ~comments
                  ~get_typed_method_key:(fun _ ->
                    Ast.Expression.Object.Property.PrivateName (id_loc, id)
                )
              | Body.Method
                  ( method_loc,
                    {
                      Method.key =
                        Ast.Expression.Object.Property.Identifier
                          (id_loc, ({ Ast.Identifier.name; comments = _ } as id));
                      value = (func_loc, func);
                      kind;
                      static;
                      decorators;
                      comments;
                    }
                  ) ->
                add_method_sig_and_element
                  ~method_loc
                  ~name
                  ~id_loc
                  ~func_loc
                  ~func
                  ~kind
                  ~private_:false
                  ~static
                  ~decorators
                  ~comments
                  ~get_typed_method_key:(fun func_t ->
                    Ast.Expression.Object.Property.Identifier ((id_loc, func_t), id)
                )
              (* fields *)
              | Body.PrivateField
                  ( loc,
                    {
                      PrivateField.key = (id_loc, { Ast.PrivateName.name; comments = _ }) as key;
                      annot;
                      value;
                      static;
                      variance;
                      decorators;
                      comments;
                    }
                  ) ->
                let reason = mk_reason (RPrivateProperty name) loc in
                let polarity = Anno.polarity cx variance in
                let decorators =
                  Base.List.map ~f:Tast_utils.error_mapper#class_decorator decorators
                in
                let (field, annot_t, annot_ast, get_value) =
                  mk_field cx tparams_map_with_this reason annot value
                in
                let get_element () =
                  Body.PrivateField
                    ( (loc, annot_t),
                      {
                        PrivateField.key;
                        annot = annot_ast;
                        value = get_value ();
                        static;
                        variance;
                        decorators;
                        comments;
                      }
                    )
                in
                let public_seen_names' =
                  check_duplicate_name
                    public_seen_names
                    id_loc
                    name
                    ~static
                    ~private_:true
                    Class_Member_Field
                in
                ( Class_stmt_sig.add_private_field ~static name id_loc polarity field c,
                  get_element :: rev_elements,
                  public_seen_names'
                )
              | Body.Property
                  ( loc,
                    {
                      Property.key =
                        Ast.Expression.Object.Property.Identifier
                          (id_loc, ({ Ast.Identifier.name; comments = _ } as id));
                      annot;
                      value;
                      static;
                      variance;
                      decorators;
                      comments;
                    }
                  ) ->
                let reason = mk_reason (RProperty (Some (OrdinaryName name))) loc in
                let polarity = Anno.polarity cx variance in
                let decorators =
                  Base.List.map ~f:Tast_utils.error_mapper#class_decorator decorators
                in
                let (field, annot_t, annot, get_value) =
                  mk_field cx tparams_map_with_this reason annot value
                in
                let get_element () =
                  Body.Property
                    ( (loc, annot_t),
                      {
                        Property.key =
                          Ast.Expression.Object.Property.Identifier ((id_loc, annot_t), id);
                        annot;
                        value = get_value ();
                        static;
                        variance;
                        decorators;
                        comments;
                      }
                    )
                in
                let public_seen_names' =
                  check_duplicate_name
                    public_seen_names
                    id_loc
                    name
                    ~static
                    ~private_:false
                    Class_Member_Field
                in
                ( Class_stmt_sig.add_field ~static name id_loc polarity field c,
                  get_element :: rev_elements,
                  public_seen_names'
                )
              (* literal LHS *)
              | ( Body.Method
                    ( loc,
                      {
                        Method.key =
                          Ast.Expression.Object.Property.(
                            StringLiteral _ | NumberLiteral _ | BigIntLiteral _);
                        _;
                      }
                    )
                | Body.Property
                    ( loc,
                      {
                        Property.key =
                          Ast.Expression.Object.Property.(
                            StringLiteral _ | NumberLiteral _ | BigIntLiteral _);
                        _;
                      }
                    ) ) as elem ->
                Flow.add_output
                  cx
                  (Error_message.EUnsupportedSyntax
                     (loc, Flow_intermediate_error_types.ClassPropertyLiteral)
                  );
                ( c,
                  (fun () -> Tast_utils.error_mapper#class_element elem) :: rev_elements,
                  public_seen_names
                )
              (* computed LHS *)
              | ( Body.Method (loc, { Method.key = Ast.Expression.Object.Property.Computed _; _ })
                | Body.Property
                    (loc, { Property.key = Ast.Expression.Object.Property.Computed _; _ }) ) as elem
                ->
                Flow.add_output
                  cx
                  (Error_message.EUnsupportedSyntax
                     (loc, Flow_intermediate_error_types.ClassPropertyComputed)
                  );
                ( c,
                  (fun () -> Tast_utils.error_mapper#class_element elem) :: rev_elements,
                  public_seen_names
                )
            )
            (class_sig, [], empty_seen_names)
            elements
        in
        let ({ Loc_env.class_bindings; _ } as env) = Context.environment cx in
        Context.set_environment
          cx
          {
            env with
            Loc_env.class_bindings =
              Loc_collections.ALocMap.add
                class_loc
                (Class_stmt_sig.mk_class_binding cx class_sig)
                class_bindings;
          };
        let elements = List.rev rev_elements in
        let (instance_this_default, static_this_default, super, static_super) =
          Type_env.in_class_scope cx class_loc (fun () -> Class_stmt_sig.make_thises cx class_sig)
        in
        Type_env.bind_class_instance_this cx instance_this_default class_loc;
        Type_env.bind_class_static_this cx static_this_default class_loc;
        Type_env.bind_class_instance_super cx super class_loc;
        Type_env.bind_class_static_super cx static_super class_loc;
        let (class_t_internal, class_t) = Class_stmt_sig.classtype cx class_sig in
        ( class_t,
          class_t_internal,
          class_sig,
          fun class_t ->
            {
              Ast.Class.id = Base.Option.map ~f:(fun (loc, name) -> ((loc, class_t), name)) id;
              body =
                ( body_loc,
                  {
                    Ast.Class.Body.body = Base.List.map ~f:(fun f -> f ()) elements;
                    comments = body_comments;
                  }
                );
              tparams = tparams_ast;
              extends = extends_ast_f ();
              implements = implements_ast;
              class_decorators = class_decorators_ast;
              comments;
            }
        )
    in
    fun cx ~name_loc ~class_loc reason cls ->
      let rec lazy_sig_info =
        lazy
          (let self =
             Tvar.mk_fully_resolved_lazy cx reason (Lazy.map (fun (_, t, _, _) -> t) lazy_sig_info)
           in
           mk_class_sig_with_self cx ~name_loc ~class_loc reason self cls
          )
      in
      Lazy.force lazy_sig_info

  and mk_component_sig =
    let mk_param_annot cx tparams_map reason = function
      | Ast.Type.Missing loc when Context.typing_mode cx <> Context.CheckingMode ->
        let t = Context.mk_placeholder cx reason in
        (t, Ast.Type.Missing (loc, t))
      | Ast.Type.Missing loc ->
        let t = Type_env.find_write cx Env_api.FunctionParamLoc reason in
        (t, Ast.Type.Missing (loc, t))
      | Ast.Type.Available annot ->
        let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
        (t, Ast.Type.Available ast_annot)
    in
    let id_param cx tparams_map id mk_reason =
      let { Ast.Pattern.Identifier.name; annot; optional } = id in
      let (id_loc, ({ Ast.Identifier.name; comments = _ } as id)) = name in
      let reason = mk_reason name in
      let (t, annot) = mk_param_annot cx tparams_map reason annot in
      let name = ((id_loc, t), id) in
      (t, { Ast.Pattern.Identifier.name; annot; optional })
    in
    let mk_param cx tparams_map param =
      let ( loc,
            {
              Ast.Statement.ComponentDeclaration.Param.local = (ploc, patt);
              default;
              name;
              shorthand;
            }
          ) =
        param
      in
      let has_param_anno =
        match Destructuring.type_of_pattern (ploc, patt) with
        | Ast.Type.Missing _ -> false
        | Ast.Type.Available _ -> true
      in
      let (t, pattern) =
        match patt with
        | Ast.Pattern.Identifier id ->
          let (t, id) =
            id_param cx tparams_map id (fun name -> mk_reason (RParameter (Some name)) ploc)
          in
          (t, Component_sig_types.DeclarationParamConfig.Id id)
        | Ast.Pattern.Object { Ast.Pattern.Object.annot; properties; comments } ->
          let reason = mk_reason RDestructuring ploc in
          let (t, annot) = mk_param_annot cx tparams_map reason annot in
          (t, Component_sig_types.DeclarationParamConfig.Object { annot; properties; comments })
        | Ast.Pattern.Array { Ast.Pattern.Array.annot; elements; comments } ->
          let reason = mk_reason RDestructuring ploc in
          let (t, annot) = mk_param_annot cx tparams_map reason annot in
          (t, Component_sig_types.DeclarationParamConfig.Array { annot; elements; comments })
        | Ast.Pattern.Expression _ -> failwith "unexpected expression pattern in param"
      in
      Component_sig_types.DeclarationParamConfig.Param
        { t; loc; ploc; pattern; default; has_anno = has_param_anno; name; shorthand }
    in
    let mk_rest cx tparams_map rest =
      let ( loc,
            {
              Ast.Statement.ComponentDeclaration.RestParam.argument = (ploc, patt);
              comments = rest_comments;
            }
          ) =
        rest
      in
      let has_param_anno =
        match Destructuring.type_of_pattern (ploc, patt) with
        | Ast.Type.Missing _ -> false
        | Ast.Type.Available _ -> true
      in
      match patt with
      | Ast.Pattern.Identifier id ->
        let (t, id) =
          id_param cx tparams_map id (fun name -> mk_reason (RParameter (Some name)) ploc)
        in
        Ok
          (Component_sig_types.DeclarationParamConfig.Rest
             {
               t;
               loc;
               ploc;
               pattern = Component_sig_types.DeclarationParamConfig.Id id;
               has_anno = has_param_anno;
               comments = rest_comments;
             }
          )
      | Ast.Pattern.Object { Ast.Pattern.Object.annot; properties; comments } ->
        let reason = mk_reason RDestructuring ploc in
        let (t, annot) = mk_param_annot cx tparams_map reason annot in
        let pattern =
          Component_sig_types.DeclarationParamConfig.Object { annot; properties; comments }
        in
        Ok
          (Component_sig_types.DeclarationParamConfig.Rest
             { t; loc; ploc; pattern; has_anno = has_param_anno; comments = rest_comments }
          )
      | Ast.Pattern.Array { Ast.Pattern.Array.annot; elements; comments } ->
        Flow_js.add_output cx Error_message.(EInvalidComponentRestParam ploc);
        let reason = mk_reason RDestructuring ploc in
        let (t, annot) = mk_param_annot cx tparams_map reason annot in
        let pattern =
          Component_sig_types.DeclarationParamConfig.Array { annot; elements; comments }
        in
        Ok
          (Component_sig_types.DeclarationParamConfig.Rest
             { t; loc; ploc; pattern; has_anno = has_param_anno; comments = rest_comments }
          )
      | Ast.Pattern.Expression _ -> Error Error_message.(EInvalidComponentRestParam ploc)
    in
    let mk_params cx tparams_map params =
      let (loc, { Ast.Statement.ComponentDeclaration.Params.params; rest; comments }) = params in
      let cparams =
        Component_declaration_params.empty (fun params rest ->
            (loc, { Ast.Statement.ComponentDeclaration.Params.params; rest; comments })
        )
      in
      let cparams =
        Base.List.fold
          ~f:(fun acc param ->
            Component_declaration_params.add_param (mk_param cx tparams_map param) acc)
          ~init:cparams
          params
      in
      let cparams =
        Base.Option.fold
          ~f:(fun acc rest ->
            match mk_rest cx tparams_map rest with
            | Ok rest -> Component_declaration_params.add_rest rest acc
            | Error err ->
              Flow_js.add_output cx err;
              acc)
          ~init:cparams
          rest
      in
      cparams
    in
    fun cx tparams_map reason component ->
      let cache = Context.node_cache cx in
      let {
        Ast.Statement.ComponentDeclaration.tparams;
        renders;
        body;
        params;
        id;
        sig_loc;
        comments = _;
      } =
        component
      in
      match Node_cache.get_component_sig cache sig_loc with
      | Some x -> x
      | None ->
        let (tparams, tparams_map, tparams_ast) =
          Anno.mk_type_param_declarations cx ~tparams_map tparams
        in
        let cparams = mk_params cx tparams_map params in
        let (ret_loc, renders_t, renders_ast) =
          match renders with
          | Ast.Type.AvailableRenders (loc, annot) ->
            let (t, renders_ast) =
              Anno.convert_render_type cx ~allow_generic_t:true tparams_map loc annot
            in
            (loc, t, Ast.Type.AvailableRenders (loc, renders_ast))
          | Ast.Type.MissingRenders loc ->
            let ret_reason = mk_reason RReturn loc in
            let t = Flow.get_builtin_type cx ret_reason "React$Node" in
            let renders_t = TypeUtil.mk_renders_type ret_reason RendersNormal t in
            (loc, renders_t, Ast.Type.MissingRenders (loc, renders_t))
        in
        let (id_loc, ({ Ast.Identifier.name; comments = _ } as name_ast)) = id in
        ( {
            Component_sig_types.Component_declaration_sig_types.reason;
            tparams;
            cparams;
            body;
            renders_t;
            ret_annot_loc = ret_loc;
            id_opt = Some (id_loc, name);
          },
          fun params body component_type ->
            {
              component with
              Ast.Statement.ComponentDeclaration.id = ((id_loc, component_type), name_ast);
              params;
              body;
              renders = renders_ast;
              tparams = tparams_ast;
            }
        )

  and mk_func_sig =
    let predicate_function_kind cx predicate body _loc _params =
      let pred_synth = Name_def.predicate_synthesizable predicate body in
      if pred_synth = Name_def.FunctionSynthesizable then begin
        let body_loc =
          match body with
          | Ast.Function.BodyExpression (loc, _) -> loc
          | Ast.Function.BodyBlock (loc, _) -> loc
        in
        Flow_js.add_output
          cx
          (Error_message.EUnsupportedSyntax
             (body_loc, Flow_intermediate_error_types.PredicateInvalidBody)
          )
      end;
      match pred_synth with
      | Name_def.FunctionPredicateSynthesizable (ret_loc, _) -> begin
        match Type_env.predicate_refinement_maps cx ret_loc with
        | Some (expr_reason, maps) ->
          let reason = update_desc_reason (fun d -> RPredicateOf d) expr_reason in
          Func.Predicate (PredBased (reason, maps))
        | None -> Func.Ordinary
      end
      | Name_def.FunctionSynthesizable
      | Name_def.MissingReturn _ ->
        Func.Ordinary
    in
    let function_kind cx ~body ~constructor ~async ~generator ~predicate ~params ~ret_loc =
      let open Func_class_sig_types.Func in
      match (constructor, async, generator, predicate) with
      | (true, _, _, _) -> Ctor
      | (false, true, true, None) -> AsyncGenerator { return_loc = ret_loc }
      | (false, true, false, None) -> Async
      | (false, false, true, None) -> Generator { return_loc = ret_loc }
      | (false, false, false, None) -> Ordinary
      | (false, false, false, Some (loc, _)) -> predicate_function_kind cx predicate body loc params
      | (false, _, _, _) -> Utils_js.assert_false "(async || generator) && pred"
    in
    let mk_param_annot cx tparams_map reason = function
      | Ast.Type.Missing loc when Context.typing_mode cx <> Context.CheckingMode ->
        let t = Context.mk_placeholder cx reason in
        (t, Ast.Type.Missing (loc, t))
      | Ast.Type.Missing loc ->
        let t = Type_env.find_write cx Env_api.FunctionParamLoc reason in
        (t, Ast.Type.Missing (loc, t))
      | Ast.Type.Available annot ->
        let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
        (t, Ast.Type.Available ast_annot)
    in
    let id_param cx tparams_map id mk_reason =
      let { Ast.Pattern.Identifier.name; annot; optional } = id in
      let (id_loc, ({ Ast.Identifier.name; comments = _ } as id)) = name in
      let reason = mk_reason name in
      let (t, annot) = mk_param_annot cx tparams_map reason annot in
      let name = ((id_loc, t), id) in
      (t, { Ast.Pattern.Identifier.name; annot; optional })
    in
    let mk_param cx tparams_map param =
      let (loc, { Ast.Function.Param.argument = (ploc, patt); default }) = param in
      let has_param_anno =
        match Destructuring.type_of_pattern (ploc, patt) with
        | Ast.Type.Missing _ -> false
        | Ast.Type.Available _ -> true
      in
      let (t, pattern) =
        match patt with
        | Ast.Pattern.Identifier id ->
          let (t, id) =
            id_param cx tparams_map id (fun name -> mk_reason (RParameter (Some name)) ploc)
          in
          (t, Func_stmt_config_types.Types.Id id)
        | Ast.Pattern.Object { Ast.Pattern.Object.annot; properties; comments } ->
          let reason = mk_reason RDestructuring ploc in
          let (t, annot) = mk_param_annot cx tparams_map reason annot in
          (t, Func_stmt_config_types.Types.Object { annot; properties; comments })
        | Ast.Pattern.Array { Ast.Pattern.Array.annot; elements; comments } ->
          let reason = mk_reason RDestructuring ploc in
          let (t, annot) = mk_param_annot cx tparams_map reason annot in
          (t, Func_stmt_config_types.Types.Array { annot; elements; comments })
        | Ast.Pattern.Expression _ -> failwith "unexpected expression pattern in param"
      in
      Func_stmt_config_types.Types.Param
        { t; loc; ploc; pattern; default; has_anno = has_param_anno }
    in
    let mk_rest cx tparams_map rest =
      let (loc, { Ast.Function.RestParam.argument = (ploc, patt); comments = _ }) = rest in
      let has_param_anno =
        match Destructuring.type_of_pattern (ploc, patt) with
        | Ast.Type.Missing _ -> false
        | Ast.Type.Available _ -> true
      in
      match patt with
      | Ast.Pattern.Identifier id ->
        let (t, id) =
          id_param cx tparams_map id (fun name -> mk_reason (RRestParameter (Some name)) ploc)
        in
        Ok (Func_stmt_config_types.Types.Rest { t; loc; ploc; id; has_anno = has_param_anno })
      | Ast.Pattern.Object _
      | Ast.Pattern.Array _
      | Ast.Pattern.Expression _ ->
        (* TODO: this should be a parse error, unrepresentable AST *)
        Error Error_message.(EInternal (ploc, RestParameterNotIdentifierPattern))
    in
    let mk_this
        cx tparams_map (loc, { Ast.Function.ThisParam.annot = (annot_loc, annot); comments = _ }) =
      let (((_, t), _) as annot) = Anno.convert cx tparams_map annot in
      Func_stmt_config_types.Types.This { t; loc; annot = (annot_loc, annot) }
    in
    let mk_params cx tparams_map params =
      let (loc, { Ast.Function.Params.params; rest; this_; comments }) = params in
      let fparams =
        Func_stmt_params.empty (fun params rest this_ ->
            Some (loc, { Ast.Function.Params.params; rest; this_; comments })
        )
      in
      let fparams =
        Base.List.fold
          ~f:(fun acc param -> Func_stmt_params.add_param (mk_param cx tparams_map param) acc)
          ~init:fparams
          params
      in
      let fparams =
        Base.Option.fold
          ~f:(fun acc rest ->
            match mk_rest cx tparams_map rest with
            | Ok rest -> Func_stmt_params.add_rest rest acc
            | Error err ->
              Flow_js.add_output cx err;
              acc)
          ~init:fparams
          rest
      in
      let fparams =
        Base.Option.fold
          ~f:(fun acc this -> Func_stmt_params.add_this (mk_this cx tparams_map this) acc)
          ~init:fparams
          this_
      in
      fparams
    in
    (* This check to be performed after the function has been checked to ensure all
     * entries have been prepared for type checking. *)
    let check_type_guard_consistency cx reason one_sided param_loc tg_param tg_reason type_guard =
      let env = Context.environment cx in
      let { Loc_env.var_info; _ } = env in
      let { Env_api.type_guard_consistency_maps; _ } = var_info in
      let (name_loc, name) = tg_param in
      let param_reason = mk_reason (RParameter (Some name)) param_loc in
      match Loc_collections.ALocMap.find_opt name_loc type_guard_consistency_maps with
      | None ->
        (* Entry missing when function does not return. Error raised in Func_sig. *)
        ()
      | Some (Some havoced_loc_set, _) ->
        Flow_js_utils.add_output
          cx
          Error_message.(
            ETypeGuardFunctionParamHavoced
              {
                param_reason;
                type_guard_reason = tg_reason;
                call_locs = Loc_collections.ALocSet.elements havoced_loc_set;
              }
          )
      | Some (None, reads) ->
        (* Each read corresponds to a return expression. *)
        Base.List.iter
          reads
          ~f:(fun (ret_expr, return_reason, { Env_api.write_locs = pos_write_locs; _ }, neg_refi) ->
            let is_return_false_statement =
              match ret_expr with
              | Some (_, Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = false; _ }) ->
                true
              | _ -> false
            in
            let return_loc = Reason.loc_of_reason return_reason in
            match
              Type_env.type_guard_at_return
                cx
                param_reason
                ~param_loc
                ~return_loc
                ~pos_write_locs
                ~neg_refi:(neg_refi, param_loc, Pattern_helper.Root)
            with
            | Ok (t, neg_pred) ->
              (* Positive *)
              let guard_type_reason = reason_of_t type_guard in
              let use_op =
                Op
                  (PositiveTypeGuardConsistency
                     {
                       reason;
                       param_reason;
                       guard_type_reason;
                       return_reason;
                       is_return_false_statement;
                     }
                  )
              in
              Flow.flow cx (t, UseT (use_op, type_guard));
              (* Negative *)
              if Context.one_sided_type_guards cx && not one_sided then
                let type_guard_with_neg_pred =
                  Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx tg_reason (fun tout ->
                      Flow.flow cx (type_guard, PredicateT (neg_pred, tout))
                  )
                in
                if
                  not
                    (Flow_js.FlowJs.speculative_subtyping_succeeds
                       cx
                       type_guard_with_neg_pred
                       (EmptyT.at ALoc.none)
                    )
                then
                  Flow_js_utils.add_output
                    cx
                    Error_message.(
                      ENegativeTypeGuardConsistency
                        { reason; return_reason; type_reason = reason_of_t type_guard }
                    )
            | Error write_locs ->
              Flow_js_utils.add_output
                cx
                Error_message.(
                  ETypeGuardFunctionInvalidWrites
                    { reason = return_reason; type_guard_reason = tg_reason; write_locs }
                )
        )
    in
    let predicate_checks cx pred params =
      let err_with_desc desc pred_reason binding_loc =
        let binding_reason = mk_reason desc binding_loc in
        Flow_js.add_output
          cx
          Error_message.(EPredicateInvalidParameter { pred_reason; binding_reason })
      in
      let error_on_non_root_binding name expr_reason binding =
        let open Pattern_helper in
        match binding with
        | (_, Root) -> ()
        | (loc, Rest) -> err_with_desc (RRestParameter (Some name)) expr_reason loc
        | (loc, Select _) -> err_with_desc (RPatternParameter name) expr_reason loc
      in
      let type_guard_based_checks reason one_sided tg_param type_guard binding_opt =
        let (name_loc, name) = tg_param in
        let tg_reason = mk_reason (RTypeGuardParam name) name_loc in
        let open Pattern_helper in
        match binding_opt with
        | None -> Flow_js_utils.add_output cx Error_message.(ETypeGuardParamUnbound tg_reason)
        | Some (param_loc, Root) ->
          check_type_guard_consistency cx reason one_sided param_loc tg_param tg_reason type_guard
        | Some binding -> error_on_non_root_binding name tg_reason binding
      in
      match pred with
      | TypeGuardBased { reason; one_sided; param_name; type_guard } ->
        let bindings = Pattern_helper.bindings_of_params params in
        let matching_binding = SMap.find_opt (snd param_name) bindings in
        type_guard_based_checks reason one_sided param_name type_guard matching_binding
      | PredBased (expr_reason, (lazy (p_map, _))) ->
        let required_bindings =
          Base.List.filter_map (Key_map.keys p_map) ~f:(function
              | (OrdinaryName name, _) -> Some name
              | _ -> None
              )
        in
        if not (Base.List.is_empty required_bindings) then
          let bindings = Pattern_helper.bindings_of_params params in
          Base.List.iter required_bindings ~f:(fun name ->
              Base.Option.iter (SMap.find_opt name bindings) ~f:(fun binding ->
                  error_on_non_root_binding name expr_reason binding
              )
          )
    in
    fun cx ~require_return_annot ~constructor ~getset ~statics tparams_map reason func ->
      let {
        Ast.Function.tparams;
        return;
        body;
        predicate;
        params;
        id;
        async;
        generator;
        effect;
        sig_loc;
        comments = _;
      } =
        func
      in
      let cache = Context.node_cache cx in
      match Node_cache.get_function_sig cache sig_loc with
      | Some x -> x
      | None ->
        let loc = loc_of_reason reason in
        let ret_loc =
          match return with
          | Ast.Function.ReturnAnnot.Available (loc, _)
          | Ast.Function.ReturnAnnot.TypeGuard (loc, _)
          | Ast.Function.ReturnAnnot.Missing loc ->
            loc
        in
        let kind =
          function_kind cx ~body ~constructor ~async ~generator ~predicate ~params ~ret_loc
        in
        Anno.error_on_unsupported_variance_annotation cx ~kind:"function" tparams;
        let (tparams, tparams_map, tparams_ast) =
          Anno.mk_type_param_declarations cx ~tparams_map tparams
        in
        let fparams = mk_params cx tparams_map params in
        let body = Some body in
        let ret_reason = mk_reason RReturn (Func_sig.return_loc func) in
        let open Func_class_sig_types.Func in
        let has_nonvoid_return =
          Nonvoid_return.might_have_nonvoid_return loc func
          || (kind <> Ordinary && kind <> Async && kind <> Ctor)
        in
        let type_guard_incompatible =
          match kind with
          | Async
          | Generator _
          | AsyncGenerator _
          | FieldInit _
          | Ctor ->
            Some (Func_class_sig_types.Func.string_of_kind kind)
          | _ when getset -> Some "getter/setter"
          | _ -> None
        in
        let from_generic_function = Base.Option.is_some tparams in
        let require_return_annot = require_return_annot || from_generic_function in
        Base.Option.iter id ~f:(hook_check cx effect);
        let (return_t, return, type_guard_opt) =
          match (return, type_guard_incompatible) with
          | (Ast.Function.ReturnAnnot.TypeGuard (_, (loc, _)), Some kind) ->
            let return_t = BoolT.at loc in
            let return = Tast_utils.error_mapper#function_return_annotation return in
            Flow_js_utils.add_output
              cx
              Error_message.(ETypeGuardIncompatibleWithFunctionKind { loc; kind });
            (Annotated return_t, return, None)
          | (Ast.Function.ReturnAnnot.Missing loc, _) when not has_nonvoid_return ->
            let void_t = VoidT.why ret_reason in
            let t =
              if kind = Async then
                let reason =
                  mk_annot_reason (RType (OrdinaryName "Promise")) (loc_of_reason ret_reason)
                in
                Flow.get_builtin_typeapp cx reason "Promise" [void_t]
              else
                void_t
            in
            (Inferred t, Ast.Function.ReturnAnnot.Missing (loc, t), None)
          | (Ast.Function.ReturnAnnot.Missing loc, _)
            when has_nonvoid_return && require_return_annot ->
            let reason = repos_reason ret_loc ret_reason in
            Flow_js.add_output
              cx
              (Error_message.EMissingLocalAnnotation
                 { reason; hint_available = false; from_generic_function }
              );
            let t = AnyT.make (AnyError (Some MissingAnnotation)) reason in
            (Inferred t, Ast.Function.ReturnAnnot.Missing (loc, t), None)
          | (Ast.Function.ReturnAnnot.Missing loc, _) when kind = Func_class_sig_types.Func.Ctor ->
            let t = VoidT.make (mk_reason RConstructorVoidReturn ret_loc) in
            (Inferred t, Ast.Function.ReturnAnnot.Missing (loc, t), None)
          (* TODO we could probably take the same shortcut for functions with an explicit `void` annotation
             and no explicit returns *)
          | (Ast.Function.ReturnAnnot.Missing loc, _) ->
            let t =
              if Context.typing_mode cx <> Context.CheckingMode then
                Context.mk_placeholder cx ret_reason
              else
                Tvar.mk cx ret_reason
            in
            (Inferred t, Ast.Function.ReturnAnnot.Missing (loc, t), None)
          | (Ast.Function.ReturnAnnot.Available annot, _) ->
            let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
            (Annotated t, Ast.Function.ReturnAnnot.Available ast_annot, None)
          | ( Ast.Function.ReturnAnnot.TypeGuard
                (loc, (gloc, { Ast.Type.TypeGuard.guard = (id_name, Some t); kind; comments })),
              _
            )
            when kind = Ast.Type.TypeGuard.Default
                 || (kind = Ast.Type.TypeGuard.Implies && Context.one_sided_type_guards cx) ->
            let (bool_t, guard', predicate) =
              Anno.convert_type_guard
                cx
                tparams_map
                (Func_stmt_params.value fparams)
                gloc
                kind
                id_name
                t
                comments
            in
            (Annotated bool_t, Ast.Function.ReturnAnnot.TypeGuard (loc, guard'), predicate)
          | (Ast.Function.ReturnAnnot.TypeGuard annot, _) ->
            let (_, (_, { Ast.Type.TypeGuard.kind; _ })) = annot in
            let ((_, (loc, _)) as t_ast') = Tast_utils.error_mapper#type_guard_annotation annot in
            Flow_js_utils.add_output
              cx
              (Error_message.EUnsupportedSyntax
                 (loc, Flow_intermediate_error_types.UserDefinedTypeGuards { kind })
              );
            ( Annotated (AnyT.at (AnyError None) loc),
              Ast.Function.ReturnAnnot.TypeGuard t_ast',
              None
            )
        in
        (* Now that we've seen the return annotation we might need to update `kind`. *)
        let kind =
          match type_guard_opt with
          | Some p -> Predicate p
          | None -> kind
        in
        let (return_t, predicate) =
          let open Ast.Type.Predicate in
          match (predicate, kind) with
          | (Some ((loc, { kind = Ast.Type.Predicate.Inferred; _ }) as pred), Func.Predicate _) ->
            Flow_js.add_output cx (Error_message.EDeprecatedPredicate loc);
            (return_t, Some pred)
          | (Some ((loc, { kind = Declared (expr_loc, _); comments = _ }) as pred), _) ->
            Flow_js.add_output cx (Error_message.EDeprecatedPredicate loc);
            Flow_js.add_output
              cx
              (Error_message.EUnsupportedSyntax
                 (expr_loc, Flow_intermediate_error_types.PredicateDeclarationForImplementation)
              );
            (Inferred (AnyT.error ret_reason), Some (Tast_utils.error_mapper#predicate pred))
          | _ -> (return_t, Base.Option.map ~f:Tast_utils.error_mapper#predicate predicate)
        in
        let () =
          match (return, kind) with
          | (Ast.Function.ReturnAnnot.Missing _, Func_class_sig_types.Func.Ctor) -> ()
          | (_, Func_class_sig_types.Func.Ctor) ->
            let return_t = TypeUtil.type_t_of_annotated_or_inferred return_t in
            let use_op = Op (FunReturnStatement { value = reason_of_t return_t }) in
            (* Check delayed so that we don't have to force return_t which might contain a currently
             * unresolved OpenT with implicit this tparam. *)
            Context.add_post_inference_subtyping_check
              cx
              return_t
              use_op
              (VoidT.make (mk_reason RConstructorVoidReturn ret_loc))
          | _ -> ()
        in
        let return_t =
          mk_inference_target_with_annots ~has_hint:(Type_env.has_hint cx ret_loc) return_t
        in
        let statics_t =
          let props =
            SMap.fold
              (fun name (kind, loc) acc ->
                let expr_t =
                  Type_env.find_write cx kind (mk_reason (RIdentifier (OrdinaryName name)) loc)
                in
                let field =
                  Field
                    {
                      preferred_def_locs = None;
                      key_loc = Some loc;
                      type_ = expr_t;
                      polarity = Polarity.Neutral;
                    }
                in
                NameUtils.Map.add (OrdinaryName name) field acc)
              statics
              NameUtils.Map.empty
          in
          Obj_type.mk_with_proto cx reason (FunProtoT reason) ~obj_kind:Type.Inexact ~props
        in
        let effect =
          match effect with
          | Ast.Function.Hook -> HookDecl (Context.make_aloc_id cx loc)
          | Ast.Function.Arbitrary -> ArbitraryEffect
          | Ast.Function.Idempotent -> IdempotentEffect
          | Ast.Function.Parametric n -> ParametricEffect n
        in
        let func_stmt_sig =
          {
            Func_stmt_sig_types.reason;
            kind;
            tparams;
            fparams;
            body;
            return_t;
            ret_annot_loc = ret_loc;
            statics = Some statics_t;
            effect;
          }
        in
        let reconstruct_ast params_tast body fun_type =
          let () =
            if Context.typing_mode cx = Context.CheckingMode then
              match kind with
              | Predicate p -> predicate_checks cx p params
              | _ -> ()
          in
          {
            func with
            Ast.Function.id =
              Base.Option.map ~f:(fun (id_loc, name) -> ((id_loc, fun_type), name)) id;
            params = params_tast;
            body;
            predicate;
            return;
            tparams = tparams_ast;
          }
        in
        (func_stmt_sig, reconstruct_ast)

  (* Given a function declaration and types for `this` and `super`, extract a
     signature consisting of type parameters, parameter types, parameter names,
     and return type, check the body against that signature by adding `this`
     and super` to the environment, and return the signature. *)
  and function_decl cx ~fun_loc ~arrow ~statics reason func default_this =
    let (func_sig, reconstruct_func) =
      mk_func_sig
        cx
        ~require_return_annot:false
        ~constructor:false
        ~getset:false
        ~statics
        Subst_name.Map.empty
        reason
        func
    in
    let default_this =
      match default_this with
      | Some t -> t
      | None -> dummy_this (loc_of_reason reason)
    in
    let fun_type = Func_stmt_sig.functiontype cx ~arrow fun_loc default_this func_sig in
    if Context.typing_mode cx <> Context.CheckingMode then
      let { Ast.Function.params; body; _ } = func in
      let mapper = Typed_ast_utils.placeholder_mapper cx in
      let params_ast = mapper#function_params params in
      let body_ast = mapper#function_body_any body in
      (fun_type, reconstruct_func params_ast body_ast)
    else
      let (params_ast, body_ast, _) = Func_stmt_sig.toplevels cx func_sig in
      ( fun_type,
        reconstruct_func (Base.Option.value_exn params_ast) (Base.Option.value_exn body_ast)
      )

  (* Process a function declaration, returning a (polymorphic) function type. *)
  and mk_function_declaration cx ?tast_fun_type reason fun_loc func =
    mk_function cx ?tast_fun_type ~needs_this_param:true ~statics:SMap.empty reason fun_loc func

  (* Process a function expression, returning a (polymorphic) function type. *)
  and mk_function_expression cx ~needs_this_param reason fun_loc func =
    mk_function cx ~needs_this_param ?tast_fun_type:None ~statics:SMap.empty reason fun_loc func

  (* Internal helper function. Use `mk_function_declaration` and `mk_function_expression` instead. *)
  and mk_function
      cx
      ~needs_this_param
      ?tast_fun_type
      ~statics
      reason
      fun_loc
      ({ Ast.Function.id = func_id; _ } as func) =
    let node_cache = Context.node_cache cx in
    let cached =
      func_id |> Base.Option.value_map ~default:fun_loc ~f:fst |> Node_cache.get_function node_cache
    in
    match cached with
    | Some cached ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Function cache hit at %s" (ALoc.debug_to_string (loc_of_reason reason))]);
      cached
    | None ->
      (* The default behavior of `this` still depends on how it
         was created, so we must provide the recipe based on where `function_decl`
         is invoked. *)
      let default_this = Flow_js_utils.default_this_type cx ~needs_this_param func in
      let (fun_type, reconstruct_ast) =
        function_decl
          cx
          ~fun_loc:(Base.Option.some_if needs_this_param fun_loc)
          ~arrow:false
          ~statics
          reason
          func
          (Base.Option.some_if needs_this_param default_this)
      in
      let tast_fun_type = Base.Option.value ~default:fun_type tast_fun_type in
      (fun_type, reconstruct_ast tast_fun_type)

  (* Process an arrow function, returning a (polymorphic) function type. *)
  and mk_arrow cx ~statics reason func =
    (* Do not expose the type of `this` in the function's type. This call to
       function_decl has already done the necessary checking of `this` in
       the body of the function. Now we want to avoid re-binding `this` to
       objects through which the function may be called. *)
    let default_this = None in
    let (fun_type, reconstruct_ast) =
      function_decl cx ~fun_loc:None ~arrow:true ~statics reason func default_this
    in
    (fun_type, reconstruct_ast fun_type)

  and declare_function_to_function_declaration cx =
    let add_output l =
      Flow.add_output
        cx
        (match l with
        | Declare_function_utils.PredicateDeclarationWithoutExpression loc ->
          Error_message.EUnsupportedSyntax
            (loc, Flow_intermediate_error_types.PredicateDeclarationWithoutExpression)
        | Declare_function_utils.PredicateDeclarationAnonymousParameters loc ->
          Error_message.EUnsupportedSyntax
            (loc, Flow_intermediate_error_types.PredicateDeclarationAnonymousParameters))
    in
    let copy_t (_, t) l = (l, t) in
    let loc_of_tloc = fst in
    Declare_function_utils.declare_function_to_function_declaration ~add_output ~copy_t ~loc_of_tloc

  and check_default_pattern cx left right =
    let left_loc = fst left in
    let right_loc = fst right in
    let update_excuses update_fun =
      let exists_excuses = Context.exists_excuses cx in
      let exists_excuse =
        Loc_collections.ALocMap.find_opt left_loc exists_excuses
        |> Base.Option.value ~default:ExistsCheck.empty
        |> update_fun
      in
      let exists_excuses = Loc_collections.ALocMap.add left_loc exists_excuse exists_excuses in
      Context.set_exists_excuses cx exists_excuses
    in
    let open ExistsCheck in
    match snd right with
    | Ast.Expression.StringLiteral { Ast.StringLiteral.value = ""; _ } ->
      update_excuses (fun excuse -> { excuse with string_loc = Some right_loc })
    | Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value = false; _ } ->
      update_excuses (fun excuse -> { excuse with bool_loc = Some right_loc })
    | Ast.Expression.NumberLiteral { Ast.NumberLiteral.value = 0.; _ } ->
      update_excuses (fun excuse -> { excuse with number_loc = Some right_loc })
    | Ast.Expression.BigIntLiteral { Ast.BigIntLiteral.value = Some 0L; _ } ->
      update_excuses (fun excuse -> { excuse with bigint_loc = Some right_loc })
    | _ ->
      (* There's no valid default value for mixed to create an excuse. *)
      ()

  and is_valid_enum_member_name name =
    (not @@ Base.String.is_empty name) && (not @@ Base.Char.is_lowercase name.[0])

  and enum_exhaustive_check_of_switch_cases cases_ast =
    let open Flow_ast in
    let open Flow_ast.Statement.Switch in
    let exhaustive_check =
      List.fold_left
        (fun acc -> function
          | ( _,
              {
                Case.test =
                  Some
                    ( (case_test_loc, _),
                      Expression.Member
                        Expression.Member.
                          {
                            _object = ((_, obj_t), _);
                            property = PropertyIdentifier (_, { Identifier.name; _ });
                            _;
                          }
                    );
                _;
              }
            )
            when is_valid_enum_member_name name ->
            (match acc with
            | EnumExhaustiveCheckInvalid _ -> acc
            | EnumExhaustiveCheckPossiblyValid { tool; possible_checks; checks; default_case_loc }
              ->
              let possible_check = (obj_t, EnumCheck { case_test_loc; member_name = name }) in
              EnumExhaustiveCheckPossiblyValid
                {
                  tool;
                  possible_checks = possible_check :: possible_checks;
                  checks;
                  default_case_loc;
                })
          | (default_case_loc, { Case.test = None; _ }) ->
            (match acc with
            | EnumExhaustiveCheckInvalid _ -> acc
            | EnumExhaustiveCheckPossiblyValid
                { tool; possible_checks; checks; default_case_loc = _ } ->
              EnumExhaustiveCheckPossiblyValid
                { tool; possible_checks; checks; default_case_loc = Some default_case_loc })
          | (_, { Case.test = Some ((case_test_loc, _), _); _ }) ->
            (match acc with
            | EnumExhaustiveCheckInvalid invalid_checks ->
              EnumExhaustiveCheckInvalid (case_test_loc :: invalid_checks)
            | EnumExhaustiveCheckPossiblyValid _ -> EnumExhaustiveCheckInvalid [case_test_loc]))
        (EnumExhaustiveCheckPossiblyValid
           {
             tool = EnumResolveDiscriminant;
             possible_checks = [];
             checks = [];
             default_case_loc = None;
           }
        )
        cases_ast
    in
    match exhaustive_check with
    | EnumExhaustiveCheckInvalid invalid_checks ->
      EnumExhaustiveCheckInvalid (List.rev invalid_checks)
    | EnumExhaustiveCheckPossiblyValid _ ->
      (* As we process `possible_checks` into `checks`, we reverse the list back
       * into the correct order. *)
      exhaustive_check

  and enum_declaration cx loc enum =
    let open Ast.Statement.EnumDeclaration in
    let { id = (name_loc, ident); body; comments } = enum in
    let { Ast.Identifier.name; _ } = ident in
    let reason = mk_reason (REnum { name = Some name }) name_loc in
    let t =
      if Context.enable_enums cx then (
        let enum_info = ConcreteEnum (mk_enum cx ~enum_reason:reason name_loc name body) in
        let t = mk_enum_object_type reason enum_info in
        let use_op =
          Op
            (AssignVar
               { var = Some (mk_reason (RIdentifier (OrdinaryName name)) name_loc); init = reason }
            )
        in
        Type_env.init_implicit_const cx ~use_op t name_loc;
        t
      ) else (
        Flow.add_output cx (Error_message.EEnumsNotEnabled loc);
        AnyT.error reason
      )
    in
    let id' = ((name_loc, t), ident) in
    { id = id'; body; comments }

  and mk_enum cx ~enum_reason name_loc enum_name body =
    let open Ast.Statement.EnumDeclaration in
    let defaulted_members =
      Base.List.fold
        ~init:SMap.empty
        ~f:(fun acc (member_loc, { DefaultedMember.id = (_, { Ast.Identifier.name; _ }) }) ->
          SMap.add name member_loc acc
      )
    in
    let enum_id = Context.make_aloc_id cx name_loc in
    let (representation_t, members, has_unknown_members) =
      match body with
      | (_, BooleanBody { BooleanBody.members; has_unknown_members; _ }) ->
        let reason = mk_reason RBoolean (loc_of_reason enum_reason) in
        let (members, bool_type, _) =
          Base.List.fold_left
            ~f:
              (fun (members_map, bool_type, seen_values)
                   (member_loc, { InitializedMember.id = (_, { Ast.Identifier.name; _ }); init }) ->
              let (init_loc, { Ast.BooleanLiteral.value = init_value; _ }) = init in
              let bool_type =
                match bool_type with
                (* we have seen one value *)
                | None -> Some init_value
                (* we have now seen both values *)
                | Some _ -> None
              in
              let seen_values =
                match BoolMap.find_opt init_value seen_values with
                | Some prev_use_loc ->
                  Flow.add_output
                    cx
                    (Error_message.EEnumMemberDuplicateValue
                       { loc = init_loc; prev_use_loc; enum_reason }
                    );
                  seen_values
                | None -> BoolMap.add init_value member_loc seen_values
              in
              (SMap.add name member_loc members_map, bool_type, seen_values))
            ~init:(SMap.empty, None, BoolMap.empty)
            members
        in
        (DefT (reason, BoolT bool_type), members, has_unknown_members)
      | (_, NumberBody { NumberBody.members; has_unknown_members; _ }) ->
        let reason = mk_reason RNumber (loc_of_reason enum_reason) in
        let (members, num_type, _) =
          Base.List.fold_left
            ~f:
              (fun (members_map, num_type, seen_values)
                   (member_loc, { InitializedMember.id = (_, { Ast.Identifier.name; _ }); init }) ->
              let (init_loc, { Ast.NumberLiteral.value = init_value; _ }) = init in
              let num_type =
                if init_value = 0.0 then
                  AnyLiteral
                else
                  num_type
              in
              let seen_values =
                match NumberMap.find_opt init_value seen_values with
                | Some prev_use_loc ->
                  Flow.add_output
                    cx
                    (Error_message.EEnumMemberDuplicateValue
                       { loc = init_loc; prev_use_loc; enum_reason }
                    );
                  seen_values
                | None -> NumberMap.add init_value member_loc seen_values
              in
              (SMap.add name member_loc members_map, num_type, seen_values))
            ~init:(SMap.empty, Truthy, NumberMap.empty)
            members
        in
        (DefT (reason, NumT num_type), members, has_unknown_members)
      | (_, BigIntBody { BigIntBody.members; has_unknown_members; _ }) ->
        let reason = mk_reason RBigInt (loc_of_reason enum_reason) in
        let (members, num_type, _) =
          Base.List.fold_left
            ~f:
              (fun (members_map, bigint_type, seen_values)
                   (member_loc, { InitializedMember.id = (_, { Ast.Identifier.name; _ }); init }) ->
              let (init_loc, { Ast.BigIntLiteral.value = init_value; _ }) = init in
              let bigint_type =
                if init_value = Some 0L then
                  AnyLiteral
                else
                  bigint_type
              in
              let seen_values =
                match BigIntOptionMap.find_opt init_value seen_values with
                | Some prev_use_loc ->
                  Flow.add_output
                    cx
                    (Error_message.EEnumMemberDuplicateValue
                       { loc = init_loc; prev_use_loc; enum_reason }
                    );
                  seen_values
                | None -> BigIntOptionMap.add init_value member_loc seen_values
              in
              (SMap.add name member_loc members_map, bigint_type, seen_values))
            ~init:(SMap.empty, Truthy, BigIntOptionMap.empty)
            members
        in
        (DefT (reason, BigIntT num_type), members, has_unknown_members)
      | ( _,
          StringBody { StringBody.members = StringBody.Initialized members; has_unknown_members; _ }
        ) ->
        let reason = mk_reason RString (loc_of_reason enum_reason) in
        let (members, str_type, _) =
          Base.List.fold_left
            ~f:
              (fun (members_map, str_type, seen_values)
                   (member_loc, { InitializedMember.id = (_, { Ast.Identifier.name; _ }); init }) ->
              let (init_loc, { Ast.StringLiteral.value = init_value; _ }) = init in
              let str_type =
                if init_value = "" then
                  AnyLiteral
                else
                  str_type
              in
              let seen_values =
                match SMap.find_opt init_value seen_values with
                | Some prev_use_loc ->
                  Flow.add_output
                    cx
                    (Error_message.EEnumMemberDuplicateValue
                       { loc = init_loc; prev_use_loc; enum_reason }
                    );
                  seen_values
                | None -> SMap.add init_value member_loc seen_values
              in
              (SMap.add name member_loc members_map, str_type, seen_values))
            ~init:(SMap.empty, Truthy, SMap.empty)
            members
        in
        (DefT (reason, StrT str_type), members, has_unknown_members)
      | (_, StringBody { StringBody.members = StringBody.Defaulted members; has_unknown_members; _ })
        ->
        let reason = mk_reason RString (loc_of_reason enum_reason) in
        ( DefT (reason, StrT Truthy (* Member names can't be the empty string *)),
          defaulted_members members,
          has_unknown_members
        )
      | (_, SymbolBody { SymbolBody.members; has_unknown_members; comments = _ }) ->
        let reason = mk_reason RSymbol (loc_of_reason enum_reason) in
        (DefT (reason, SymbolT), defaulted_members members, has_unknown_members)
    in
    { enum_name; enum_id; members; representation_t; has_unknown_members }
end
