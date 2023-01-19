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
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

module Flow = Flow_js
open Utils_js
open Reason
open Type
open TypeUtil
open Func_class_sig_types
module Eq_test = Eq_test.Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)

module Make
    (Destructuring : Destructuring_sig.S)
    (Func_stmt_config : Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types)
    (Statement : Statement_sig.S) : Statement_sig.S = struct
  module Anno = Type_annotation.Make (Type_annotation.FlowJS) (Statement)
  module Class_type_sig = Anno.Class_type_sig
  module Func_stmt_config = Func_stmt_config
  open Env.LookupMode

  (*************)
  (* Utilities *)
  (*************)

  module ChainingConf = struct
    type ('a, 'b) t = {
      refinement_action: ('a -> Type.t -> Type.t -> Type.t) option;
      refine: unit -> Type.t option;
      subexpressions: unit -> 'a * 'b;
      get_result: 'a -> Reason.t -> Type.t -> Type.t;
      test_hooks: Type.t -> Type.tvar option;
      get_opt_use: 'a -> Reason.t -> Type.t -> Type.opt_use_t;
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

    let mk_object_from_spread_acc cx acc reason ~frozen ~default_proto =
      match elements_rev acc with
      | (Slice { slice_pmap }, []) ->
        let proto = Base.Option.value ~default:default_proto (proto acc) in
        let obj_t =
          Obj_type.mk_with_proto cx reason ~obj_kind:Exact ~frozen ~props:slice_pmap proto
        in
        if obj_key_autocomplete acc then
          let get_autocomplete_t () =
            Tvar.mk_where cx reason (fun tvar -> Flow_js.flow_t cx (obj_t, tvar))
          in
          if Context.lti cx then
            let (_, lazy_hint) = Env.get_hint cx (Reason.aloc_of_reason reason) in
            lazy_hint reason |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:get_autocomplete_t
          else
            get_autocomplete_t ()
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
                      })
                ts
            in
            (t, ts, None)
          | (Slice { slice_pmap = prop_map }, Spread t :: ts) ->
            let head_slice =
              { Type.Object.Spread.reason; prop_map; dict = None; generics = Generic.spread_empty }
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
                      })
                ts
            in
            (t, ts, Some head_slice)
          | _ -> failwith "Invariant Violation: spread list has two slices in a row"
        in
        let seal = Obj_type.mk_seal ~frozen in
        let target = Object.Spread.Value { make_seal = seal } in
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
        let tout = Tvar.mk cx reason in
        let use_op = Op (ObjectSpread { op = reason }) in
        let l = Flow.widen_obj_type cx ~use_op reason t in
        Flow.flow cx (l, ObjKitT (use_op, reason, tool, Type.Object.Spread (target, state), tout));
        if Context.lti cx && obj_key_autocomplete acc then
          let (_, lazy_hint) = Env.get_hint cx (Reason.aloc_of_reason reason) in
          lazy_hint reason |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:(fun () -> tout)
        else
          tout
  end

  let mk_ident ~comments name = { Ast.Identifier.name; comments }

  let snd_fst ((_, x), _) = x

  let inference_hook_tvar cx ploc =
    let r = mk_annot_reason (AnyT.desc (Unsound InferenceHooks)) ploc in
    let tvar = Tvar.mk_no_wrap cx r in
    Flow.flow
      cx
      ( OpenT (r, tvar),
        BecomeT { reason = r; t = Unsoundness.at InferenceHooks ploc; empty_success = true }
      );
    (r, tvar)

  let translate_identifier_or_literal_key t =
    let open Ast.Expression.Object in
    function
    | Property.Identifier (loc, name) -> Property.Identifier ((loc, t), name)
    | Property.Literal (loc, lit) -> Property.Literal ((loc, t), lit)
    | Property.PrivateName _
    | Property.Computed _ ->
      assert_false "precondition not met"

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
                   | Object.Property.Literal (_, { Literal.raw = name; _ }) ->
                     mk_reason (RMethod (Some name)) prop_loc
                   | _ -> mk_reason (RMethod None) prop_loc
                 in
                 Flow_js.add_output cx (Error_message.EObjectThisReference (loc, reason))
             )
        | _ -> ()
        )

  module Func_stmt_params =
    Func_params.Make (Func_stmt_config_types.Types) (Func_stmt_config) (Func_stmt_params_types)
  module Func_stmt_sig =
    Func_sig.Make (Statement) (Func_stmt_config_types.Types) (Func_stmt_config) (Func_stmt_params)
      (Func_stmt_sig_types)
  module Class_stmt_sig =
    Class_sig.Make (Func_stmt_config_types.Types) (Func_stmt_config) (Func_stmt_params)
      (Func_stmt_sig)
      (Class_stmt_sig_types)
  module PinTypes = Implicit_instantiation.PinTypes (Flow.FlowJs)

  (* In positions where an annotation may be present or an annotation can be pushed down,
   * we should prefer the annotation over the pushed-down annotation. *)
  let mk_inference_target_with_annots ~has_hint annot_or_inferred =
    match (annot_or_inferred, has_hint) with
    | (Annotated _, _) -> annot_or_inferred
    | (_, true) -> Annotated (type_t_of_annotated_or_inferred annot_or_inferred)
    | _ -> annot_or_inferred

  (************)
  (* Visitors *)
  (************)

  (***************************************************************
   * local inference pass: visit AST statement list, calling
   * flow to check types/create graphs for merge-time checking
   ***************************************************************)

  (* can raise Abnormal.(Exn (Stmt _, _)) *)
  let rec statement cx : 'a -> (ALoc.t, ALoc.t * Type.t) Ast.Statement.t =
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
    let check cx b =
      Abnormal.catch_stmts_control_flow_exception (fun () ->
          Toplevels.toplevels statement cx b.Block.body
      )
    in
    let catch_clause cx catch_clause =
      let { Try.CatchClause.param; body = (b_loc, b); comments } = catch_clause in
      let open Ast.Pattern in
      match param with
      | Some p ->
        (match p with
        | (loc, Identifier { Identifier.name = (name_loc, id); annot; optional }) ->
          let r = mk_reason (RCustom "catch") loc in
          let (t, ast_annot) =
            match annot with
            | Ast.Type.Missing mloc ->
              let t = AnyT.why CatchAny r in
              (t, Ast.Type.Missing (mloc, t))
            | Ast.Type.Available ((_, (_, (Ast.Type.Any _ | Ast.Type.Mixed _))) as annot) ->
              (* Not relevant with our limited accepted annotations. *)
              let tparams_map = Subst_name.Map.empty in
              let (t, ast_annot) = Anno.mk_type_available_annotation cx tparams_map annot in
              (t, Ast.Type.Available ast_annot)
            | Ast.Type.Available (_, (loc, _)) ->
              Flow.add_output cx (Error_message.EInvalidCatchParameterAnnotation loc);
              (AnyT.why CatchAny r, Tast_utils.error_mapper#type_annotation_hint annot)
          in
          let (stmts, abnormal_opt) = check cx b in
          ( {
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
              body = (b_loc, { Block.body = stmts; comments = b.Block.comments });
              comments;
            },
            abnormal_opt
          )
        | (loc, _) ->
          Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, CatchParameterDeclaration));
          (Tast_utils.error_mapper#catch_clause catch_clause, None))
      | None ->
        let (stmts, abnormal_opt) = check cx b in
        ( {
            Try.CatchClause.param = None;
            body = (b_loc, { Block.body = stmts; comments = b.Block.comments });
            comments;
          },
          abnormal_opt
        )
    in
    let function_ loc func =
      match func with
      | { Ast.Function.id = None; _ } -> failwith "unexpected anonymous function statement"
      | { Ast.Function.id = Some id; _ } ->
        let { Ast.Function.sig_loc; async; generator; _ } = func in
        let (name_loc, _) = id in
        let reason = func_reason ~async ~generator sig_loc in
        let general = Env.read_declared_type cx reason name_loc in
        let (fn_type, func_ast) = mk_function_declaration cx ~general reason loc func in
        (fn_type, id, (loc, FunctionDeclaration func_ast))
    in
    function
    | (_, Empty _) as stmt -> stmt
    | (loc, Block { Block.body; comments }) ->
      let (body, abnormal_opt) =
        Abnormal.catch_stmts_control_flow_exception (fun () -> Toplevels.toplevels statement cx body)
      in
      Abnormal.check_stmt_control_flow_exception
        ((loc, Block { Block.body; comments }), abnormal_opt)
    | (loc, Expression { Expression.expression = e; directive; comments }) ->
      let expr = expression cx e in
      let ((_, expr_t), expr_ast) = expr in
      begin
        if Env.in_async_scope cx then
          match expr_ast with
          | Flow_ast.Expression.Assignment _ -> ()
          | _ ->
            Flow_js.flow
              cx
              (expr_t, CheckUnusedPromiseT (mk_reason (RCustom "unused promise lint") loc))
      end;
      (loc, Expression { Expression.expression = expr; directive; comments })
    (* Refinements for `if` are derived by the following Hoare logic rule:

       [Pre & c] S1 [Post1]
       [Pre & ~c] S2 [Post2]
       Post = Post1 | Post2
       ----------------------------
       [Pre] if c S1 else S2 [Post]
    *)
    | (loc, If { If.test; consequent; alternate; comments }) ->
      let test_ast = condition ~cond:OtherTest cx test in
      let (then_ast, then_abnormal) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx consequent)
      in
      let (else_ast, else_abnormal) =
        match alternate with
        | None -> (None, None)
        | Some (loc, { If.Alternate.body; comments }) ->
          let (body_ast, else_abnormal) =
            Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
          in
          (Some (loc, { If.Alternate.body = body_ast; comments }), else_abnormal)
      in

      let ast =
        (loc, If { If.test = test_ast; consequent = then_ast; alternate = else_ast; comments })
      in
      (* handle control flow in cases where we've thrown from both sides *)
      begin
        match (then_abnormal, else_abnormal) with
        | (Some Abnormal.Throw, Some Abnormal.Return)
        | (Some Abnormal.Return, Some Abnormal.Throw) ->
          Abnormal.throw_stmt_control_flow_exception ast Abnormal.Return
        | (Some then_exn, Some else_exn) when then_exn = else_exn ->
          Abnormal.throw_stmt_control_flow_exception ast then_exn
        | (Some (Abnormal.Break then_opt_label), Some (Abnormal.Continue else_opt_label))
        | (Some (Abnormal.Continue then_opt_label), Some (Abnormal.Break else_opt_label))
          when then_opt_label = else_opt_label ->
          Abnormal.throw_stmt_control_flow_exception ast (Abnormal.Continue then_opt_label)
        | _ -> ast
      end
    | ( top_loc,
        Labeled
          { Labeled.label = (_, { Ast.Identifier.name; comments = _ }) as lab_ast; body; comments }
      ) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _) ->
        ignore loc;
        let label = Some name in

        let (body_ast, body_abnormal) =
          Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
          |> Abnormal.ignore_break_or_continue_to_label label
        in
        let ast = (top_loc, Labeled { Labeled.label = lab_ast; body = body_ast; comments }) in
        ignore
          ( Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)
            : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t
            );

        ast
      | _ ->
        let label = Some name in
        let (body_ast, body_abnormal) =
          Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
          |> Abnormal.ignore_break_to_label label
        in
        let ast = (top_loc, Labeled { Labeled.label = lab_ast; body = body_ast; comments }) in
        ignore
          ( Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)
            : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t
            );

        ast)
    | (loc, Break { Break.label; comments }) ->
      (* save environment at unlabeled breaks, prior to activation clearing *)
      let (label_opt, label_ast) =
        match label with
        | None -> (None, None)
        | Some ((_, { Ast.Identifier.name; comments = _ }) as lab_ast) -> (Some name, Some lab_ast)
      in
      let ast = (loc, Break { Break.label = label_ast; comments }) in
      let abnormal = Abnormal.Break label_opt in
      Abnormal.throw_stmt_control_flow_exception ast abnormal
    | (loc, Continue { Continue.label; comments }) ->
      let (label_opt, label_ast) =
        match label with
        | None -> (None, None)
        | Some ((_, { Ast.Identifier.name; comments = _ }) as lab_ast) -> (Some name, Some lab_ast)
      in
      let ast = (loc, Continue { Continue.label = label_ast; comments }) in
      let abnormal = Abnormal.Continue label_opt in
      Abnormal.throw_stmt_control_flow_exception ast abnormal
    | (_, With _) as s ->
      (* TODO or disallow? *)
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
    (*******************************************************)
    | (switch_loc, Switch { Switch.discriminant; cases; comments; exhaustive_out }) ->
      (* typecheck discriminant *)
      let discriminant_ast = expression cx discriminant in
      let exhaustive_check_incomplete_out =
        Tvar.mk cx (mk_reason (RCustom "exhaustive check incomplete out") switch_loc)
      in

      (* traverse case list, get list of control flow exits and list of ASTs *)
      let (exits_rev, cases_ast_rev, fallthrough_case, has_default) =
        cases
        |> Base.List.fold_left
             ~init:([], [], None, false)
             ~f:(fun
                  (exits, cases_ast, _fallthrough_case, has_default)
                  (loc, { Switch.Case.test; consequent; comments })
                ->
               (* compute predicates implied by case expr or default *)
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
                   let case_test_reason = mk_reason (RCustom "case test") (fst expr) in
                   let switch_discriminant_reason =
                     mk_reason (RCustom "switch discriminant") (fst discriminant)
                   in
                   let (_, fake_ast) =
                     condition
                       cx
                       ~cond:(SwitchTest { case_test_reason; switch_discriminant_reason })
                       fake
                   in
                   let expr_ast =
                     match fake_ast with
                     | Ast.Expression.(Binary { Binary.right; _ }) -> right
                     | _ -> assert false
                   in
                   Some expr_ast
               in

               (* process statements, track control flow exits: exit will be an
                  unconditional exit. *)
               let (consequent_ast, exit) =
                 Abnormal.catch_stmts_control_flow_exception (fun () ->
                     Toplevels.toplevels statement cx consequent
                 )
               in
               (* track fallthrough to next case and/or break to switch end *)
               let falls_through =
                 match exit with
                 | Some Abnormal.Throw
                 | Some Abnormal.Return
                 | Some (Abnormal.Break _)
                 | Some (Abnormal.Continue _) ->
                   false
                 | None -> true
               in
               (* save state for fallthrough *)
               let fallthrough_case =
                 if falls_through then
                   Some loc
                 else
                   None
               in

               ( exit :: exits,
                 (loc, { Switch.Case.test = test_ast; consequent = consequent_ast; comments })
                 :: cases_ast,
                 fallthrough_case,
                 has_default || Base.Option.is_none test
               )
           )
      in
      let cases_ast = List.rev cases_ast_rev in
      let exits = List.rev exits_rev in
      (* If no default was present, record a write to maybe_exhaustively_checked and then update
       * the switch state to account for this write in the total/partial writes. We need to also
       * merge in the fallthrough case if one existed. *)
      let () =
        if not has_default then (
          if Base.Option.is_none fallthrough_case then
            Env.init_let
              cx
              ~use_op:unknown_use
              exhaustive_check_incomplete_out
              (loc_of_t exhaustive_check_incomplete_out);
          (* If we handle the fallthrough case explicitly here then there is no need to merge
           * in those changes a second time. Instead, we set the fallthrough_case to None *)
          ()
        ) else
          ()
      in

      (* abnormal exit: if every case exits abnormally the same way (or falls
          through to a case that does), then the switch as a whole exits that way.
         (as with if/else, we merge `throw` into `return` when both appear) *)
      let uniform_switch_exit case_exits =
        let rec loop = function
          | (acc, fallthrough, []) ->
            (* end of cases: if nothing is falling through, we made it *)
            if fallthrough then
              None
            else
              acc
          | (_, _, Some (Abnormal.Break _) :: _) ->
            (* break wrecks everything *)
            None
          | (acc, _, None :: exits) ->
            (* begin or continue to fall through *)
            loop (acc, true, exits)
          | (acc, _, exit :: exits) when exit = acc ->
            (* current case exits the same way as prior cases *)
            loop (acc, acc = None, exits)
          | (Some Abnormal.Throw, _, Some Abnormal.Return :: exits)
          | (Some Abnormal.Return, _, Some Abnormal.Throw :: exits) ->
            (* fuzz throw into return *)
            loop (Some Abnormal.Return, false, exits)
          | (None, _, exit :: exits) ->
            (* terminate an initial sequence of fall-thruugh cases *)
            (* (later sequences will have acc = Some _ ) *)
            loop (exit, false, exits)
          | (_, _, _) ->
            (* the new case exits differently from previous ones - fail *)
            None
        in
        if has_default then
          loop (None, false, case_exits)
        else
          None
      in
      let enum_exhaustive_check = enum_exhaustive_check_of_switch_cases cases_ast in
      let ((_, discriminant_t), _) = discriminant_ast in
      let discriminant_after_check =
        if not has_default then
          let refinement_key = Refinement.key ~allow_optional:true discriminant in
          Env.discriminant_after_negated_cases cx switch_loc refinement_key
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
      let ast =
        ( switch_loc,
          Switch
            {
              Switch.discriminant = discriminant_ast;
              cases = cases_ast;
              comments;
              exhaustive_out = (exhaustive_out, exhaustive_check_incomplete_out);
            }
        )
      in
      (match uniform_switch_exit exits with
      | None -> ast
      | Some abnormal -> Abnormal.throw_stmt_control_flow_exception ast abnormal)
    (*******************************************************)
    | (loc, Return { Return.argument; comments; return_out }) ->
      let reason = mk_reason (RCustom "return") loc in
      let (t, argument_ast) =
        match argument with
        | None -> (VoidT.at loc |> with_trust literal_trust, None)
        | Some expr ->
          if Env.in_predicate_scope cx then
            let (((_, t), _) as ast) = condition ~cond:OtherTest cx expr in
            let (p_map, n_map) = Env.predicate_refinement_maps cx loc in
            let pred_reason = update_desc_reason (fun desc -> RPredicateOf desc) reason in
            (OpenPredT { reason = pred_reason; base_t = t; m_pos = p_map; m_neg = n_map }, Some ast)
          else
            let (((_, t), _) as ast) = expression cx expr in
            (t, Some ast)
      in
      Abnormal.throw_stmt_control_flow_exception
        (loc, Return { Return.argument = argument_ast; comments; return_out = (return_out, t) })
        Abnormal.Return
    | (loc, Throw { Throw.argument; comments }) ->
      let argument_ast = expression cx argument in
      Abnormal.throw_stmt_control_flow_exception
        (loc, Throw { Throw.argument = argument_ast; comments })
        Abnormal.Throw
    (***************************************************************************)
    (* Try-catch-finally statements have a lot of control flow possibilities. (To
     simplify matters, a missing catch block is considered to to be a catch
     block that throws, and a missing finally block is considered to be an empty
     block.)

     A try block may either

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the catch block.

     A catch block may either:

     * exit normally: in this case, it proceeds to the finally block.

     * exit abnormally: in this case, it proceeds to the finally block and
     throws at the end of the finally block.

     A finally block may either:

     * exit normally: in this case, the try-catch-finally statement exits
     normally. (Note that to be in this case, either the try block exited
     normally, or it didn't and the catch block exited normally.)

     * exit abnormally: in this case, the try-catch-finally statement exits
     abnormally.

     Based on these possibilities, approximations for the local state at various
     points in a try-catch-finally statement can be derived.

     * The start of a catch block is reachable via anywhere in the try
     block. Thus, the local state must be conservative.

     * The start of a finally block is reachable via the end of the try block,
     or anywhere in the catch block. Thus, the local state must be conservative.

     * The end of a try-catch-finally statement is reachable via the end of the
     finally block. However, in this case we can assume that either

     ** the try block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of the
     try block.

     ** the catch block exited normally, in which case the local state at the
     start of the finally block is the same as the local state at the end of
     the catch block.

     Thus, a finally block should be analyzed twice, with each of the following
     assumptions for the local state at its start: (1) conservative (to model
     abnormal exits in the try or catch blocks); (2) whatever is at the end of
     the try block merged with whatever is at the end of the catch block (for
     normal exits in the try and catch blocks).

     Important to understand: since (1) is conservative, it should produce
     errors whenever (2) does, so that's not why we do them separately.
     But since (2) models exactly the states from which subsequent code is
     reachable, we can use its tighter approximation as the basis for
     subsequent analysis without loss of soundness.
     *)
    (***************************************************************************)
    | (loc, Try { Try.block = (b_loc, b); handler; finalizer; comments }) ->
      let (try_block_ast, try_abnormal) =
        Abnormal.catch_stmts_control_flow_exception (fun () ->
            Toplevels.toplevels statement cx b.Block.body
        )
      in
      (* traverse catch block, save exceptions *)
      let (catch_ast, catch_abnormal) =
        match handler with
        | None ->
          (* a missing catch is equivalent to a catch that always throws *)
          (None, Some Abnormal.Throw)
        | Some (h_loc, h) ->
          let (catch_block_ast, catch_abnormal) = catch_clause cx h in
          (Some (h_loc, catch_block_ast), catch_abnormal)
      in
      let (finally_ast, finally_abnormal) =
        match finalizer with
        | None -> (None, None)
        | Some (f_loc, { Block.body; comments }) ->
          let (finally_block_ast, finally_abnormal) =
            Abnormal.catch_stmts_control_flow_exception (fun () ->
                Toplevels.toplevels statement cx body
            )
          in
          (Some (f_loc, { Block.body = finally_block_ast; comments }), finally_abnormal)
      in

      let ast =
        ( loc,
          Try
            {
              Try.block = (b_loc, { Block.body = try_block_ast; comments = b.Block.comments });
              handler = catch_ast;
              finalizer = finally_ast;
              comments;
            }
        )
      in
      (* if finally has abnormal control flow, we throw here *)
      ignore
        ( Abnormal.check_stmt_control_flow_exception (ast, finally_abnormal)
          : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t
          );

      (* other ways we throw due to try/catch abends *)
      begin
        match (try_abnormal, catch_abnormal) with
        | (Some (Abnormal.Throw as try_abnormal), Some Abnormal.Throw)
        | (Some (Abnormal.Return as try_abnormal), Some _) ->
          Abnormal.throw_stmt_control_flow_exception ast try_abnormal
        | (Some Abnormal.Throw, Some (Abnormal.Return as catch_abnormal)) ->
          Abnormal.throw_stmt_control_flow_exception ast catch_abnormal
        | _ -> ast
      end
    (***************************************************************************)
    (* Refinements for `while` are derived by the following Hoare logic rule:

       [Pre' & c] S [Post']
       Pre' = Pre | Post'
       Post = Pre' & ~c
       ----------------------
       [Pre] while c S [Post]
    *)
    (***************************************************************************)
    | (loc, While { While.test; body; comments }) ->
      (* generate loop test preds and their complements *)
      let test_ast = condition ~cond:OtherTest cx test in
      (* traverse loop body - after this, body_env = Post' *)
      let (body_ast, _) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
      in
      (loc, While { While.test = test_ast; body = body_ast; comments })
    (***************************************************************************)
    (* Refinements for `do-while` are derived by the following Hoare logic rule:

       [Pre'] S [Post']
       Pre' = Pre | (Post' & c)
       Post = Post' & ~c
       -------------------------
       [Pre] do S while c [Post]
    *)
    (***************************************************************************)
    | (loc, DoWhile { DoWhile.body; test; comments }) ->
      let (body_ast, body_abnormal) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
        |> Abnormal.ignore_break_or_continue_to_label None
      in
      let test_ast = condition ~cond:OtherTest cx test in
      let ast = (loc, DoWhile { DoWhile.body = body_ast; test = test_ast; comments }) in
      Abnormal.check_stmt_control_flow_exception (ast, body_abnormal)
    (***************************************************************************)
    (* Refinements for `for` are derived by the following Hoare logic rule:

       [Pre] i [Init]
       [Pre' & c] S;u [Post']
       Pre' = Init | Post'
       Post = Pre' & ~c
       --------------------------
       [Pre] for (i;c;u) S [Post]

       NOTE: This rule is similar to that for `while`.
    *)
    (***************************************************************************)
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
      let (body_ast, _) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
      in
      let update_ast = Base.Option.map ~f:(expression cx) update in
      ( loc,
        For { For.init = init_ast; test = test_ast; update = update_ast; body = body_ast; comments }
      )
    (***************************************************************************)
    (* Refinements for `for-in` are derived by the following Hoare logic rule:

       [Pre] o [Init]
       [Pre'] S [Post']
       Pre' = Init | Post'
       Post = Pre'
       --------------------------
       [Pre] for (i in o) S [Post]
    *)
    (***************************************************************************)
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
          let (id_ast, _) =
            variable cx kind id None ~if_uninitialized:(StrT.at %> with_trust bogus_trust)
          in
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
          let t = StrT.at pat_loc |> with_trust bogus_trust in
          let use_op =
            Op
              (AssignVar
                 {
                   var = Some (mk_reason (RIdentifier (OrdinaryName name_str)) pat_loc);
                   init = reason_of_t t;
                 }
              )
          in
          Env.set_var cx ~use_op name_str t pat_loc;
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

      let (body_ast, _) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
      in

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
          Env.set_var cx ~use_op name_str elem_t pat_loc;
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
      let (body_ast, _) =
        Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx body)
      in

      (loc, ForOf { ForOf.left = left_ast; right = right_ast; body = body_ast; await; comments })
    | (_, Debugger _) as stmt -> stmt
    | (loc, FunctionDeclaration func) ->
      let (_, _, node) = function_ loc func in
      node
    | (loc, EnumDeclaration enum) ->
      let enum_ast = enum_declaration cx loc enum in
      (loc, EnumDeclaration enum_ast)
    | (loc, DeclareVariable { DeclareVariable.id = (id_loc, id); annot; comments }) ->
      let (t, annot_ast) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
      (loc, DeclareVariable { DeclareVariable.id = ((id_loc, t), id); annot = annot_ast; comments })
    | (loc, DeclareFunction declare_function) ->
      (match declare_function_to_function_declaration cx loc declare_function with
      | Some (FunctionDeclaration func, reconstruct_ast) ->
        let (_, _, node) = function_ loc func in
        (loc, DeclareFunction (reconstruct_ast node))
      | _ ->
        (* error case *)
        let { DeclareFunction.id = (id_loc, id_name); annot; predicate; comments } =
          declare_function
        in
        let (t, annot_ast) = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
        let predicate = Base.Option.map ~f:Tast_utils.error_mapper#type_predicate predicate in
        ( loc,
          DeclareFunction
            { DeclareFunction.id = ((id_loc, t), id_name); annot = annot_ast; predicate; comments }
        ))
    | (loc, VariableDeclaration decl) -> (loc, VariableDeclaration (variables cx decl))
    | (_, ClassDeclaration { Ast.Class.id = None; _ }) ->
      failwith "unexpected anonymous class declaration"
    | (class_loc, ClassDeclaration ({ Ast.Class.id = Some id; _ } as c)) ->
      let (name_loc, { Ast.Identifier.name; comments = _ }) = id in
      let name = OrdinaryName name in
      let reason = DescFormat.instance_reason name name_loc in
      let general = Env.read_declared_type cx reason name_loc in
      (* ClassDeclarations are statements, so we will never have an annotation to push down here *)
      let (class_t, c_ast) = mk_class cx class_loc ~name_loc ~general reason c in
      let use_op =
        Op
          (AssignVar
             { var = Some (mk_reason (RIdentifier name) name_loc); init = reason_of_t class_t }
          )
      in
      Env.init_implicit_let cx ~use_op class_t name_loc;
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
      Env.init_var cx ~use_op t name_loc;
      (loc, DeclareClass decl_ast)
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
      let (_, decl_ast) = declare_module cx loc module_ in
      (loc, DeclareModule decl_ast)
    | (loc, DeclareExportDeclaration decl) ->
      let module D = DeclareExportDeclaration in
      let { D.default; declaration; specifiers; source; comments = _ } = decl in
      let declaration =
        let export_maybe_default_binding ?is_function id =
          let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
          let name = OrdinaryName name in
          match default with
          | None ->
            Import_export.export_binding cx ?is_function name id_loc Ast.Statement.ExportValue
          | Some default_loc ->
            let t = Env.get_var_declared_type ~lookup_mode:ForType cx name id_loc in
            Import_export.export cx (OrdinaryName "default") default_loc t
        in
        (* error-handling around calls to `statement` is omitted here because we
           don't expect declarations to have abnormal control flow *)
        let f = function
          | D.Variable (loc, ({ DeclareVariable.id; _ } as v)) ->
            let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
            let dec_var = statement cx (loc, DeclareVariable v) in
            Import_export.export_binding cx (OrdinaryName name) id_loc Ast.Statement.ExportValue;
            begin
              match dec_var with
              | (_, DeclareVariable v_ast) -> D.Variable (loc, v_ast)
              | _ -> assert_false "DeclareVariable typed AST doesn't preserve structure"
            end
          | D.Function (loc, f) ->
            let dec_fun = statement cx (loc, DeclareFunction f) in
            export_maybe_default_binding ~is_function:true f.DeclareFunction.id;
            begin
              match dec_fun with
              | (_, DeclareFunction f_ast) -> D.Function (loc, f_ast)
              | _ -> assert_false "DeclareFunction typed AST doesn't preserve structure"
            end
          | D.Class (loc, c) ->
            let dec_class = statement cx (loc, DeclareClass c) in
            export_maybe_default_binding c.DeclareClass.id;
            begin
              match dec_class with
              | (_, DeclareClass c_ast) -> D.Class (loc, c_ast)
              | _ -> assert_false "DeclareClass typed AST doesn't preserve structure"
            end
          | D.DefaultType (loc, t) ->
            let default_loc = Base.Option.value_exn default in
            let (((_, t), _) as t_ast) = Anno.convert cx Subst_name.Map.empty (loc, t) in
            Import_export.export cx (OrdinaryName "default") default_loc t;
            D.DefaultType t_ast
          | D.NamedType (loc, ({ TypeAlias.id; _ } as t)) ->
            let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
            let type_alias = statement cx (loc, TypeAlias t) in
            Import_export.export_binding cx (OrdinaryName name) id_loc Ast.Statement.ExportType;
            begin
              match type_alias with
              | (_, TypeAlias talias) -> D.NamedType (loc, talias)
              | _ -> assert_false "TypeAlias typed AST doesn't preserve structure"
            end
          | D.NamedOpaqueType (loc, ({ OpaqueType.id; _ } as t)) ->
            let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
            let opaque_type = statement cx (loc, OpaqueType t) in
            Import_export.export_binding cx (OrdinaryName name) id_loc Ast.Statement.ExportType;
            begin
              match opaque_type with
              | (_, OpaqueType opaque_t) -> D.NamedOpaqueType (loc, opaque_t)
              | _ -> assert_false "OpaqueType typed AST doesn't preserve structure"
            end
          | D.Interface (loc, ({ Interface.id; _ } as i)) ->
            let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
            let int_dec = statement cx (loc, InterfaceDeclaration i) in
            Import_export.export_binding cx (OrdinaryName name) id_loc Ast.Statement.ExportType;
            begin
              match int_dec with
              | (_, InterfaceDeclaration i_ast) -> D.Interface (loc, i_ast)
              | _ -> assert_false "InterfaceDeclaration typed AST doesn't preserve structure"
            end
          | D.Enum (loc, ({ EnumDeclaration.id; _ } as enum)) ->
            let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
            let enum_ast = enum_declaration cx loc enum in
            if Context.enable_enums cx then
              Import_export.export_binding cx (OrdinaryName name) id_loc Ast.Statement.ExportType;
            D.Enum (loc, enum_ast)
        in
        Option.map f declaration
      in
      Option.iter (export_specifiers cx loc source Ast.Statement.ExportValue) specifiers;
      (loc, DeclareExportDeclaration { decl with D.declaration })
    | (loc, DeclareModuleExports { Ast.Statement.DeclareModuleExports.annot = (t_loc, t); comments })
      ->
      let (((_, t), _) as t_ast) = Anno.convert cx Subst_name.Map.empty t in
      Import_export.cjs_clobber cx loc t;
      let reason =
        let filename = Context.file cx in
        mk_reason
          (RModule (OrdinaryName (File_key.to_string filename)))
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
          begin
            match stmt with
            | FunctionDeclaration { Ast.Function.id = Some id; _ }
            | ClassDeclaration { Ast.Class.id = Some id; _ }
            | TypeAlias { TypeAlias.id; _ }
            | OpaqueType { OpaqueType.id; _ }
            | InterfaceDeclaration { Interface.id; _ }
            | EnumDeclaration { EnumDeclaration.id; _ } ->
              let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
              Type_inference_hooks_js.dispatch_export_named_hook name id_loc;
              Import_export.export_binding cx (OrdinaryName name) id_loc export_kind
            | VariableDeclaration { VariableDeclaration.declarations; _ } ->
              Flow_ast_utils.fold_bindings_of_variable_declarations
                (fun _ () id ->
                  let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
                  Type_inference_hooks_js.dispatch_export_named_hook name id_loc;
                  Import_export.export_binding cx (OrdinaryName name) id_loc export_kind)
                ()
                declarations
            | _ -> failwith "Parser Error: Invalid export-declaration type!"
          end;
          Some stmt'
      in
      Option.iter (export_specifiers cx loc source export_kind) specifiers;
      (loc, ExportNamedDeclaration { export_decl with ExportNamedDeclaration.declaration })
    | (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration; comments }) ->
      let module D = ExportDefaultDeclaration in
      Type_inference_hooks_js.dispatch_export_named_hook "default" default;
      let (export_loc, t, declaration) =
        match declaration with
        | D.Declaration (loc, stmt) ->
          let (export_loc, t, stmt) =
            match stmt with
            | FunctionDeclaration ({ Ast.Function.id = None; _ } as fn) ->
              let { Ast.Function.sig_loc; async; generator; _ } = fn in
              let reason = func_reason ~async ~generator sig_loc in
              let general = Tvar.mk cx reason in
              let (t, fn) = mk_function_declaration cx ~general reason loc fn in
              Flow_js.flow_t cx (t, general);
              (loc, general, (loc, FunctionDeclaration fn))
            | ClassDeclaration ({ Ast.Class.id = None; _ } as c) ->
              let reason = DescFormat.instance_reason (internal_name "*default*") loc in
              let general = Tvar.mk cx reason in
              let (t, c) = mk_class cx loc ~name_loc:loc ~general reason c in
              Flow_js.flow_t cx (t, general);
              (loc, general, (loc, ClassDeclaration c))
            | FunctionDeclaration { Ast.Function.id = Some id; _ }
            | ClassDeclaration { Ast.Class.id = Some id; _ }
            | EnumDeclaration { EnumDeclaration.id; _ } ->
              let stmt = statement cx (loc, stmt) in
              let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
              let t =
                Env.get_var_declared_type ~lookup_mode:ForValue cx (OrdinaryName name) id_loc
              in
              (id_loc, t, stmt)
            | _ -> failwith "unexpected default export declaration"
          in
          (export_loc, t, D.Declaration stmt)
        | D.Expression expr ->
          let (((loc, t), _) as expr) = expression cx expr in
          (loc, t, D.Expression expr)
      in
      Import_export.export cx (OrdinaryName "default") export_loc t;
      (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration; comments })
    | (import_loc, ImportDeclaration import_decl) ->
      if File_key.is_lib_file (Context.file cx) && Env.in_global_scope cx then
        Flow_js.add_output cx Error_message.(EToplevelLibraryImport import_loc);
      let { ImportDeclaration.source; specifiers; default; import_kind; comments } = import_decl in
      let (source_loc, ({ Ast.StringLiteral.value = module_name; _ } as source_literal)) = source in

      let source_ast =
        let reason = mk_reason (RModule (OrdinaryName module_name)) source_loc in
        (* force a non-strict import and allow an untyped module because we don't
           want to check for untyped-import or nonstrict-import warnings
           on the module name, which is not an actual "use" of the module like
           the specifiers are. *)
        let source_t =
          Import_export.import_ns
            ~strict:false
            ~allow_untyped:true
            cx
            reason
            (source_loc, module_name)
        in
        ((source_loc, source_t), source_literal)
      in

      let specifiers_ast =
        match specifiers with
        | Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers) ->
          let named_specifiers_ast =
            named_specifiers
            |> Base.List.map ~f:(function
                   | { Ast.Statement.ImportDeclaration.local; remote; kind } ->
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
                   let imported_t =
                     import_named_specifier_type
                       cx
                       import_reason
                       import_kind
                       ~source_loc
                       ~module_name
                       ~remote_name_loc
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
                   { Ast.Statement.ImportDeclaration.local = local_ast; remote = remote_ast; kind }
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
              import_namespace_specifier_type
                cx
                import_reason
                import_kind
                ~source_loc
                ~module_name
                ~local_loc
            in
            ((local_loc, t), local_id)
          in
          Some (ImportDeclaration.ImportNamespaceSpecifier (loc_with_star, namespace_specifier_ast))
        | None -> None
      in
      let default_ast =
        match default with
        | Some (loc, ({ Ast.Identifier.name = local_name; comments = _ } as id)) ->
          let import_reason = mk_reason (RDefaultImportedType (local_name, module_name)) loc in
          let imported_t =
            import_default_specifier_type
              cx
              import_reason
              import_kind
              ~source_loc
              ~module_name
              ~local_loc:loc
              ~local_name
          in
          Some ((loc, imported_t), id)
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

  and for_of_elemt cx right_t reason await =
    let elem_t = Tvar.mk cx reason in
    let loc = aloc_of_reason reason in
    (* Second and third args here are never relevant to the loop, but they should be as
       general as possible to allow iterating over arbitrary generators *)
    let targs =
      [
        elem_t;
        MixedT.why reason |> with_trust bogus_trust;
        EmptyT.why reason |> with_trust bogus_trust;
      ]
    in
    let (async, iterable_reason) =
      if await then
        (true, mk_reason (RCustom "async iteration expected on AsyncIterable") loc)
      else
        (false, mk_reason (RCustom "iteration expected on Iterable") loc)
    in
    Flow.flow
      cx
      (right_t, AssertIterableT { use_op = unknown_use; reason = iterable_reason; async; targs });
    elem_t

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
        poly_type_of_tparams
          (Type.Poly.generate_id ())
          tparams
          (DefT (r, bogus_trust (), TypeT (TypeAliasKind, t)))
      in
      begin
        match tparams with
        | None -> ()
        | Some (_, tps) ->
          (* TODO: use tparams_map *)
          let tparams =
            Nel.fold_left (fun acc tp -> Subst_name.Map.add tp.name tp acc) Subst_name.Map.empty tps
          in
          Flow.check_polarity cx tparams Polarity.Positive t
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

  and opaque_type
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
          Base.Option.iter underlying_t ~f:(Flow.check_polarity cx tparams Polarity.Positive);
          Base.Option.iter super_t ~f:(Flow.check_polarity cx tparams Polarity.Positive)
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
        poly_type_of_tparams
          (Type.Poly.generate_id ())
          tparams
          (DefT (r, bogus_trust (), TypeT (OpaqueKind, t)))
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

  and type_kind_of_kind = function
    | Ast.Statement.ImportDeclaration.ImportType -> Type.ImportType
    | Ast.Statement.ImportDeclaration.ImportTypeof -> Type.ImportTypeof
    | Ast.Statement.ImportDeclaration.ImportValue -> Type.ImportValue

  and get_imported_t cx get_reason module_name module_t import_kind remote_export_name local_name =
    Tvar.mk_where cx get_reason (fun t ->
        let import_type =
          if remote_export_name = "default" then
            ImportDefaultT
              (get_reason, import_kind, (local_name, module_name), t, Context.is_strict cx)
          else
            ImportNamedT
              (get_reason, import_kind, remote_export_name, module_name, t, Context.is_strict cx)
        in
        Flow.flow cx (module_t, import_type)
    )

  and import_named_specifier_type
      cx
      import_reason
      import_kind
      ~source_loc
      ~module_name
      ~remote_name_loc
      ~remote_name
      ~local_name =
    let module_t = OpenT (Import_export.import cx (source_loc, module_name)) in
    if Type_inference_hooks_js.dispatch_member_hook cx remote_name remote_name_loc module_t then
      Unsoundness.why InferenceHooks import_reason
    else
      let import_kind = type_kind_of_kind import_kind in
      get_imported_t cx import_reason module_name module_t import_kind remote_name local_name

  and import_namespace_specifier_type
      cx import_reason import_kind ~source_loc ~module_name ~local_loc =
    let open Ast.Statement in
    match import_kind with
    | ImportDeclaration.ImportType -> assert_false "import type * is a parse error"
    | ImportDeclaration.ImportTypeof ->
      let bind_reason = repos_reason local_loc import_reason in
      let module_ns_t = Import_export.import_ns cx import_reason (source_loc, module_name) in
      Tvar.mk_where cx bind_reason (fun t ->
          Flow.flow cx (module_ns_t, ImportTypeofT (bind_reason, "*", t))
      )
    | ImportDeclaration.ImportValue ->
      let reason = mk_reason (RModule (OrdinaryName module_name)) local_loc in
      Import_export.import_ns cx reason (source_loc, module_name)

  and import_default_specifier_type
      cx import_reason import_kind ~source_loc ~module_name ~local_loc ~local_name =
    let module_t = OpenT (Import_export.import cx (source_loc, module_name)) in
    if Type_inference_hooks_js.dispatch_member_hook cx "default" local_loc module_t then
      Unsoundness.why InferenceHooks import_reason
    else
      let import_kind = type_kind_of_kind import_kind in
      get_imported_t cx import_reason module_name module_t import_kind "default" local_name

  and export_specifiers cx loc source export_kind =
    let open Ast.Statement in
    let module E = ExportNamedDeclaration in
    let lookup_mode =
      match export_kind with
      | Ast.Statement.ExportValue -> ForValue
      | Ast.Statement.ExportType -> ForType
    in
    let source =
      match source with
      | Some (loc, { Ast.StringLiteral.value; raw = _; comments = _ }) -> Some (loc, value)
      | None -> None
    in
    (* [declare] export [type] {foo [as bar]}; *)
    let export_ref loc local_name remote_name =
      let t = Env.var_ref ~lookup_mode cx local_name loc in
      match export_kind with
      | Ast.Statement.ExportType ->
        let reason = mk_reason (RType local_name) loc in
        let t =
          Tvar.mk_where cx reason (fun tout ->
              Flow.flow cx (t, AssertExportIsTypeT (reason, local_name, tout))
          )
        in
        Import_export.export_type cx remote_name (Some loc) t
      | Ast.Statement.ExportValue -> Import_export.export cx remote_name loc t
    in
    (* [declare] export [type] {foo [as bar]} from 'module' *)
    let export_from loc (source_loc, source) local_name remote_name =
      let source_ns_t =
        let reason = mk_reason (RModule (OrdinaryName source)) source_loc in
        Import_export.import_ns cx reason (source_loc, source)
      in
      let t =
        let reason = mk_reason (RIdentifier local_name) loc in
        Tvar.mk_no_wrap_where cx reason (fun tout ->
            let use_t = GetPropT (unknown_use, reason, None, Named (reason, local_name), tout) in
            Flow.flow cx (source_ns_t, use_t)
        )
      in
      match export_kind with
      | Ast.Statement.ExportType -> Import_export.export_type cx remote_name (Some loc) t
      | Ast.Statement.ExportValue -> Import_export.export cx remote_name loc t
    in
    let export_specifier (loc, { E.ExportSpecifier.local; exported }) =
      let (local_name, remote_name, local_name_loc) =
        let (local_name_loc, { Ast.Identifier.name = local_name; comments = _ }) = local in
        let local_name = OrdinaryName local_name in
        match exported with
        | None -> (local_name, local_name, local_name_loc)
        | Some (_, { Ast.Identifier.name = remote_name; comments = _ }) ->
          (local_name, OrdinaryName remote_name, local_name_loc)
      in
      match source with
      | Some source -> export_from loc source local_name remote_name
      | None -> export_ref local_name_loc local_name remote_name
    in
    function
    (* [declare] export [type] {foo [as bar]} [from ...]; *)
    | E.ExportSpecifiers specifiers -> List.iter export_specifier specifiers
    (* [declare] export [type] * as id from "source"; *)
    | E.ExportBatchSpecifier (_, Some id) ->
      let (id_loc, { Ast.Identifier.name; comments = _ }) = id in
      let reason = mk_reason (RIdentifier (OrdinaryName name)) id_loc in
      let remote_namespace_t = Import_export.import_ns cx reason (Base.Option.value_exn source) in
      Import_export.export cx (OrdinaryName name) loc remote_namespace_t
    (* [declare] export [type] * from "source"; *)
    | E.ExportBatchSpecifier (_, None) ->
      let source_module_t = OpenT (Import_export.import cx (Base.Option.value_exn source)) in
      let reason = mk_reason (RCustom "batch export") loc in
      Flow.flow cx (source_module_t, CheckUntypedImportT (reason, ImportValue));
      (match export_kind with
      | Ast.Statement.ExportValue -> Import_export.export_star cx loc source_module_t
      | Ast.Statement.ExportType -> Import_export.export_type_star cx loc source_module_t)

  and interface_helper cx loc (iface_sig, self) =
    let def_reason = mk_reason (desc_of_t self) loc in
    Class_type_sig.check_super cx def_reason iface_sig;
    Class_type_sig.check_implements cx def_reason iface_sig;
    Class_type_sig.check_methods cx def_reason iface_sig;
    let (t_internal, t) = Class_type_sig.classtype ~check_polarity:false cx iface_sig in
    Flow.unify cx self t_internal;
    t

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
      let reason = DescFormat.instance_reason (OrdinaryName name) name_loc in
      let (iface_sig, iface_t, decl_ast) = Anno.mk_interface_sig cx loc reason decl in
      let t = interface_helper cx loc (iface_sig, iface_t) in
      (t, decl_ast)

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
      let reason = DescFormat.instance_reason (OrdinaryName name) name_loc in
      let (class_sig, class_t, decl_ast) = Anno.mk_declare_class_sig cx loc reason decl in
      let t = interface_helper cx loc (class_sig, class_t) in
      (t, decl_ast)

  and declare_module cx loc { Ast.Statement.DeclareModule.id; body; kind; comments } =
    let open Ast.Statement in
    let node_cache = Context.node_cache cx in
    match Node_cache.get_declared_module node_cache loc with
    | Some x -> x
    | None ->
      let (id_loc, name) =
        match id with
        | DeclareModule.Identifier (id_loc, { Ast.Identifier.name = value; comments = _ })
        | DeclareModule.Literal (id_loc, { Ast.StringLiteral.value; _ }) ->
          (id_loc, value)
      in
      let (body_loc, { Ast.Statement.Block.body = elements; comments = elements_comments }) =
        body
      in
      let prev_scope_kind = Env.set_scope_kind cx Name_def.Ordinary in
      Context.push_declare_module cx (Module_info.empty_cjs_module ());

      let (elements_ast, elements_abnormal) =
        Abnormal.catch_stmts_control_flow_exception (fun () ->
            Toplevels.toplevels statement cx elements
        )
      in
      let reason = mk_reason (RModule (OrdinaryName name)) id_loc in
      Env.init_declare_module_synthetic_module_exports
        cx
        ~export_type:Import_export.export_type
        loc
        reason;
      let module_t = Import_export.mk_module_t cx reason loc in
      let ast =
        {
          DeclareModule.id =
            begin
              match id with
              | DeclareModule.Identifier (id_loc, id) ->
                DeclareModule.Identifier ((id_loc, module_t), id)
              | DeclareModule.Literal (id_loc, lit) ->
                DeclareModule.Literal ((id_loc, module_t), lit)
            end;
          body = (body_loc, { Block.body = elements_ast; comments = elements_comments });
          kind;
          comments;
        }
      in
      ignore
        ( Abnormal.check_stmt_control_flow_exception ((loc, DeclareModule ast), elements_abnormal)
          : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t
          );

      Context.pop_declare_module cx;
      ignore @@ Env.set_scope_kind cx prev_scope_kind;

      (module_t, ast)

  and object_prop cx acc prop =
    let open Ast.Expression.Object in
    match prop with
    (* named prop *)
    | Property
        ( prop_loc,
          Property.Init
            {
              key =
                ( Property.Identifier (loc, { Ast.Identifier.name; _ })
                | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String name; _ }) ) as
                key;
              value = v;
              shorthand;
            }
        ) ->
      let (acc, key, value) =
        if Type_inference_hooks_js.dispatch_obj_prop_decl_hook cx name loc then
          let t = Unsoundness.at InferenceHooks loc in
          let key = translate_identifier_or_literal_key t key in
          (* don't add `name` to `acc` because `name` is the autocomplete token *)
          let acc = ObjectExpressionAcc.set_obj_key_autocomplete acc in
          let (((_, _t), _) as value) = expression cx v in
          (acc, key, value)
        else
          let (((_, t), _) as value) = expression cx v in
          let key = translate_identifier_or_literal_key t key in
          let acc =
            ObjectExpressionAcc.add_prop
              (Properties.add_field (OrdinaryName name) Polarity.Neutral (Some loc) t)
              acc
          in
          (acc, key, value)
      in
      (acc, Property (prop_loc, Property.Init { key; value; shorthand }))
    (* named method *)
    | Property
        ( prop_loc,
          Property.Method
            {
              key =
                ( Property.Identifier (loc, { Ast.Identifier.name; comments = _ })
                | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String name; _ }) ) as
                key;
              value = (fn_loc, func);
            }
        ) ->
      let reason = func_reason ~async:false ~generator:false prop_loc in
      let tvar = Tvar.mk cx reason in
      let (t, func) =
        mk_function_expression cx ~general:tvar ~needs_this_param:false reason fn_loc func
      in
      Flow.flow_t cx (t, tvar);
      ( ObjectExpressionAcc.add_prop (Properties.add_method (OrdinaryName name) (Some loc) t) acc,
        Property
          ( prop_loc,
            Property.Method
              { key = translate_identifier_or_literal_key t key; value = (fn_loc, func) }
          )
      )
    (* We enable some unsafe support for getters and setters. The main unsafe bit
     *  is that we don't properly havok refinements when getter and setter methods
     *  are called. *)
    (* unsafe getter property *)
    | Property
        ( loc,
          Property.Get
            {
              key =
                ( Property.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
                | Property.Literal (id_loc, { Ast.Literal.value = Ast.Literal.String name; _ }) ) as
                key;
              value = (vloc, func);
              comments;
            }
        ) ->
      Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
      let reason = func_reason ~async:false ~generator:false vloc in
      let tvar = Tvar.mk cx reason in
      let (function_type, func) =
        mk_function_expression cx ~general:tvar ~needs_this_param:false reason vloc func
      in
      Flow.flow_t cx (function_type, tvar);
      let return_t = Type.extract_getter_type function_type in
      ( ObjectExpressionAcc.add_prop
          (Properties.add_getter (OrdinaryName name) (Some id_loc) return_t)
          acc,
        Property
          ( loc,
            Property.Get
              {
                key = translate_identifier_or_literal_key return_t key;
                value = (vloc, func);
                comments;
              }
          )
      )
    (* unsafe setter property *)
    | Property
        ( loc,
          Property.Set
            {
              key =
                ( Property.Identifier (id_loc, { Ast.Identifier.name; comments = _ })
                | Property.Literal (id_loc, { Ast.Literal.value = Ast.Literal.String name; _ }) ) as
                key;
              value = (vloc, func);
              comments;
            }
        ) ->
      Flow_js.add_output cx (Error_message.EUnsafeGettersSetters loc);
      let reason = func_reason ~async:false ~generator:false vloc in
      let tvar = Tvar.mk cx reason in
      let (function_type, func) =
        mk_function_expression cx ~general:tvar ~needs_this_param:false reason vloc func
      in
      Flow.flow_t cx (function_type, tvar);
      let param_t = Type.extract_setter_type function_type in
      ( ObjectExpressionAcc.add_prop
          (Properties.add_setter (OrdinaryName name) (Some id_loc) param_t)
          acc,
        Property
          ( loc,
            Property.Set
              {
                key = translate_identifier_or_literal_key param_t key;
                value = (vloc, func);
                comments;
              }
          )
      )
    (* non-string literal LHS *)
    | Property (loc, Property.Init { key = Property.Literal _; _ })
    | Property (loc, Property.Method { key = Property.Literal _; _ })
    | Property (loc, Property.Get { key = Property.Literal _; _ })
    | Property (loc, Property.Set { key = Property.Literal _; _ }) ->
      Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ObjectPropertyLiteralNonString));
      (acc, Tast_utils.error_mapper#object_property_or_spread_property prop)
    (* computed getters and setters aren't supported yet regardless of the
       `enable_getters_and_setters` config option *)
    | Property (loc, Property.Get { key = Property.Computed _; _ })
    | Property (loc, Property.Set { key = Property.Computed _; _ }) ->
      Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ObjectPropertyComputedGetSet));
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
          let (map, prop) = object_prop cx map prop in
          (map, prop :: rev_prop_asts))
        (ObjectExpressionAcc.empty (), [])
        props
    in
    (acc.ObjectExpressionAcc.obj_pmap, List.rev rev_prop_asts)

  and object_ cx reason ~frozen props =
    let open Ast.Expression.Object in
    (* Use the same reason for proto and the ObjT so we can walk the proto chain
       and use the root proto reason to build an error. *)
    let obj_proto = ObjProtoT reason in
    let mk_computed key value =
      Tvar.mk_no_wrap_where cx reason (fun tout_tvar ->
          Flow.flow
            cx
            ( key,
              CreateObjWithComputedPropT
                { reason = reason_of_t key; reason_obj = reason; value; tout_tvar }
            )
      )
    in
    let (acc, rev_prop_asts) =
      List.fold_left
        (fun (acc, rev_prop_asts) -> function
          | SpreadProperty (prop_loc, { SpreadProperty.argument; comments }) ->
            let (((_, spread), _) as argument) = expression cx argument in
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
            let (((_, kt), _) as k) = expression cx k in
            let (((_, vt), _) as v) = expression cx v in
            let computed = mk_computed kt vt in
            ( ObjectExpressionAcc.add_spread computed acc,
              Property
                ( prop_loc,
                  Property.Init
                    {
                      key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k; comments });
                      value = v;
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
            let (((_, kt), _) as k) = expression cx k in
            let ((_, vt), v) = expression cx (fn_loc, Ast.Expression.Function fn) in
            let fn =
              match v with
              | Ast.Expression.Function fn -> fn
              | _ -> assert false
            in
            let computed = mk_computed kt vt in
            ( ObjectExpressionAcc.add_spread computed acc,
              Property
                ( prop_loc,
                  Property.Method
                    {
                      key = Property.Computed (k_loc, { Ast.ComputedKey.expression = k; comments });
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
                      | Property.Literal
                          (_, { Ast.Literal.value = Ast.Literal.String "__proto__"; _ }) ) as key;
                    value = v;
                    shorthand = false;
                  }
              ) ->
            let reason = mk_reason RPrototype (fst v) in
            let (((_, vt), _) as v) = expression cx v in
            let t =
              Tvar.mk_where cx reason (fun t -> Flow.flow cx (vt, ObjTestProtoT (reason, t)))
            in
            ( ObjectExpressionAcc.add_proto t acc,
              Property
                ( prop_loc,
                  Property.Init
                    {
                      key = translate_identifier_or_literal_key vt key;
                      value = v;
                      shorthand = false;
                    }
                )
              :: rev_prop_asts
            )
          | prop ->
            let (acc, prop) = object_prop cx acc prop in
            (acc, prop :: rev_prop_asts))
        (ObjectExpressionAcc.empty (), [])
        props
    in
    let t =
      ObjectExpressionAcc.mk_object_from_spread_acc cx acc reason ~frozen ~default_proto:obj_proto
    in
    (t, List.rev rev_prop_asts)

  and variable cx kind ?if_uninitialized id init =
    let open Ast.Statement in
    let (init_var, declare_var) =
      match kind with
      | VariableDeclaration.Const -> (Env.init_const, (fun _ _ _ -> ()))
      | VariableDeclaration.Let -> (Env.init_let, (fun _ _ _ -> ()))
      | VariableDeclaration.Var -> (Env.init_var, (fun _ _ _ -> ()))
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
    let (annot_or_inferred, annot_ast) =
      Anno.mk_type_annotation cx Subst_name.Map.empty id_reason annot
    in
    let annot_t = type_t_of_annotated_or_inferred annot_or_inferred in
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
        let t = VoidT.at ploc |> with_trust bogus_trust in
        let r = reason_of_t t in
        (Some (t, r), None)
    in

    let id_ast =
      match id with
      | (ploc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; annot = _; optional }) ->
        let (id_loc, { Ast.Identifier.name; comments }) = name in
        (* move const/let bindings from undeclared to declared *)
        declare_var cx (OrdinaryName name) id_loc;
        if has_anno then
          ()
        else if Base.Option.is_some init_ast || Base.Option.is_some if_uninitialized then
          (* TODO: When there is no annotation, we still need to populate `annot_t` when we can.
             In this case, we unify it with the type of the env entry binding. *)
          Flow.unify cx annot_t (Env.read_declared_type cx id_reason id_loc);
        begin
          match init_opt with
          | Some (init_t, init_reason) ->
            let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
            init_var cx ~use_op init_t id_loc
          | None -> ()
        end;
        Type_inference_hooks_js.(dispatch_lval_hook cx name id_loc (Val annot_t));
        let ast_t = Env.constraining_type ~default:annot_t cx id_loc in
        ( (ploc, ast_t),
          Ast.Pattern.Identifier
            {
              Ast.Pattern.Identifier.name = ((id_loc, ast_t), { Ast.Identifier.name; comments });
              annot = annot_ast;
              optional;
            }
        )
      | _ ->
        Base.Option.iter init_opt ~f:(fun (init_t, init_reason) ->
            let use_op = Op (AssignVar { var = Some id_reason; init = init_reason }) in
            Flow.flow cx (init_t, UseT (use_op, annot_t))
        );
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
            declare_var cx (OrdinaryName name) name_loc;

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
                Env.constraining_type
                  ~default:(Env.get_var_declared_type cx (OrdinaryName name) name_loc)
                  cx
                  name_loc
              )
            in
            Flow.flow cx (t, AssertImportIsValueT (reason, name));
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

  and array_elements cx undef_loc =
    let open Ast.Expression.Array in
    Base.Fn.compose
      List.split
      (Base.List.map ~f:(fun e ->
           match e with
           | Expression e ->
             let (((_, t), _) as e) = expression cx e in
             (UnresolvedArg (t, None), Expression e)
           | Hole loc ->
             (UnresolvedArg (EmptyT.at undef_loc |> with_trust bogus_trust, None), Hole loc)
           | Spread (loc, { Ast.Expression.SpreadElement.argument; comments }) ->
             let (((_, t), _) as argument) = expression cx argument in
             ( UnresolvedSpreadArg t,
               Spread (loc, { Ast.Expression.SpreadElement.argument; comments })
             )
       )
      )

  and empty_array cx loc =
    let reason = mk_reason REmptyArrayLit loc in
    let element_reason = mk_reason Reason.unknown_elem_empty_array_desc loc in
    let (has_hint, lazy_hint) = Env.get_hint cx loc in
    let elemt = Tvar.mk cx element_reason in
    (* empty array, analogous to object with implicit properties *)
    ( if Context.array_literal_providers cx then
      if not has_hint then begin
        if Context.lti cx then
          Flow.flow_t cx (EmptyT.make (mk_reason REmptyArrayElement loc) (bogus_trust ()), elemt)
      end else if not (Context.lti cx) then
        ()
      else
        match lazy_hint element_reason with
        | HintAvailable hint ->
          let elemt' = Tvar.mk cx element_reason in
          if
            Context.run_in_implicit_instantiation_mode cx (fun () ->
                Type_hint.sandbox_flow_succeeds
                  cx
                  (DefT (reason, bogus_trust (), ArrT (ArrayAT (elemt', Some []))), hint)
            )
          then
            Flow.unify cx elemt (PinTypes.pin_type cx ~use_op:unknown_use element_reason elemt')
          else
            (* A hint is available, but cannot be used to provide a type for this
             * array element. In this case, the element type of the array is likely
             * useless (does not escape the annotation), so we can use `empty` as
             * its type. *)
            Flow.flow_t cx (EmptyT.make (mk_reason REmptyArrayElement loc) (bogus_trust ()), elemt)
        | DecompositionError ->
          (* Similar to the case above, but the error occured earlier in the
           * hint decomposition chain. *)
          Flow.flow_t cx (EmptyT.make (mk_reason REmptyArrayElement loc) (bogus_trust ()), elemt)
        | NoHint
        | EncounteredPlaceholder ->
          (* If there is no hint then raise an error. The EncounteredPlaceholder case
           * corresponds to code like `const set = new Set([]);`. This case will
           * raise a [missing-empty-array-annot] error on `[]`. *)
          Flow.add_output cx (Error_message.EEmptyArrayNoProvider { loc });
          if Context.lti cx then Flow.flow_t cx (AnyT.at Untyped loc, elemt)
    );
    (reason, elemt)

  (* can raise Abnormal.(Exn (Stmt _, _))
   * annot should become a Type.t option when we have the ability to
   * inspect annotations and recurse into them *)
  and expression ?cond cx (loc, e) =
    let node_cache = Context.node_cache cx in
    let (((_, t), _) as res) =
      match Node_cache.get_expression node_cache loc with
      | Some node ->
        Debug_js.Verbose.print_if_verbose_lazy
          cx
          (lazy [spf "Expression cache hit at %s" (ALoc.debug_to_string loc)]);
        node
      | None -> expression_ ~cond cx loc e
    in
    Tvar_resolver.resolve cx t;
    res

  and this_ cx loc this =
    let open Ast.Expression in
    match Refinement.get ~allow_optional:true cx (loc, This this) loc with
    | Some t -> t
    | None -> Env.var_ref cx (internal_name "this") loc

  and super_ cx loc = Env.var_ref cx (internal_name "super") loc

  and expression_ ~cond cx loc e : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t =
    let make_trust = Context.trust_constructor cx in
    let ex = (loc, e) in
    let open Ast.Expression in
    match e with
    | Ast.Expression.Literal lit -> ((loc, literal cx loc lit), Ast.Expression.Literal lit)
    (* Treat the identifier `undefined` as an annotation for error reporting
     * purposes. Like we do with other literals. Otherwise we end up pointing to
     * `void` in `core.js`. While possible to re-declare `undefined`, it is
     * unlikely. The tradeoff is worth it. *)
    | Identifier (id_loc, ({ Ast.Identifier.name = "undefined"; comments = _ } as name)) ->
      let t = Flow.reposition cx loc ~annot_loc:loc (identifier cx name loc) in
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
    | TypeCast { TypeCast.expression = e; annot; comments } ->
      let (t, annot') = Anno.mk_type_available_annotation cx Subst_name.Map.empty annot in
      let (((_, infer_t), _) as e') = expression cx e in
      let use_op = Op (Cast { lower = mk_expression_reason e; upper = reason_of_t t }) in
      Flow.flow cx (infer_t, TypeCastT (use_op, t));
      ((loc, t), TypeCast { TypeCast.expression = e'; annot = annot'; comments })
    | TSTypeCast ({ TSTypeCast.kind; _ } as cast) ->
      let error_kind =
        match kind with
        | TSTypeCast.AsConst -> `AsConst
        | TSTypeCast.As _ -> `As
        | TSTypeCast.Satisfies _ -> `Satisfies
      in
      Flow_js_utils.add_output
        cx
        (Error_message.ETSSyntax { kind = Error_message.TSTypeCast error_kind; loc });
      let t = AnyT.at (AnyError None) loc in
      ((loc, t), TSTypeCast (Tast_utils.error_mapper#ts_type_cast cast))
    | Member _ -> subscript ~cond cx ex
    | OptionalMember _ -> subscript ~cond cx ex
    | Object { Object.properties; comments } ->
      error_on_this_uses_in_object_methods cx properties;
      let reason = mk_reason RObjectLit loc in
      let (t, properties) = object_ ~frozen:false cx reason properties in
      ((loc, t), Object { Object.properties; comments })
    | Array { Array.elements; comments } ->
      (match elements with
      | [] when Context.in_synthesis_mode cx ->
        let reason = mk_reason REmptyArrayLit loc in
        let element_reason = mk_reason Reason.unknown_elem_empty_array_desc loc in
        let elemt = Context.mk_placeholder cx element_reason in
        ( (loc, DefT (reason, make_trust (), ArrT (ArrayAT (elemt, Some [])))),
          Array { Array.elements = []; comments }
        )
      | [] ->
        let (reason, elemt) = empty_array cx loc in
        ( (loc, DefT (reason, make_trust (), ArrT (ArrayAT (elemt, Some [])))),
          Array { Array.elements = []; comments }
        )
      | elems ->
        let reason = mk_reason RArrayLit loc in
        let (elem_spread_list, elements) = array_elements cx loc elems in
        ( ( loc,
            Tvar.mk_where cx reason (fun tout ->
                let reason_op = reason in
                let element_reason =
                  replace_desc_reason Reason.inferred_union_elem_array_desc reason_op
                in
                let elem_t = Tvar.mk cx element_reason in
                let resolve_to = ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout) in
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
              Flow.flow_t cx (t, StrT.at loc |> with_trust bogus_trust))
          argts;
        let reason = mk_reason (RCustom "new Function(..)") loc in
        let proto = ObjProtoT reason in
        ( ( loc,
            DefT
              ( reason,
                bogus_trust (),
                FunT
                  ( dummy_static reason,
                    mk_functiontype
                      reason
                      []
                      ~rest_param:None
                      ~def_reason:reason
                      ~params_names:[]
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
        Flow.flow_t cx (arg_t, DefT (length_reason, bogus_trust (), NumT AnyLiteral));
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
        ( (loc, new_call cx loc reason_call ~use_op id_t targ_ts [Arg arg_t]),
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
      ( (loc, new_call cx loc reason ~use_op class_ targts argts),
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
        | (Some Abnormal.Throw, Some Abnormal.Throw) -> EmptyT.at loc |> with_trust bogus_trust
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
        | _ ->
          (* The only kind of abnormal control flow that should be raised from
             an expression is a Throw. The other kinds (return, break, continue)
             can only arise from statements, and while statements can appear within
             expressions (eg function expressions), any abnormals will be handled
             before they get here. *)
          assert_false "Unexpected abnormal control flow from within expression"
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
          Abnormal.throw_expr_control_flow_exception loc ast then_exn
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
          Error_message.(EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression))
      | _ -> ());
      let reason = func_reason ~async ~generator sig_loc in
      let tvar = Tvar.mk cx reason in
      let (t, func) =
        match id with
        | None -> mk_function_expression cx reason ~needs_this_param:true ~general:tvar loc func
        | Some (name_loc, { Ast.Identifier.name; comments = _ }) ->
          let prev_scope_kind = Env.set_scope_kind cx Name_def.Ordinary in
          let name = OrdinaryName name in
          Env.bind_fun cx name tvar name_loc;
          let (t, func) =
            mk_function_expression cx reason ~needs_this_param:true ~general:tvar loc func
          in
          ignore @@ Env.set_scope_kind cx prev_scope_kind;
          (t, func)
      in
      Flow.flow_t cx (t, tvar);
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
        | Ok module_name -> Import_export.require cx (loc, module_name) loc
        | Error err ->
          Flow.add_output cx (Error_message.EInvalidGraphQL (loc, err));
          let reason = mk_reason (RCustom "graphql tag") loc in
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
          let quasi_t =
            DefT
              ( reason_array,
                bogus_trust (),
                ArrT (ArrayAT (StrT.why reason |> with_trust bogus_trust, None))
              )
          in
          let exprs_t = Base.List.map ~f:(fun ((_, t), _) -> Arg t) expressions in
          Arg quasi_t :: exprs_t
        in
        let ft = mk_functioncalltype reason None args ret in
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
        CallT { use_op; reason; call_action = Funcalltype ft; return_hint = Env.get_hint cx loc }
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
                { TemplateLiteral.Element.value = { TemplateLiteral.Element.raw; cooked }; _ }
              ) =
            head
          in
          let lit = { Ast.Literal.value = Ast.Literal.String cooked; raw; comments = None } in
          (literal cx elem_loc lit, [])
        | _ ->
          let t_out = StrT.at loc |> with_trust bogus_trust in
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
      let tvar = Tvar.mk cx reason in
      (match c.Ast.Class.id with
      | Some _ ->
        let (class_t, c) = mk_class cx class_loc ~name_loc ~general:tvar reason c in
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
          Env.init_implicit_let cx ~use_op class_t name_loc
        in
        Flow.flow_t cx (class_t, tvar);
        ((class_loc, class_t), Class c)
      | None ->
        let (class_t, c) = mk_class cx class_loc ~name_loc ~general:tvar reason c in
        Flow.flow_t cx (class_t, tvar);
        ((class_loc, class_t), Class c))
    | Yield { Yield.argument; delegate = false; comments; result_out } ->
      let (t, argument_ast) =
        match argument with
        | Some expr ->
          let (((_, t), _) as expr) = expression cx expr in
          (t, Some expr)
        | None -> (VoidT.at loc |> with_trust bogus_trust, None)
      in
      ( (loc, Env.get_next cx loc),
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
      let next = Env.get_next cx loc in
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
        if Env.in_async_scope cx then
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
    (* TODO *)
    | Comprehension _ ->
      Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ComprehensionExpression));
      Tast_utils.error_mapper#expression ex
    | Generator _ ->
      Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, GeneratorExpression));
      Tast_utils.error_mapper#expression ex
    | MetaProperty
        {
          MetaProperty.meta = (_, { Ast.Identifier.name = "new"; _ }) as meta;
          property = (_, { Ast.Identifier.name = "target"; _ }) as property;
          comments;
        } ->
      let t = bogus_trust () |> MixedT.at loc in
      ((loc, t), MetaProperty { MetaProperty.meta; property; comments })
    | MetaProperty
        {
          MetaProperty.meta = (_, { Ast.Identifier.name = "import"; _ }) as meta;
          property = (_, { Ast.Identifier.name = "meta"; _ }) as property;
          comments;
        } ->
      let reason = mk_reason (RCustom "import.meta") loc in
      let t = Flow.get_builtin_type cx reason (OrdinaryName "Import$Meta") in
      ((loc, t), MetaProperty { MetaProperty.meta; property; comments })
    | MetaProperty _ ->
      Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, MetaPropertyExpression));
      Tast_utils.error_mapper#expression ex
    | Import { Import.argument; comments } ->
      (match argument with
      | ( source_loc,
          Ast.Expression.Literal
            { Ast.Literal.value = Ast.Literal.String module_name; raw; comments = _ }
        )
      | ( source_loc,
          TemplateLiteral
            {
              TemplateLiteral.quasis =
                [
                  ( _,
                    {
                      TemplateLiteral.Element.value =
                        { TemplateLiteral.Element.cooked = module_name; raw };
                      _;
                    }
                  );
                ];
              expressions = [];
              comments = _;
            }
        ) ->
        let literal_comments =
          match argument with
          | (_, Ast.Expression.Literal { Ast.Literal.comments; _ }) -> comments
          | _ -> None
        in
        let imported_module_t =
          let import_reason = mk_reason (RModule (OrdinaryName module_name)) loc in
          Import_export.import_ns cx import_reason (source_loc, module_name)
        in
        let reason = mk_annot_reason RAsyncImport loc in
        let t = Flow.get_builtin_typeapp cx reason (OrdinaryName "Promise") [imported_module_t] in
        ( (loc, t),
          Import
            {
              Import.argument =
                ( (source_loc, t),
                  Ast.Expression.Literal
                    {
                      Ast.Literal.value = Ast.Literal.String module_name;
                      raw;
                      comments = literal_comments;
                    }
                );
              comments;
            }
        )
      | _ ->
        let ignore_non_literals = Context.should_ignore_non_literal_requires cx in
        if not ignore_non_literals then (
          Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ImportDynamicArgument));
          Tast_utils.error_mapper#expression ex
        ) else
          Tast_utils.unchecked_mapper#expression ex)

  (* Handles operations that may traverse optional chains

     Returns a tuple:
       * type of expression if no optional chains short-circuited,
       * optional type of all possible short-circuitings,
       * typed AST of expression, where the type is the combination of
         short-circuiting and non short-circuiting (i.e. representing the actual
         range of possible types of the expression)
  *)
  and optional_chain ~cond cx ((loc, e) as ex) =
    let open Ast.Expression in
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
      let call_ast call ty =
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
        when not (Env.local_scope_entry_exists cx id_loc) ->
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
                            Ast.Expression.Literal
                              { Ast.Literal.value = Ast.Literal.String module_name; _ }
                          ) as lit_exp
                        );
                    ];
                  comments;
                }
              )
            ) ->
            ( Import_export.require cx (source_loc, module_name) loc,
              (args_loc, { ArgList.arguments = [Expression (expression cx lit_exp)]; comments })
            )
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
            ( Import_export.require cx (source_loc, module_name) loc,
              (args_loc, { ArgList.arguments = [Expression (expression cx lit_exp)]; comments })
            )
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
              Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, RequireDynamicArgument));
            (AnyT.at (AnyError None) loc, Tast_utils.error_mapper#arg_list arguments)
        in
        let id_t = bogus_trust () |> MixedT.at callee_loc in
        Some
          ( (loc, lhs_t),
            call_ast
              {
                Call.callee = ((callee_loc, id_t), Identifier ((id_loc, id_t), name));
                targs;
                arguments;
                comments;
              }
              lhs_t
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
            let t = bogus_trust () |> MixedT.at callee_loc in
            call_ast
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
              lhs_t
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
        let reason_lookup = mk_reason (RProperty (Some (OrdinaryName name))) callee_loc in
        let reason_prop = mk_reason (RProperty (Some (OrdinaryName name))) ploc in
        let super_t = super_ cx super_loc in
        let meth_generic_this = Tvar.mk cx reason in
        let (targts, targs) = convert_call_targs_opt cx targs in
        let (argts, arguments_ast) = arg_list cx arguments in
        Type_inference_hooks_js.dispatch_call_hook cx name ploc super_t;
        let prop_t = Tvar.mk cx reason_prop in
        let lhs_t =
          Tvar.mk_no_wrap_where cx reason (fun t ->
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
                      Named (reason_prop, OrdinaryName name),
                      CallM { methodcalltype; return_hint = Env.get_hint cx loc },
                      prop_t
                    )
                )
          )
        in
        Some
          ( (loc, lhs_t),
            call_ast
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
              lhs_t
          )
      | Call { Call.callee = (super_loc, Super super) as callee; targs; arguments; comments } ->
        let (targts, targs) = convert_call_targs_opt cx targs in
        let reason = mk_reason (RFunctionCall RSuper) loc in
        let super_t = super_ cx super_loc in
        let (argts, arguments_ast) = arg_list cx arguments in

        let super_reason = reason_of_t super_t in
        let prop_t = Tvar.mk cx (reason_of_t super_t) in
        let lhs_t =
          Tvar.mk_no_wrap_where cx reason (fun t ->
              let methodcalltype = mk_methodcalltype targts argts t in
              let propref = Named (super_reason, OrdinaryName "constructor") in
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
                      CallM { methodcalltype; return_hint = Type.hint_unavailable },
                      prop_t
                    )
                )
          )
        in
        Some
          ( (loc, lhs_t),
            call_ast
              {
                Call.callee = ((super_loc, super_t), Super super);
                targs;
                arguments = arguments_ast;
                comments;
              }
              lhs_t
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
        let arguments =
          match (targs, arguments) with
          | (None, (args_loc, { ArgList.arguments = []; comments = args_comments })) ->
            (* invariant() is treated like a throw *)
            Abnormal.throw_expr_control_flow_exception
              loc
              ( (loc, VoidT.at loc |> with_trust bogus_trust),
                Ast.Expression.Call
                  {
                    Call.callee;
                    targs;
                    arguments = (args_loc, { ArgList.arguments = []; comments = args_comments });
                    comments;
                  }
              )
              Abnormal.Throw
          | ( None,
              ( args_loc,
                {
                  ArgList.arguments =
                    Expression
                      ( ( _,
                          Ast.Expression.Literal
                            { Ast.Literal.value = Ast.Literal.Boolean false; _ }
                        ) as lit_exp
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
            Abnormal.throw_expr_control_flow_exception
              loc
              ( (loc, VoidT.at loc |> with_trust bogus_trust),
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
              Abnormal.Throw
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
            ( args_loc,
              { ArgList.arguments = Expression cond :: arguments; comments = args_comments }
            )
          | (_, (_, { ArgList.arguments = Spread _ :: _; comments = _ })) ->
            ignore (arg_list cx arguments);
            Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, InvariantSpreadArgument));
            Tast_utils.error_mapper#arg_list arguments
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
            Tast_utils.error_mapper#arg_list arguments
        in
        let lhs_t = VoidT.at loc |> with_trust bogus_trust in
        Some ((loc, lhs_t), call_ast { Call.callee; targs; arguments; comments } lhs_t)
      | Member
          {
            Member._object =
              ( object_loc,
                Identifier
                  ( id_loc,
                    { Ast.Identifier.name = "ReactGraphQL" | "ReactGraphQLLegacy"; comments = _ }
                  )
              );
            property =
              Member.PropertyIdentifier
                (ploc, ({ Ast.Identifier.name = "Mixin"; comments = _ } as name));
            comments;
          } ->
        let reason = mk_reason (RCustom "ReactGraphQLMixin") loc in
        let lhs_t = Flow.get_builtin cx (OrdinaryName "ReactGraphQLMixin") reason in
        Some
          ( (loc, lhs_t),
            (* TODO(vijayramamurthy) what's the type of "ReactGraphQL"? *)
            let t = AnyT.at Untyped object_loc in
            let property = Member.PropertyIdentifier ((ploc, t), name) in
            member_ast
              {
                Member._object = ((object_loc, t), Identifier ((id_loc, t), name));
                property;
                comments;
              }
              lhs_t
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
            if Type_inference_hooks_js.dispatch_member_hook cx name ploc super_t then
              Unsoundness.at InferenceHooks ploc
            else
              let use_op = Op (GetProperty (mk_expression_reason ex)) in
              get_prop ~use_op ~cond cx expr_reason super_t (prop_reason, name)
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
       * T2: optionally, a type representing the union of all optional chain
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
      match voided with
      | None -> filtered
      | Some void ->
        Tvar.mk_where cx (reason_of_t filtered) (fun t ->
            Flow.flow_t cx (filtered, t);
            Flow.flow_t cx (void, t)
        )
    in
    let noop _ = None in
    let handle_new_chain conf lhs_reason loc (chain_t, voided_t, object_ast) ~this_reason =
      let { ChainingConf.subexpressions; get_reason; test_hooks; get_opt_use; _ } = conf in
      (* We've encountered an optional chaining operator.
         We need to flow the "success" type of object_ into a OptionalChainT
         type, which will "filter out" VoidT and NullT from the type of
         object_ and flow them into `voided_out`, and then flow any non-void
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
      let mem_tvar =
        match test_hooks chain_t with
        | Some hit -> hit
        | None -> (reason, Tvar.mk_no_wrap cx reason)
      in
      let voided_out =
        Tvar.mk_where cx reason (fun t ->
            Base.Option.iter ~f:(fun voided_t -> Flow.flow_t cx (voided_t, t)) voided_t
        )
      in
      let this_t = Tvar.mk cx this_reason in
      let opt_use = get_opt_use subexpression_types reason this_t in
      Flow.flow
        cx
        ( chain_t,
          OptionalChainT
            {
              reason = chain_reason;
              lhs_reason;
              this_t;
              t_out = apply_opt_use opt_use mem_tvar;
              voided_out;
            }
        );
      let lhs_t =
        Tvar.mk_where cx reason (fun t ->
            Flow.flow_t cx (OpenT mem_tvar, t);
            Flow.flow_t cx (voided_out, t)
        )
      in
      (OpenT mem_tvar, Some voided_out, lhs_t, chain_t, object_ast, subexpression_asts)
    in
    let handle_continue_chain conf (chain_t, voided_t, object_ast) =
      let {
        ChainingConf.refine;
        refinement_action;
        subexpressions;
        get_result;
        test_hooks;
        get_reason;
        _;
      } =
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
        match (test_hooks chain_t, refine ()) with
        | (Some hit, _) -> OpenT hit
        | (None, Some refi) ->
          Base.Option.value_map
            ~f:(fun refinement_action -> refinement_action subexpression_types chain_t refi)
            ~default:refi
            refinement_action
        | (None, None) -> get_result subexpression_types reason chain_t
      in
      let lhs_t = join_optional_branches voided_t res_t in
      (res_t, voided_t, lhs_t, chain_t, object_ast, subexpression_asts)
    in
    let handle_chaining conf opt object_ loc ~this_reason =
      let {
        ChainingConf.refinement_action;
        refine;
        subexpressions;
        get_result;
        test_hooks;
        get_reason;
        _;
      } =
        conf
      in
      match opt with
      | NonOptional ->
        (* Proceeding as normal: no need to worry about optionality, so T2 from
           above is None. We don't need to consider optional short-circuiting, so
           we can call expression_ rather than optional_chain. *)
        let (((_, obj_t), _) as object_ast) = expression cx object_ in
        let (subexpression_types, subexpression_asts) = subexpressions () in
        let reason = get_reason obj_t in
        let lhs_t =
          match (test_hooks obj_t, refine ()) with
          | (Some hit, _) -> OpenT hit
          | (None, Some refi) ->
            Base.Option.value_map
              ~f:(fun refinement_action -> refinement_action subexpression_types obj_t refi)
              ~default:refi
              refinement_action
          | (None, None) -> get_result subexpression_types reason obj_t
        in
        (lhs_t, None, lhs_t, obj_t, object_ast, subexpression_asts)
      | NewChain ->
        let lhs_reason = mk_expression_reason object_ in
        let ((filtered_t, voided_t, object_ast) as object_data) =
          optional_chain ~cond:None cx object_
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
          | _ -> handle_new_chain conf lhs_reason loc object_data ~this_reason
        end
      | ContinueChain -> handle_continue_chain conf (optional_chain ~cond:None cx object_)
    in
    match try_non_chain cx loc e' ~call_ast ~member_ast with
    | Some (((_, lhs_t), _) as res) ->
      (* Nothing to do with respect to optional chaining, because we're in a
         case where chaining isn't allowed. *)
      (lhs_t, None, res)
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
        let get_opt_use tind _ _ = OptGetElemT (use_op, reason, false (* annot *), tind) in
        let get_mem_t tind reason obj_t =
          Tvar.mk_no_wrap_where cx reason (fun t ->
              let use = apply_opt_use (get_opt_use tind reason obj_t) t in
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
            test_hooks = noop;
            get_opt_use;
            get_reason = Fun.const reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, index) =
          handle_chaining conf opt_state _object loc ~this_reason:(mk_expression_reason _object)
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
        let opt_use = get_prop_opt_use ~cond expr_reason ~use_op (prop_reason, name) in
        let test_hooks obj_t =
          if Type_inference_hooks_js.dispatch_member_hook cx name ploc obj_t then
            Some (inference_hook_tvar cx ploc)
          else
            None
        in
        let get_mem_t () _ obj_t =
          Tvar.mk_no_wrap_where cx expr_reason (fun t ->
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
            test_hooks;
            get_opt_use = (fun _ _ _ -> opt_use);
            get_reason = Fun.const expr_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
          handle_chaining conf opt_state _object loc ~this_reason:(mk_expression_reason _object)
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
                Member.PropertyPrivateName (ploc, { Ast.PrivateName.name; comments = _ }) as
                property;
              comments;
            },
          _
        ) ->
        let expr_reason = mk_reason (RPrivateProperty name) loc in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        let opt_use = get_private_field_opt_use cx expr_reason ~use_op name in
        let test_hooks obj_t =
          if Type_inference_hooks_js.dispatch_member_hook cx name ploc obj_t then
            Some (inference_hook_tvar cx ploc)
          else
            None
        in
        let get_mem_t () _ obj_t =
          Tvar.mk_no_wrap_where cx expr_reason (fun t ->
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
            test_hooks;
            get_opt_use = (fun _ _ _ -> opt_use);
            get_reason = Fun.const expr_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, _) =
          handle_chaining conf opt_state _object loc ~this_reason:(mk_expression_reason _object)
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
        let ( filtered_out,
              lookup_voided_out,
              call_voided_out,
              member_lhs_t,
              prop_t,
              obj_filtered_out,
              object_ast,
              property,
              argument_asts
            ) =
          match property with
          | Member.PropertyPrivateName (prop_loc, { Ast.PrivateName.name; comments = _ })
          | Member.PropertyIdentifier (prop_loc, { Ast.Identifier.name; comments = _ }) ->
            let reason_call = mk_reason (RMethodCall (Some name)) loc in
            let reason_prop = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
            let this_reason = mk_expression_reason callee in
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
            let get_opt_use argts _ _ =
              method_call_opt_use
                cx
                opt_state
                ~prop_t
                ~voided_out:call_voided_out
                reason_call
                ~use_op
                ~private_
                prop_loc
                (callee, name)
                loc
                targts
                argts
            in
            let test_hooks obj_t =
              if Type_inference_hooks_js.dispatch_member_hook cx name prop_loc obj_t then
                Some (inference_hook_tvar cx prop_loc)
              else
                None
            in
            let handle_refined_callee argts obj_t f =
              Tvar.mk_no_wrap_where cx reason_call (fun t ->
                  let app = mk_boundfunctioncalltype obj_t targts argts t ~call_strict_arity:true in
                  Flow.unify cx f prop_t;
                  let call_t =
                    match opt_state with
                    | NewChain ->
                      let chain_reason = mk_reason ROptionalChain loc in
                      let lhs_reason = mk_expression_reason callee in
                      let this_t = Tvar.mk cx this_reason in
                      OptionalChainT
                        {
                          reason = chain_reason;
                          lhs_reason;
                          this_t;
                          t_out =
                            CallT
                              {
                                use_op;
                                reason = reason_call;
                                call_action = Funcalltype app;
                                return_hint = Env.get_hint cx loc;
                              };
                          voided_out = OpenT t;
                        }
                    | _ ->
                      CallT
                        {
                          use_op;
                          reason = reason_call;
                          call_action = Funcalltype app;
                          return_hint = Env.get_hint cx loc;
                        }
                  in
                  Flow.flow cx (f, call_t)
              )
            in
            let get_mem_t argts reason obj_t =
              Type_inference_hooks_js.dispatch_call_hook cx name prop_loc obj_t;
              Tvar.mk_no_wrap_where cx reason_call (fun t ->
                  let use = apply_opt_use (get_opt_use argts reason obj_t) t in
                  Flow.flow cx (obj_t, use)
              )
            in
            let eval_args () = arg_list cx arguments in
            let conf =
              {
                ChainingConf.subexpressions = eval_args;
                get_result = get_mem_t;
                test_hooks;
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
              handle_chaining conf member_opt _object lookup_loc ~this_reason
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
              argument_asts
            )
          | Member.PropertyExpression expr ->
            let reason_call = mk_reason (RMethodCall None) loc in
            let reason_lookup = mk_reason (RProperty None) lookup_loc in
            let call_voided_out = Tvar.mk cx expr_reason in
            let prop_t = Tvar.mk cx reason_lookup in
            let get_opt_use (argts, elem_t) _ _ =
              elem_call_opt_use
                opt_state
                ~prop_t
                ~voided_out:call_voided_out
                ~reason_call
                ~reason_lookup
                ~reason_expr:expr_reason
                ~reason_chain:(mk_reason ROptionalChain loc)
                targts
                argts
                elem_t
            in
            let get_mem_t arg_and_elem_ts reason obj_t =
              Tvar.mk_no_wrap_where cx reason_call (fun t ->
                  let use = apply_opt_use (get_opt_use arg_and_elem_ts reason obj_t) t in
                  Flow.flow cx (obj_t, use);
                  Flow.flow_t cx (obj_t, prop_t)
              )
            in
            let eval_args_and_expr () =
              let (((_, elem_t), _) as expr) = expression cx expr in
              let (argts, arguments_ast) = arg_list cx arguments in
              ((argts, elem_t), (arguments_ast, expr))
            in
            let this_reason = mk_expression_reason callee in
            let conf =
              {
                ChainingConf.refinement_action = None;
                subexpressions = eval_args_and_expr;
                get_result = get_mem_t;
                test_hooks = noop;
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
              handle_chaining conf member_opt _object lookup_loc ~this_reason
            in
            ( filtered_out,
              lookup_voided_out,
              call_voided_out,
              member_lhs_t,
              prop_t,
              obj_filtered_out,
              object_ast,
              Member.PropertyExpression expr_ast,
              argument_asts
            )
        in
        let voided_out = join_optional_branches lookup_voided_out call_voided_out in
        let lhs_t =
          Tvar.mk_where cx (reason_of_t member_lhs_t) (fun t ->
              Flow.flow_t cx (member_lhs_t, t);
              Flow.flow_t cx (voided_out, t)
          )
        in
        ( filtered_out,
          Some voided_out,
          ( (loc, lhs_t),
            call_ast
              {
                Call.callee =
                  ( (lookup_loc, prop_t),
                    receiver_ast
                      { Member._object = object_ast; property; comments = member_comments }
                      obj_filtered_out
                  );
                targs;
                arguments = argument_asts;
                comments;
              }
              filtered_out
          )
        )
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
        let get_opt_use argts reason _ = func_call_opt_use cx loc reason ~use_op targts argts in
        let get_reason lhs_t = mk_reason (RFunctionCall (desc_of_t lhs_t)) loc in
        let get_result argts reason f =
          Tvar.mk_no_wrap_where cx reason (fun t ->
              let use = apply_opt_use (get_opt_use argts reason f) t in
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
            test_hooks = noop;
            get_opt_use;
            get_reason;
          }
        in
        let (filtered_out, voided_out, lhs_t, _, object_ast, argument_asts) =
          handle_chaining conf opt_state callee loc ~this_reason:(mk_expression_reason ex)
        in
        let exp callee =
          call_ast { Call.callee; targs; arguments = argument_asts; comments } filtered_out
        in
        (filtered_out, voided_out, ((loc, lhs_t), exp object_ast))
      | _ ->
        let (((_, t), _) as res) = expression ?cond cx ex in
        (t, None, res))

  and arg_list cx (args_loc, { Ast.Expression.ArgList.arguments; comments }) =
    let (argts, arg_asts) = arguments |> Base.List.map ~f:(expression_or_spread cx) |> List.split in
    (argts, (args_loc, { Ast.Expression.ArgList.arguments = arg_asts; comments }))

  and subscript ~cond cx ex =
    let (_, _, ast) = optional_chain ~cond cx ex in
    ast

  (* We assume that constructor functions return void
     and constructions return objects.
     TODO: This assumption does not always hold.
     If construction functions return non-void values (e.g., functions),
     then those values are returned by constructions.
  *)
  and new_call cx loc reason ~use_op class_ targs args =
    Tvar.mk_where cx reason (fun tout ->
        Flow.flow
          cx
          ( class_,
            ConstructorT { use_op; reason; targs; args; tout; return_hint = Env.get_hint cx loc }
          )
    )

  and func_call_opt_use cx loc reason ~use_op ?(call_strict_arity = true) targts argts =
    let opt_app = mk_opt_functioncalltype reason targts argts call_strict_arity in
    let return_hint = Env.get_hint cx loc in
    OptCallT { use_op; reason; opt_funcalltype = opt_app; return_hint }

  and func_call cx loc reason ~use_op ?(call_strict_arity = true) func_t targts argts =
    let opt_use = func_call_opt_use cx loc reason ~use_op ~call_strict_arity targts argts in
    Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (func_t, apply_opt_use opt_use t))

  and method_call_opt_use
      cx
      opt_state
      ~voided_out
      ~prop_t
      reason
      ~use_op
      ~private_
      ?(call_strict_arity = true)
      prop_loc
      (expr, name)
      chain_loc
      targts
      argts =
    let (expr_loc, _) = expr in
    let reason_prop = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
    let reason_expr = mk_reason (RProperty (Some (OrdinaryName name))) expr_loc in
    let opt_methodcalltype = mk_opt_methodcalltype targts argts call_strict_arity in
    let propref = Named (reason_prop, OrdinaryName name) in
    let action =
      match opt_state with
      | NewChain ->
        let exp_reason = mk_reason ROptionalChain chain_loc in
        OptChainM
          {
            exp_reason;
            lhs_reason = mk_expression_reason expr;
            this = prop_t;
            opt_methodcalltype;
            voided_out;
            return_hint = Type.hint_unavailable;
          }
      | _ -> OptCallM { opt_methodcalltype; return_hint = Env.get_hint cx chain_loc }
    in
    if private_ then
      let class_entries = Env.get_class_entries cx in
      OptPrivateMethodT (use_op, reason, reason_expr, name, class_entries, false, action, prop_t)
    else
      OptMethodT (use_op, reason, reason_expr, propref, action, prop_t)

  (* returns (type of method itself, type returned from method) *)
  and method_call
      cx reason ~use_op ?(call_strict_arity = true) prop_loc (expr, obj_t, name) targts argts =
    Type_inference_hooks_js.dispatch_call_hook cx name prop_loc obj_t;
    let (expr_loc, _) = expr in
    match Refinement.get ~allow_optional:true cx expr (aloc_of_reason reason) with
    | Some f ->
      (* note: the current state of affairs is that we understand
         member expressions as having refined types, rather than
         understanding receiver objects as carrying refined properties.
         generalizing this properly is a todo, and will deliver goodness.
         meanwhile, here we must hijack the property selection normally
         performed by the flow algorithm itself. *)
      ( f,
        Tvar.mk_no_wrap_where cx reason (fun t ->
            let app = mk_boundfunctioncalltype obj_t targts argts t ~call_strict_arity in
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
      let reason_prop = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
      let prop_t = Tvar.mk cx reason_prop in
      ( prop_t,
        Tvar.mk_no_wrap_where cx reason (fun t ->
            let reason_expr = mk_reason (RProperty (Some (OrdinaryName name))) expr_loc in
            let methodcalltype =
              mk_methodcalltype targts argts t ~meth_strict_arity:call_strict_arity
            in
            let propref = Named (reason_prop, OrdinaryName name) in
            Flow.flow
              cx
              ( obj_t,
                MethodT
                  ( use_op,
                    reason,
                    reason_expr,
                    propref,
                    CallM { methodcalltype; return_hint = Type.hint_unavailable },
                    prop_t
                  )
              )
        )
      )

  and elem_call_opt_use
      opt_state
      ~voided_out
      ~prop_t
      ~reason_call
      ~reason_lookup
      ~reason_expr
      ~reason_chain
      targts
      argts
      elem_t =
    let opt_methodcalltype = mk_opt_methodcalltype targts argts true in
    let action =
      match opt_state with
      | NewChain ->
        OptChainM
          {
            exp_reason = reason_chain;
            lhs_reason = reason_expr;
            this = prop_t;
            opt_methodcalltype;
            voided_out;
            return_hint = Type.hint_unavailable;
          }
      | _ -> OptCallM { opt_methodcalltype; return_hint = Type.hint_unavailable }
    in
    OptCallElemT (reason_call, reason_lookup, elem_t, action)

  and identifier_ cx name loc =
    let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
    let get_checking_mode_type () =
      let t = Env.var_ref ~lookup_mode:ForValue cx (OrdinaryName name) loc in
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
        Tvar.mk_where cx reason (Flow.unify cx t)
    in
    match (Context.lti cx, Type_inference_hooks_js.dispatch_id_hook cx name loc) with
    | (true, true) ->
      let (_, lazy_hint) = Env.get_hint cx loc in
      lazy_hint reason
      |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:(fun () ->
             EmptyT.at loc |> with_trust bogus_trust
         )
    | (false, true) -> Tvar.mk cx reason
    | (_, false) -> get_checking_mode_type ()

  and identifier cx { Ast.Identifier.name; comments = _ } loc =
    let t = identifier_ cx name loc in
    t

  (* traverse a literal expression, return result type *)
  and literal cx loc lit =
    let get_checking_mode_type () =
      let make_trust = Context.trust_constructor cx in
      let open Ast.Literal in
      match lit.Ast.Literal.value with
      | String s -> begin
        match Context.haste_module_ref_prefix cx with
        | Some prefix when String.starts_with ~prefix s ->
          let m = String_utils.lstrip s prefix in
          let t = Import_export.require cx (loc, m) loc in
          let reason = mk_reason (RCustom "module reference") loc in
          Flow.get_builtin_typeapp cx reason (OrdinaryName "$Flow$ModuleRef") [t]
        | _ ->
          (* It's too expensive to track literal information for large strings.*)
          let max_literal_length = Context.max_literal_length cx in
          let (lit, r_desc) =
            if max_literal_length = 0 || String.length s <= max_literal_length then
              (Literal (None, OrdinaryName s), RString)
            else
              (AnyLiteral, RLongStringLit max_literal_length)
          in
          DefT (mk_annot_reason r_desc loc, make_trust (), StrT lit)
      end
      | Boolean b -> DefT (mk_annot_reason RBoolean loc, make_trust (), BoolT (Some b))
      | Null -> NullT.at loc |> with_trust make_trust
      | Number f ->
        DefT (mk_annot_reason RNumber loc, make_trust (), NumT (Literal (None, (f, lit.raw))))
      | BigInt n ->
        DefT (mk_annot_reason RBigInt loc, make_trust (), BigIntT (Literal (None, (n, lit.raw))))
      | RegExp _ -> Flow.get_builtin_type cx (mk_annot_reason RRegExp loc) (OrdinaryName "RegExp")
    in
    match (Context.lti cx, Type_inference_hooks_js.dispatch_literal_hook cx loc) with
    | (true, true) ->
      let (_, lazy_hint) = Env.get_hint cx loc in
      lazy_hint (mk_reason (RCustom "literal") loc)
      |> Type_hint.with_hint_result ~ok:Base.Fn.id ~error:(fun () ->
             EmptyT.at loc |> with_trust bogus_trust
         )
    | (false, true) -> Tvar.mk cx (mk_reason (RCustom "literal") loc)
    | (_, false) -> get_checking_mode_type ()

  (* traverse a unary expression, return result type *)
  and unary cx ~cond loc =
    let open Ast.Expression.Unary in
    function
    | { operator = Not; argument; comments } ->
      let (((_, arg), _) as argument) = expression cx ?cond argument in
      let reason = mk_reason (RUnaryOperator ("not", desc_of_t arg)) loc in
      let tout =
        match cond with
        | Some _ -> BoolT.at loc |> with_trust bogus_trust
        | None -> Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (arg, NotT (reason, t)))
      in
      (tout, { operator = Not; argument; comments })
    | { operator = Plus; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      let reason = mk_reason (desc_of_t argt) loc in
      ( Tvar.mk_where cx reason (fun result_t ->
            Flow.flow cx (argt, UnaryArithT { reason; result_t; kind = UnaryArithKind.Plus })
        ),
        { operator = Plus; argument; comments }
      )
    | { operator = Minus; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      ( begin
          match argt with
          | DefT (reason, trust, NumT (Literal (sense, (value, raw)))) ->
            (* special case for negative number literals, to avoid creating an unnecessary tvar. not
               having a tvar allows other special cases that match concrete lower bounds to proceed
               (notably, Object.freeze upgrades literal props to singleton types, and a tvar would
               make a negative number not look like a literal.) *)
            let annot_loc = loc in
            let reason = annot_reason ~annot_loc @@ repos_reason annot_loc reason in
            let (value, raw) = Flow_ast_utils.negate_number_literal (value, raw) in
            DefT (reason, trust, NumT (Literal (sense, (value, raw))))
          | arg ->
            let reason = mk_reason (desc_of_t arg) loc in
            Tvar.mk_where cx reason (fun result_t ->
                Flow.flow cx (arg, UnaryArithT { reason; result_t; kind = UnaryArithKind.Minus })
            )
        end,
        { operator = Minus; argument; comments }
      )
    | { operator = BitNot; argument; comments } ->
      let (((_, argt), _) as argument) = expression cx argument in
      let reason = mk_reason (desc_of_t argt) loc in
      ( Tvar.mk_where cx reason (fun result_t ->
            Flow.flow cx (argt, UnaryArithT { reason; result_t; kind = UnaryArithKind.BitNot })
        ),
        { operator = BitNot; argument; comments }
      )
    | { operator = Typeof; argument; comments } ->
      let argument = expression cx argument in
      (StrT.at loc |> with_trust literal_trust, { operator = Typeof; argument; comments })
    | { operator = Void; argument; comments } ->
      let argument = expression cx argument in
      (VoidT.at loc |> with_trust literal_trust, { operator = Void; argument; comments })
    | { operator = Ast.Expression.Unary.Delete; argument; comments } ->
      let argument = delete cx loc argument in
      ( BoolT.at loc |> with_trust literal_trust,
        { operator = Ast.Expression.Unary.Delete; argument; comments }
      )
    | { operator = Await; argument; comments } ->
      (* TODO: await should look up Promise in the environment instead of going
         directly to the core definition. Otherwise, the following won't work
         with a polyfilled Promise! **)
      (* see declaration of $await in core.js:
         if argument is a Promise<T>, then (await argument) returns T.
         otherwise it just returns the argument type.
         TODO update this comment when recursive unwrapping of
         Promise is done.
      *)
      let reason = mk_reason (RCustom "await") loc in
      let await = Flow.get_builtin cx (OrdinaryName "$await") reason in
      let (((_, arg), _) as argument_ast) = expression cx argument in
      let use_op =
        Op
          (FunCall
             {
               op = reason;
               fn = reason_of_t await;
               args = [mk_expression_reason argument];
               local = true;
             }
          )
      in
      ( func_call cx loc reason ~use_op await None [Arg arg],
        { operator = Await; argument = argument_ast; comments }
      )

  (* numeric pre/post inc/dec *)
  and update cx loc expr =
    let open Ast.Expression.Update in
    let reason = mk_reason (RCustom "update") loc in
    let { argument; _ } = expr in
    let (((_, arg_t), _) as arg_ast) = expression cx argument in
    let result_t =
      Tvar.mk_where cx reason (fun result_t ->
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
        Env.set_var cx ~use_op name result_t id_loc;
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
      (BoolT.at loc |> with_trust literal_trust, { operator; left; right; comments })
    | In ->
      let (loc1, _) = left in
      let (loc2, _) = right in
      let (((_, t1), _) as left) = expression cx left in
      let (((_, t2), _) as right) = expression cx right in
      let reason_lhs = mk_reason (RCustom "LHS of `in` operator") loc1 in
      let reason_rhs = mk_reason (RCustom "RHS of `in` operator") loc2 in
      Flow.flow cx (t1, AssertBinaryInLHST reason_lhs);
      Flow.flow cx (t2, AssertBinaryInRHST reason_rhs);
      (BoolT.at loc |> with_trust literal_trust, { operator; left; right; comments })
    | StrictEqual
    | StrictNotEqual ->
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
      Flow.flow cx (t1, StrictEqT { reason; cond_context = cond; flip = false; arg = t2 });
      (BoolT.at loc |> with_trust literal_trust, { operator; left; right; comments })
    | Instanceof ->
      let left = expression cx left in
      let (((right_loc, right_t), _) as right) = expression cx right in
      let reason_rhs = mk_reason (RCustom "RHS of `instanceof` operator") right_loc in
      Flow.flow cx (right_t, AssertInstanceofRHST reason_rhs);
      (BoolT.at loc |> with_trust literal_trust, { operator; left; right; comments })
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
      (BoolT.at loc |> with_trust literal_trust, { operator; left; right; comments })
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
      ( Tvar.mk_where cx reason (fun t ->
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
     *
     * Note that the only kind of abnormal control flow that should be raised from an
     * expression is a Throw. The other kinds (Return, Break, Continue) can only arise from
     * statements, and while statements can appear within expressions (e.g. function expressions),
     * any abnormals will be handled before they get here.
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
        | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
        | None -> t2
        | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
      in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (t1, OrT (reason, t2, t))),
        { operator = Or; left; right; comments }
      )
    | And ->
      let (((_, t1), _) as left) = condition ~cond:OtherTest cx left in
      let ((((_, t2), _) as right), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx ?cond right)
      in
      let t2 =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
        | None -> t2
        | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
      in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (t1, AndT (reason, t2, t))),
        { operator = And; left; right; comments }
      )
    | NullishCoalesce ->
      let (((_, t1), _) as left) = expression cx left in
      let ((((_, t2), _) as right), right_abnormal) =
        Abnormal.catch_expr_control_flow_exception (fun () -> expression cx right)
      in
      let t2 =
        match right_abnormal with
        | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
        | None -> t2
        | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
      in
      let reason = mk_reason (RLogical ("??", desc_of_t t1, desc_of_t t2)) loc in
      ( Tvar.mk_no_wrap_where cx reason (fun t ->
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
           and this-type for the optional chain are mixed.
        *)
        let mixed = MixedT.at lhs_loc (literal_trust ()) in
        OptionalChainT { reason; lhs_reason; this_t = mixed; t_out = use_t; voided_out = mixed }
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
      let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
      let super_t = super_ cx super_loc in
      let prop_t = Tvar.mk cx prop_reason in
      let use_op =
        make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) prop_loc)
      in
      Flow.flow
        cx
        ( super_t,
          SetPropT
            (use_op, reason, Named (prop_reason, OrdinaryName name), mode, Normal, t, Some prop_t)
        );
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
        match (_object, Env.var_scope_kind cx) with
        | ((_, This _), Name_def.Ctor) -> ThisInCtor
        | _ -> Normal
      in
      let prop_t =
        (* if we fire this hook, it means the assignment is a sham. *)
        if Type_inference_hooks_js.dispatch_member_hook cx name prop_loc o then
          Unsoundness.at InferenceHooks prop_loc
        else
          let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
          (* flow type to object property itself *)
          let class_entries = Env.get_class_entries cx in
          let prop_reason = mk_reason (RPrivateProperty name) prop_loc in
          let prop_t = Tvar.mk cx prop_reason in
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
          Flow.flow cx (o, upper);
          prop_t
      in
      ((lhs_loc, prop_t), reconstruct_ast { Member._object; property; comments } prop_t)
    (* _object.name = e *)
    | {
     Member._object;
     property = Member.PropertyIdentifier (prop_loc, ({ Ast.Identifier.name; comments = _ } as id));
     comments;
    } ->
      let wr_ctx =
        match (_object, Env.var_scope_kind cx) with
        | ((_, This _), Name_def.Ctor) -> ThisInCtor
        | _ -> Normal
      in
      let lhs_reason = mk_expression_reason _object in
      let (o, _object) = typecheck_object _object in
      let prop_t =
        (* if we fire this hook, it means the assignment is a sham. *)
        if Type_inference_hooks_js.dispatch_member_hook cx name prop_loc o then
          Unsoundness.at InferenceHooks prop_loc
        else
          let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
          let prop_reason = mk_reason (RProperty (Some (OrdinaryName name))) prop_loc in
          (* flow type to object property itself *)
          let prop_t = Tvar.mk cx prop_reason in
          let use_op =
            make_op ~lhs:reason ~prop:(mk_reason (desc_of_reason lhs_prop_reason) prop_loc)
          in
          let upper =
            maybe_chain
              lhs_reason
              (SetPropT
                 ( use_op,
                   reason,
                   Named (prop_reason, OrdinaryName name),
                   mode,
                   wr_ctx,
                   t,
                   Some prop_t
                 )
              )
          in
          Flow.flow cx (o, upper);
          prop_t
      in
      let lhs_t =
        match (_object, name) with
        | ( ( _,
              Ast.Expression.Identifier
                ((id_loc, _), { Ast.Identifier.name = "module"; comments = _ })
            ),
            "exports"
          )
          when not (Env.local_scope_entry_exists cx id_loc) ->
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
    (* update env, add constraints arising from LHS structure,
       handle special cases, etc. *)
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
    Tvar.mk_where cx reason (fun result_t ->
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
        Env.set_var cx ~use_op name result_t id_loc
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
            | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
            | None -> rhs_t
            | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
          in
          let result_t =
            Tvar.mk_no_wrap_where cx reason (fun t ->
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
            | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
            | None -> rhs_t
            | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
          in
          let result_t =
            Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (lhs_t, AndT (reason, rhs_t, t)))
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
            | Some Abnormal.Throw -> EmptyT.at loc |> with_trust bogus_trust
            | None -> rhs_t
            | Some _ -> assert_false "Unexpected abnormal control flow from within expression"
          in
          let result_t =
            Tvar.mk_no_wrap_where cx reason (fun t -> Flow.flow cx (lhs_t, OrT (reason, rhs_t, t)))
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
    let void = VoidT.at loc |> with_trust literal_trust in
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
      Env.set_var cx ~use_op name void loc;
      expression cx target
    | _ ->
      let (((_, t), _) as target) = expression cx target in
      Flow.add_output cx Error_message.(ECannotDelete (loc, reason_of_t t));
      target

  and collapse_children cx (children_loc, children) :
      Type.unresolved_param list * (ALoc.t * (ALoc.t, ALoc.t * Type.t) Ast.JSX.child list) =
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
    let (children_loc, _) = frag_children in
    let fragment_t =
      match Context.react_runtime cx with
      | Options.ReactRuntimeAutomatic ->
        let reason = mk_reason (RIdentifier (OrdinaryName "Fragment")) expr_loc in
        Flow.get_builtin_type cx reason (OrdinaryName "React$FragmentType")
      | Options.ReactRuntimeClassic ->
        let reason = mk_reason (RIdentifier (OrdinaryName "React.Fragment")) expr_loc in
        let react = Env.var_ref ~lookup_mode:ForValue cx (OrdinaryName "React") expr_loc in
        let use_op = Op (GetProperty reason) in
        get_prop ~cond:None cx reason ~use_op react (reason, "Fragment")
    in
    let (unresolved_params, frag_children) = collapse_children cx frag_children in
    let locs = (expr_loc, frag_opening_element, children_loc) in
    let t =
      jsx_desugar
        cx
        "React.Fragment"
        fragment_t
        (NullT.at expr_loc |> with_trust bogus_trust)
        []
        unresolved_params
        locs
    in
    Tvar_resolver.resolve cx t;
    (t, { frag_opening_element; frag_children; frag_closing_element; frag_comments })

  and jsx_title cx opening_element children closing_element locs =
    let open Ast.JSX in
    let make_trust = Context.trust_constructor cx in
    let (loc_element, _, _) = locs in
    let (loc, { Opening.name; attributes; self_closing }) = opening_element in
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
        let t = Flow.get_builtin_type cx fbt_reason (OrdinaryName custom_jsx_type) in
        (* TODO check attribute types against an fbt API *)
        let (_, attributes, _, children) = jsx_mk_props cx fbt_reason name attributes children in
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
            | Options.Jsx_react -> mk_reason (RReactElement (Some (OrdinaryName name))) loc_element
            | Options.Jsx_pragma _ -> mk_reason (RJSXElement (Some name)) loc_element
          in
          let c =
            if name = String.capitalize_ascii name then
              identifier cx (mk_ident ~comments:None name) loc
            else
              let strt =
                (* TODO: why are these different? *)
                match jsx_mode with
                | Options.Jsx_react -> SingletonStrT (OrdinaryName name)
                | Options.Jsx_pragma _ -> StrT (Literal (None, OrdinaryName name))
              in
              DefT (mk_reason (RIdentifier (OrdinaryName name)) loc, make_trust (), strt)
          in
          let (o, attributes', unresolved_params, children) =
            jsx_mk_props cx reason name attributes children
          in
          let t = jsx_desugar cx name c o attributes unresolved_params locs in
          let name = Identifier ((loc, c), { Identifier.name; comments }) in
          (t, name, attributes', children)
      | (MemberExpression member, Options.Jsx_react, _) ->
        let name = jsx_title_member_to_string member in
        let el = RReactElement (Some (OrdinaryName name)) in
        let reason = mk_reason el loc_element in
        let m_expr = jsx_title_member_to_expression member in
        let ((m_loc, t), m_expr') = expression cx m_expr in
        let c = mod_reason_of_t (replace_desc_reason (RIdentifier (OrdinaryName name))) t in
        let (o, attributes', unresolved_params, children) =
          jsx_mk_props cx reason name attributes children
        in
        let t = jsx_desugar cx name c o attributes unresolved_params locs in
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
        let (_o, attributes', _, children) = jsx_mk_props cx reason el_name attributes children in
        (t, name', attributes', children)
      | (NamespacedName namespace, _, _) ->
        (* TODO? covers namespaced names as element names *)
        let t = Unsoundness.at InferenceHooks loc_element in
        let name' = Tast_utils.error_mapper#jsx_element_name name in
        let el_name = jsx_title_namespaced_name_to_string namespace in
        let reason = mk_reason (RJSXElement (Some el_name)) loc_element in
        let (_o, attributes', _, children) = jsx_mk_props cx reason el_name attributes children in
        (t, name', attributes', children)
    in
    let closing_element =
      match closing_element with
      | Some (c_loc, { Closing.name = cname }) ->
        Some (c_loc, { Closing.name = jsx_match_closing_element name cname })
      | None -> None
    in
    (t, (loc, { Opening.name; self_closing; attributes }), children, closing_element)

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

  and jsx_mk_props cx reason name attributes children =
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
              | Some (Attribute.Literal (loc, lit)) ->
                let t = literal cx loc lit in
                (t, Some (Attribute.Literal ((loc, t), lit)))
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
                let (((_, t), _) as e) = expression cx (loc, e) in
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
                let t = EmptyT.at attr_loc |> with_trust bogus_trust in
                (t, Some (Tast_utils.unchecked_mapper#jsx_attribute_value ec))
              (* <element name /> *)
              | None -> (DefT (mk_reason RBoolean attr_loc, bogus_trust (), BoolT (Some true)), None)
            in
            let acc =
              if Type_inference_hooks_js.dispatch_jsx_hook cx aname id_loc then
                (* don't add `aname` to the prop map because it is the autocomplete token *)
                acc
              else
                ObjectExpressionAcc.add_prop
                  (Properties.add_field (OrdinaryName aname) Polarity.Neutral (Some id_loc) atype)
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
            let (((_, spread), _) as argument) = expression cx argument in
            let acc = ObjectExpressionAcc.add_spread spread acc in
            let att =
              Opening.SpreadAttribute (spread_loc, { SpreadAttribute.argument; comments })
            in
            (acc, att :: atts))
        (ObjectExpressionAcc.empty (), [])
        attributes
    in
    let attributes = List.rev atts in
    let (unresolved_params, children) = collapse_children cx children in
    let acc =
      match unresolved_params with
      | [] -> acc
      (* We add children to the React.createElement() call for React. Not to the
       * props as other JSX users may support. *)
      | _ when is_react -> acc
      | _ ->
        let arr =
          Tvar.mk_where cx reason (fun tout ->
              let reason_op = reason in
              let element_reason =
                replace_desc_reason Reason.inferred_union_elem_array_desc reason_op
              in
              let elem_t = Tvar.mk cx element_reason in
              Flow.resolve_spread_list
                cx
                ~use_op:unknown_use
                ~reason_op:reason
                unresolved_params
                (ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout))
          )
        in
        ObjectExpressionAcc.add_prop
          (Properties.add_field (OrdinaryName "children") Polarity.Neutral None arr)
          acc
    in
    let t =
      ObjectExpressionAcc.mk_object_from_spread_acc
        cx
        acc
        reason_props
        ~frozen:false
        ~default_proto:proto
    in
    (t, attributes, unresolved_params, children)

  and jsx_desugar cx name component_t props attributes children locs =
    let (loc_element, loc_opening, loc_children) = locs in
    let return_hint = Env.get_hint cx loc_element in
    match Context.jsx cx with
    | Options.Jsx_react ->
      let reason = mk_reason (RReactElement (Some (OrdinaryName name))) loc_element in
      let children =
        Base.List.map
          ~f:(function
            | UnresolvedArg (a, _) -> a
            | UnresolvedSpreadArg a ->
              Flow.add_output cx Error_message.(EUnsupportedSyntax (loc_children, SpreadArgument));
              reason_of_t a |> AnyT.error)
          children
      in
      let tvar = (reason, Tvar.mk_no_wrap cx reason) in
      let args = [Arg component_t; Arg props] @ Base.List.map ~f:(fun c -> Arg c) children in
      (match Context.react_runtime cx with
      | Options.ReactRuntimeAutomatic ->
        (* TODO(jmbrown): Model jsx more faithfully. children are now passed in as part of the props
         * object. See https://github.com/reactjs/rfcs/blob/createlement-rfc/text/0000-create-element-changes.md
         * for more details. *)
        let reason_jsx = mk_reason (RFunction RNormal) loc_element in
        let use_op =
          Op
            (ReactCreateElementCall
               { op = reason_jsx; component = reason_of_t component_t; children = loc_children }
            )
        in
        let jsx_fun = CustomFunT (reason_jsx, ReactCreateElement) in
        let call_action = Funcalltype (mk_functioncalltype reason_jsx None args tvar) in
        Flow.flow cx (jsx_fun, CallT { use_op; reason; call_action; return_hint })
      | Options.ReactRuntimeClassic ->
        let reason_createElement =
          mk_reason (RProperty (Some (OrdinaryName "createElement"))) loc_element
        in
        let use_op =
          Op
            (ReactCreateElementCall
               {
                 op = reason_createElement;
                 component = reason_of_t component_t;
                 children = loc_children;
               }
            )
        in
        let react = Env.var_ref ~lookup_mode:ForValue cx (OrdinaryName "React") loc_element in
        let prop_t = Tvar.mk cx reason_createElement in
        Flow.flow
          cx
          ( react,
            MethodT
              ( use_op,
                reason,
                reason_createElement,
                Named (reason_createElement, OrdinaryName "createElement"),
                CallM
                  {
                    methodcalltype =
                      mk_methodcalltype
                        None
                        ([Arg component_t; Arg props] @ Base.List.map ~f:(fun c -> Arg c) children)
                        tvar;
                    return_hint;
                  },
                prop_t
              )
          ));
      OpenT tvar
    | Options.Jsx_pragma (raw_jsx_expr, jsx_expr) ->
      let reason = mk_reason (RJSXFunctionCall raw_jsx_expr) loc_element in
      (* A JSX element with no attributes should pass in null as the second
       * arg *)
      let props =
        match attributes with
        | [] -> NullT.at loc_opening |> with_trust bogus_trust
        | _ -> props
      in
      let argts =
        [Arg component_t; Arg props]
        @ Base.List.map
            ~f:(function
              | UnresolvedArg (c, _) -> Arg c
              | UnresolvedSpreadArg c -> SpreadArg c)
            children
      in
      let use_op = Op (JSXCreateElement { op = reason; component = reason_of_t component_t }) in
      let open Ast.Expression in
      (match jsx_expr with
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
             None
             argts
          )
      | _ ->
        let f = jsx_pragma_expression cx raw_jsx_expr loc_element jsx_expr in
        func_call cx loc_element reason ~use_op ~call_strict_arity:false f None argts)

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
      Env.var_ref ~lookup_mode:ForValue cx (OrdinaryName name) loc ~desc
    | expr ->
      (* Oh well, we tried *)
      let ((_, t), _) = expression cx expr in
      t

  and jsx_body cx (loc, child) =
    let open Ast.JSX in
    let make_trust = Context.trust_constructor cx in
    match child with
    | Element e ->
      let (t, e) = jsx cx loc e in
      (Some (UnresolvedArg (t, None)), (loc, Element e))
    | Fragment f ->
      let (t, f) = jsx_fragment cx loc f in
      (Some (UnresolvedArg (t, None)), (loc, Fragment f))
    | ExpressionContainer ec ->
      ExpressionContainer.(
        let { expression = ex; ExpressionContainer.comments } = ec in
        let (unresolved_param, ex) =
          match ex with
          | Expression e ->
            let (((_, t), _) as e) = expression cx e in
            (Some (UnresolvedArg (t, None)), Expression e)
          | EmptyExpression -> (None, EmptyExpression)
        in
        ( unresolved_param,
          (loc, ExpressionContainer { expression = ex; ExpressionContainer.comments })
        )
      )
    | SpreadChild { SpreadChild.expression = expr; comments } ->
      let (((_, t), _) as e) = expression cx expr in
      (Some (UnresolvedSpreadArg t), (loc, SpreadChild { SpreadChild.expression = e; comments }))
    | Text { Text.value; raw } ->
      let unresolved_param_opt =
        match jsx_trim_text make_trust loc value with
        | Some c -> Some (UnresolvedArg (c, None))
        | None -> None
      in
      (unresolved_param_opt, (loc, Text { Text.value; raw }))

  and jsx_trim_text make_trust loc value =
    match Utils_jsx.trim_jsx_text (ALoc.to_loc_exn loc) value with
    | Some (loc, trimmed) ->
      Some
        (DefT
           ( mk_reason RJSXText (loc |> ALoc.of_loc),
             make_trust (),
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
    let class_entries = Env.get_class_entries cx in
    OptGetPrivatePropT (use_op, reason, name, class_entries, false)

  (* Property lookups become non-strict when processing conditional expressions
     (see above).

     TODO: It should be possible to factor the processing of LHS / reference
     expressions out of `expression`, somewhat like what assignment_lhs does. That
     would make everything involving Refinement be in the same place.
  *)
  and get_prop_opt_use ~cond reason ~use_op (prop_reason, name) =
    let id = mk_id () in
    if Base.Option.is_some cond then
      OptTestPropT (use_op, reason, id, Named (prop_reason, OrdinaryName name))
    else
      OptGetPropT (use_op, reason, Some id, Named (prop_reason, OrdinaryName name))

  and get_prop ~cond cx reason ~use_op tobj (prop_reason, name) =
    let opt_use = get_prop_opt_use ~cond reason ~use_op (prop_reason, name) in
    Tvar.mk_no_wrap_where cx reason (fun t ->
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
      Tvar.mk_where cx arr_reason (fun tvar ->
          let keys_reason =
            update_desc_reason
              (fun desc -> RCustom (spf "element of %s" (string_of_desc desc)))
              reason
          in
          Flow.flow cx (obj_t, GetKeysT (keys_reason, UseT (use_op, tvar)))
      )
    in
    let get_values ~arr_reason obj_t =
      Tvar.mk_where cx arr_reason (fun tvar ->
          let values_reason =
            update_desc_reason
              (fun desc -> RCustom (spf "element of %s" (string_of_desc desc)))
              reason
          in
          Flow.flow cx (obj_t, GetDictValuesT (values_reason, UseT (use_op, tvar)))
      )
    in
    match (m, targs, args) with
    | ("create", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let (((_, e_t), _) as e_ast) = expression cx e in
      let proto =
        let reason = mk_reason RPrototype (fst e) in
        Tvar.mk_where cx reason (fun t -> Flow.flow cx (e_t, ObjTestProtoT (reason, t)))
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
        Tvar.mk_where cx reason (fun t -> Flow.flow cx (e_t, ObjTestProtoT (reason, t)))
      in
      let (pmap, properties) = prop_map_of_object cx properties in
      let propdesc_type = Flow.get_builtin cx (OrdinaryName "PropertyDescriptor") reason in
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
                Tvar.mk_where cx reason (fun tvar ->
                    let loc = aloc_of_reason reason in
                    let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [tvar] in
                    Flow.flow cx (spec, UseT (use_op, propdesc))
                )
              in
              let p = Field (loc, t, Polarity.Neutral) in
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
      ( DefT (arr_reason, bogus_trust (), ArrT (ArrayAT (keys_t, None))),
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ("values", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let arr_reason = mk_reason RArrayType loc in
      let (((_, o), _) as e_ast) = expression cx e in
      ( DefT (arr_reason, bogus_trust (), ArrT (ArrayAT (get_values ~arr_reason o, None))),
        None,
        (args_loc, { ArgList.arguments = [Expression e_ast]; comments })
      )
    | ("entries", None, (args_loc, { ArgList.arguments = [Expression e]; comments })) ->
      let arr_reason = mk_reason RArrayType loc in
      let (((_, o), _) as e_ast) = expression cx e in
      let keys_t = get_keys ~arr_reason o in
      let values_t = get_values ~arr_reason o in
      let elem_t = UnionT (mk_reason RTupleElement loc, UnionRep.make keys_t values_t []) in
      let entry_t =
        DefT
          ( mk_reason RTupleType loc,
            bogus_trust (),
            ArrT
              (TupleAT
                 {
                   elem_t;
                   elements =
                     [
                       TupleElement { name = Some "key"; t = keys_t; polarity = Polarity.Neutral };
                       TupleElement
                         { name = Some "value"; t = values_t; polarity = Polarity.Neutral };
                     ];
                 }
              )
          )
      in
      ( DefT (arr_reason, bogus_trust (), ArrT (ArrayAT (entry_t, None))),
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
                  ( (ploc, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String x; _ })
                  as key
                  );
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
      let loc = aloc_of_reason reason in
      let propdesc_type = Flow.get_builtin cx (OrdinaryName "PropertyDescriptor") reason in
      let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [ty] in
      let (((_, o), _) as e_ast) = expression cx e in
      let key_ast = expression cx key in
      let (((_, spec), _) as config_ast) = expression cx config in
      let prop_reason = mk_reason (RProperty (Some (OrdinaryName x))) ploc in
      Flow.flow cx (spec, UseT (use_op, propdesc));
      let prop_t = Tvar.mk cx prop_reason in
      Flow.flow
        cx
        ( o,
          SetPropT
            (use_op, reason, Named (prop_reason, OrdinaryName x), Assign, Normal, ty, Some prop_t)
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
      let propdesc_type = Flow.get_builtin cx (OrdinaryName "PropertyDescriptor") reason in
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
               let loc = aloc_of_reason reason in
               let propdesc = implicit_typeapp ~annot_loc:loc propdesc_type [tvar] in
               Flow.flow cx (spec, UseT (use_op, propdesc));
               Flow.flow
                 cx
                 (o, SetPropT (use_op, reason, Named (reason, x), Assign, Normal, tvar, None))
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
        let (t, properties) = object_ ~frozen:true cx reason properties in
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

  and mk_class cx class_loc ~name_loc ~general reason c =
    let node_cache = Context.node_cache cx in
    match Node_cache.get_class node_cache class_loc with
    | Some x ->
      Debug_js.Verbose.print_if_verbose_lazy
        cx
        (lazy [spf "Class cache hit at %s" (ALoc.debug_to_string (aloc_of_reason reason))]);
      x
    | None ->
      let def_reason = repos_reason class_loc reason in
      let this_in_class = Class_stmt_sig.This.in_class c in
      let self = Env.read_class_self_type cx class_loc in
      let (class_t, _, class_sig, class_ast_f) =
        mk_class_sig cx ~name_loc ~class_loc reason self c
      in

      let public_property_map =
        Class_stmt_sig.fields_to_prop_map cx
        @@ Class_stmt_sig.public_fields_of_signature ~static:false class_sig
      in
      let private_property_map =
        Class_stmt_sig.fields_to_prop_map cx
        @@ Class_stmt_sig.private_fields_of_signature ~static:false class_sig
      in
      Class_stmt_sig.check_super cx def_reason class_sig;
      Class_stmt_sig.check_implements cx def_reason class_sig;
      Class_stmt_sig.check_methods cx def_reason class_sig;
      if this_in_class || not (Class_stmt_sig.This.is_bound_to_empty class_sig) then
        Class_stmt_sig.toplevels cx class_sig;

      let class_body = Ast.Class.((snd c.body).Body.body) in
      Context.add_voidable_check
        cx
        {
          Context.public_property_map;
          private_property_map;
          errors = Property_assignment.eval_property_assignment class_body;
        };
      (class_t, class_ast_f general)

  (* Process a class definition, returning a (polymorphic) class type. A class
     type is a wrapper around an instance type, which contains types of instance
     members, a pointer to the super instance type, and a container for types of
     static members. The static members can be thought of as instance members of a
     "metaclass": thus, the static type is itself implemented as an instance
     type. *)
  and mk_class_sig =
    let open Class_stmt_sig in
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
      let (annot_or_inferred, annot_ast) = Anno.mk_type_annotation cx tparams_map reason annot in
      let annot_t = type_t_of_annotated_or_inferred annot_or_inferred in
      let (field, get_init) =
        match init with
        | Ast.Class.Property.Declared -> (Annot annot_t, Fun.const Ast.Class.Property.Declared)
        | Ast.Class.Property.Uninitialized ->
          (Annot annot_t, Fun.const Ast.Class.Property.Uninitialized)
        | Ast.Class.Property.Initialized expr ->
          begin
            match (expr, annot_or_inferred) with
            | ( ( _,
                  Ast.Expression.Literal
                    {
                      Ast.Literal.value =
                        Ast.Literal.(String _ | Boolean _ | Number _ | BigInt _ | RegExp _);
                      _;
                    }
                ),
                Inferred _
              ) ->
              let ((_, t), _) = expression cx expr in
              Flow.flow_t cx (t, annot_t)
            | ((_, Ast.Expression.(ArrowFunction function_ | Function function_)), Inferred _) ->
              let { Ast.Function.sig_loc; _ } = function_ in
              let cache = Context.node_cache cx in
              let ((func_sig, _) as sig_data) =
                mk_func_sig
                  cx
                  ~required_this_param_type:None
                  ~constructor:false
                  ~require_return_annot:true
                  ~statics:SMap.empty
                  tparams_map
                  reason
                  function_
              in
              if not (Context.in_synthesis_mode cx) then
                Node_cache.set_function_sig cache sig_loc sig_data;
              let this_t =
                match expr with
                | (_, Ast.Expression.ArrowFunction _) -> dummy_this (aloc_of_reason reason)
                | _ ->
                  if
                    Signature_utils.This_finder.found_this_in_body_or_params
                      function_.Ast.Function.body
                      function_.Ast.Function.params
                  then
                    Tvar.mk cx (mk_reason RThis sig_loc)
                  else
                    Type.implicit_mixed_this reason
              in
              let (arrow, function_loc_opt) =
                match expr with
                | (loc, Ast.Expression.Function _) -> (false, Some loc)
                | (_, Ast.Expression.ArrowFunction _) -> (true, None)
                | _ -> failwith "expr can only be Function or ArrowFunction"
              in
              let t =
                Statement.Func_stmt_sig.functiontype cx ~arrow function_loc_opt this_t func_sig
              in
              Flow.flow_t cx (t, annot_t)
            | (_, Inferred _) ->
              let annot_loc =
                match annot with
                | Ast.Type.Missing loc
                | Ast.Type.Available (loc, _) ->
                  loc
              in
              Flow.add_output
                cx
                (Error_message.EMissingLocalAnnotation
                   {
                     reason = repos_reason annot_loc reason;
                     hint_available = false;
                     from_generic_function = false;
                   }
                );
              if Context.lti cx then
                Flow.flow_t cx (AnyT.make (AnyError (Some MissingAnnotation)) reason, annot_t)
            | _ -> ()
          end;
          let value_ref : (ALoc.t, ALoc.t * Type.t) Ast.Expression.t option ref = ref None in
          ( Infer
              ( Func_stmt_sig.field_initializer tparams_map reason expr annot_or_inferred,
                (fun (_, _, value_opt) -> value_ref := Some (Base.Option.value_exn value_opt))
              ),
            fun () ->
              Ast.Class.Property.Initialized
                (Base.Option.value !value_ref ~default:(Tast_utils.error_mapper#expression expr))
          )
      in
      (match (init, annot_or_inferred) with
      | ((Ast.Class.Property.Declared | Ast.Class.Property.Uninitialized), Inferred _) ->
        Flow.add_output
          cx
          (Error_message.EMissingLocalAnnotation
             { reason; hint_available = false; from_generic_function = false }
          );
        if Context.lti cx then
          Flow.flow_t cx (AnyT.make (AnyError (Some MissingAnnotation)) reason, annot_t)
      | _ -> ());
      (field, annot_t, annot_ast, get_init)
    in
    let mk_method cx ~constructor =
      mk_func_sig
        cx
        ~required_this_param_type:None
        ~require_return_annot:(not constructor)
        ~constructor
        ~statics:SMap.empty
    in
    let mk_extends cx tparams_map = function
      | None -> (Implicit { null = false }, (fun () -> None))
      | Some (loc, { Ast.Class.Extends.expr; targs; comments }) ->
        let (c, expr) =
          let open Ast.Expression in
          let rec super_expr (loc, expr) =
            match expr with
            | Identifier (id_loc, id) ->
              let t = identifier cx id id_loc in
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
              let tout = get_prop ~use_op ~cond:None cx expr_reason t (prop_reason, name) in
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
            | TypeCast { TypeCast.expression = expr; annot; comments } ->
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
            | _ ->
              Flow.add_output cx Error_message.(EInvalidExtends (mk_expression_reason (loc, expr)));
              (AnyT.at (AnyError None) loc, (fun () -> expression cx (loc, expr)))
          in
          super_expr expr
        in
        let (t, targs) = Anno.mk_super cx tparams_map loc c targs in
        (Explicit t, (fun () -> Some (loc, { Ast.Class.Extends.expr = expr (); targs; comments })))
    in
    fun cx ~name_loc ~class_loc reason self cls ->
      let node_cache = Context.node_cache cx in
      match Node_cache.get_class_sig node_cache class_loc with
      | Some x ->
        Debug_js.Verbose.print_if_verbose_lazy
          cx
          (lazy [spf "Class sig cache hit at %s" (ALoc.debug_to_string (aloc_of_reason reason))]);
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
        let (this_tparam, this_t) = mk_this self cx reason tparams in
        let tparams_map_with_this =
          Subst_name.Map.add (Subst_name.Name "this") this_t tparams_map
        in
        let (class_sig, extends_ast_f, implements_ast) =
          let id = Context.make_aloc_id cx name_loc in
          let (extends, extends_ast_f) = mk_extends cx tparams_map_with_this extends in
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
                       let c = Env.get_var ~lookup_mode:ForType cx name id_loc in
                       let (typeapp, targs) =
                         match targs with
                         | None -> ((loc, c, None), None)
                         | Some (targs_loc, { Ast.Type.TypeArgs.arguments = targs; comments }) ->
                           let (ts, targs_ast) = Anno.convert_list cx tparams_map_with_this targs in
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
          (empty id class_loc reason tparams tparams_map super, extends_ast_f, implements_ast)
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
            add_default_constructor reason class_sig
        in
        (* All classes have a static "name" property. *)
        let class_sig = add_name_field class_sig in

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
                Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;
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
                      ~default:(Tast_utils.error_mapper#function_body func.Ast.Function.body)
                  in
                  let func_t =
                    Base.Option.value
                      !func_t_ref
                      ~default:(EmptyT.at id_loc |> with_trust bogus_trust)
                  in
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
                    let add = add_constructor ~id_loc:(Some id_loc) ~set_asts ~set_type in
                    (add, None)
                  | Method.Method ->
                    let add =
                      if private_ then
                        add_private_method
                          ~static
                          name
                          ~id_loc
                          ~this_write_loc:(Some func_loc)
                          ~set_asts
                          ~set_type
                      else
                        add_method
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
                      add_getter
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
                      add_setter
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
                      comments;
                    }
                  ) ->
                Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;
                let reason = mk_reason (RPrivateProperty name) loc in
                let polarity = Anno.polarity cx variance in
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
                ( add_private_field ~static name id_loc polarity field c,
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
                      comments;
                    }
                  ) ->
                Type_inference_hooks_js.dispatch_class_member_decl_hook cx self static name id_loc;
                let reason = mk_reason (RProperty (Some (OrdinaryName name))) loc in
                let polarity = Anno.polarity cx variance in
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
                ( add_field ~static name id_loc polarity field c,
                  get_element :: rev_elements,
                  public_seen_names'
                )
              (* literal LHS *)
              | ( Body.Method (loc, { Method.key = Ast.Expression.Object.Property.Literal _; _ })
                | Body.Property (loc, { Property.key = Ast.Expression.Object.Property.Literal _; _ })
                  ) as elem ->
                Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ClassPropertyLiteral));
                ( c,
                  (fun () -> Tast_utils.error_mapper#class_element elem) :: rev_elements,
                  public_seen_names
                )
              (* computed LHS *)
              | ( Body.Method (loc, { Method.key = Ast.Expression.Object.Property.Computed _; _ })
                | Body.Property
                    (loc, { Property.key = Ast.Expression.Object.Property.Computed _; _ }) ) as elem
                ->
                Flow.add_output cx Error_message.(EUnsupportedSyntax (loc, ClassPropertyComputed));
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
          Env.in_class_scope cx class_loc (fun () -> Class_stmt_sig.make_thises cx class_sig)
        in
        Env.bind_class_instance_this cx instance_this_default class_loc;
        Env.bind_class_static_this cx static_this_default class_loc;
        Env.bind_class_instance_super cx super class_loc;
        Env.bind_class_static_super cx static_super class_loc;
        let (class_t_internal, class_t) = Class_stmt_sig.classtype cx class_sig in
        Env.bind_class_self_type cx class_loc class_t_internal;
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

  and mk_func_sig =
    let predicate_function_kind cx loc params =
      let open Error_message in
      let (_, { Ast.Function.Params.params; rest; this_ = _; comments = _ }) = params in
      let kind = Func_class_sig_types.Func.Predicate in
      let kind =
        List.fold_left
          (fun kind (_, param) ->
            let open Flow_ast.Function.Param in
            match param.argument with
            | (ploc, Flow_ast.Pattern.Object _)
            | (ploc, Flow_ast.Pattern.Array _)
            | (ploc, Flow_ast.Pattern.Expression _) ->
              let reason = mk_reason RDestructuring ploc in
              Flow_js.add_output cx (EUnsupportedSyntax (loc, PredicateInvalidParameter reason));
              Func_class_sig_types.Func.Ordinary
            | (_, Flow_ast.Pattern.Identifier _) -> kind)
          kind
          params
      in
      match rest with
      | Some (rloc, { Flow_ast.Function.RestParam.argument; comments = _ }) ->
        let desc = Reason.code_desc_of_pattern argument in
        let reason = mk_reason (RRestParameter (Some desc)) rloc in
        Flow_js.add_output cx (EUnsupportedSyntax (loc, PredicateInvalidParameter reason));
        Func_class_sig_types.Func.Ordinary
      | None -> kind
    in
    let function_kind cx ~constructor ~async ~generator ~predicate ~params ~ret_loc =
      let open Ast.Type.Predicate in
      let open Func_class_sig_types.Func in
      match (constructor, async, generator, predicate) with
      | (true, false, false, None) -> Ctor
      | (true, _, _, _) ->
        Flow.add_output
          cx
          Error_message.(
            EInvalidConstructorDefinition
              { loc = ret_loc; async; generator; predicate = Base.Option.is_some predicate }
          );
        Ctor
      | (false, true, true, None) -> AsyncGenerator { return_loc = ret_loc }
      | (false, true, false, None) -> Async
      | (false, false, true, None) -> Generator { return_loc = ret_loc }
      | (false, false, false, None) -> Ordinary
      | ( false,
          false,
          false,
          Some (loc, { kind = Ast.Type.Predicate.Inferred | Declared _; comments = _ })
        ) ->
        predicate_function_kind cx loc params
      | (false, _, _, _) -> Utils_js.assert_false "(async || generator) && pred"
    in
    let mk_param_annot cx tparams_map reason = function
      | Ast.Type.Missing loc when Context.in_synthesis_mode cx ->
        let t = Context.mk_placeholder cx reason in
        (t, Ast.Type.Missing (loc, t))
      | Ast.Type.Missing loc ->
        let t = Env.find_write cx Env_api.FunctionParamLoc reason in
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
    let require_this_annot cx func param_loc = function
      | (Some t, None)
        when Signature_utils.This_finder.found_this_in_body_or_params
               func.Ast.Function.body
               func.Ast.Function.params ->
        let reason = mk_reason (RImplicitThis (RFunction RNormal)) param_loc in
        Flow.add_output
          cx
          (Error_message.EMissingLocalAnnotation
             { reason; hint_available = false; from_generic_function = false }
          );
        if Context.lti cx then
          Flow_js.flow_t cx (AnyT.make (AnyError (Some MissingAnnotation)) reason, t)
      | _ -> ()
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
    let free_bound_ts cx t =
      let finder =
        object (_self)
          inherit [Loc_collections.ALocSet.t] Type_visitor.t as super

          val mutable tparams : Subst_name.t list = []

          method! type_ cx pole acc t =
            match t with
            | DefT (_, _, PolyT { tparams = tps; _ }) ->
              let old_tparams = tparams in
              Nel.iter (fun tp -> tparams <- tp.name :: tparams) tps;
              let acc = super#type_ cx pole acc t in
              tparams <- old_tparams;
              acc
            | GenericT { name; _ } when not (List.exists (fun x -> x = name) tparams) ->
              Loc_collections.ALocSet.add (TypeUtil.loc_of_t t) acc
            | _ -> super#type_ cx pole acc t
        end
      in
      finder#type_ cx Polarity.Neutral Loc_collections.ALocSet.empty t
    in
    fun cx
        ~required_this_param_type
        ~require_return_annot
        ~constructor
        ~statics
        tparams_map
        reason
        func ->
      let {
        Ast.Function.tparams;
        return;
        body;
        predicate;
        params;
        id;
        async;
        generator;
        sig_loc;
        comments = _;
      } =
        func
      in
      let cache = Context.node_cache cx in
      match Node_cache.get_function_sig cache sig_loc with
      | Some x -> x
      | None ->
        let loc = aloc_of_reason reason in
        let ret_loc =
          match return with
          | Ast.Type.Available (loc, _)
          | Ast.Type.Missing loc ->
            loc
        in
        let kind = function_kind cx ~constructor ~async ~generator ~predicate ~params ~ret_loc in
        let (tparams, tparams_map, tparams_ast) =
          Anno.mk_type_param_declarations cx ~tparams_map tparams
        in
        let fparams = mk_params cx tparams_map params in
        require_this_annot
          cx
          func
          (fst params)
          (required_this_param_type, Ast.Function.Params.((snd params).this_));
        let body = Some body in
        let ret_reason = mk_reason RReturn (Func_sig.return_loc func) in
        let (return_annotated_or_inferred, return) =
          let open Func_class_sig_types.Func in
          let has_nonvoid_return =
            Nonvoid_return.might_have_nonvoid_return loc func
            || (kind <> Ordinary && kind <> Async && kind <> Ctor)
          in
          let return =
            Anno.mk_return_type_annotation
              cx
              tparams_map
              ret_reason
              ~void_return:(not has_nonvoid_return)
              ~async:(kind = Async)
              return
          in
          let from_generic_function = Base.Option.is_some tparams in
          let require_return_annot =
            require_return_annot || (Context.lti cx && from_generic_function)
          in
          begin
            match return with
            | (Inferred t, _) when has_nonvoid_return && require_return_annot ->
              let reason = repos_reason ret_loc ret_reason in
              Flow_js.add_output
                cx
                (Error_message.EMissingLocalAnnotation
                   { reason; hint_available = false; from_generic_function }
                );
              if Context.lti cx then
                Flow.flow_t cx (AnyT.make (AnyError (Some MissingAnnotation)) reason, t)
            | _ -> ()
          end;
          return
        in
        let (return_annotated_or_inferred, predicate) =
          let open Ast.Type.Predicate in
          match predicate with
          | None -> (return_annotated_or_inferred, None)
          | Some ((_, { kind = Ast.Type.Predicate.Inferred; comments = _ }) as pred) ->
            (* Predicate Functions
             *
             * function f(x: S): [T] %checks { return e; }
             *
             * The return type we assign to this function will be used for refining the
             * input x. The type annotation T may not have this ability (if it's an
             * annotation). Instead we introduce a fresh type T'. T' will receive lower
             * bounds from the return expression e, but is also checked against the
             * return type (annotation) T:
             *
             *   OpenPred(typeof e, preds) ~> T'
             *   T' ~> T
             *
             * The function signature will be
             *
             *  (x: S) => T' (%checks)
             *)
            let bounds =
              free_bound_ts cx (type_t_of_annotated_or_inferred return_annotated_or_inferred)
            in
            if Loc_collections.ALocSet.is_empty bounds then
              let return_annotated_or_inferred' =
                map_annotated_or_inferred
                  (fun return_t -> Tvar.mk_where cx reason (fun t -> Flow.flow_t cx (t, return_t)))
                  return_annotated_or_inferred
              in
              (return_annotated_or_inferred', Some pred)
            else
              (* If T is a polymorphic type P<X>, this approach can lead to some
               * complications. The 2nd constraint from above would become
               *
               *   T' ~> P<X>
               *
               * which was previously ill-formed since it was outside a check_with_generics
               * call (led to Not_expect_bounds exception). We disallow this case
               * and instead propagate the original return type T; with the removal of
               * check_with_generics this may no longer be necessary.
               *)
              let () =
                Loc_collections.ALocSet.iter
                  (fun loc ->
                    Flow_js.add_output
                      cx
                      Error_message.(EUnsupportedSyntax (loc, PredicateFunctionAbstractReturnType)))
                  bounds
              in
              (return_annotated_or_inferred, Some (Tast_utils.error_mapper#type_predicate pred))
          | Some ((loc, { kind = Declared _; comments = _ }) as pred) ->
            let (annotated_or_inferred, _) =
              Anno.mk_type_annotation cx tparams_map ret_reason (Ast.Type.Missing loc)
            in
            Flow_js.add_output
              cx
              Error_message.(EUnsupportedSyntax (loc, PredicateDeclarationForImplementation));
            (annotated_or_inferred, Some (Tast_utils.error_mapper#type_predicate pred))
        in
        let return_t =
          mk_inference_target_with_annots
            ~has_hint:(Env.has_hint cx ret_loc)
            return_annotated_or_inferred
        in
        let () =
          if kind = Func_class_sig_types.Func.Ctor then
            let return_t = TypeUtil.type_t_of_annotated_or_inferred return_t in
            let use_op = Op (FunReturnStatement { value = TypeUtil.reason_of_t return_t }) in
            Flow.unify
              cx
              ~use_op
              return_t
              (VoidT.make (mk_reason RConstructorVoidReturn ret_loc) (literal_trust ()))
        in
        let statics_t =
          let props =
            SMap.fold
              (fun name (kind, loc) acc ->
                let expr_t =
                  Env.find_write cx kind (mk_reason (RIdentifier (OrdinaryName name)) loc)
                in
                let field = Field (Some loc, expr_t, Polarity.Neutral) in
                NameUtils.Map.add (OrdinaryName name) field acc)
              statics
              NameUtils.Map.empty
          in
          Obj_type.mk_with_proto cx reason (FunProtoT reason) ~obj_kind:Type.Inexact ~props
        in
        ( {
            Func_stmt_sig_types.reason;
            kind;
            tparams;
            tparams_map;
            fparams;
            body;
            return_t;
            statics = Some statics_t;
          },
          fun params body fun_type ->
            {
              func with
              Ast.Function.id =
                Base.Option.map ~f:(fun (id_loc, name) -> ((id_loc, fun_type), name)) id;
              params;
              body;
              predicate;
              return;
              tparams = tparams_ast;
            }
        )

  (* Given a function declaration and types for `this` and `super`, extract a
     signature consisting of type parameters, parameter types, parameter names,
     and return type, check the body against that signature by adding `this`
     and super` to the environment, and return the signature. *)
  and function_decl cx ~required_this_param_type ~fun_loc ~arrow ~statics reason func default_this =
    let (func_sig, reconstruct_func) =
      mk_func_sig
        cx
        ~required_this_param_type
        ~require_return_annot:false
        ~constructor:false
        ~statics
        Subst_name.Map.empty
        reason
        func
    in
    let default_this =
      match default_this with
      | Some t -> t
      | None -> dummy_this (aloc_of_reason reason)
    in
    let fun_type = Func_stmt_sig.functiontype cx ~arrow fun_loc default_this func_sig in
    if Context.in_synthesis_mode cx then
      let { Ast.Function.params; body; _ } = func in
      let params_ast = Typed_ast_utils.error_mapper#function_params params in
      let body_ast = Typed_ast_utils.error_mapper#function_body body in
      (fun_type, reconstruct_func params_ast body_ast)
    else
      let (params_ast, body_ast, _) = Func_stmt_sig.toplevels cx func_sig in
      ( fun_type,
        reconstruct_func (Base.Option.value_exn params_ast) (Base.Option.value_exn body_ast)
      )

  (* Process a function declaration, returning a (polymorphic) function type. *)
  and mk_function_declaration cx ~general reason fun_loc func =
    mk_function cx ~general ~needs_this_param:true ~statics:SMap.empty reason fun_loc func

  (* Process a function expression, returning a (polymorphic) function type. *)
  and mk_function_expression cx ~general ~needs_this_param reason fun_loc func =
    mk_function cx ~needs_this_param ~general ~statics:SMap.empty reason fun_loc func

  (* Internal helper function. Use `mk_function_declaration` and `mk_function_expression` instead. *)
  and mk_function
      cx
      ~needs_this_param
      ~general
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
        (lazy [spf "Function cache hit at %s" (ALoc.debug_to_string (aloc_of_reason reason))]);
      cached
    | None ->
      let loc = aloc_of_reason reason in

      (* The default behavior of `this` still depends on how it
         was created, so we must provide the recipe based on where `function_decl`
         is invoked. *)
      let default_this =
        if
          Signature_utils.This_finder.found_this_in_body_or_params
            func.Ast.Function.body
            func.Ast.Function.params
        then
          Tvar.mk cx (mk_reason RThis loc)
        else
          Type.implicit_mixed_this reason
      in
      let (fun_type, reconstruct_ast) =
        function_decl
          cx
          ~required_this_param_type:(Base.Option.some_if needs_this_param default_this)
          ~fun_loc:(Base.Option.some_if needs_this_param fun_loc)
          ~arrow:false
          ~statics
          reason
          func
          (Base.Option.some_if needs_this_param default_this)
      in
      (fun_type, reconstruct_ast general)

  (* Process an arrow function, returning a (polymorphic) function type. *)
  and mk_arrow cx ~statics reason func =
    (* Do not expose the type of `this` in the function's type. This call to
       function_decl has already done the necessary checking of `this` in
       the body of the function. Now we want to avoid re-binding `this` to
       objects through which the function may be called. *)
    let default_this = None in
    let (fun_type, reconstruct_ast) =
      function_decl
        cx
        ~required_this_param_type:None
        ~fun_loc:None
        ~arrow:true
        ~statics
        reason
        func
        default_this
    in
    (fun_type, reconstruct_ast fun_type)

  and declare_function_to_function_declaration cx =
    let add_output l =
      Flow.add_output
        cx
        (match l with
        | Declare_function_utils.PredicateDeclarationWithoutExpression loc ->
          Error_message.(EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression))
        | Declare_function_utils.PredicateDeclarationAnonymousParameters loc ->
          Error_message.(EUnsupportedSyntax (loc, PredicateDeclarationAnonymousParameters)))
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
    match snd right with
    | Ast.Expression.Literal literal ->
      ExistsCheck.(
        begin
          match literal.Ast.Literal.value with
          | Ast.Literal.String "" ->
            update_excuses (fun excuse -> { excuse with string_loc = Some right_loc })
          | Ast.Literal.Boolean false ->
            update_excuses (fun excuse -> { excuse with bool_loc = Some right_loc })
          | Ast.Literal.Number 0. ->
            update_excuses (fun excuse -> { excuse with number_loc = Some right_loc })
          | Ast.Literal.BigInt (Some 0L) ->
            update_excuses (fun excuse -> { excuse with bigint_loc = Some right_loc })
          (* There's no valid default value for mixed to create an excuse. *)
          | _ -> ()
        end
      )
    | _ -> ()

  and mk_initial_arguments_reason =
    let open Ast.Expression in
    let rec helper = function
      | [] -> []
      | Expression x :: args -> mk_expression_reason x :: helper args
      | Spread _ :: _ -> []
    in
    (fun (_args_loc, { Ast.Expression.ArgList.arguments; comments = _ }) -> helper arguments)

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
            | EnumExhaustiveCheckPossiblyValid { tool; possible_checks; checks; default_case } ->
              let reason = mk_reason (RCustom "case") case_test_loc in
              let possible_check = (obj_t, EnumCheck { reason; member_name = name }) in
              EnumExhaustiveCheckPossiblyValid
                { tool; possible_checks = possible_check :: possible_checks; checks; default_case })
          | (default_case_loc, { Case.test = None; _ }) ->
            (match acc with
            | EnumExhaustiveCheckInvalid _ -> acc
            | EnumExhaustiveCheckPossiblyValid { tool; possible_checks; checks; default_case = _ }
              ->
              EnumExhaustiveCheckPossiblyValid
                {
                  tool;
                  possible_checks;
                  checks;
                  default_case = Some (mk_reason (RCustom "default case") default_case_loc);
                })
          | (_, { Case.test = Some ((case_test_loc, _), _); _ }) ->
            let case_reason = Reason.mk_reason (Reason.RCustom "case") case_test_loc in
            (match acc with
            | EnumExhaustiveCheckInvalid invalid_checks ->
              EnumExhaustiveCheckInvalid (case_reason :: invalid_checks)
            | EnumExhaustiveCheckPossiblyValid _ -> EnumExhaustiveCheckInvalid [case_reason]))
        (EnumExhaustiveCheckPossiblyValid
           {
             tool = EnumResolveDiscriminant;
             possible_checks = [];
             checks = [];
             default_case = None;
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
    let reason = mk_reason (REnum name) name_loc in
    let t =
      if Context.enable_enums cx then (
        let enum_t = mk_enum cx ~enum_reason:reason name_loc body in
        let t = DefT (reason, literal_trust (), EnumObjectT enum_t) in
        let use_op =
          Op
            (AssignVar
               { var = Some (mk_reason (RIdentifier (OrdinaryName name)) name_loc); init = reason }
            )
        in
        Env.init_implicit_const cx ~use_op t name_loc;
        t
      ) else (
        Flow.add_output cx (Error_message.EEnumsNotEnabled loc);
        AnyT.error reason
      )
    in
    let id' = ((name_loc, t), ident) in
    { id = id'; body; comments }

  and mk_enum cx ~enum_reason name_loc body =
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
        let reason = mk_reason (REnumRepresentation RBoolean) (aloc_of_reason enum_reason) in
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
        (DefT (reason, literal_trust (), BoolT bool_type), members, has_unknown_members)
      | (_, NumberBody { NumberBody.members; has_unknown_members; _ }) ->
        let reason = mk_reason (REnumRepresentation RNumber) (aloc_of_reason enum_reason) in
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
        (DefT (reason, literal_trust (), NumT num_type), members, has_unknown_members)
      | (_, BigIntBody { BigIntBody.members; has_unknown_members; _ }) ->
        let reason = mk_reason (REnumRepresentation RBigInt) (aloc_of_reason enum_reason) in
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
        (DefT (reason, literal_trust (), BigIntT num_type), members, has_unknown_members)
      | ( _,
          StringBody { StringBody.members = StringBody.Initialized members; has_unknown_members; _ }
        ) ->
        let reason = mk_reason (REnumRepresentation RString) (aloc_of_reason enum_reason) in
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
        (DefT (reason, literal_trust (), StrT str_type), members, has_unknown_members)
      | (_, StringBody { StringBody.members = StringBody.Defaulted members; has_unknown_members; _ })
        ->
        let reason = mk_reason (REnumRepresentation RString) (aloc_of_reason enum_reason) in
        ( DefT (reason, literal_trust (), StrT Truthy (* Member names can't be the empty string *)),
          defaulted_members members,
          has_unknown_members
        )
      | (_, SymbolBody { SymbolBody.members; has_unknown_members; comments = _ }) ->
        let reason = mk_reason (REnumRepresentation RSymbol) (aloc_of_reason enum_reason) in
        (DefT (reason, literal_trust (), SymbolT), defaulted_members members, has_unknown_members)
    in
    { enum_id; members; representation_t; has_unknown_members }
end
