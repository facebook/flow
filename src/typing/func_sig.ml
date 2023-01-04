(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module Flow = Flow_js
open Reason
open Type
open TypeUtil
include Func_sig_intf

class func_scope_visitor cx ~has_return_annot ~return_t ~yield_t ~next_t ~body_loc kind exhaust =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable no_return = true

    val mutable has_throw = false

    val mutable no_yield = true

    method on_type_annot x = x

    method on_loc_annot x = x

    method! function_ fn = fn

    method visit statements =
      ignore @@ this#statement_list statements;
      if not has_return_annot then (
        ( if no_return && has_throw then
          let reason = mk_reason REmpty body_loc in
          let t = EmptyT.make reason (Trust.literal_trust ()) in
          Flow.flow cx (t, UseT (unknown_use, return_t))
        );
        if no_yield then
          Flow.flow_t cx (VoidT.make (mk_reason RVoid body_loc) (Trust.literal_trust ()), yield_t)
      )

    method! switch ({ Ast.Statement.Switch.exhaustive_out = (loc, t); _ } as switch) =
      Base.Option.iter exhaust ~f:(fun (exhaustive_t, exhaust_locs, _) ->
          if Base.List.mem ~equal:ALoc.equal exhaust_locs loc then Flow.flow_t cx (t, exhaustive_t)
      );
      super#switch switch

    method! yield ({ Ast.Expression.Yield.result_out = (_, t); argument; delegate; _ } as yield) =
      let use_op =
        if delegate then
          unknown_use
        else
          Op
            (GeneratorYield
               {
                 value =
                   Base.Option.value_map argument ~default:(reason_of_t t) ~f:(fun expr ->
                       mk_expression_reason (Typed_ast_utils.untyped_ast_mapper#expression expr)
                   );
               }
            )
      in
      Flow.flow cx (t, UseT (use_op, yield_t));
      no_yield <- false;
      super#yield yield

    (* Override statement so that we have the loc for return *)
    method! statement (loc, stmt) =
      begin
        match stmt with
        | Ast.Statement.Return return -> this#custom_return loc return
        | Ast.Statement.Throw _ -> has_throw <- true
        | _ -> ()
      end;
      super#statement (loc, stmt)

    method! call loc expr =
      let open Ast.Expression.Call in
      let { callee; arguments; _ } = expr in
      ( if Flow_ast_utils.is_call_to_invariant callee then
        match arguments with
        (* invariant() and invariant(false, ...) are treated like throw *)
        | (_, { Ast.Expression.ArgList.arguments = []; comments = _ })
        | ( _,
            {
              Ast.Expression.ArgList.arguments =
                Ast.Expression.Expression
                  (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Boolean false; _ })
                :: _;
              comments = _;
            }
          ) ->
          has_throw <- true
        | _ -> ()
      );
      super#call loc expr

    method custom_return loc { Ast.Statement.Return.return_out = (_, t); argument; _ } =
      let open Func_class_sig_types.Func in
      let t =
        match kind with
        | Async ->
          (* Convert the return expression's type T to Promise<T>. If the
             * expression type is itself a Promise<T>, ensure we still return
             * a Promise<T> via Promise.resolve. *)
          let reason = mk_reason (RCustom "async return") loc in
          let t' =
            Flow.get_builtin_typeapp
              cx
              reason
              (OrdinaryName "Promise")
              [
                Tvar.mk_where cx reason (fun tvar ->
                    let funt = Flow.get_builtin cx (OrdinaryName "$await") reason in
                    let callt = mk_functioncalltype reason None [Arg t] (open_tvar tvar) in
                    let reason = repos_reason (aloc_of_reason (reason_of_t t)) reason in
                    Flow.flow
                      cx
                      ( funt,
                        CallT
                          {
                            use_op = unknown_use;
                            reason;
                            call_action = Funcalltype callt;
                            return_hint = Type.hint_unavailable;
                          }
                      )
                );
              ]
          in
          Flow.reposition cx ~desc:(desc_of_t t) loc t'
        | Generator _ ->
          (* Convert the return expression's type R to Generator<Y,R,N>, where
           * Y and R are internals, installed earlier. *)
          let reason = mk_reason (RCustom "generator return") loc in
          let t' =
            Flow.get_builtin_typeapp
              cx
              reason
              (OrdinaryName "Generator")
              [yield_t; Tvar.mk_where cx reason (fun tvar -> Flow.flow_t cx (t, tvar)); next_t]
          in
          Flow.reposition cx ~desc:(desc_of_t t) loc t'
        | AsyncGenerator _ ->
          let reason = mk_reason (RCustom "async generator return") loc in
          let t' =
            Flow.get_builtin_typeapp
              cx
              reason
              (OrdinaryName "AsyncGenerator")
              [yield_t; Tvar.mk_where cx reason (fun tvar -> Flow.flow_t cx (t, tvar)); next_t]
          in
          Flow.reposition cx ~desc:(desc_of_t t) loc t'
        | _ -> t
      in
      let use_op =
        Op
          (FunReturnStatement
             {
               value =
                 Base.Option.value_map argument ~default:(reason_of_t t) ~f:(fun expr ->
                     mk_expression_reason (Typed_ast_utils.untyped_ast_mapper#expression expr)
                 );
             }
          )
      in
      Flow.flow cx (t, UseT (use_op, return_t));
      no_return <- false
  end

module Make
    (Statement : Statement_sig.S)
    (CT : Func_class_sig_types.Config.S)
    (C : Func_params.Config with module Types := CT)
    (F : Func_params.S with module Config_types := CT and module Config := C)
    (T : Func_class_sig_types.Func.S with module Config := CT and module Param := F.Types) :
  S with module Config_types := CT and module Config := C and module Param := F and module Types = T =
struct
  module Types = T
  open Func_class_sig_types.Func

  let this_param = F.this

  let default_constructor reason =
    {
      T.reason;
      kind = Ctor;
      tparams = None;
      tparams_map = Subst_name.Map.empty;
      fparams = F.empty (fun _ _ _ -> None);
      body = None;
      return_t = Annotated (VoidT.why reason |> with_trust bogus_trust);
      statics = None;
    }

  let field_initializer tparams_map reason expr return_annot_or_inferred =
    {
      T.reason;
      kind = FieldInit expr;
      tparams = None;
      tparams_map;
      fparams = F.empty (fun _ _ _ -> None);
      body = None;
      return_t = return_annot_or_inferred;
      statics = None;
    }

  let functiontype
      cx
      ~arrow
      func_loc
      this_default
      ({ T.reason; kind; tparams; fparams; return_t; statics; _ } as x) =
    let make_trust = Context.trust_constructor cx in
    let this_type = F.this fparams |> Base.Option.value ~default:this_default in
    let funtype =
      {
        Type.this_t = (this_type, This_Function);
        params = F.value fparams;
        rest_param = F.rest fparams;
        return_t =
          (match return_t with
          | Inferred t
            when Context.in_synthesis_mode cx && (not @@ F.all_params_annotated x.T.fparams) ->
            Context.mk_placeholder cx (TypeUtil.reason_of_t t)
          | _ -> TypeUtil.type_t_of_annotated_or_inferred return_t);
        is_predicate = kind = Predicate;
        def_reason = reason;
      }
    in
    let statics_t =
      match statics with
      | Some t -> t
      | None -> Obj_type.mk_with_proto cx reason (Type.FunProtoT reason) ~obj_kind:Type.Inexact
    in
    let t = DefT (reason, make_trust (), FunT (statics_t, funtype)) in
    if not arrow then Base.Option.iter func_loc ~f:(Env.bind_function_this cx this_type);
    poly_type_of_tparams (Type.Poly.generate_id ()) tparams t

  let methodtype cx method_this_loc this_default { T.reason; tparams; fparams; return_t; _ } =
    let params = F.value fparams in
    let (params_names, params_tlist) = List.split params in
    let rest_param = F.rest fparams in
    let def_reason = reason in
    let this_anno_t = F.this fparams in
    Base.Option.both this_anno_t method_this_loc
    |> Base.Option.iter ~f:(fun (t, loc) -> Env.bind_function_this cx t loc);
    let param_this_t = Base.Option.value ~default:this_default this_anno_t in
    let t =
      DefT
        ( reason,
          bogus_trust (),
          FunT
            ( dummy_static reason,
              mk_boundfunctiontype
                ~this:param_this_t
                ~subtyping:(This_Method { unbound = false })
                params_tlist
                ~rest_param
                ~def_reason
                ~params_names
                (TypeUtil.type_t_of_annotated_or_inferred return_t)
            )
        )
    in
    poly_type_of_tparams (Type.Poly.generate_id ()) tparams t

  let gettertype ({ T.return_t; _ } : T.t) = TypeUtil.type_t_of_annotated_or_inferred return_t

  let settertype { T.fparams; _ } =
    match F.value fparams with
    | [(_, param_t)] -> param_t
    | _ -> failwith "Setter property with unexpected type"

  let toplevels cx x =
    let { T.reason = reason_fn; kind; tparams_map; fparams; body; return_t; _ } = x in
    let body_loc =
      let open Ast.Function in
      match body with
      | Some (BodyBlock (loc, _)) -> loc
      | Some (BodyExpression (loc, _)) -> loc
      | None -> ALoc.none
    in
    let reason = mk_reason RFunctionBody body_loc in

    (* Set the scope early so default exprs can reference earlier params *)
    let prev_scope_kind =
      let var_scope_kind =
        match kind with
        | Ordinary
        | FieldInit _ ->
          Name_def.Ordinary
        | Predicate -> Name_def.Predicate
        | Async -> Name_def.Async
        | Generator _ -> Name_def.Generator
        | AsyncGenerator _ -> Name_def.AsyncGenerator
        | Ctor -> Name_def.Ctor
      in
      Env.set_scope_kind cx var_scope_kind
    in

    (* bind type params *)
    Subst_name.Map.iter
      (fun name t ->
        let r = reason_of_t t in
        let loc = aloc_of_reason r in
        if Subst_name.string_of_subst_name name <> "this" then
          ()
        else
          Env.bind_this_tparam (DefT (r, bogus_trust (), TypeT (TypeParamKind, t))) loc)
      tparams_map;

    (* add param bindings *)
    let params_ast = F.eval cx fparams in

    let (yield_t, next_t) =
      match kind with
      | Generator _
      | AsyncGenerator _ ->
        let yield_t = Tvar.mk cx (replace_desc_reason (RCustom "yield") reason) in
        let next_t =
          match return_t with
          | Annotated _ -> Tvar.mk cx (replace_desc_reason (RCustom "next") reason)
          | Inferred _ ->
            VoidT.make (replace_desc_reason RUnannotatedNext reason) |> with_trust bogus_trust
        in
        let return_targ = Tvar.mk cx reason in
        let (iterable, generator) =
          match kind with
          | Generator _ -> ("$Iterable", "Generator")
          | AsyncGenerator _ -> ("$AsyncIterable", "AsyncGenerator")
          | _ -> failwith "Bad kind"
        in
        let () =
          let t =
            Flow.get_builtin_typeapp
              cx
              reason
              (OrdinaryName iterable)
              [yield_t; return_targ; next_t]
          in
          let t =
            Flow.reposition
              cx
              ~desc:(desc_of_t t)
              (type_t_of_annotated_or_inferred return_t |> reason_of_t |> aloc_of_reason)
              t
          in
          Flow.flow_t cx (type_t_of_annotated_or_inferred return_t, t)
        in
        let () =
          let t =
            Flow.get_builtin_typeapp
              cx
              reason
              (OrdinaryName generator)
              [yield_t; return_targ; next_t]
          in
          let t =
            Flow.reposition
              cx
              ~desc:(desc_of_t t)
              (type_t_of_annotated_or_inferred return_t |> reason_of_t |> aloc_of_reason)
              t
          in
          Flow.flow_t cx (t, type_t_of_annotated_or_inferred return_t)
        in
        (yield_t, next_t)
      | _ ->
        ( DefT
            ( replace_desc_reason (RCustom "no yield") reason,
              bogus_trust (),
              MixedT Mixed_everything
            ),
          DefT
            (replace_desc_reason (RCustom "no next") reason, bogus_trust (), MixedT Mixed_everything)
        )
    in

    let (statements, reconstruct_body) =
      let open Ast.Statement in
      match body with
      | None -> ([], Fun.const None)
      | Some (Ast.Function.BodyBlock (loc, { Block.body; comments })) ->
        (body, (fun body -> Some (Ast.Function.BodyBlock (loc, { Block.body; comments }))))
      | Some (Ast.Function.BodyExpression expr) ->
        ( [
            ( fst expr,
              Return { Return.argument = Some expr; comments = None; return_out = fst expr }
            );
          ],
          (function
          | [(_, Return { Return.argument = Some expr; comments = _; return_out = _ })]
          | [(_, Expression { Expression.expression = expr; _ })] ->
            Some (Ast.Function.BodyExpression expr)
          | _ -> failwith "expected return body")
        )
    in
    (* NOTE: Predicate functions can currently only be of the form:
       function f(...) { return <exp>; }
    *)
    Ast.Statement.(
      match kind with
      | Predicate -> begin
        match statements with
        | [(_, Return { Return.argument = Some _; comments = _; return_out = _ })] -> ()
        | _ ->
          let loc = aloc_of_reason reason in
          Flow_js.add_output cx Error_message.(EUnsupportedSyntax (loc, PredicateInvalidBody))
      end
      | _ -> ()
    );

    let (has_return_annot, return_t) =
      match (return_t, kind) with
      | (Inferred t, _) -> (false, t)
      | (Annotated t, (Async | Generator _ | AsyncGenerator _)) -> (true, t)
      | (Annotated t, _) -> (true, t)
    in

    (* statement visit pass *)
    let (statements_ast, statements_abnormal) =
      Abnormal.catch_stmts_control_flow_exception (fun () ->
          Toplevels.toplevels Statement.statement cx statements
      )
    in

    let maybe_void =
      Abnormal.(
        match statements_abnormal with
        | Some Return -> false
        | Some Throw -> false (* NOTE *)
        | Some (Break _)
        | Some (Continue _) ->
          failwith "Illegal toplevel abnormal directive"
        | None -> true
      )
    in
    let body_ast = reconstruct_body statements_ast in
    (* build return type for void funcs *)
    let (init_ast, exhaust) =
      if maybe_void then
        let loc = loc_of_t return_t in
        (* Some branches add an ImplicitTypeParam frame to force our flow_use_op
         * algorithm to pick use_ops outside the provided loc. *)
        let (use_op, void_t, init_ast) =
          match kind with
          | Ordinary
          | Ctor ->
            let t = VoidT.at loc |> with_trust bogus_trust in
            let use_op = Op (FunImplicitReturn { fn = reason_fn; upper = reason_of_t return_t }) in
            (use_op, t, None)
          | Async ->
            let reason = mk_annot_reason (RType (OrdinaryName "Promise")) loc in
            let void_t = VoidT.at loc |> with_trust bogus_trust in
            let t = Flow.get_builtin_typeapp cx reason (OrdinaryName "Promise") [void_t] in
            let use_op = Op (FunImplicitReturn { fn = reason_fn; upper = reason_of_t return_t }) in
            let use_op = Frame (ImplicitTypeParam, use_op) in
            (use_op, t, None)
          | Generator _ ->
            let reason = mk_annot_reason (RType (OrdinaryName "Generator")) loc in
            let void_t = VoidT.at loc |> with_trust bogus_trust in
            let t =
              Flow.get_builtin_typeapp
                cx
                reason
                (OrdinaryName "Generator")
                [yield_t; void_t; next_t]
            in
            let use_op = Op (FunImplicitReturn { fn = reason_fn; upper = reason_of_t return_t }) in
            let use_op = Frame (ImplicitTypeParam, use_op) in
            (use_op, t, None)
          | AsyncGenerator _ ->
            let reason = mk_annot_reason (RType (OrdinaryName "AsyncGenerator")) loc in
            let void_t = VoidT.at loc |> with_trust bogus_trust in
            let t =
              Flow.get_builtin_typeapp
                cx
                reason
                (OrdinaryName "AsyncGenerator")
                [yield_t; void_t; next_t]
            in
            let use_op = Op (FunImplicitReturn { fn = reason_fn; upper = reason_of_t return_t }) in
            let use_op = Frame (ImplicitTypeParam, use_op) in
            (use_op, t, None)
          | FieldInit e ->
            let (((_, t), _) as ast) = Statement.expression ?cond:None cx e in
            let body = mk_expression_reason e in
            let use_op = Op (InitField { op = reason_fn; body }) in
            (use_op, t, Some ast)
          | Predicate ->
            let loc = aloc_of_reason reason in
            Flow_js.add_output cx Error_message.(EUnsupportedSyntax (loc, PredicateVoidReturn));
            let t = VoidT.at loc |> with_trust bogus_trust in
            let use_op = Op (FunImplicitReturn { fn = reason_fn; upper = reason_of_t return_t }) in
            (use_op, t, None)
        in

        let exhaust =
          match body with
          | None ->
            Flow.flow cx (void_t, UseT (use_op, return_t));
            None
          | Some _ ->
            let (exhaustive, undeclared) = Context.exhaustive_check cx body_loc in
            Some
              ( Tvar.mk_where
                  cx
                  (replace_desc_reason (RCustom "maybe_exhaustively_checked") reason_fn)
                  (fun t ->
                    if undeclared then Flow.flow_t cx (VoidT.at body_loc (bogus_trust ()), t)
                ),
                exhaustive,
                FunImplicitVoidReturnT
                  { use_op; reason = reason_of_t return_t; return = return_t; void_t }
              )
        in
        (init_ast, exhaust)
      else
        (None, None)
    in

    let () =
      (new func_scope_visitor cx ~has_return_annot ~return_t ~yield_t ~next_t ~body_loc kind exhaust)
        #visit
        statements_ast
    in

    Base.Option.iter exhaust ~f:(fun (maybe_exhaustively_checked, _, implicit_return) ->
        Flow.flow cx (maybe_exhaustively_checked, implicit_return)
    );

    ignore @@ Env.set_scope_kind cx prev_scope_kind;

    (* return a tuple of (function body AST option, field initializer AST option).
       - the function body option is Some _ if the Param sig's body was Some, and
         None if the Param sig's body was None.
       - the field initializer is Some expr' if the Param sig's kind was FieldInit expr,
         where expr' is the typed AST translation of expr.
    *)
    (params_ast, body_ast, init_ast)

  let to_ctor_sig f = { f with T.kind = Ctor }
end

let return_loc = function
  | { Ast.Function.return = Ast.Type.Available (_, (loc, _)); _ }
  | { Ast.Function.body = Ast.Function.BodyExpression (loc, _); _ } ->
    loc
  | { Ast.Function.body = Ast.Function.BodyBlock (loc, _); _ } ->
    loc |> ALoc.to_loc_exn |> Loc.char_before |> ALoc.of_loc
