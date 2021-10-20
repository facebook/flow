(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

(* We use this value to indicate places where values stored in the environment can have
 * an annotation with some more work *)
let annotated_todo t = Inferred t

module Make
    (Env : Env_sig.S)
    (Abnormal : module type of Abnormal.Make (Env)) (F : Func_params.S) =
    struct
  type func_params = F.t

  type func_params_tast = (ALoc.t * Type.t) F.ast

  type t = {
    reason: reason;
    kind: kind;
    tparams: Type.typeparams;
    tparams_map: Type.t SMap.t;
    fparams: func_params;
    body: (ALoc.t, ALoc.t) Ast.Function.body option;
    return_t: Type.annotated_or_inferred;
    (* To be unified with the type of the function. *)
    knot: Type.t;
  }

  let this_param = F.this

  let default_constructor reason =
    {
      reason;
      kind = Ctor;
      tparams = None;
      tparams_map = SMap.empty;
      fparams = F.empty (fun _ _ _ -> None);
      body = None;
      return_t = Annotated (VoidT.why reason |> with_trust bogus_trust);
      (* This can't be directly recursively called. In case this type is accidentally used downstream,
       * stub it out with mixed. *)
      knot = MixedT.why reason |> with_trust bogus_trust;
    }

  let field_initializer tparams_map reason expr return_annot_or_inferred =
    {
      reason;
      kind = FieldInit expr;
      tparams = None;
      tparams_map;
      fparams = F.empty (fun _ _ _ -> None);
      body = None;
      return_t = return_annot_or_inferred;
      (* This can't be recursively called. In case this type is accidentally used downstream, stub it
       * out with mixed. *)
      knot = MixedT.why reason |> with_trust bogus_trust;
    }

  let subst cx map x =
    let { tparams; tparams_map; fparams; return_t; _ } = x in
    (* Remove shadowed type params from `map`, but allow bounds/defaults to be
       substituted if they refer to a type param before it is shadowed. *)
    let tparams =
      tparams
      |> TypeParams.map (fun tp ->
             let bound = Flow.subst cx map tp.bound in
             let default = Base.Option.map ~f:(Flow.subst cx map) tp.default in
             { tp with bound; default }
         )
    in
    let map =
      TypeParams.to_list tparams |> List.fold_left (fun map tp -> SMap.remove tp.name map) map
    in
    let tparams_map = SMap.map (Flow.subst cx map) tparams_map in
    let fparams = F.subst cx map fparams in
    let return_t = TypeUtil.map_annotated_or_inferred (Flow.subst cx map) return_t in
    { x with tparams; tparams_map; fparams; return_t }

  let check_with_generics cx f x =
    let { tparams; tparams_map; fparams; return_t; _ } = x in
    Flow_js_utils.check_with_generics cx (tparams |> TypeParams.to_list) (fun map ->
        f
          {
            x with
            tparams_map = SMap.map (Flow.subst cx map) tparams_map;
            fparams = F.subst cx map fparams;
            return_t = TypeUtil.map_annotated_or_inferred (Flow.subst cx map) return_t;
          }
    )

  let functiontype cx this_default { reason; kind; tparams; fparams; return_t; knot; _ } =
    let make_trust = Context.trust_constructor cx in
    let static =
      let proto = FunProtoT reason in
      Obj_type.mk_unsealed cx reason ~proto
    in
    let prototype =
      let reason = replace_desc_reason RPrototype reason in
      Obj_type.mk_unsealed cx reason
    in
    let funtype =
      {
        Type.this_t = (F.this fparams |> Base.Option.value ~default:this_default, true);
        params = F.value fparams;
        rest_param = F.rest fparams;
        return_t = TypeUtil.type_t_of_annotated_or_inferred return_t;
        is_predicate = kind = Predicate;
        def_reason = reason;
      }
    in
    let t = DefT (reason, make_trust (), FunT (static, prototype, funtype)) in
    let t = poly_type_of_tparams (Context.generate_poly_id cx) tparams t in
    Flow.unify cx t knot;
    t

  let methodtype cx this_default { reason; tparams; fparams; return_t; _ } =
    let params = F.value fparams in
    let (params_names, params_tlist) = List.split params in
    let rest_param = F.rest fparams in
    let def_reason = reason in
    let param_this_t = F.this fparams |> Base.Option.value ~default:this_default in
    let t =
      DefT
        ( reason,
          bogus_trust (),
          FunT
            ( dummy_static reason,
              dummy_prototype,
              mk_boundfunctiontype
                ~this:param_this_t
                ~subtyping:false
                params_tlist
                ~rest_param
                ~def_reason
                ~params_names
                (TypeUtil.type_t_of_annotated_or_inferred return_t)
            )
        )
    in
    poly_type_of_tparams (Context.generate_poly_id cx) tparams t

  let gettertype ({ return_t; _ } : t) = TypeUtil.type_t_of_annotated_or_inferred return_t

  let settertype { fparams; _ } =
    match F.value fparams with
    | [(_, param_t)] -> param_t
    | _ -> failwith "Setter property with unexpected type"

  let toplevels
      id
      cx
      this_recipe
      super
      ~decls
      ~stmts
      ~expr
      { reason = reason_fn; kind; tparams_map; fparams; body; return_t; knot; _ } =
    let loc =
      let open Ast.Function in
      match body with
      | Some (BodyBlock (loc, _)) -> loc
      | Some (BodyExpression (loc, _)) -> loc
      | None -> ALoc.none
    in
    let reason = mk_reason RFunctionBody loc in
    let env = Env.peek_env () in
    let new_env = Env.clone_env env in
    Env.update_env loc new_env;
    Env.havoc_all ();

    (* create and prepopulate function scope *)
    let function_scope =
      let var_scope_kind =
        match kind with
        | Ordinary
        | FieldInit _ ->
          Scope.Ordinary
        | Predicate -> Scope.Predicate
        | Async -> Scope.Async
        | Generator -> Scope.Generator
        | AsyncGenerator -> Scope.AsyncGenerator
        | Ctor -> Scope.Ctor
      in
      Scope.fresh ~var_scope_kind ()
    in
    (* push the scope early so default exprs can reference earlier params *)
    Env.push_var_scope function_scope;

    let (this_t, this) = this_recipe fparams in

    (* add `this` and `super` before looking at parameter bindings as when using
     * `this` in default parameter values it refers to the function scope and
     * `super` should resolve to the method's [[HomeObject]]
     *)
    Scope.add_entry (internal_name "this") this function_scope;
    Scope.add_entry (internal_name "super") super function_scope;

    (* bind type params *)
    SMap.iter
      (fun name t ->
        let r = reason_of_t t in
        let loc = aloc_of_reason r in
        Env.bind_type
          cx
          name
          (DefT (r, bogus_trust (), TypeT (TypeParamKind, t)))
          loc
          ~state:Scope.State.Initialized)
      tparams_map;

    (* add param bindings *)
    let params_ast = F.eval cx fparams in
    (* early-add our own name binding for recursive calls. *)
    Base.Option.iter id ~f:(fun (loc, { Ast.Identifier.name; comments = _ }) ->
        let entry = annotated_todo knot |> Scope.Entry.new_var ~loc ~provider:knot in
        Scope.add_entry (OrdinaryName name) entry function_scope
    );

    let (yield_t, next_t) =
      if kind = Generator || kind = AsyncGenerator then
        ( Tvar.mk cx (replace_desc_reason (RCustom "yield") reason),
          Tvar.mk cx (replace_desc_reason (RCustom "next") reason)
        )
      else
        ( DefT
            ( replace_desc_reason (RCustom "no yield") reason,
              bogus_trust (),
              MixedT Mixed_everything
            ),
          DefT
            (replace_desc_reason (RCustom "no next") reason, bogus_trust (), MixedT Mixed_everything)
        )
    in
    let (yield, next, return) =
      Scope.(
        let new_entry t =
          Entry.(
            let loc = loc_of_t (TypeUtil.type_t_of_annotated_or_inferred t) in
            let state = State.Initialized in
            new_const ~loc ~state t
          )
        in
        (new_entry (Inferred yield_t), new_entry (Inferred next_t), new_entry return_t)
      )
    in
    Scope.add_entry (internal_name "yield") yield function_scope;
    Scope.add_entry (internal_name "next") next function_scope;
    Scope.add_entry (internal_name "return") return function_scope;

    let maybe_exhaustively_checked_t =
      Tvar.mk cx (replace_desc_reason (RCustom "maybe_exhaustively_checked") reason)
    in
    let maybe_exhaustively_checked =
      Scope.Entry.new_let
        ~loc:(loc_of_t maybe_exhaustively_checked_t)
        ~state:Scope.State.Declared
        (Inferred maybe_exhaustively_checked_t)
        ~provider:maybe_exhaustively_checked_t
    in
    Scope.add_entry
      (internal_name "maybe_exhaustively_checked")
      maybe_exhaustively_checked
      function_scope;

    let (statements, reconstruct_body) =
      let open Ast.Statement in
      match body with
      | None -> ([], Fn.const None)
      | Some (Ast.Function.BodyBlock (loc, { Block.body; comments })) ->
        (body, (fun body -> Some (Ast.Function.BodyBlock (loc, { Block.body; comments }))))
      | Some (Ast.Function.BodyExpression expr) ->
        ( [(fst expr, Return { Return.argument = Some expr; comments = None })],
          (function
          | [(_, Return { Return.argument = Some expr; comments = _ })]
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
      | Predicate ->
        begin
          match statements with
          | [(_, Return { Return.argument = Some _; comments = _ })] -> ()
          | _ ->
            let loc = aloc_of_reason reason in
            Flow_js.add_output cx Error_message.(EUnsupportedSyntax (loc, PredicateInvalidBody))
        end
      | _ -> ()
    );

    (* decl/type visit pre-pass *)
    decls cx statements;

    (* statement visit pass *)
    let (statements_ast, statements_abnormal) =
      Abnormal.catch_stmts_control_flow_exception (fun () -> stmts cx statements)
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
    let init_ast =
      let (return_t, return_annot) =
        match return_t with
        | Inferred t -> (t, None)
        | Annotated t -> (t, Some ())
      in
      if maybe_void then (
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
          | Generator ->
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
          | AsyncGenerator ->
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
            let (((_, t), _) as ast) = expr ?cond:None cx ~annot:return_annot e in
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
        Flow.flow
          cx
          ( Env.get_internal_var cx "maybe_exhaustively_checked" loc,
            FunImplicitVoidReturnT
              { use_op; reason = reason_of_t return_t; return = return_t; void_t }
          );
        init_ast
      ) else
        None
    in
    Env.pop_var_scope ();

    Env.update_env loc env;

    (* return a tuple of (function body AST option, field initializer AST option).
       - the function body option is Some _ if the func sig's body was Some, and
         None if the func sig's body was None.
       - the field initializer is Some expr' if the func sig's kind was FieldInit expr,
         where expr' is the typed AST translation of expr.
    *)
    (this_t, params_ast, body_ast, init_ast)

  let to_ctor_sig f = { f with kind = Ctor }
end

let return_loc = function
  | { Ast.Function.return = Ast.Type.Available (_, (loc, _)); _ }
  | { Ast.Function.body = Ast.Function.BodyExpression (loc, _); _ } ->
    loc
  | { Ast.Function.body = Ast.Function.BodyBlock (loc, _); _ } ->
    loc |> ALoc.to_loc_exn |> Loc.char_before |> ALoc.of_loc
