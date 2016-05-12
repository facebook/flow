(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module contains the traversal functions which set up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

module Env = Env_js
module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Cls = Classy
module Flow = Flow_js

open Utils_js
open Reason_js
open Type
open Env.LookupMode

open Destructuring
open Import_export

(*************)
(* Utilities *)
(*************)

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Set.Make(struct
  include Type
  let compare = reasonless_compare
end)

let ident_name (_, ident) =
  ident.Ast.Identifier.name

let summarize cx t = match t with
  | OpenT _ ->
      let reason = reason_of_t t in
      Flow.mk_tvar_where cx reason (fun tvar ->
        Flow.flow cx (t, SummarizeT (reason, tvar))
      )
  (* These remaining cases simulate SummarizeT semantics, and are a slight
     optimization in that they avoid creation of fresh type
     variables. Semantically, we could have the above case fire unconditionally,
     since the fresh type variable is unified in all cases. *)
  | StrT (_, AnyLiteral) -> t
  | StrT (reason, _) -> StrT.why reason
  | NumT (_, AnyLiteral) -> t
  | NumT (reason, _) -> NumT.why reason
  | _ -> t

(* AST helpers *)

let function_desc ~async ~generator desc =
  match async, generator with
  | true, true -> assert_false "async && generator"
  | true, false -> spf "async %s" desc
  | false, true -> spf "generator %s" desc
  | false, false -> desc

(*TJP: Should I be rejecting *object* methods too? *)
let reject_explicit_this cx msg = function
  | {Ast.Function.this = Ast.Type.Function.ThisParam.Explicit (loc, _); _} ->
    FlowError.add_error cx (loc, [msg])
  | _ -> ()

(*
let warn_or_ignore_export_star_as cx name =
  if name = None then () else
  match Context.esproposal_export_star_as cx, name with
  | Options.ESPROPOSAL_WARN, Some(loc, _) ->
    FlowError.add_warning cx (loc, [
      "Experimental `export * as` usage";
      "`export * as` is an active early stage feature proposal that may " ^
        "change. You may opt-in to using it anyway by putting " ^
        "`esproposal.export_star_as=enable` into the [options] section " ^
        "of your .flowconfig"
    ])
  | _ -> ()
*)

(************)
(* Visitors *)
(************)

(********************************************************************
 * local inference preliminary pass: traverse AST, collecting
 * declarations and populating variable environment (scope stack)
 * in prep for main pass
 ********************************************************************)

let rec variable_decl cx type_params_map entry = Ast.Statement.(
  let value_kind, bind = match entry.VariableDeclaration.kind with
    | VariableDeclaration.Const ->
      Scope.Entry.(Const ConstVarBinding), Env.bind_const
    | VariableDeclaration.Let ->
      Scope.(Entry.Let Entry.LetVarBinding), Env.bind_let
    | VariableDeclaration.Var -> Scope.Entry.Var, Env.bind_var
  in

  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in

  let declarator = Ast.(function
    | (loc, Pattern.Identifier (_, { Identifier.name; typeAnnotation; _ })) ->
      let r = mk_reason (spf "%s `%s`" str_of_kind name) loc in
      let t = Anno.mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      bind cx name t r
    | (loc, _) as p ->
      let pattern_name = internal_pattern_name loc in
      let r = mk_reason (spf "%s _" str_of_kind) loc in
      let t = type_of_pattern p |>
        Anno.mk_type_annotation cx type_params_map r in
      bind cx pattern_name t r;
      p |> destructuring cx t None None (fun cx loc name _default t ->
        Hashtbl.replace (Context.type_table cx) loc t;
        bind cx name t r
      )
  ) in

  VariableDeclaration.(entry.declarations |> List.iter (function
    | (_, { Declarator.id; _; }) -> declarator id
  ));
)

and toplevel_decls cx type_params_map =
  List.iter (statement_decl cx type_params_map)

(* TODO: detect structural misuses abnormal control flow constructs *)
and statement_decl cx type_params_map = Ast.Statement.(

  let block_body cx { Block.body } =
    Env.in_lex_scope cx (fun () ->
      toplevel_decls cx type_params_map body
    )
  in

  let catch_clause cx { Try.CatchClause.body = (_, b); _ } =
    block_body cx b
  in

  function

  | (_, Empty) -> ()

  | (_, Block b) ->
      block_body cx b

  | (_, Expression _) -> ()

  | (_, If { If.consequent; alternate; _ }) ->
      statement_decl cx type_params_map consequent;
      (match alternate with
        | None -> ()
        | Some st -> statement_decl cx type_params_map st
      )

  | (_, Labeled { Labeled.body; _ }) ->
      statement_decl cx type_params_map body

  | (_, Break _) -> ()

  | (_, Continue _) -> ()

  | (_, With _) ->
      (* TODO disallow or push vars into env? *)
      ()
  | (_, TypeAlias { TypeAlias.id; _ } ) ->
      let name_loc, { Ast.Identifier.name; _ } = id in
      let r = DescFormat.type_reason name name_loc in
      let tvar = Flow.mk_tvar cx r in
      Env.bind_type cx name tvar r

  | (_, Switch { Switch.cases; _ }) ->
      Env.in_lex_scope cx (fun () ->
        cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
          toplevel_decls cx type_params_map consequent
        )
      )

  | (_, Return _) -> ()

  | (_, Throw _) -> ()

  | (_, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      block_body cx b;

      (match handler with
        | None -> ()
        | Some (_, h) -> catch_clause cx h
      );

      List.iter (fun (_, h) ->
        catch_clause cx h
      ) guardedHandlers;

      (match finalizer with
        | None -> ()
        | Some (_, b) -> block_body cx b
      )

  | (_, While { While.body; _ }) ->
      statement_decl cx type_params_map body

  | (_, DoWhile { DoWhile.body; _ }) ->
      statement_decl cx type_params_map body

  | (_, For { For.init; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match init with
          | Some (For.InitDeclaration (_, decl)) ->
              variable_decl cx type_params_map decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (_, ForIn { ForIn.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForIn.LeftDeclaration (_, decl) ->
              variable_decl cx type_params_map decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (_, ForOf { ForOf.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForOf.LeftDeclaration (_, decl) ->
              variable_decl cx type_params_map decl
          | _ -> ()
        );
        statement_decl cx type_params_map body
      )

  | (_, Let _) ->
      (* TODO *)
      ()

  | (_, Debugger) -> ()

  | (loc, FunctionDeclaration { Ast.Function.id; async; generator; _ }) ->
      (match id with
      | Some (_, {Ast.Identifier.name; _}) ->
        let desc = function_desc ~async ~generator (spf "function %s" name) in
        let r = mk_reason desc loc in
        let tvar = Flow.mk_tvar cx r in
        Env.bind_fun cx name tvar r
      | None ->
        failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
      )

  | (loc, DeclareVariable { DeclareVariable.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = Anno.mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env.bind_declare_var cx name t r

  | (loc, DeclareFunction { DeclareFunction.id; }) ->
      let _, { Ast.Identifier.name; typeAnnotation; _; } = id in
      let r = mk_reason (spf "declare %s" name) loc in
      let t = Anno.mk_type_annotation cx type_params_map r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env.bind_declare_fun cx name t r

  | (_, VariableDeclaration decl) ->
      variable_decl cx type_params_map decl

  | (_, ClassDeclaration { Ast.Class.id; _ }) -> (
      match id with
      | Some id ->
        let name_loc, { Ast.Identifier.name; _ } = id in
        let r = mk_reason (spf "class `%s`" name) name_loc in
        let tvar = Flow.mk_tvar cx r in
        Env.bind_implicit_let Scope.Entry.ClassNameBinding cx name tvar r
      | None -> ()
    )

  | (loc, DeclareClass { Interface.id; _ })
  | (loc, InterfaceDeclaration { Interface.id; _ }) as stmt ->
      let is_interface = match stmt with
      | (_, InterfaceDeclaration _) -> true
      | _ -> false in
      let _, { Ast.Identifier.name; _ } = id in
      let r = mk_reason (spf "class `%s`" name) loc in
      let tvar = Flow.mk_tvar cx r in
      (* interface is a type alias, declare class is a var *)
      if is_interface
      then Env.bind_type cx name tvar r
      else Env.bind_declare_var cx name tvar r

  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, id) ->
        id.Ast.Identifier.name
      | DeclareModule.Literal (_, {
          Ast.Literal.value = Ast.Literal.String str; _;
        }) ->
        str
      | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
      let r = mk_reason (spf "module `%s`" name) loc in
      let t = Flow.mk_tvar cx r in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env.bind_declare_var cx (internal_module_name name) t r

  | _,
    DeclareExportDeclaration {
      DeclareExportDeclaration.default; declaration; _
    } ->
        DeclareExportDeclaration.(match declaration with
        | Some (Variable (loc, v)) ->
            statement_decl cx type_params_map (loc, DeclareVariable v)
        | Some (Function (loc, f)) ->
            statement_decl cx type_params_map (loc, DeclareFunction f)
        | Some (Class (loc, c)) ->
            statement_decl cx type_params_map (loc, DeclareClass c)
        | Some (DefaultType _) -> ()
        | None ->
            if not default
            then ()
            else failwith (
              "Parser Error: declare export default must always have an " ^
              "associated declaration or type!"
            )
        )

  | (_, DeclareModuleExports _) -> ()

  | (_, ExportDeclaration { ExportDeclaration.default; declaration; _ }) -> (
      match declaration with
      | Some(ExportDeclaration.Declaration(stmt)) ->
        let stmt = if default then nameify_default_export_decl stmt else stmt in
        statement_decl cx type_params_map stmt
      | Some(ExportDeclaration.Expression(_)) -> ()
      | None -> if not default then () else failwith (
          "Parser Error: Default exports must always have an associated " ^
          "declaration or expression!"
        )
    )
  | (_, ImportDeclaration import_decl) ->
      let open ImportDeclaration in

      let module_name = (
        let (_, source_literal) = import_decl.source in
        match source_literal.Ast.Literal.value with
        | Ast.Literal.String(value) -> value
        | _ -> failwith  (
            "Parser error: Invalid source type! Must be a string literal."
          )
      ) in

      let (import_str, isType) = (
        match import_decl.importKind with
        | ImportDeclaration.ImportType -> "import type", true
        | ImportDeclaration.ImportTypeof -> "import typeof", true
        | ImportDeclaration.ImportValue -> "import", false
      ) in

      import_decl.specifiers |> List.iter (fun specifier ->
        let (local_name, reason) = (match specifier with
          | ImportNamedSpecifier {local; remote;} ->
            let remote_name = ident_name remote in
            let (local_name, reason) = (
              match local with
              | Some local ->
                let local_name = ident_name local in
                let reason_str =
                  spf "%s { %s as %s }" import_str remote_name local_name
                in
                let loc = Loc.btwn (fst remote) (fst local) in
                (local_name, mk_reason reason_str loc)
              | None ->
                let reason_str = spf "%s { %s }" import_str remote_name in
                (remote_name, mk_reason reason_str (fst remote))
            ) in
            (local_name, reason)
          | ImportDefaultSpecifier local ->
            let local_name = ident_name local in
            let reason_str =
              spf "%s %s from %S" import_str local_name module_name
            in
            let reason = mk_reason reason_str (fst local) in
            (local_name, reason)
          | ImportNamespaceSpecifier (_, local) ->
            let local_name = ident_name local in
            let reason_str =
              spf "%s * as %s from %S" import_str local_name module_name
            in
            let reason = mk_reason reason_str (fst local) in
            (local_name, reason)
        ) in
        let tvar = Flow.mk_tvar cx reason in
        let state = Scope.State.Initialized in
        if isType
        then Env.bind_type ~state cx local_name tvar reason
        else Env.bind_var ~state cx local_name tvar reason
      )
)

(***************************************************************
 * local inference main pass: visit AST statement list, calling
 * flow to check types/create graphs for merge-time checking
 ***************************************************************)

and toplevels cx type_params_map stmts =
  let stmts = List.filter Ast.Statement.(function
    | (_, Empty) -> false
    | _ -> true
  ) stmts
  in
  let n = ref 0 in
  match Abnormal.catch_control_flow_exception (fun () ->
    stmts |> List.iter (fun stmt ->
      statement cx type_params_map stmt;
      incr n (* n is bumped whenever stmt doesn't exit abnormally *)
    )
  ) with
  | Some exn ->
    (* control flow exit out of a flat list:
       check for unreachable code and rethrow *)
    (* !n is the index of the statement that exits abnormally, so !n+1 is the
       index of possibly unreachable code. *)
    let uc = !n+1 in
    if uc < List.length stmts
    then (
      let warn_unreachable loc = FlowError.(
        add_warning cx (loc, ["unreachable code"])
      ) in
      let rec drop n lst = match (n, lst) with
        | (_, []) -> []
        | (0, l) -> l
        | (x, _ :: t) -> drop (pred x) t
      in
      let trailing = drop uc stmts in
      trailing |> List.iter Ast.Statement.(function
        (* function declarations are hoisted, so not unreachable *)
        | (_, FunctionDeclaration _ ) -> ()
        (* variable declarations are hoisted, but associated assignments are not,
           so skip variable declarations with no assignments.
           Note: this does not seem like a practice anyone would use *)
        | (_, VariableDeclaration d) -> VariableDeclaration.(d.declarations |>
            List.iter Declarator.(function
            | (_, { init = Some (loc, _); _ } ) -> warn_unreachable loc
            | _ -> ()
          ))
        | (loc, _) -> warn_unreachable loc
      )
    );
    Abnormal.throw_control_flow_exception exn
  | None -> ()

and statement cx type_params_map = Ast.Statement.(

  let variables cx { VariableDeclaration.declarations; kind } =
    List.iter (variable cx type_params_map kind) declarations
  in

  let classy fn_classtype fn_init cx tparams_map loc i =
    let {Interface.id = (_, {Ast.Identifier.name; _}); _} = i in
    let reason = DescFormat.instance_reason name loc in
    let self = Flow.mk_tvar cx reason in
    let t = fn_classtype cx tparams_map loc reason self i in
    Flow.unify cx self t;
    Hashtbl.replace (Context.type_table cx) loc t;
    fn_init cx name t reason
  in

  let interface cx loc i =
    let iface_t cx tparams_map loc reason self i =
      let iface_sig =
        Cls.Interface.mk_sig cx tparams_map expression loc reason self i in
      iface_sig |> Cls.Interface.generate_tests cx (fun test_iface_sig ->
        Cls.Interface.check_super cx test_iface_sig
      );
      Cls.Interface.classtype cx iface_sig
    in
    let init cx name interface_t reason =
      (* interface is a type alias *)
      Env.init_type cx name interface_t reason
    in
    classy iface_t init cx type_params_map loc i
  in

  let declare_class cx loc i =
    let decl_t cx tparams_map loc reason self i =
      let decl_sig =
        Cls.DeclClass.mk_sig cx tparams_map expression loc reason self i in
      decl_sig |> Cls.DeclClass.generate_tests cx (fun test_decl_sig ->
        Cls.DeclClass.check_super cx test_decl_sig
      );
      Cls.DeclClass.classtype cx decl_sig
    in
    let init cx name declare_class_t reason =
      (* declare class is a var *)
      Env.init_var ~has_anno:false cx name declare_class_t reason
    in
    classy decl_t init cx type_params_map loc i
  in

  let catch_clause cx { Try.CatchClause.param; guard = _; body = (_, b) } =
    Ast.Pattern.(match param with
      | loc, Identifier (_, {
          Ast.Identifier.name; typeAnnotation = None; _
        }) ->
          let r = mk_reason "catch" loc in
          let t = Flow.mk_tvar cx r in

          (match Env.in_lex_scope cx (fun () ->
            Scope.(Env.bind_implicit_let
              ~state:State.Initialized Entry.CatchParamBinding cx name t r);

            Abnormal.catch_control_flow_exception (fun () ->
              toplevel_decls cx type_params_map b.Block.body;
              toplevels cx type_params_map b.Block.body
            )
          ) with
          | Some exn -> Abnormal.throw_control_flow_exception exn
          | None -> ()
          )

      | loc, Identifier _ ->
          let msg = "type annotations for catch params not yet supported" in
          FlowError.add_error cx (loc, [msg])

      | loc, _ ->
          let msg = "unsupported catch parameter declaration" in
          FlowError.add_error cx (loc, [msg])
    )
  in

  function

  | (_, Empty) -> ()

  | (_, Block { Block.body }) ->
      Env.in_lex_scope cx (fun () ->
        toplevel_decls cx type_params_map body;
        toplevels cx type_params_map body
      )

  | (_, Expression { Expression.expression = e }) ->
      ignore (expression cx type_params_map e)

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let reason = mk_reason "if" loc in

      let _, preds, not_preds, xts =
        predicates_of_condition cx type_params_map test in

      (* grab a reference to the incoming env -
         we'll restore it and merge branched envs later *)
      let start_env =  Env.peek_env () in
      let oldset = Changeset.clear () in

      (* swap in a refined clone of initial env for then *)
      Env.(
        update_env cx reason (clone_env start_env);
        ignore (refine_with_preds cx reason preds xts)
      );

      let exception_then = Abnormal.catch_control_flow_exception
        (fun () -> statement cx type_params_map consequent)
      in

      (* grab a reference to env after then branch *)
      let then_env = Env.peek_env () in

      (* then swap in a refined clone of initial env for else *)
      Env.(
        update_env cx reason (clone_env start_env);
        ignore (refine_with_preds cx reason not_preds xts)
      );

      let exception_else = match alternate with
        | None -> None
        | Some st ->
          Abnormal.catch_control_flow_exception
            (fun () -> statement cx type_params_map st)
      in

      (* grab a reference to env after else branch *)
      let else_env = Env.peek_env () in

      (* snapshot if-else changes and merge old changes back into state *)
      let newset = Changeset.merge oldset in

      (* adjust post-if environment. if we've returned from one arm,
         swap in the env generated by the other, otherwise merge *)
      let end_env = match exception_then, exception_else with
      | Some Abnormal.Return, None
      | Some Abnormal.Throw, None ->
        else_env

      | None, Some Abnormal.Return
      | None, Some Abnormal.Throw ->
        then_env

      | _ ->
        Env.merge_env cx reason (start_env, then_env, else_env) newset;
        start_env
      in
      Env.update_env cx reason end_env;

      (* handle control flow in cases where we've thrown from both sides *)
      begin match exception_then, exception_else with
      | Some Abnormal.Throw, Some Abnormal.Return
      | Some Abnormal.Return, Some Abnormal.Throw ->
        Abnormal.throw_control_flow_exception Abnormal.Return;

      | Some then_exn, Some else_exn when then_exn = else_exn ->
        Abnormal.throw_control_flow_exception then_exn

      | _ -> ()
      end

  | (_, Labeled { Labeled.label = _, { Ast.Identifier.name; _ }; body }) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _)
        ->
        let reason = mk_reason "label" loc in
        let oldset = Changeset.clear () in
        let label = Some name in
        let save_break = Abnormal.clear_saved (Abnormal.Break label) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue label) in

        let env = Env.peek_env () in
        Env.widen_env cx reason;

        let loop_env = Env.clone_env env in
        Env.update_env cx reason loop_env;

        Abnormal.(
          check_control_flow_exception (
            ignore_break_or_continue_to_label label (
              fun () -> statement cx type_params_map body)));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue label) save_continue <> None
        then Env.havoc_vars newset;

        Env.copy_env cx reason (env,loop_env) newset;

        if Abnormal.swap_saved (Abnormal.Break label) save_break <> None
        then Env.havoc_vars newset

      | _ ->
        let oldset = Changeset.clear () in
        let label = Some name in
        let save_break = Abnormal.clear_saved (Abnormal.Break label) in

        Abnormal.(
          check_control_flow_exception (
            ignore_break_to_label label (
              fun () -> statement cx type_params_map body)));

        let newset = Changeset.merge oldset in
        if Abnormal.swap_saved (Abnormal.Break label) save_break <> None
        then Env.havoc_vars newset
      )

  | (loc, Break { Break.label }) ->
      (* save environment at unlabeled breaks, prior to activation clearing *)
      let label_opt, env = match label with
        | None -> None, Env.(clone_env (peek_env ()))
        | Some (_, { Ast.Identifier.name; _ }) -> Some name, []
      in
      Env.reset_current_activation (mk_reason "break" loc);
      Abnormal.save_and_throw (Abnormal.Break label_opt) ~env

  | (loc, Continue { Continue.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, { Ast.Identifier.name; _ }) -> Some name
      in
      Env.reset_current_activation (mk_reason "continue" loc);
      Abnormal.save_and_throw (Abnormal.Continue label_opt)

  | (_, With _) ->
      (* TODO or disallow? *)
      ()

  | (loc, TypeAlias { TypeAlias.id; typeParameters; right; } ) ->
      let name_loc, { Ast.Identifier.name; _ } = id in
      let r = DescFormat.type_reason name name_loc in
      let typeparams, type_params_map =
        Anno.mk_type_param_declarations cx type_params_map typeParameters in
      let t = Anno.convert cx type_params_map right in
      let type_ =
        if typeparams = []
        then TypeT (r, t)
        else PolyT(typeparams, TypeT (r, t))
      in
      Hashtbl.replace (Context.type_table cx) loc type_;
      Env.init_type cx name type_ r

  (*******************************************************)

  | (switch_loc, Switch { Switch.discriminant; cases; _ }) ->

    (* add default if absent *)
    let cases = Switch.Case.(
      if List.exists (fun (_, { test; _ }) -> test = None) cases
      then cases
      else cases @ [switch_loc, { test = None; consequent = [] }]
    ) in

    (* typecheck discriminant *)
    ignore (expression cx type_params_map discriminant);

    (* switch body is a single lexical scope *)
    Env.in_lex_scope cx (fun () ->

      (* save incoming env state, clear changeset *)
      let incoming_changes = Changeset.clear () in
      let incoming_env = Env.peek_env () in
      let incoming_depth = List.length incoming_env in

      (* set up all bindings *)
      cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
        toplevel_decls cx type_params_map consequent
      );

      (** each case starts with this env - begins as clone of incoming_env
          plus bindings, also accumulates negative refis from case tests *)
      let case_start_env = Env.clone_env incoming_env in

      (* Some (env, writes, refis, reason) when a case falls through *)
      let fallthrough_case = ref None in

      (* switch_state tracks case effects and is used to create outgoing env *)
      let switch_state = ref None in
      let update_switch_state (case_env, case_writes, _test_refis, reason) =
        let case_env = ListUtils.last_n incoming_depth case_env in
        let state = match !switch_state with
        | None ->
          case_env, Changeset.empty, case_writes
        | Some (env, partial_writes, total_writes) ->
          let case_diff = Changeset.comp case_writes total_writes in
          let partial_writes = Changeset.union partial_writes case_diff in
          let total_writes = Changeset.inter case_writes total_writes in
          (* merge new case into switch env *)
          Env.merge_env cx reason (env, env, case_env) case_writes;
          env, partial_writes, total_writes
        in switch_state := Some state
      in

      (* traverse case list, get list of control flow exits *)
      let exits = cases |> List.map (
        fun (loc, { Switch.Case.test; consequent }) ->

        (* compute predicates implied by case expr or default *)
        let reason, (_, preds, not_preds, xtypes) = match test with
        | None ->
          mk_reason "default" loc,
          Scope.(EmptyT.at loc, KeyMap.empty, KeyMap.empty, KeyMap.empty)
        | Some expr ->
          mk_reason "case" loc,
          let fake_ast = loc, Ast.Expression.(Binary {
            Binary.operator = Binary.StrictEqual;
            left = discriminant; right = expr
          }) in
          predicates_of_condition cx type_params_map fake_ast
        in

        (* swap in case's starting env and clear changeset *)
        let case_env = Env.clone_env case_start_env in
        Env.update_env cx reason case_env;
        let save_changes = Changeset.clear () in

        (* add test refinements - save changelist for later *)
        let test_refis = Env.refine_with_preds cx reason preds xtypes in

        (* merge env changes from fallthrough case, if present *)
        Option.iter !fallthrough_case ~f:(fun (env, writes, refis, _) ->
          let chg = Changeset.union writes refis in
          Env.merge_env cx reason (case_env, case_env, env) chg
        );

        (** process statements, track control flow exits: exit will be an
            unconditional exit, break_opt will be any break *)
        let save_break = Abnormal.clear_saved (Abnormal.Break None) in
        let exit = Abnormal.catch_control_flow_exception (
          fun () -> toplevels cx type_params_map consequent
        ) in
        let break_opt = Abnormal.swap_saved (Abnormal.Break None) save_break in

        (* restore ambient changes and save case writes *)
        let case_writes =
          let case_changes = Changeset.merge save_changes in
          Changeset.include_writes case_changes
        in

        (* track fallthrough to next case and/or break to switch end *)
        let falls_through, breaks_to_end = match exit with
          | Some Abnormal.Throw
          | Some Abnormal.Return
          | Some Abnormal.Break (Some _)
          | Some Abnormal.Continue _ ->
            false, false
          | Some Abnormal.Break None ->
            false, true
          | None ->
            true, Option.is_some break_opt
        in

        (* save state for fallthrough *)
        fallthrough_case := if falls_through
          then Some (case_env, case_writes, test_refis, reason)
          else None;

        (* if we break to end, add effects to terminal state *)
        if breaks_to_end then begin match break_opt with
          | None ->
            let msg = "internal error: break env missing for case" in
            FlowError.add_internal_error cx (loc, [msg])
          | Some break_env ->
            update_switch_state (break_env, case_writes, test_refis, reason)
        end;

        (* add negative refis of this case's test to common start env *)
        (* TODO add API to do this without having to swap in env *)
        Env.update_env cx reason case_start_env;
        let _ = Env.refine_with_preds cx reason not_preds xtypes in

        exit
      ) in

    (* if last case fell out, update terminal switch state with it *)
    Option.iter !fallthrough_case ~f:update_switch_state;

    (** env in switch_state has accumulated switch effects. now merge in
        original types for partially written values, and swap env in *)
    Option.iter !switch_state ~f:(fun (env, partial_writes, _) ->
      let reason = mk_reason "switch" switch_loc in
      Env.merge_env cx reason (env, env, incoming_env) partial_writes;
      Env.update_env cx reason env);

    (* merge original changeset back in *)
    let _ = Changeset.merge incoming_changes in

    (** abnormal exit: if every case exits abnormally the same way (or falls
        through to a case that does), then the switch as a whole exits that way.
       (as with if/else, we merge `throw` into `return` when both appear) *)
    let uniform_switch_exit case_exits =
      let rec loop = function
      | acc, fallthrough, [] ->
        (* end of cases: if nothing is falling through, we made it *)
        if fallthrough then None else acc
      | _, _, Some (Abnormal.Break _) :: _ ->
        (* break wrecks everything *)
        None
      | acc, _, None :: exits ->
        (* begin or continue to fall through *)
        loop (acc, true, exits)
      | acc, _, exit :: exits when exit = acc ->
        (* current case exits the same way as prior cases *)
        loop (acc, acc = None, exits)
      | Some Abnormal.Throw, _, Some Abnormal.Return :: exits
      | Some Abnormal.Return, _, Some Abnormal.Throw :: exits ->
        (* fuzz throw into return *)
        loop (Some Abnormal.Return, false, exits)
      | None, _, exit :: exits ->
        (* terminate an initial sequence of fall-thruugh cases *)
        (* (later sequences will have acc = Some _ ) *)
        loop (exit, false, exits)
      | _, _, _ ->
        (* the new case exits differently from previous ones - fail *)
        None
      in loop (None, false, case_exits)
    in
    begin match uniform_switch_exit exits with
    | None -> ()
    | Some exn -> Abnormal.throw_control_flow_exception exn
    end
  )

  (*******************************************************)

  | (loc, Return { Return.argument }) ->
      let reason = mk_reason "return" loc in
      let ret =
        Env.get_var cx (internal_name "return") reason
      in
      let t = match argument with
        | None -> VoidT.at loc
        | Some expr -> expression cx type_params_map expr
      in
      let t =
        if Env.in_async_scope () then
          (* Convert the return expression's type T to Promise<T>. If the
           * expression type is itself a Promise<T>, ensure we still return
           * a Promise<T> via Promise.resolve. *)
          let reason = mk_reason "async return" loc in
          let promise = Flow.get_builtin cx "Promise" reason in
          Flow.mk_tvar_where cx reason (fun tvar ->
            let call = Flow.mk_methodtype promise [t] tvar in
            Flow.flow cx
              (promise, MethodT (reason, (reason, "resolve"), call))
          )
        else if Env.in_generator_scope () then
          (* Convert the return expression's type R to Generator<Y,R,N>, where
           * Y and R are internals, installed earlier. *)
          let reason = mk_reason "generator return" loc in
          Flow.get_builtin_typeapp cx reason "Generator" [
            Env.get_var cx (internal_name "yield") reason;
            t;
            Env.get_var cx (internal_name "next") reason
          ]
        else t
      in
      Flow.flow cx (t, UseT (FunReturn, ret));
      Env.reset_current_activation reason;
      Abnormal.save_and_throw Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      let reason = mk_reason "throw" loc in
      ignore (expression cx type_params_map argument);
      Env.reset_current_activation reason;
      Abnormal.save_and_throw Abnormal.Throw

  (***************************************************************************)
  (* Try-catch-finally statements have a lot of control flow possibilities. (To
     simplify matters, a missing catch block is considered to to be a catch block
     that throws, and a missing finally block is considered to be an empty block.)

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
  | (loc, Try { Try.block = (_, b); handler; guardedHandlers; finalizer }) ->
      let reason = mk_reason "try" loc in
      let oldset = Changeset.clear () in

      (* save ref to initial env and swap in a clone *)
      let start_env = Env.peek_env () in
      Env.(update_env cx reason (clone_env start_env));

      let exception_try = Env.in_lex_scope cx (fun () ->
        Abnormal.catch_control_flow_exception (fun () ->
          toplevel_decls cx type_params_map b.Block.body;
          toplevels cx type_params_map b.Block.body
        )
      ) in

      (* save ref to env at end of try *)
      let try_env = Env.peek_env () in

      (* traverse catch block, save exceptions *)
      let exception_catch = match handler with
      | None ->
        (* a missing catch is equivalent to a catch that always throws *)
        Some Abnormal.Throw

      | Some (_, h) ->
        (* if try throws to here, we need an env that's conservative
           over everything that happened from start_env to try_env *)
        Env.(
          let e = clone_env start_env in
          merge_env cx reason (e, e, try_env) (Changeset.peek ());
          update_env cx reason e
        );

        Abnormal.catch_control_flow_exception
          (fun () -> catch_clause cx h)
      in

      (* save ref to env at end of catch *)
      let catch_env = Env.peek_env () in

      (* build initial env for non-throwing finally *)
      let nonthrow_finally_env = Env.(match exception_catch with
      | None ->
        (* if catch ends normally, then non-throwing finally can be
           reached via it or a non-throwing try. merge terminal states *)
        let e = clone_env try_env in
        merge_env cx reason (e, e, catch_env) (Changeset.peek ());
        e
      | Some _ ->
        (* if catch throws, then the only way into non-throwing finally
           is via non-throwing try *)
        try_env
      ) in

      assert (guardedHandlers = []); (* remove from AST *)

      (* traverse finally block, save exceptions,
         and leave in place the terminal env of the non-throwing case
         (in which subsequent code is reachable) *)
      let exception_finally = match finalizer with
      | None ->
        Env.update_env cx reason nonthrow_finally_env;
        None

      | Some (_, { Block.body }) ->
        (* analyze twice, with different start states *)

        (* 1. throwing-finally case. *)
        (* env may be in any state from start of try through end of catch *)
        Env.(
          let e = clone_env start_env in
          merge_env cx reason (e, e, catch_env) (Changeset.peek ());
          update_env cx reason e
        );

        let result = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx type_params_map body;
            toplevels cx type_params_map body
          )
        ) in

        (* 2. non-throwing finally case. *)
        Env.update_env cx reason nonthrow_finally_env;

        (* (exceptions will be the same in both cases) *)
        let _ = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx type_params_map body;
            toplevels cx type_params_map body
          )
        ) in

        result
      in

      let newset = Changeset.merge oldset in
      ignore newset;

      (* if finally has abnormal control flow, we throw here *)
      Abnormal.check_control_flow_exception exception_finally;

      (* other ways we throw due to try/catch abends *)
      begin match exception_try, exception_catch with
      | Some (Abnormal.Throw as try_exn), Some Abnormal.Throw
      | Some (Abnormal.Return as try_exn), Some _ ->
          Abnormal.throw_control_flow_exception try_exn

      | Some Abnormal.Throw, Some (Abnormal.Return as catch_exn) ->
          Abnormal.throw_control_flow_exception catch_exn

      | _ -> ()
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
  | (loc, While { While.test; body }) ->

      let reason = mk_reason "while" loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      (* generate loop test preds and their complements *)
      let _, preds, not_preds, orig_types =
        predicates_of_condition cx type_params_map test in

      (* save current changeset and install an empty one *)
      let oldset = Changeset.clear () in

      (* widen_env wraps specifics in tvars, anticipating widening inflows *)
      Env.widen_env cx reason;

      (* start_env is Pre above: env as of loop top *)
      let start_env = Env.peek_env () in

      (* swap in Pre & c *)
      Env.(
        update_env cx reason (clone_env start_env);
        ignore (refine_with_preds cx reason preds orig_types)
      );

      (* traverse loop body - after this, body_env = Post' *)
      ignore (Abnormal.catch_control_flow_exception
        (fun () -> statement cx type_params_map body));

      (* save ref to env after loop body *)
      let body_env = Env.peek_env () in

      (* save loop body changeset to newset, install merged changes *)
      let newset = Changeset.merge oldset in

      (* if we continued out of the loop, havoc vars changed by loop body *)
      if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
      then Env.havoc_vars newset;

      (* widen start_env with new specifics from body_env
         (turning Pre into Pre' = Pre | Post')
         then reinstall and add ~c to make Post *)
      Env.(
        copy_env cx reason (start_env, body_env) newset;
        update_env cx reason start_env;
        ignore (refine_with_preds cx reason not_preds orig_types)
      );

      (* if we broke out of the loop, havoc vars changed by loop body *)
      if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
      then Env.havoc_vars newset

  (***************************************************************************)
  (* Refinements for `do-while` are derived by the following Hoare logic rule:

     [Pre'] S [Post']
     Pre' = Pre | (Post' & c)
     Post = Post' & ~c
     -------------------------
     [Pre] do S while c [Post]
  *)
  (***************************************************************************)
  | (loc, DoWhile { DoWhile.body; test }) ->
      let reason = mk_reason "do-while" loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let env =  Env.peek_env () in
      let oldset = Changeset.clear () in
      (* env = Pre *)
      (* ENV = [env] *)

      Env.widen_env cx reason;
      (* env = Pre', Pre' > Pre *)

      let body_env = Env.clone_env env in
      Env.update_env cx reason body_env;
      (* body_env = Pre' *)
      (* ENV = [body_env] *)

      let exception_ = Abnormal.(
        ignore_break_or_continue_to_label None (
          fun () -> statement cx type_params_map body)
      ) in

      if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
      then Env.havoc_vars (Changeset.peek ());

      let _, preds, not_preds, xtypes =
        predicates_of_condition cx type_params_map test in
      (* body_env = Post' *)

      let done_env = Env.clone_env body_env in
      (* done_env = Post' *)

      let _ = Env.refine_with_preds cx reason preds xtypes in
      (* body_env = Post' & c *)

      let newset = Changeset.merge oldset in
      Env.copy_env cx reason (env, body_env) newset;
      (* Pre' > Post' & c *)

      Env.update_env cx reason done_env;
      let _ = Env.refine_with_preds cx reason not_preds xtypes in
      if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
      then Env.havoc_vars newset;
      (* ENV = [done_env] *)
      (* done_env = Post' & ~c *)

      Abnormal.check_control_flow_exception exception_

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
  | (loc, For { For.init; test; update; body }) ->
      Env.in_lex_scope cx (fun () ->
        let reason = mk_reason "for" loc in
        let save_break = Abnormal.clear_saved (Abnormal.Break None) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
        (match init with
          | None -> ()
          | Some (For.InitDeclaration (_, decl)) ->
              variable_decl cx type_params_map decl;
              variables cx decl
          | Some (For.InitExpression expr) ->
              ignore (expression cx type_params_map expr)
        );

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let do_env = Env.clone_env env in
        Env.update_env cx reason do_env;

        let _, preds, not_preds, xtypes = match test with
          | None ->
              EmptyT.at loc, Scope.KeyMap.empty, Scope.KeyMap.empty,
              Scope.KeyMap.empty (* TODO: prune the "not" case *)
          | Some expr ->
              predicates_of_condition cx type_params_map expr
        in

        let body_env = Env.clone_env do_env in
        Env.update_env cx reason body_env;
        let _ = Env.refine_with_preds cx reason preds xtypes in

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars (Changeset.peek ());

        (match update with
          | None -> ()
          | Some expr ->
              ignore (expression cx type_params_map expr)
        );

        let newset = Changeset.merge oldset in
        Env.copy_env cx reason (env, body_env) newset;

        Env.update_env cx reason do_env;
        let _ = Env.refine_with_preds cx reason not_preds xtypes in
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
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
  | (loc, ForIn { ForIn.left; right; body; _ }) ->
      let reason = mk_reason "for-in" loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let t = expression cx type_params_map right in
      let o = Flow.mk_object cx (mk_reason "iteration expected on object" loc)
      in Flow.flow_t cx (t, MaybeT o); (* null/undefined are allowed *)

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let body_env = Env.clone_env env in
        Env.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx type_params_map right in
        let _ = Env.refine_with_preds cx reason preds xtypes in

        (match left with
          | ForIn.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              variable_decl cx type_params_map decl;
              variable cx type_params_map kind ~if_uninitialized:StrT.at vdecl

          | ForIn.LeftExpression (loc, Ast.Expression.Identifier ident) ->
              let name = ident_name ident in
              let reason = mk_reason (spf "for..in `%s`" name) loc in
              ignore Env.(set_var cx name (StrT.at loc) reason)

          | _ ->
              let msg = "unexpected LHS in for...in" in
              FlowError.add_error cx (loc, [msg])
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx reason (env,body_env) newset;

        Env.update_env cx reason env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (loc, ForOf { ForOf.left; right; body; }) ->
      let reason = mk_reason "for-of" loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let t = expression cx type_params_map right in

      let element_tvar = Flow.mk_tvar cx reason in
      let o = Flow.get_builtin_typeapp
        cx
        (mk_reason "iteration expected on Iterable" loc)
        "$Iterable"
        [element_tvar; AnyT.at loc; AnyT.at loc] in

      Flow.flow_t cx (t, o); (* null/undefined are NOT allowed *)

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let body_env = Env.clone_env env in
        Env.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx type_params_map right in
        let _ = Env.refine_with_preds cx reason preds xtypes in

        (match left with
          | ForOf.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              let repos_tvar loc =
                Flow.reposition cx (repos_reason loc reason) element_tvar
              in
              variable_decl cx type_params_map decl;
              variable cx type_params_map kind ~if_uninitialized:repos_tvar vdecl

          | ForOf.LeftExpression (loc, Ast.Expression.Identifier ident) ->
              let name = ident_name ident in
              let reason = mk_reason (spf "for..of `%s`" name) loc in
              ignore Env.(set_var cx name element_tvar reason)

          | _ ->
              let msg = "unexpected LHS in for...of" in
              FlowError.add_error cx (loc, [msg])
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx type_params_map body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx reason (env,body_env) newset;

        Env.update_env cx reason env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (_, Let _) ->
      (* TODO *)
      ()

  | (_, Debugger) ->
      ()

  | (loc, FunctionDeclaration func) ->
      let {Ast.Function.id; _} = func in
      let reason = mk_reason "function" loc in
      let fn_type = mk_function None cx type_params_map reason func in
      (**
       * Use the loc for the function name in the types table. When the function
       * has no name (i.e. for `export default function() ...`), generate a loc
       * that will span the `function` keyword as a next-best-thing location.
       *)
      let type_table_loc =
        match id with
        | Some (loc, _) -> loc
        | None -> Loc.({
            source = loc.source;
            start = loc.start;
            _end = {
              line = loc.start.line;

              (* len('function') is 8 *)
              column = loc.start.column + 8;
              offset = loc.start.offset + 8;
            };
          })
      in
      Hashtbl.replace (Context.type_table cx) type_table_loc fn_type;
      (match id with
      | Some(_, {Ast.Identifier.name; _ }) ->
        Env.init_fun cx name fn_type reason
      | None -> ())

  | (_, DeclareVariable _)
  | (_, DeclareFunction _) -> ()

  | (_, VariableDeclaration decl) ->
      variables cx decl

  | (class_loc, ClassDeclaration c) ->
      let (name_loc, name) = extract_class_name class_loc c in
      let reason = DescFormat.instance_reason name name_loc in
      Env.declare_implicit_let Scope.Entry.ClassNameBinding cx name reason;
      let class_t = mk_class cx type_params_map class_loc reason c in
      Hashtbl.replace (Context.type_table cx) class_loc class_t;
      Env.init_implicit_let
        Scope.Entry.ClassNameBinding
        cx
        name
        ~has_anno:false
        class_t
        reason

  | (loc, DeclareClass decl) ->
    declare_class cx loc decl

  | (loc, InterfaceDeclaration decl) ->
    interface cx loc decl

  | (loc, DeclareModule { DeclareModule.id; body; }) ->
    let name = match id with
    | DeclareModule.Identifier ident -> ident_name ident
    | DeclareModule.Literal (_, { Ast.Literal.value = Ast.Literal.String str; _; }) ->
        str
    | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
    let _, { Ast.Statement.Block.body = elements } = body in

    let reason = mk_reason (spf "module `%s`" name) loc in
    let t = Env.get_var_declared_type cx (internal_module_name name) reason in

    let module_scope = Scope.fresh () in
    Env.push_var_scope cx module_scope;
    Context.set_in_declare_module cx true;

    toplevel_decls cx type_params_map elements;
    toplevels cx type_params_map elements;

    Context.set_in_declare_module cx false;
    Env.pop_var_scope ();

    (**
     * TODO(jeffmo): `declare var exports` is deprecated (in favor of
     *               `declare module.exports`). v0.25 retains support for it as
     *               a transitionary grace period, but this will be removed as
     *               early as v0.26.
     *)
    let legacy_exports = Scope.get_entry "exports" module_scope in
    let declared_module_exports =
      Scope.get_entry (internal_name "declare_module.exports") module_scope
    in

    let (type_exports, cjs_module_exports) = Scope.(Entry.(
      match (legacy_exports, declared_module_exports) with
      (* TODO: Eventually drop support for legacy "declare var exports" *)
      | (Some (Value {specific=exports; _;}), None)
      | (_, Some (Value {specific=exports; _;})) ->
        let type_exports = SMap.filter (fun _ ->
          function
          | Value _ -> false
          | Type _ -> true
        ) module_scope.entries in
        type_exports, exports

      | (_, Some (Type _)) ->
        assert_false (
          "Internal Error: `declare module.exports` was created as a type " ^
          "binding. This should never happen!"
        )

      | (Some (Type _), None)
      | (None, None) ->
        let type_exports, value_exports = SMap.partition (
          fun _ entry -> match entry with
          | Value _ -> false
          | Type _ -> true
        ) module_scope.entries in

        let map = SMap.map (
          function
          | Value { specific; _ } -> specific
          | Type _ -> assert_false "type entry in value_exports"
        ) value_exports in

        let proto = MixedT (reason, Mixed_everything) in

        type_exports,
        Flow.mk_object_with_map_proto cx reason map proto
    )) in

    let module_t = mk_commonjs_module_t cx reason reason cjs_module_exports in
    let module_t = Flow.mk_tvar_where cx reason (fun t ->
      Flow.flow cx (
        module_t,
        ExportNamedT(
          reason,
          SMap.map Scope.Entry.(
            function
            | Type { _type; _ } -> _type
            | _ -> assert_false "non-type entry in for_types"
          ) type_exports,
          t
        )
      )
    ) in
    Flow.unify cx module_t t;

  | (loc, DeclareExportDeclaration {
      DeclareExportDeclaration.default;
      DeclareExportDeclaration.declaration;
      DeclareExportDeclaration.specifiers;
      DeclareExportDeclaration.source;
    }) ->
      let open DeclareExportDeclaration in
      let export_info = match declaration with
      | Some (Variable (loc, v)) ->
          let { DeclareVariable.id = (_, { Ast.Identifier.name; _; }) } = v in
          statement cx type_params_map (loc, DeclareVariable v);
          [(spf "var %s" name, loc, name, None)]
      | Some (Function (loc, f)) ->
          let { DeclareFunction.id = (_, { Ast.Identifier.name; _; }) } = f in
          statement cx type_params_map (loc, DeclareFunction f);
          [(spf "function %s() {}" name, loc, name, None)]
      | Some (Class (loc, c)) ->
          let { Interface.id = (name_loc, { Ast.Identifier.name; _; }); _; }
            = c in
          statement cx type_params_map (loc, DeclareClass c);
          [(spf "class %s {}" name, name_loc, name, None)]
      | Some (DefaultType (loc, t)) ->
          let _type = Anno.convert cx type_params_map (loc, t) in
          [( "<<type>>", loc, "default", Some _type)]
      | None ->
          [] in

      export_statement cx type_params_map loc
        default export_info specifiers source
        Ast.Statement.ExportDeclaration.ExportValue

  | (loc, DeclareModuleExports annot) ->
    let t = Anno.convert cx SMap.empty (snd annot) in

    if Context.in_declare_module cx then (
      let name = internal_name "declare_module.exports" in
      let reason = mk_reason "declare module.exports" loc in
      Env.bind_declare_var cx name t reason
    ) else (
      let reason = mk_reason "declare module.exports" loc in
      mark_exports_type cx reason (Context.CommonJSModule(Some loc));
      set_module_exports cx reason t
    )

  | (loc, ExportDeclaration {
      ExportDeclaration.default;
      ExportDeclaration.declaration;
      ExportDeclaration.specifiers;
      ExportDeclaration.source;
      ExportDeclaration.exportKind;
    }) ->
      let export_info = match declaration with
      | Some (ExportDeclaration.Declaration decl) ->
          let decl = if default then nameify_default_export_decl decl
            else decl in
          statement cx type_params_map decl;
          (match decl with
          | loc, FunctionDeclaration {Ast.Function.id = None; _} ->
            if default then
              [("function() {}", loc, internal_name "*default*", None)]
            else failwith (
              "Parser Error: Immediate exports of nameless functions can " ^
              "only exist for default exports!"
            )
          | loc, FunctionDeclaration {Ast.Function.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "function %s() {}" name, loc, name, None)]
          | loc, ClassDeclaration {Ast.Class.id = None; _} ->
            if default then
              [("class {}", loc, internal_name "*default*", None)]
            else failwith (
              "Parser Error: Immediate exports of nameless classes can " ^
              "only exist for default exports"
            )
          | _, ClassDeclaration {Ast.Class.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "class %s {}" name, (fst ident), name, None)]
          | _, VariableDeclaration {VariableDeclaration.declarations; _} ->
            let decl_to_bindings accum (_, decl) =
              let id = snd decl.VariableDeclaration.Declarator.id in
              List.rev (extract_destructured_bindings accum id)
            in
            let bound_names = List.fold_left decl_to_bindings [] declarations in
            bound_names |> List.map (fun (loc, name) ->
              (spf "var %s" name, loc, name, None)
            )
          | _, TypeAlias {TypeAlias.id; _} ->
            let name = ident_name id in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration {Interface.id; _} ->
            let name = ident_name id in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | Some (ExportDeclaration.Expression expr) ->
          if not default then failwith (
            "Parser Error: Exporting an expression is only possible for " ^
            "`export default`!"
          );

          let expr_t = expression cx type_params_map expr in
          [( "<<expression>>", fst expr, "default", Some expr_t)]
      | None -> [] in

      export_statement cx type_params_map loc
        default export_info specifiers source exportKind

  | (import_loc, ImportDeclaration import_decl) ->
    let open ImportDeclaration in

    let module_name = (
      match (snd import_decl.source).Ast.Literal.value with
      | Ast.Literal.String value -> value
      | _ -> failwith (
          "Internal Parser Error: Invalid import source type! Must be a string " ^
          "literal."
        )
    ) in

    let (import_str, import_kind) = (
      match import_decl.importKind with
      | ImportDeclaration.ImportType -> "import type", Type.ImportType
      | ImportDeclaration.ImportTypeof -> "import typeof", Type.ImportTypeof
      | ImportDeclaration.ImportValue -> "import", Type.ImportValue
    ) in

    let module_t = import cx module_name (fst import_decl.source) in

    let get_imported_t get_reason remote_export_name local_name =
      Flow.mk_tvar_where cx get_reason (fun t ->
        let import_type =
          if remote_export_name = "default"
          then ImportDefaultT
            (get_reason, import_kind, (local_name, module_name), t)
          else ImportNamedT
            (get_reason, import_kind, remote_export_name, t)
        in
        Flow.flow cx (module_t, import_type)
      )
    in

    import_decl.specifiers |> List.iter (fun specifier ->
      let (reason, local_name, t) = (
        match specifier with
        | ImportNamedSpecifier {local; remote;} ->
          let remote_name = ident_name remote in

          let import_reason_str =
            spf "Named import from module `%s`" module_name
          in

          let (local_name, import_reason, bind_reason) = (
            match local with
            | Some local ->
              let local_name = ident_name local in
              let import_reason = mk_reason import_reason_str (fst remote) in
              let bind_reason_str =
                spf "%s { %s as %s } from %S"
                  import_str
                  remote_name
                  local_name
                  module_name
              in
              let bind_loc = Loc.btwn (fst remote) (fst local) in
              let bind_reason = mk_reason bind_reason_str bind_loc in
              (local_name, import_reason, bind_reason)
            | None ->
              let import_reason = mk_reason import_reason_str (fst remote) in
              let bind_reason_str =
                spf "%s { %s } from %S"
                  import_str
                  remote_name
                  module_name
              in
              let bind_reason = mk_reason bind_reason_str (fst remote) in
              (remote_name, import_reason, bind_reason)
          ) in
          let imported_t =
            get_imported_t import_reason remote_name local_name
          in
          (bind_reason, local_name, imported_t)

        | ImportDefaultSpecifier local ->
          let local_name = ident_name local in

          let import_reason_str =
            spf "Default import from `%s`" module_name
          in
          let import_reason = mk_reason import_reason_str (fst local) in

          let bind_reason_str =
            spf "%s %s from %S" import_str local_name module_name
          in
          let bind_reason = mk_reason bind_reason_str (fst local) in

          let imported_t =
            get_imported_t import_reason "default" local_name
          in
          (bind_reason, local_name, imported_t)

        | ImportNamespaceSpecifier (_, local) ->
          let local_name = ident_name local in

          let import_reason_str = spf "%s * as %s" import_str local_name in
          let import_reason = mk_reason import_reason_str import_loc in

          (match import_kind with
            | Type.ImportType ->
              let msg =
                spf
                  ("This is invalid syntax. Maybe you meant: `import type " ^^
                   "%s from %S`?")
                  local_name
                  module_name
              in
              FlowError.(add_error cx (mk_info import_reason [msg]));
              (import_reason, local_name, AnyT.why import_reason)
            | Type.ImportTypeof ->
              let bind_reason = repos_reason (fst local) import_reason in
              let module_ns_t =
                import_ns cx import_reason module_name (fst import_decl.source)
              in
              let module_ns_typeof =
                Flow.mk_tvar_where cx bind_reason (fun t ->
                  Flow.flow cx (module_ns_t,
                    ImportTypeofT (bind_reason, "*", t))
                )
              in
              (import_reason, local_name, module_ns_typeof)
            | Type.ImportValue ->
              let reason =
                mk_reason (spf "exports of %S" module_name) import_loc
              in
              let module_ns_t =
                import_ns cx reason module_name (fst import_decl.source)
              in
              let bind_reason = mk_reason import_reason_str (fst local) in
              (bind_reason, local_name, module_ns_t)
          )
      ) in

      let t_generic =
        let lookup_mode =
          match import_kind with
          | Type.ImportType | Type.ImportTypeof -> ForType
          | Type.ImportValue -> ForValue
        in
        Env.get_var_declared_type ~lookup_mode cx local_name reason
      in
      Flow.unify cx t t_generic
    );
)


and export_statement cx _type_params_map loc
  default declaration_export_info specifiers source exportKind =

  let open Ast.Statement in
  let open ExportDeclaration in
  let (lookup_mode, export_kind_start) = (
    match exportKind with
    | ExportValue -> (ForValue, "export")
    | ExportType -> (ForType, "export type")
  ) in

  let export_reason_start = spf "%s%s" export_kind_start (
    if default then " default" else ""
  ) in

  let export_from_local (export_reason, loc, local_name, local_tvar) = (
    let reason =
      mk_reason (spf "%s %s" export_reason_start export_reason) loc
    in
    let local_tvar = match local_tvar with
    | None -> Env.var_ref ~lookup_mode cx local_name reason
    | Some t -> t in

    (**
      * NOTE: We do not use type-only exports as an indicator of an
      *       ES module in order to allow CommonJS modules to export types.
      *
      *       Note that this means that modules that consist only of
      *       type-only exports will be internally considered a CommonJS
      *       module, but this should have minimal observable effect to the
      *       user given CommonJS<->ESModule interop.
      *)
    (if lookup_mode != ForType then
      mark_exports_type cx reason Context.ESModule);

    let local_name = if default then "default" else local_name in
    set_module_t cx reason (fun t ->
      Flow.flow cx (
        exports cx,
        ExportNamedT(reason, SMap.singleton local_name local_tvar, t)
      )
    )
  ) in

  (match (declaration_export_info, specifiers) with
    (* [declare] export [type] {foo, bar} [from ...]; *)
    | ([], Some(ExportSpecifiers(specifiers))) ->
      let export_specifier specifier = (
        let (reason, local_name, remote_name) = (
          match specifier with
          | loc, {
              id = (_, {Ast.Identifier.name=id; _;});
              Specifier.name=None;
            } ->
            let reason = mk_reason (spf "export {%s}" id) loc in
            (reason, id, id)
          | loc, { Specifier.
              id=(_, {Ast.Identifier.name=id; _;});
              name=Some(_, {Ast.Identifier.name; _;})
            } ->
            let reason =
              mk_reason (spf "export {%s as %s}" id name) loc
            in
            (reason, id, name)
        ) in

        (**
          * Determine if we're dealing with the `export {} from` form
          * (and if so, retrieve the ModuleNamespaceObject tvar for the
          *  source module)
          *)
        let source_module_tvar = (
          match source with
          | Some(src_loc, {
              Ast.Literal.value = Ast.Literal.String(module_name);
              _;
            }) ->
              let reason =
                mk_reason "ModuleNamespace for export {} from" src_loc
              in
              Some(import_ns cx reason module_name src_loc)
          | Some(_) -> failwith (
              "Parser Error: `export ... from` must specify a string " ^
              "literal for the source module name!"
            )
          | None -> None
        ) in

        let local_tvar = (
          match source_module_tvar with
          | Some(tvar) ->
            Flow.mk_tvar_where cx reason (fun t ->
              Flow.flow cx (tvar, GetPropT(reason, (reason, local_name), t))
            )
          | None ->
            Env.var_ref ~lookup_mode cx local_name reason
        ) in

        (**
          * NOTE: We do not use type-only exports as an indicator of an
          *       ES module in order to allow CommonJS modules to export
          *       types.
          *
          *       Note that this means that modules that consist only of
          *       type-only exports will be internally considered a
          *       CommonJS module, but this should have minimal observable
          *       effect to the user given CommonJS<->ESModule interop.
          *)
        (if lookup_mode != ForType
        then mark_exports_type cx reason Context.ESModule);

        set_module_t cx reason (fun t ->
          Flow.flow cx (
            exports cx,
            ExportNamedT(reason, SMap.singleton remote_name local_tvar, t)
          )
        )
      ) in
      List.iter export_specifier specifiers

    (* [declare] export [type] * from "source"; *)
    | ([], Some(ExportBatchSpecifier(batch_loc, star_as_name))) ->
      let source_module_name = (
        match source with
        | Some(_, {
            Ast.Literal.value = Ast.Literal.String(module_name);
            _;
          }) -> module_name
        | _ -> failwith (
          "Parser Error: `export * from` must specify a string " ^
          "literal for the source module name!"
        )
      ) in

      warn_or_ignore_export_star_as cx star_as_name;

      let parse_export_star_as = Context.esproposal_export_star_as cx in
      (match star_as_name with
      | Some ident ->
        let (_, {Ast.Identifier.name; _;}) = ident in
        let reason =
          mk_reason
            (spf "export * as %s from %S" name source_module_name)
            loc
        in
        mark_exports_type cx reason Context.ESModule;

        let remote_namespace_t =
          if parse_export_star_as = Options.ESPROPOSAL_ENABLE
          then import_ns cx reason source_module_name batch_loc
          else AnyT.why (
            let config_value =
              if parse_export_star_as = Options.ESPROPOSAL_IGNORE
              then "ignore"
              else "warn"
            in
            mk_reason
              (spf "flowconfig: esproposal.export_star_as=%s" config_value)
              batch_loc
          )
        in
        set_module_t cx reason (fun t ->
          Flow.flow cx (
            exports cx,
            ExportNamedT(reason, SMap.singleton name remote_namespace_t, t)
          )
        )
      | None ->
        let reason =
          mk_reason (spf "export * from %S" source_module_name) loc
        in
        mark_exports_type cx reason Context.ESModule;

        set_module_t cx reason (fun t ->
          Flow.flow cx (
            import ~reason cx source_module_name loc,
            ExportStarFromT(reason, exports cx, t)
          )
        )
      )

    | ([], None) -> failwith (
        "Parser Error: Export statement missing one of: Declaration, " ^
        "Expression, or Specifier list!"
      )
    | (_, Some _) -> failwith (
        "Parser Error: Export statement with a declaration/expression " ^
        "cannot also include a list of specifiers!"
      )

    (* [declare] export [type] [default] <<declaration>>; *)
    | (export_info, None) ->
      (**
        * Export each declared binding. Some declarations export multiple
        * bindings, like a multi-declarator variable declaration.
        *)
      List.iter export_from_local export_info
  )

and object_prop cx type_params_map map = Ast.Expression.Object.(function
  (* name = function expr *)
  | Property (_, { Property.kind = Property.Init;
                     key = Property.Identifier (_, {
                       Ast.Identifier.name; _ });
                     value = (vloc, Ast.Expression.Function func);
                     _ }) ->
      let reason = mk_reason "function" vloc in
      let {Ast.Function.id; _} = func in
      let ft = mk_function id cx type_params_map reason func in
      Hashtbl.replace (Context.type_table cx) vloc ft;
      SMap.add name ft map

  (* name = non-function expr *)
  | Property (_, { Property.kind = Property.Init;
      key =
        Property.Identifier (_, { Ast.Identifier.name; _ }) |
        Property.Literal (_, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
                   value = v;
                   _ }) ->
    let t = expression cx type_params_map v in
    SMap.add name t map

  (* literal LHS *)
  | Property (loc, { Property.key = Property.Literal _; _ }) ->
    let msg = "non-string literal property keys not supported" in
    FlowError.add_error cx (loc, [msg]);
    map


  (* With the enable_unsafe_getters_and_setters option set, we enable some
   * unsafe support for getters and setters. The main unsafe bit is that we
   * don't properly havok refinements when getter and setter methods are called.
   * When used in objects, they're a little strange. Technically the getter's
   * return type and the setter's param type don't have to be the same.
   *
   * To properly model this, we should keep track of which properties have
   * getters and setters. However, for now we'll be a little overly strict
   * and just enforce that any property that has had a getter and a setter
   * should just let the setter's param type flow to the getter's return type.
   *)

  (* unsafe getter property *)
  | Property (_, {
      Property.kind = Property.Get;
      key = Property.Identifier (_, { Ast.Identifier.name; _ });
      value = (vloc, Ast.Expression.Function func);
      _ })
    when Context.enable_unsafe_getters_and_setters cx ->
      let reason = mk_reason "getter function" vloc in
      let function_type = mk_function None cx type_params_map reason func in
      let return_t = extract_getter_type function_type in
      let map, prop_t = (match SMap.get name map with
      | Some prop_t -> map, prop_t
      | _ ->
        let prop_t =
          Flow.mk_tvar cx (mk_reason "getter/setter property" vloc) in
        SMap.add name prop_t map, prop_t) in
      Flow.unify cx prop_t return_t;
      map

  (* unsafe setter property *)
  | Property (_, {
    Property.kind = Property.Set;
      key = Property.Identifier (_, { Ast.Identifier.name; _ });
      value = (vloc, Ast.Expression.Function func);
      _ })
    when Context.enable_unsafe_getters_and_setters cx ->
      let reason = mk_reason "setter function" vloc in
      let function_type = mk_function None cx type_params_map reason func in
      let param_t = extract_setter_type function_type in
      let map, prop_t = (match SMap.get name map with
      | Some prop_t -> map, prop_t
      | _ ->
        let prop_t =
          Flow.mk_tvar cx (mk_reason "getter/setter property" vloc) in
        SMap.add name prop_t map, prop_t) in
      Flow.unify cx prop_t param_t;
      map

  | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
    let msg = "get/set properties not yet supported" in
    FlowError.add_error cx (loc, [msg]);
    map

  (* computed LHS *)
  | Property (_, { Property.key = Property.Computed _; _ }) ->
    map

  (* spread prop *)
  | SpreadProperty _ ->
    map
)

and prop_map_of_object cx type_params_map props =
  List.fold_left (object_prop cx type_params_map) SMap.empty props

and object_ cx type_params_map reason ?(allow_sealed=true) props =
  Ast.Expression.Object.(
  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let proto = MixedT (reason, Mixed_everything) in
  (* Return an object with specified sealing. *)
  let mk_object ?(sealed=false) map =
    Flow.mk_object_with_map_proto cx reason ~sealed map proto
  in
  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj =
    Flow.mk_tvar_where cx reason (fun t ->
      Flow.flow cx (to_obj, ObjAssignT(reason, from_obj, t, [], true));
    )
  in
  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_object ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object map) result
        else result
      in
      if not sealed then result else
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (result, ObjSealT (reason, t))
        )
  in

  let sealed, map, result = List.fold_left (fun (sealed, map, result) t ->
    match t with
    | SpreadProperty (_, { SpreadProperty.argument }) ->
        let spread = expression cx type_params_map argument in
        let obj = eval_object (map, result) in
        let result = mk_spread spread obj in
        false, SMap.empty, Some result
    | Property (_, { Property.key = Property.Computed k; value = v; _ }) ->
        let k = expression cx type_params_map k in
        let v = expression cx type_params_map v in
        let obj = eval_object (map, result) in
        Flow.flow cx (obj, SetElemT (reason, k, v));
        (* TODO: vulnerable to race conditions? *)
        let result = obj in
        sealed, SMap.empty, Some result
    | t ->
        sealed, object_prop cx type_params_map map t, result
  ) (allow_sealed, SMap.empty, None) props in

  let sealed = match result with
    | Some _ -> sealed
    | None -> sealed && not (SMap.is_empty map)
  in
  eval_object ~sealed (map, result)
)

and variable cx type_params_map kind
  ?if_uninitialized (_, vdecl) = Ast.Statement.(
  let value_kind, init_var, declare_var = Env.(match kind with
    | VariableDeclaration.Const ->
      Scope.Entry.(Const ConstVarBinding), init_const, declare_const
    | VariableDeclaration.Let ->
      Scope.(Entry.Let Entry.LetVarBinding), init_let, declare_let
    | VariableDeclaration.Var ->
      Scope.Entry.Var, init_var, (fun _ _ _ -> ())
  ) in
  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in
  let { VariableDeclaration.Declarator.id; init } = vdecl in
  match id with
    | (loc, Ast.Pattern.Identifier (_, { Ast.Identifier.
          name; typeAnnotation; optional
        })) ->
        (* simple lvalue *)
        let reason = mk_reason (spf "%s %s" str_of_kind name) loc in
        let has_anno = not (typeAnnotation = None) in
        (match init with
          | Some ((rhs_loc, _) as expr) ->
            declare_var cx name reason; (* prepare for self-refs *)
            let rhs_reason = mk_reason
              (spf "assignment of var `%s`" name) rhs_loc in
            let rhs = expression cx type_params_map expr in
            let hook_loc = Ast.Expression.(
              match expr with
              (**
               * It's common to do `var Foo = require('Bar').Foo`. In these
               * cases, we should point to the property of the member expression
               * during `get-def`. This lets us hop *in* to the property on the
               * module.exports object returned by require() rather than hopping
               * directly to the `module.exports` object itself.
               *)
              | (_, Member {Member.property; _;}) -> (
                  match property with
                  | Member.PropertyIdentifier (loc, _) -> loc
                  | Member.PropertyExpression (loc, _) -> loc
                )
              | (loc, _) -> loc
            ) in
            Type_inference_hooks_js.(
              dispatch_lval_hook cx name loc (RHSLoc hook_loc));
            let rhs = Flow.reposition cx rhs_reason rhs in
            init_var cx name ~has_anno rhs reason
          | None ->
            Type_inference_hooks_js.(
              dispatch_lval_hook cx name loc NoRHS);
            match if_uninitialized with
            | Some f ->
              if not optional
              then init_var cx name ~has_anno (f loc) reason
            | None ->
              if has_anno
              then Env.pseudo_init_declared_type cx name reason
              else declare_var cx name reason;
        )
    | loc, _ ->
        (* compound lvalue *)
        let pattern_name = internal_pattern_name loc in
        let reason = mk_reason (spf "%s _" str_of_kind) loc in
        let typeAnnotation = type_of_pattern id in
        let has_anno = not (typeAnnotation = None) in
        let t = match init with
          | Some expr -> expression cx type_params_map expr
          | None -> (
            match if_uninitialized with
            | Some f -> f loc
            | None -> VoidT.at loc
          )
        in
        init_var cx pattern_name ~has_anno t reason;
        destructuring cx t init None (fun cx loc name default t ->
          let reason = mk_reason (spf "%s %s" str_of_kind name) loc in
          Option.iter default (fun default ->
            let default_t = Flow.mk_default cx reason default
              ~expr:(expression cx type_params_map)
            in
            Flow.flow_t cx (default_t, t)
          );
          init_var cx name ~has_anno t reason
        ) id
)

and array_element cx type_params_map undef_loc el = Ast.Expression.(
  match el with
  | Some (Expression e) -> expression cx type_params_map e
  | Some (Spread (_, { SpreadElement.argument })) ->
      array_element_spread cx type_params_map argument
  | None -> EmptyT.at undef_loc
)

and expression_or_spread cx type_params_map = Ast.Expression.(function
  | Expression e -> expression cx type_params_map e
  | Spread (_, { SpreadElement.argument }) -> spread cx type_params_map argument
)

and array_element_spread cx type_params_map (loc, e) =
  let arr = expression cx type_params_map (loc, e) in
  let reason = mk_reason "spread operand" loc in
  Flow.mk_tvar_where cx reason (fun tvar ->
    Flow.flow_t cx (arr, ArrT (reason, tvar, []));
  )

and spread cx type_params_map (loc, e) =
  RestT (array_element_spread cx type_params_map (loc, e))

(* NOTE: the is_cond flag is only used when checking the type of conditions in
   `predicates_of_condition`: see comments on function `condition`. *)
and expression ?(is_cond=false) cx type_params_map (loc, e) =
  let t = expression_ ~is_cond cx type_params_map loc e in
  Hashtbl.replace (Context.type_table cx) loc t;
  t

and this_ cx r = Ast.Expression.(
  match Refinement.get cx (loc_of_reason r, This) r with
  | Some t -> t
  | None -> Env.get_var cx (internal_name "this") r
)

and super_ cx reason =
  Env.get_var cx (internal_name "super") reason

and expression_ ~is_cond cx type_params_map loc e = Ast.Expression.(match e with

  | Ast.Expression.Literal lit ->
      literal cx loc lit

  | Identifier (_, { Ast.Identifier.name; _ }) -> identifier cx name loc

  | This ->
      this_ cx (mk_reason "this" loc)

  | Unary u ->
      unary cx type_params_map loc u

  | Update u ->
      update cx type_params_map loc u

  | Binary b ->
      binary cx type_params_map loc b

  | Logical l ->
      logical cx type_params_map loc l

  | TypeCast {
        TypeCast.expression = e;
        typeAnnotation } ->
      let r = mk_reason "typecast" loc in
      let t = Anno.mk_type_annotation cx type_params_map r (Some typeAnnotation)
      in Hashtbl.replace (Context.type_table cx) loc t;
      let infer_t = expression cx type_params_map e in
      Flow.flow_t cx (infer_t, t);
      t

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      _
    } ->
      let reason = mk_reason "access of computed property/element" loc in
      (match Refinement.get cx (loc, e) reason with
      | Some t -> t
      | None ->
        let tobj = expression cx type_params_map _object in
        let tind = expression cx type_params_map index in
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (tobj, GetElemT(reason, tind, t))
        )
      )

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "module"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "exports"; _ });
      _
    } ->
      let reason = mk_reason "module.exports" loc in
      get_module_exports cx reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "ReactGraphQL" | "ReactGraphQLLegacy"; _ });
      property = Member.PropertyIdentifier (_,
        { Ast.Identifier.name = "Mixin"; _ });
      _
    } ->
      let reason = mk_reason "ReactGraphQLMixin" loc in
      Flow.get_builtin cx "ReactGraphQLMixin" reason

  | Member {
      Member._object = _, Identifier (_,
        { Ast.Identifier.name = "super"; _ });
      property = Member.PropertyIdentifier (ploc,
        { Ast.Identifier.name; _ });
      _
    } ->
      let expr_reason = mk_reason (spf "property `%s`" name) loc in
      (match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (spf "property `%s`" name) ploc in

        (* TODO: shouldn't this be `mk_reason "super" super_loc`? *)
        let super = super_ cx expr_reason in

        if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
        then AnyT.at ploc
        else (
          Flow.mk_tvar_where cx expr_reason (fun tvar ->
            Flow.flow cx (
              super, GetPropT(expr_reason, (prop_reason, name), tvar)
            )
          )
        )
      )

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, { Ast.Identifier.name; _ });
      _
    } -> (
      let expr_reason = mk_reason (spf "property `%s`" name) loc in
      match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (spf "property `%s`" name) ploc in
        let tobj = expression cx type_params_map _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else get_prop ~is_cond cx expr_reason tobj (prop_reason, name)
    )

  | Object { Object.properties } ->
    let reason = mk_reason "object literal" loc in
    object_ cx type_params_map reason properties

  | Array { Array.elements } -> (
    let reason = mk_reason "array literal" loc in
    let element_reason = mk_reason "array element" loc in
    match elements with
    | [] ->
        (* empty array, analogous to object with implicit properties *)
        let elemt = Flow.mk_tvar cx element_reason in
        ArrT (prefix_reason "empty " reason, elemt, [])
    | elems ->
        (* tup is true if no spreads *)
        (* tset is set of distinct (mod reason) elem types *)
        (* tlist is reverse list of element types if tup, else [] *)
        let _, tset, tlist = List.fold_left (fun (tup, tset, tlist) elem ->
          let elemt = array_element cx type_params_map loc elem in

          let tup = match elem with Some (Spread _) -> false | _ -> tup in
          let elemt = if tup then elemt else summarize cx elemt in
          tup,
          TypeExSet.add elemt tset,
          if tup then elemt :: tlist else []
        ) (true, TypeExSet.empty, []) elems
        in
        (* composite elem type is union *)
        let elemt = match TypeExSet.elements tset with
        | [t] -> t
        | list -> UnionT (element_reason, UnionRep.make list)
        in
        ArrT (reason, elemt, List.rev tlist)
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "require";
        _
      });
      arguments
    } when not (Env.local_scope_entry_exists "require") -> (
      match arguments with
      | [ Expression (_, Ast.Expression.Literal {
          Ast.Literal.value = Ast.Literal.String module_name; _;
        }) ] ->
        require cx module_name loc
      | _ ->
        let ignore_non_literals =
          Context.should_ignore_non_literal_requires cx in
        if not ignore_non_literals then (
          let msg =
            "The parameter passed to require() must be a literal string."
          in
          FlowError.add_error cx (loc, [msg]);
        );
        AnyT.at loc
    )

  | Call {
      Call.callee = _, Identifier (_, {
        Ast.Identifier.name = "requireLazy";
        _
      });
      arguments
    } -> (
      match arguments with
      | [Expression(_, Array({Array.elements;})); Expression(callback_expr);] ->
        (**
         * From a static perspective (and as long as side-effects aren't
         * considered in Flow), a requireLazy call can be viewed as an immediate
         * call to require() for each of the modules, and then an immediate call
         * to the requireLazy() callback with the results of each of the prior
         * calls to require().
         *
         * TODO: requireLazy() is FB-specific. Let's find a way to either
         *       generalize or toggle this only for the FB environment.
         *)

        let element_to_module_tvar tvars = (function
          | Some(Expression(_, Ast.Expression.Literal({
              Ast.Literal.value = Ast.Literal.String module_name;
              _;
            }))) ->
              let module_tvar = require cx module_name loc in
              module_tvar::tvars
          | _ ->
              let msg =
                "The first arg to requireLazy() must be a literal array of " ^
                "string literals!"
              in
              FlowError.add_error cx (loc, [msg]);
              tvars
        ) in
        let module_tvars = List.fold_left element_to_module_tvar [] elements in
        let module_tvars = List.rev module_tvars in

        let callback_expr_t = expression cx type_params_map callback_expr in
        let reason = mk_reason "requireLazy() callback" loc in
        let _ = func_call cx reason callback_expr_t module_tvars in

        NullT.at loc

      | _ ->
        let msg =
          "The first arg to requireLazy() must be a literal array of " ^
          "string literals!"
        in
        FlowError.add_error cx (loc, [msg]);

        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Function"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      List.iter (fun t ->
        Flow.flow_t cx (t, StrT.at loc)
      ) argts;
      let reason = mk_reason "new Function(..)" loc in
      let proto = MixedT (reason, Mixed_everything) in
      FunT (
        reason,
        Flow.dummy_static reason,
        Flow.dummy_prototype,
        Flow.mk_functiontype [] ~params_names:[] proto
      )
    )

  | New {
      New.callee = _, Identifier (_, { Ast.Identifier.name = "Array"; _ });
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      (match argts with
      | [argt] ->
        let reason = mk_reason "new Array(..)" loc in
        Flow.flow_t cx
          (argt, NumT (replace_reason "array length" reason, AnyLiteral));
        let element_reason = replace_reason "array element" reason in
        let t = Flow.mk_tvar cx element_reason in
        ArrT (reason, t, [])
      | _ ->
        let msg = "Use array literal instead of new Array(..)" in
        FlowError.add_error cx (loc, [msg]);
        EmptyT.at loc
      )
    )

  | New { New.callee; arguments } ->
      let class_ = expression cx type_params_map callee in
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      new_call cx loc class_ argts

  | Call {
      Call.callee = (_, Member {
        Member._object = (_, Identifier (_,
          { Ast.Identifier.name = "Object"; _ }) as obj);
        property = Member.PropertyIdentifier (prop_loc,
          { Ast.Identifier.name; _ });
        _
      } as expr);
      arguments
    } ->
      let obj_t = expression cx type_params_map obj in
      static_method_call_Object
        cx type_params_map loc prop_loc expr obj_t name arguments

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Ast.Identifier.name = "React"; _ });
        property = Member.PropertyIdentifier (_,
          { Ast.Identifier.name = "createClass"; _ });
        _
      };
      arguments = [ Expression (_, Object { Object.properties = class_props }) ]
    } ->
      react_create_class cx type_params_map loc class_props

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (super_loc,
          { Ast.Identifier.name = "super"; _ });
        property = Member.PropertyIdentifier (ploc,
          { Ast.Identifier.name; _ });
        _
      };
      arguments
    } ->
      let reason = mk_reason (spf "super.%s(...)" name) loc in
      let reason_prop = mk_reason (spf "property `%s`" name) ploc in
      let super = super_ cx (mk_reason "super" super_loc) in
      let this = this_ cx (mk_reason "this" super_loc) in
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
      Flow.mk_tvar_where cx reason (fun t ->
        let funtype = Flow.mk_methodtype this argts t in
        Flow.flow cx (super, MethodT (reason, (reason_prop, name), funtype))
      )

  | Call {
      Call.callee = (_, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc,
          { Ast.Identifier.name; _ });
        _
      }) as callee;
      arguments
    } ->
      (* method call *)
      let ot = expression cx type_params_map _object in
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      method_call cx loc prop_loc (callee, ot, name) argts

  | Call {
      Call.callee = _, Identifier (ploc, { Ast.Identifier.name = "super"; _ });
      arguments
    } ->
      let argts = List.map (expression_or_spread cx type_params_map) arguments in
      let reason = mk_reason "super(...)" loc in
      let super_reason = mk_reason "super" ploc in

      (* switch back env entries for this and super from undefined *)
      define_internal cx reason "this";
      define_internal cx reason "super";

      let this = this_ cx reason in
      let super = super_ cx super_reason in
      Flow.mk_tvar_where cx reason (fun t ->
        let funtype = Flow.mk_methodtype this argts t in
        Flow.flow cx (super,
          MethodT(reason, (super_reason, "constructor"), funtype))
      )

  (******************************************)
  (* See ~/www/static_upstream/core/ *)

  | Call {
      Call.callee = (_, Identifier (_,
        { Ast.Identifier.name = "invariant"; _ })) as callee;
      arguments
    } ->
      (* TODO: require *)
      let reason = mk_reason "invariant" loc in
      ignore (expression cx type_params_map callee);
      (match arguments with
      | (Expression (_, Ast.Expression.Literal {
          Ast.Literal.value = Ast.Literal.Boolean false; _;
        }))::arguments ->
        (* invariant(false, ...) is treated like a throw *)
        ignore (List.map (expression_or_spread cx type_params_map) arguments);
        Env.reset_current_activation reason;
        Abnormal.save_and_throw Abnormal.Throw
      | (Expression cond)::arguments ->
        ignore (List.map (expression_or_spread cx type_params_map) arguments);
        let _, preds, _, xtypes =
          predicates_of_condition cx type_params_map cond in
        let _ = Env.refine_with_preds cx reason preds xtypes in
        ()
      | _ ->
        let msg = "unsupported arguments in call to invariant()" in
        FlowError.add_error cx (loc, [msg])
      );
      VoidT.at loc

  | Call { Call.callee; arguments } ->
      let f = expression cx type_params_map callee in
      let reason = mk_reason "function call" loc in
      let argts =
        List.map (expression_or_spread cx type_params_map) arguments in
      func_call cx reason f argts

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason "conditional" loc in
      let _, preds, not_preds, xtypes =
        predicates_of_condition cx type_params_map test in
      let env =  Env.peek_env () in
      let oldset = Changeset.clear () in

      let then_env = Env.clone_env env in
      Env.update_env cx reason then_env;
      let _ = Env.refine_with_preds cx reason preds xtypes in
      let t1 = expression cx type_params_map consequent in

      let else_env = Env.clone_env env in
      Env.update_env cx reason else_env;
      let _ = Env.refine_with_preds cx reason not_preds xtypes in
      let t2 = expression cx type_params_map alternate in

      let newset = Changeset.merge oldset in
      Env.merge_env cx reason (env, then_env, else_env) newset;
      Env.update_env cx reason env;
      (* TODO call loc_of_predicate on some pred?
         t1 is wrong but hopefully close *)
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow_t cx (t1, t);
        Flow.flow_t cx (t2, t);
      )

  | Assignment { Assignment.operator; left; right } ->
      assignment cx type_params_map loc (left, operator, right)

  | Sequence { Sequence.expressions } ->
      List.fold_left
        (fun _ e -> expression cx type_params_map e)
        (VoidT.at loc)
        expressions

  | Function func ->
      let {Ast.Function.id; async; generator; _} = func in
      let desc = function_desc ~async ~generator "function" in
      let reason = mk_reason desc loc in
      mk_function id cx type_params_map reason func

  | ArrowFunction func ->
      let {Ast.Function.async; generator; _} = func in
      let desc = function_desc ~async ~generator "arrow function" in
      let reason = mk_reason desc loc in
      mk_arrow cx type_params_map reason func

  | TaggedTemplate {
      TaggedTemplate.tag = _, Identifier (_,
        { Ast.Identifier.name = "query"; _ });
      (* TODO: walk quasis? *)
      quasi = _, { TemplateLiteral.quasis = _; expressions }
    } ->
    List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
    (*parse_graphql cx encaps;*)
    VoidT.at loc

  | TaggedTemplate {
      TaggedTemplate.tag;
      (* TODO: walk quasis? *)
      quasi = _, { TemplateLiteral.quasis = _; expressions }
    } ->
      List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
      let t = expression cx type_params_map tag in
      let reason = mk_reason "encaps tag" loc in
      let reason_array = replace_reason "array" reason in
      let ret = Flow.mk_tvar cx reason in
      let ft = Flow.mk_functiontype
        [ ArrT (reason_array, StrT.why reason, []);
          RestT (AnyT.why reason) ]
        ret
      in
      Flow.flow cx (t, CallT (reason, ft));
      ret

  | TemplateLiteral {
      (* TODO: walk quasis? *)
      TemplateLiteral.quasis = _;
      expressions
    } ->
      List.iter (fun e -> ignore (expression cx type_params_map e)) expressions;
      StrT.at loc

  | JSXElement e ->
      jsx cx type_params_map e

  | Class c ->
      let (name_loc, name) = extract_class_name loc c in
      let reason = mk_reason (spf "class expr `%s`" name) loc in
      (match c.Ast.Class.id with
      | Some _ ->
          let tvar = Flow.mk_tvar cx reason in
          let scope = Scope.fresh () in
          Scope.(
            let kind = Entry.ClassNameBinding in
            let entry = Entry.(
              new_let tvar ~loc:name_loc ~state:State.Declared ~kind
            ) in
            add_entry name entry scope
          );
          Env.push_var_scope cx scope;
          let class_t = mk_class cx type_params_map loc reason c in
          Env.pop_var_scope ();
          Flow.flow_t cx (class_t, tvar);
          class_t;
      | None -> mk_class cx type_params_map loc reason c)

  | Yield { Yield.argument; delegate = false } ->
      let reason = mk_reason "yield" loc in
      let yield = Env.get_var cx (internal_name "yield") reason in
      let t = match argument with
      | Some expr -> expression cx type_params_map expr
      | None -> VoidT.at loc in
      Flow.flow_t cx (t, yield);
      let next = Env.get_var cx (internal_name "next") reason in
      OptionalT next

  | Yield { Yield.argument; delegate = true } ->
      let reason = mk_reason "yield* delegate" loc in
      let next = Env.get_var cx
        (internal_name "next")
        (prefix_reason "next of parent generator in " reason) in
      let yield = Env.get_var cx
        (internal_name "yield")
        (prefix_reason "yield of parent generator in " reason) in
      let t = match argument with
      | Some expr -> expression cx type_params_map expr
      | None -> assert_false "delegate yield without argument" in

      let ret = Flow.mk_tvar cx
        (prefix_reason "return of child generator in " reason) in

      (* widen yield with the element type of the delegated-to iterable *)
      let iterable = Flow.get_builtin_typeapp cx
        (mk_reason "iteration expected on Iterable" loc)
        "$Iterable"
        [yield; ret; next] in
      Flow.flow_t cx (t, iterable);

      ret

  (* TODO *)
  | Comprehension _
  | Generator _
  | Let _ ->
    FlowError.add_error cx (loc, ["not (sup)ported"]);
    EmptyT.at loc
)

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx tok class_ argts =
  let reason = mk_reason "constructor call" tok in
  Flow.mk_tvar_where cx reason (fun t ->
    Flow.flow cx (class_, ConstructorT (reason, argts, t));
  )

and func_call cx reason func_t argts =
  Env.havoc_heap_refinements ();
  Flow.mk_tvar_where cx reason (fun t ->
    let frame = Env.peek_frame () in
    let app = Flow.mk_functiontype2 argts t frame in
    Flow.flow cx (func_t, CallT(reason, app))
  )

and method_call cx loc prop_loc (expr, obj_t, name) argts =
  let reason = mk_reason (spf "call of method `%s`" name) loc in
  Type_inference_hooks_js.dispatch_call_hook cx name prop_loc obj_t;
  (match Refinement.get cx expr reason with
  | Some f ->
      (* note: the current state of affairs is that we understand
         member expressions as having refined types, rather than
         understanding receiver objects as carrying refined properties.
         generalizing this properly is a todo, and will deliver goodness.
         meanwhile, here we must hijack the property selection normally
         performed by the flow algorithm itself. *)
      Env.havoc_heap_refinements ();
      Flow.mk_tvar_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let app = Flow.mk_methodtype2 obj_t argts t frame in
        Flow.flow cx (f, CallT (reason, app));
      )
  | None ->
      Env.havoc_heap_refinements ();
      Flow.mk_tvar_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let reason_prop = mk_reason (spf "property `%s`" name) prop_loc in
        let app = Flow.mk_methodtype2 obj_t argts t frame in
        Flow.flow cx (obj_t, MethodT(reason, (reason_prop, name), app))
      )
  )

and identifier cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    if name = "undefined"
    then VoidT.at loc
    else (
      let reason = mk_reason (spf "identifier `%s`" name) loc in
      let t = Env.var_ref ~lookup_mode:ForValue cx name reason in
      t
    )
  )

(* traverse a literal expression, return result type *)
and literal cx loc lit = Ast.Literal.(match lit.Ast.Literal.value with
  | String s ->
      StrT (mk_reason "string" loc, Literal s)

  | Boolean b ->
      BoolT (mk_reason "boolean" loc, Some b)

  | Null ->
      NullT.at loc

  | Number f ->
      NumT (mk_reason "number" loc, Literal (f, lit.raw))

  | RegExp _ ->
      Flow.get_builtin_type cx (mk_reason "regexp" loc) "RegExp"
)

(* traverse a unary expression, return result type *)
and unary cx type_params_map loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; _ } ->
      let arg = expression cx type_params_map argument in
      let reason = mk_reason "not operator" loc in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (arg, NotT (reason, t));
      )

  | { operator = Plus; argument; _ } ->
      ignore (expression cx type_params_map argument);
      NumT.at loc

  | { operator = Minus; argument; _ } ->
      let arg = expression cx type_params_map argument in
      let reason = mk_reason "unary minus operator" loc in
      Flow.mk_tvar_derivable_where cx reason (fun t ->
        Flow.flow cx (arg, UnaryMinusT (reason, t));
      )

  | { operator = BitNot; argument; _ } ->
      let t = NumT.at loc in
      Flow.flow_t cx (expression cx type_params_map argument, t);
      t

  | { operator = Typeof; argument; _ } ->
      ignore (expression cx type_params_map argument);
      StrT.at loc

  | { operator = Void; argument; _ } ->
      ignore (expression cx type_params_map argument);
      VoidT.at loc

  | { operator = Delete; argument; _ } ->
      ignore (expression cx type_params_map argument);
      BoolT.at loc

  | { operator = Await; argument; _ } ->
    (** TODO: await should look up Promise in the environment instead of going
        directly to the core definition. Otherwise, the following won't work
        with a polyfilled Promise! **)
    (* see declaration of $await in core.js:
       if argument is a Promise<T>, then (await argument) returns T.
       otherwise it just returns the argument type.
       TODO update this comment when recursive unwrapping of
       Promise is done.
     *)
    let reason = mk_reason "await" loc in
    let await = Flow.get_builtin cx "$await" reason in
    let arg = expression cx type_params_map argument in
    func_call cx reason await [arg]
)

(* numeric pre/post inc/dec *)
and update cx type_params_map loc expr = Ast.Expression.Update.(
  let reason = mk_reason "update" loc in
  let result_t = NumT.why reason in
  (match expr.argument with
  | _, Ast.Expression.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
    let lhs_t = identifier cx name id_loc in
    Flow.flow_t cx (lhs_t, result_t);
     (* enforce state-based guards for binding update, e.g., const *)
     let id_reason = mk_reason name id_loc in
     ignore Env.(set_var cx name result_t id_reason)
  | expr ->
    let lhs_t = expression cx type_params_map expr in
    Flow.flow_t cx (lhs_t, result_t)
  );
  result_t
)

(* traverse a binary expression, return result type *)
and binary cx type_params_map loc = Ast.Expression.Binary.(function
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let reason = mk_reason "non-strict equality comparison" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow.flow cx (t1, EqT (reason,t2));
      BoolT.at loc

  | { operator = In; left = (loc1, _) as left; right = (loc2, _) as right } ->
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      let reason1 = mk_reason "LHS of `in` operator" loc1 in
      let reason2 = mk_reason "RHS of `in` operator" loc2 in
      let lhs = UnionT (reason1, UnionRep.make [StrT.why reason1; NumT.why reason1]) in
      let rhs =
        let elemt = Flow.mk_tvar cx reason2 in
        UnionT (reason2, UnionRep.make [AnyObjT reason2; ArrT (reason2, elemt, [])])
      in
      Flow.flow_t cx (t1, lhs);
      Flow.flow_t cx (t2, rhs);
      BoolT.at loc

  | { operator = StrictEqual; left; right }
  | { operator = StrictNotEqual; left; right }
  | { operator = Instanceof; left; right } ->
      ignore (expression cx type_params_map left);
      ignore (expression cx type_params_map right);
      BoolT.at loc

  | { operator = LessThan; left; right }
  | { operator = LessThanEqual; left; right }
  | { operator = GreaterThan; left; right }
  | { operator = GreaterThanEqual; left; right } ->
      let reason = mk_reason "relational comparison" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow.flow cx (t1, ComparatorT (reason,t2));
      BoolT.at loc

  | { operator = LShift; left; right }
  | { operator = RShift; left; right }
  | { operator = RShift3; left; right }
  | { operator = Minus; left; right }
  | { operator = Mult; left; right }
  | { operator = Exp; left; right }
  | { operator = Div; left; right }
  | { operator = Mod; left; right }
  | { operator = BitOr; left; right }
  | { operator = Xor; left; right }
  | { operator = BitAnd; left; right } ->
      let t = NumT.at loc in
      Flow.flow_t cx (expression cx type_params_map left, t);
      Flow.flow_t cx (expression cx type_params_map right, t);
      t

  | { operator = Plus; left; right } ->
      let reason = mk_reason "+" loc in
      let t1 = expression cx type_params_map left in
      let t2 = expression cx type_params_map right in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, AdderT (reason, t2, t));
      )
)

and logical cx type_params_map loc = Ast.Expression.Logical.(function
  | { operator = Or;
      left = (left_loc, _) as left;
      right = (right_loc, _) as right;
    } ->
      let t1, _, not_map, xtypes = predicates_of_condition cx type_params_map left in
      let reason = mk_reason "||" loc in
      let t2 = Env.in_refined_env cx reason not_map xtypes
        (fun () -> expression cx type_params_map right)
      in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, OrT (reason, (left_loc, right_loc), t2, t));
      )

  | { operator = And;
      left = (left_loc, _) as left;
      right = (right_loc, _) as right;
    } ->
      let t1, map, _, xtypes = predicates_of_condition cx type_params_map left in
      let reason = mk_reason "&&" loc in
      let t2 = Env.in_refined_env cx reason map xtypes
        (fun () -> expression cx type_params_map right)
      in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, AndT (reason, (left_loc, right_loc), t2, t));
      )
)

and assignment_lhs cx type_params_map = Ast.Pattern.(function
  | loc, Object _
  | loc, Array _ ->
      error_destructuring cx loc;
      AnyT.at loc

  | _, Identifier (loc, { Ast.Identifier.name; _ }) ->
      identifier cx name loc

  | _, Expression ((_, Ast.Expression.Member _) as m) ->
      expression cx type_params_map m

  (* parser will error before we get here *)
  | _ -> assert false
)

(* traverse assignment expressions *)
and assignment cx type_params_map loc = Ast.Expression.(function

  (* r = e *)
  | (r, Assignment.Assign, e) ->

      (* compute the type of the RHS. this is what we return *)
      let t = expression cx type_params_map e in

      (* update env, add constraints arising from LHS structure,
         handle special cases, etc. *)
      (match r with

        (* module.exports = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Ast.Expression.Identifier (_,
              { Ast.Identifier.name = "module"; _ });
            property = Member.PropertyIdentifier (_,
              { Ast.Identifier.name = "exports"; _ });
            _
          }) ->
            let reason = mk_reason "assignment of module.exports" lhs_loc in
            mark_exports_type cx reason (Context.CommonJSModule(Some(lhs_loc)));
            set_module_exports cx reason t

        (* super.name = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Identifier (_,
              { Ast.Identifier.name = "super"; _ });
            property = Member.PropertyIdentifier (ploc,
              { Ast.Identifier.name; _ });
            _
          }) ->
            let reason =
              mk_reason (spf "assignment of property `%s`" name) lhs_loc in
            let prop_reason = mk_reason (spf "property `%s`" name) ploc in
            let super = super_ cx reason in
            Flow.flow cx (super, SetPropT(reason, (prop_reason, name), t))

        (* _object.name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyIdentifier (ploc,
              { Ast.Identifier.name; _ });
            _
          }) as expr) ->
            let o = expression cx type_params_map _object in
            (* if we fire this hook, it means the assignment is a sham. *)
            if not (Type_inference_hooks_js.dispatch_member_hook cx name ploc o)
            then (
              let reason = mk_reason
                (spf "assignment of property `%s`" name) lhs_loc in
              let prop_reason = mk_reason (spf "property `%s`" name) ploc in

              (* flow type to object property itself *)
              Flow.flow cx (o, SetPropT (reason, (prop_reason, name), t));

              (* types involved in the assignment are computed
                 in pre-havoc environment. it's the assignment itself
                 which clears refis *)
              (* TODO: havoc refinements for this prop name only *)
              Env.havoc_heap_refinements_with_propname name;

              (* add type refinement if LHS is a pattern we handle *)
              match Refinement.key expr with
              | Some key ->
                (* NOTE: currently, we allow property refinements to propagate
                   even if they may turn out to be invalid w.r.t. the
                   underlying object type. If invalid, of course, they produce
                   errors, but in the future we may want to prevent the
                   invalid types from flowing downstream as well.
                   Doing so would require that we defer any subsequent flow
                   calls that are sensitive to the refined type until the
                   object and refinement types - `o` and `t` here - are
                   fully resolved.
                 *)
                ignore Env.(set_expr key reason t t)
              | None ->
                ()
            )

        (* _object[index] = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object;
            property = Member.PropertyExpression index;
            _
          }) ->
            let reason =
              mk_reason "assignment of computed property/element" lhs_loc in
            let a = expression cx type_params_map _object in
            let i = expression cx type_params_map index in
            Flow.flow cx (a, SetElemT (reason, i, t));

            (* types involved in the assignment itself are computed
               in pre-havoc environment. it's the assignment itself
               which clears refis *)
            Env.havoc_heap_refinements ();

        (* other r structures are handled as destructuring assignments *)
        | _ ->
            destructuring_assignment cx t e r
      );
      t

  | (lhs, Assignment.PlusAssign, rhs) ->
      (* lhs += rhs *)
      let reason = mk_reason "+=" loc in
      let lhs_t = assignment_lhs cx type_params_map lhs in
      let rhs_t = expression cx type_params_map rhs in
      let result_t = Flow.mk_tvar cx reason in
      (* lhs = lhs + rhs *)
      Flow.flow cx (lhs_t, AdderT (reason, rhs_t, result_t));
      Flow.flow cx (rhs_t, AdderT (reason, lhs_t, result_t));
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
        let id_reason = mk_reason name id_loc in
        ignore Env.(set_var cx name result_t id_reason)
      | _ -> ()
      );
      lhs_t

  | (lhs, Assignment.MinusAssign, rhs)
  | (lhs, Assignment.MultAssign, rhs)
  | (lhs, Assignment.ExpAssign, rhs)
  | (lhs, Assignment.DivAssign, rhs)
  | (lhs, Assignment.ModAssign, rhs)
  | (lhs, Assignment.LShiftAssign, rhs)
  | (lhs, Assignment.RShiftAssign, rhs)
  | (lhs, Assignment.RShift3Assign, rhs)
  | (lhs, Assignment.BitOrAssign, rhs)
  | (lhs, Assignment.BitXorAssign, rhs)
  | (lhs, Assignment.BitAndAssign, rhs)
    ->
      (* lhs (numop)= rhs *)
      let reason = mk_reason "(numop)=" loc in
      let result_t = NumT.why reason in
      let lhs_t = assignment_lhs cx type_params_map lhs in
      let rhs_t = expression cx type_params_map rhs in
      (* lhs = lhs (numop) rhs *)
      Flow.flow_t cx (lhs_t, result_t);
      Flow.flow_t cx (rhs_t, result_t);
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier (id_loc, { Ast.Identifier.name; _ }) ->
        let id_reason = mk_reason name id_loc in
        ignore Env.(set_var cx name result_t id_reason)
      | _ -> ()
      );
      lhs_t
)

and clone_object_with_excludes cx reason this that excludes =
  Flow.mk_tvar_where cx reason (fun tvar ->
    let u = ObjRestT(reason, excludes, tvar) in
    let t = Flow.tvar_with_constraint cx u in
    Flow.flow cx (
      this,
      ObjAssignT(reason, that, t, [], true)
    )
  )

and clone_object cx reason this that =
  clone_object_with_excludes cx reason this that []


and react_ignored_attributes = [ "key"; "ref"; ]

and react_ignore_attribute aname =
  List.mem aname react_ignored_attributes

and jsx cx type_params_map = Ast.JSX.(
  function { openingElement; children; _ } ->
  jsx_title cx type_params_map openingElement
    (List.map (jsx_body cx type_params_map) children)
)

and jsx_title cx type_params_map openingElement _children = Ast.JSX.(
  let eloc, { Opening.name; attributes; _ } = openingElement in
  let facebook_ignore_fbt = Context.should_ignore_fbt cx in

  let mk_props reason c =
    let map = ref SMap.empty in
    let spread = ref None in
    attributes |> List.iter (function
      | Opening.Attribute (aloc, { Attribute.
            name = Attribute.Identifier (_, { Identifier.name = aname });
            value
          }) ->
        if not (Type_inference_hooks_js.dispatch_jsx_hook cx aname aloc c)
        then
          let atype = (match value with
            | Some (Attribute.Literal (loc, lit)) ->
                literal cx loc lit
            | Some (Attribute.ExpressionContainer (_, {
                ExpressionContainer.expression =
                  ExpressionContainer.Expression (loc, e)
              })) ->
                expression cx type_params_map (loc, e)
            | _ ->
                (* empty or nonexistent attribute values *)
                EmptyT.at aloc
          ) in

          if not (react_ignore_attribute aname)
          then map := !map |> SMap.add aname atype

      | Opening.Attribute _ ->
          () (* TODO: attributes with namespaced names *)

      | Opening.SpreadAttribute (_, { SpreadAttribute.argument }) ->
          let ex_t = expression cx type_params_map argument in
          spread := Some (ex_t)
    );
    let reason_props = prefix_reason "props of " reason in
    let o = Flow.mk_object_with_map_proto cx reason_props
      !map (MixedT (reason_props, Mixed_everything))
    in
    match !spread with
      | None -> o
      | Some ex_t ->
          let reason_prop = prefix_reason "spread of " (reason_of_t ex_t) in
          clone_object_with_excludes cx
            reason_prop o ex_t react_ignored_attributes
  in

  match name with
  | Identifier (_, { Identifier.name })
      when name = "fbt" && facebook_ignore_fbt ->
    AnyT.why (mk_reason "<fbt />" eloc)

  | Identifier (_, { Identifier.name }) when name = String.capitalize name ->
      let reason = mk_reason (spf "React element `%s`" name) eloc in
      let c = Env.get_var cx name reason in
      let o = mk_props reason c in
      (* TODO: children *)
      let react = require cx "react" eloc in
      Flow.mk_tvar_where cx reason (fun tvar ->
        let reason_createElement = mk_reason "property `createElement`" eloc in
        Flow.flow cx (react, MethodT(
          reason,
          (reason_createElement, "createElement"),
          Flow.mk_methodtype react [c;o] tvar
        ))
      )

  | Identifier (_, { Identifier.name }) ->
      (**
       * For JSX intrinsics, we assume a built-in global
       * object type: $JSXIntrinsics. The keys of this object type correspond to
       * each JSX intrinsic name, and the type of the value for that key is the
       * type signature of the intrinsic ReactComponent.
       *
       * We use a single object type for this (rather than several individual
       * globals) to allow for a default `type $JSXIntrinsics = Object;` that
       * ships with lib/core.js. This allows JSX to work out of the box where
       * all intrinsics are typed as `any`. Users can then refine the set of
       * intrinsics their application uses with a more specific libdef.
       *)
      let jsx_intrinsics =
        Env.get_var
          ~lookup_mode:Env.LookupMode.ForType
          cx
          "$JSXIntrinsics"
          (mk_reason "JSX Intrinsics lookup" eloc)
      in

      (**
       * Because $JSXIntrinsics is a type alias, extracting a property off of it
       * will result in a TypeT as well. This presents a problem because we need
       * a value type that can be passed in to React.creatElement; So we first
       * reify the TypeT into it's value, then pass this along.
       *
       * This is a bit strange but it's fallout from the decision to model
       * $JSXIntrinsics using a type alias rather than a "value". Modeling with
       * a value would be disingenous because no such value really exists (JSX
       * intrinsics are just React components that are implicitly defined
       * dynamically in library code such as `React.createElement`)
       *)
      let component_t_reason =
        mk_reason (spf "JSX Intrinsic: `%s`" name) eloc
      in
      let component_t = Flow.mk_tvar_where cx component_t_reason (fun t ->
        let prop_t = get_prop
          ~is_cond:false
          cx
          component_t_reason
          jsx_intrinsics
          (component_t_reason, name)
        in
        Flow.flow cx (prop_t, ReifyTypeT(component_t_reason, t))
      ) in
      let o = mk_props component_t_reason component_t in
      (* TODO: children *)
      let react = require cx "react" eloc in
      let reason = mk_reason (spf "React element: `%s`" name) eloc in
      Flow.mk_tvar_where cx reason (fun tvar ->
        let reason_createElement = mk_reason "property `createElement`" eloc in
        Flow.flow cx (react, MethodT(
          reason,
          (reason_createElement, "createElement"),
          Flow.mk_methodtype react [component_t;o] tvar
        ))
      )

  | _ ->
      (* TODO? covers namespaced names, member expressions as element names *)
      AnyT.at eloc
)

and jsx_body cx type_params_map = Ast.JSX.(function
  | _, Element e -> jsx cx type_params_map e
  | _, ExpressionContainer ec -> (
      let open ExpressionContainer in
      let { expression = ex } = ec in
      match ex with
        | Expression (loc, e) -> expression cx type_params_map (loc, e)
        | EmptyExpression loc -> EmptyT (mk_reason "empty jsx body" loc)
    )
  | loc, Text _ -> StrT.at loc (* TODO: create StrT (..., Literal ...)) *)
)

(* Native support for React.PropTypes validation functions, which are
   interpreted as type annotations for React props. This strategy is reasonable
   because the validation functions enforce types at run time (during
   development), and we can always insist on them because they are turned off in
   production. *)

and mk_proptype cx type_params_map = Ast.Expression.(function
  | _, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "isRequired"; _ });
      _object = e;
      _
    } ->
      mk_proptype cx type_params_map e

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "number"; _ });
      _
    } ->
      NumT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "string"; _ });
      _
    } ->
      StrT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "bool"; _ });
      _
    } ->
      BoolT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "array"; _ });
      _
    } ->
      ArrT (mk_reason "array" vloc, AnyT.at vloc, [])

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "func"; _ });
      _
    } ->
      AnyFunT (mk_reason "func" vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "object"; _ });
      _
    } ->
      AnyObjT (mk_reason "object" vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "node"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "element"; _ });
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "arrayOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      ArrT (mk_reason "arrayOf" vloc, mk_proptype cx type_params_map e, [])

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "instanceOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      Flow.mk_instance cx ~for_type:false (mk_reason "instanceOf" vloc)
        (expression cx type_params_map e)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "objectOf"; _ });
         _
      };
      arguments = [Expression e];
    } ->
      let flags = {
        frozen = false;
        sealed = UnsealedInFile (Loc.source vloc);
        exact = true
      } in
      let dict = Some {
        dict_name = None;
        key = AnyT.t;
        value = mk_proptype cx type_params_map e
      } in
      let pmap = Flow.mk_propmap cx SMap.empty in
      let proto = MixedT (reason_of_string "Object", Mixed_everything) in
      ObjT (mk_reason "objectOf" vloc, Flow.mk_objecttype ~flags dict pmap proto)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOf"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec string_literals lits es = match (es) with
        | Some (Expression (_, Ast.Expression.Literal { Ast.Literal.
            value = Ast.Literal.String lit; _
          })) :: tl ->
            string_literals (lit :: lits) tl
        | [] -> Some lits
        | _  -> None in
      (match string_literals [] elements with
        | Some lits ->
            let reason = mk_reason "oneOf" vloc in
            Anno.mk_keys_type reason lits
        | None -> AnyT.at vloc)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "oneOfType"; _ });
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec proptype_elements ts es = match es with
        | Some (Expression e) :: tl ->
            proptype_elements (mk_proptype cx type_params_map e :: ts) tl
        | [] -> Some ts
        | _ -> None in
      let reason = mk_reason "oneOfType" vloc in
      (match proptype_elements [] elements with
        | Some ts -> UnionT (reason, UnionRep.make ts)
        | None -> AnyT.at vloc)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, {Ast.Identifier.name = "shape"; _ });
         _
      };
      arguments = [Expression (_, Object { Object.properties })];
    } ->
      let reason = mk_reason "shape" vloc in
      let amap, omap, dict = mk_proptypes cx type_params_map properties in
      let map = SMap.union amap (SMap.map (fun t -> OptionalT t) omap) in
      let proto = MixedT (reason, Mixed_everything) in
      Flow.mk_object_with_map_proto cx reason ?dict map proto

  (* Support for FB-specific ReactPropTypes validators. *)
  (** TODO: instead, route to custom lib defs, somehow...details of which have
      not been set up or even worked out yet. **)
  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, {Ast.Identifier.name = "Fbt"; _ });
      _
    } ->
      (* We assume that there is a Fbt type defined in the global scope. *)
      Flow.get_builtin_type cx (mk_reason "Fbt" vloc) "Fbt"

  | vloc, _ -> AnyT.at vloc
)

and mk_proptypes cx type_params_map props = Ast.Expression.Object.(
  List.fold_left (fun (amap, omap, dict) -> function

    (* required prop *)
    | Property (_, { Property.
        kind = Property.Init;
        key = Property.Identifier (_, {
          Ast.Identifier.name; _ });
        value = (_, Ast.Expression.Member {
          Ast.Expression.Member.
          property = Ast.Expression.Member.PropertyIdentifier (_, {
            Ast.Identifier.name = "isRequired"; _ });
          _object = e;
          _
        });
        _ }) ->
        let tvar = mk_proptype cx type_params_map e in
        SMap.add name tvar amap,
        omap,
        dict

    (* other prop *)
    | Property (_, { Property.kind = Property.Init;
        key =
          Property.Identifier (_, { Ast.Identifier.name; _ }) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = v;
        _ }) ->
        let tvar = mk_proptype cx type_params_map v in
        amap,
        SMap.add name tvar omap,
        dict

    (* spread prop *)
    | SpreadProperty _ ->
      (* Instead of modeling the spread precisely, we instead make the propTypes
         extensible. This has the effect of loosening the check for properties
         added by the spread, while leaving the checking of other properties as
         is. FWIW we use a similar approximation for mixins. It would be nice to
         be more precise here, but given that this only affects legacy React
         classes, and that reconstructing props is already fairly delicate in
         that world, it may not be worth it to spend time on this right now. *)
      amap, omap, Some { dict_name=None; key=StrT.t; value=AnyT.t; }

    (* literal LHS *)
    | Property (loc, { Property.key = Property.Literal _; _ }) ->
      let msg =
        "non-string literal property keys not supported for React propTypes" in
      FlowError.add_error cx (loc, [msg]);
      amap, omap, dict

    (* get/set kind *)
    | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
      let msg = "get/set properties not supported for React propTypes" in
      FlowError.add_error cx (loc, [msg]);
      amap, omap, dict

    (* computed LHS *)
    | Property (loc, { Property.key = Property.Computed _; _ }) ->
      let msg = "computed property keys not supported for React propTypes" in
      FlowError.add_error cx (loc, [msg]);
      amap, omap, dict

  ) (SMap.empty, SMap.empty, None) props
)

(* Legacy: generate React class from specification object. *)
and react_create_class cx type_params_map loc class_props = Ast.Expression.(
  let reason_class = mk_reason "React class" loc in
  let reason_component = mk_reason "React component" loc in
  let this = Flow.mk_tvar cx reason_component in
  let mixins = ref [] in
  let static_reason = prefix_reason "statics of " reason_class in
  let static = ref (Flow.mk_object cx static_reason) in
  let default_reason = prefix_reason "default props of " reason_component in
  let default = ref (Flow.mk_object cx default_reason) in
  let reason_state = prefix_reason "state of " reason_component in
  let state = ref (Flow.mk_object cx reason_state) in

  let props_reason = prefix_reason "props of " reason_component in
  (* TODO - this probably should be the empty object AND we should enforce it *)
  let props = ref (AnyObjT props_reason) in

  let (fmap, mmap) =
    List.fold_left Ast.Expression.Object.(fun (fmap, mmap) -> function

      (* mixins *)
      | Property (_, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, { Ast.Identifier.name = "mixins"; _ });
          value = aloc, Array { Array.elements };
          _ }) ->
        mixins := List.map (array_element cx type_params_map aloc) elements;
        fmap, mmap

      (* statics *)
      | Property (_, { Property.kind = Property.Init;
            key = Property.Identifier (nloc, {
            Ast.Identifier.name = "statics"; _ });
          value = _, Object { Object.properties };
          _ }) ->
        let reason = mk_reason "statics" nloc in
        static :=
          object_ cx type_params_map reason ~allow_sealed:false properties;
        fmap, mmap

      (* propTypes *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (nloc, {
            Ast.Identifier.name = "propTypes"; _ });
          value = _, Object { Object.properties } as value;
          _ }) ->
        ignore (expression cx type_params_map value);
        let reason = mk_reason "propTypes of React component" nloc in
        let amap, omap, dict = mk_proptypes cx type_params_map properties in
        let map = SMap.fold (fun k v map ->
          SMap.add k (OptionalT v) map
        ) omap amap in
        let proto = MixedT (reason, Mixed_everything) in
        props := Flow.mk_object_with_map_proto cx reason ?dict map proto;
        fmap, mmap

      (* getDefaultProps *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name = "getDefaultProps"; _ });
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let reason = mk_reason "defaultProps" vloc in
          let t = mk_method cx type_params_map reason func this in
          let ret_loc = Func_sig.return_loc func in
          let ret_reason = repos_reason ret_loc reason in
          let default_tvar = Flow.mk_tvar cx (derivable_reason ret_reason) in
          let override_default = Flow.tvar_with_constraint cx
            (BecomeT(ret_reason, default_tvar)) in
          default := default_tvar;
          Flow.flow cx (t,
            CallT (reason,
              Flow.mk_functiontype [] override_default));
          fmap, mmap

      (* getInitialState *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name = "getInitialState"; _ });
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let reason = mk_reason "initial state of React component" vloc in
          let t = mk_method cx type_params_map reason func this in
          (* since the call to getInitialState happens internally, we need to
             fake a location to pretend the call happened. using the position
             of the return type makes it act like an IIFE. *)
          let ret_loc = Func_sig.return_loc func in
          let ret_reason = repos_reason ret_loc reason in
          let state_tvar = Flow.mk_tvar cx (derivable_reason ret_reason) in
          let override_state = Flow.tvar_with_constraint cx
            (BecomeT (ret_reason, state_tvar)) in
          state := state_tvar;
          Flow.flow cx (t,
            CallT (ret_reason,
              Flow.mk_functiontype [] override_state));
          fmap, mmap

      (* name = function expr *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (_, {
            Ast.Identifier.name; _ });
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let reason = mk_reason "function" vloc in
          let t = mk_method cx type_params_map reason func this in
          fmap, SMap.add name t mmap

      (* name = non-function expr *)
      | Property (_, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, { Ast.Identifier.name; _ }) |
            Property.Literal (_, {
              Ast.Literal.value = Ast.Literal.String name; _;
            });
          value = v;
          _ }) ->
        let t = expression cx type_params_map v in
        SMap.add name t fmap, mmap

      | _ ->
        let msg = "unsupported property specification in createClass" in
        FlowError.add_error cx (loc, [msg]);
        fmap, mmap

    ) (SMap.empty, SMap.empty) class_props in

  let type_args = [!default; !props; !state] in
  let super_reason = prefix_reason "super of " reason_component in
  let super =
    Flow.get_builtin_typeapp cx super_reason
      "LegacyReactComponent" type_args
  in

  let extract_map (from_map,to_map) name =
    match SMap.get name from_map with
    | Some t -> SMap.remove name from_map, SMap.add name t to_map
    | None -> from_map, to_map
  in
  let fmap = SMap.add "state" !state fmap in
  let fmap, smap =
    List.fold_left extract_map (fmap, SMap.empty)
      ["contextTypes";"childContextTypes";"displayName"]
  in
  let override_statics =
    Flow.mk_object_with_map_proto cx
      static_reason smap (MixedT (static_reason, Mixed_everything))
  in
  let super_static = Flow.mk_tvar_where cx static_reason (fun t ->
    Flow.flow cx (super,
      GetPropT (static_reason, (static_reason, "statics"), t));
  ) in
  Flow.flow_t cx (super_static, override_statics);
  static := clone_object cx static_reason !static super_static;

  let itype = {
    class_id = 0;
    type_args = SMap.empty;
    arg_polarities = SMap.empty;
    fields_tmap = Flow.mk_propmap cx fmap;
    methods_tmap = Flow.mk_propmap cx mmap;
    mixins = !mixins <> [];
    structural = false;
  } in
  Flow.flow cx (super, SuperT (super_reason, itype));

  (* TODO: Mixins are handled quite superficially. *)
  (* mixins' statics are copied into static to inherit static properties *)
  (* mixins must be consistent with instance properties *)
  !mixins |> List.iter (fun mixin ->
    static := clone_object cx static_reason !static mixin;
    Flow.flow cx (mixin, SuperT (super_reason, itype))
  );

  let instance = InstanceT (reason_component,!static,super,itype) in
  Flow.flow_t cx (instance, this);

  ClassT(instance)
)

(* given an expression found in a test position, notices certain
   type refinements which follow from the test's success or failure,
   and returns a quad:
   - result type of the test (not always bool)
   - map (lookup key -> type) of refinements which hold if
   the test is true
   - map of refinements which hold if the test is false
   - map of unrefined types for lvalues found in refinement maps
 *)
and predicates_of_condition cx type_params_map e = Ast.(Expression.(

  (* refinement key if expr is eligible, along with unrefined type *)
  let refinable_lvalue e =
    Refinement.key e, condition cx type_params_map e
  in

  (* package empty result (no refinements derived) from test type *)
  let empty_result test_t =
    Scope.(test_t, KeyMap.empty, KeyMap.empty, KeyMap.empty)
  in

  let add_predicate key unrefined_t pred sense (test_t, ps, notps, tmap) =
    let p, notp = if sense
      then pred, NotP pred
      else NotP pred, pred
    in
    (test_t,
      Scope.KeyMap.add key p ps,
      Scope.KeyMap.add key notp notps,
      Scope.KeyMap.add key unrefined_t tmap)
  in

  (* package result quad from test type, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_t key unrefined_t pred sense =
    empty_result test_t |> add_predicate key unrefined_t pred sense
  in

  (* inspect a null equality test *)
  let null_test loc ~sense ~strict e =
    let refinement = match refinable_lvalue e with
    | None, _ -> None
    | Some name, t ->
        let pred = if strict then NullP else MaybeP in
        Some (name, t, pred, sense)
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect an undefined equality test *)
  let undef_test loc ~sense ~strict e =
    let refinement = match refinable_lvalue e with
    | None, _ -> None
    | Some name, t ->
        let pred = if strict then VoidP else MaybeP in
        Some (name, t, pred, sense)
    in
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* a wrapper around `condition` (which is a wrapper around `expression`) that
     evaluates `expr`. if this is a sentinel property check (determined by
     a strict equality check against a member expression `_object.prop_name`),
     then also returns the refinement of `_object`.

     this is used by other tests such as `bool_test` such that if given
     `foo.bar === false`, `foo.bar` is refined to be `false` (by `bool_test`)
     and `foo` is refined to eliminate branches that don't have a `false` bar
     property (by this function). *)
  let condition_of_maybe_sentinel cx type_params_map ~sense ~strict expr val_t =
    match strict, expr with
    | true,
      (expr_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc,
          { Identifier.name = prop_name; _ });
        _
      }) ->

      (* use `expression` instead of `condition` because `_object` is the object
         in a member expression; if it itself is a member expression, it must
         exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here as
         `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
      let obj_t = expression cx type_params_map _object in

      let prop_reason = mk_reason (spf "property `%s`" prop_name) prop_loc in
      Flow.flow cx (obj_t, HasPropT (prop_reason, None, TypeTerm.Literal prop_name));

      let expr_reason = mk_reason (spf "property `%s`" prop_name) expr_loc in
      let prop_t = match Refinement.get cx expr expr_reason with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else get_prop ~is_cond:true cx expr_reason obj_t (prop_reason, prop_name)
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      let refinement = match Refinement.key _object with
      | None -> None
      | Some name ->
          let pred = LeftP (SentinelProp prop_name, val_t) in
          Some (name, obj_t, pred, sense)
      in
      prop_t, refinement
    | _ ->
      condition cx type_params_map expr, None
  in

  let literal_test loc ~strict ~sense expr val_t pred =
    let t, sentinel_refinement = condition_of_maybe_sentinel cx type_params_map
      ~sense ~strict expr val_t in
    let refinement = if strict then Refinement.key expr else None in
    let out = match refinement with
    | Some name -> result (BoolT.at loc) name t pred sense
    | None -> empty_result (BoolT.at loc)
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  (* inspect a typeof equality test *)
  let typeof_test loc sense arg typename str_loc =
    match refinable_lvalue arg with
    | Some name, t ->
        let pred = match typename with
        | "boolean" -> Some BoolP
        | "function" -> Some FunP
        | "number" -> Some NumP
        | "object" -> Some ObjP
        | "string" -> Some StrP
        | "undefined" -> Some VoidP
        | _ -> None
        in
        begin match pred with
        | Some pred -> result BoolT.t name t pred sense
        | None ->
          FlowError.add_warning cx (str_loc, [
            spf "string literal `%s`" typename;
            "This value is not a valid `typeof` return value"
          ]);
          empty_result (BoolT.at loc)
        end
    | None, _ -> empty_result (BoolT.at loc)
  in

  let sentinel_prop_test loc ~sense ~strict expr val_t =
    let _, sentinel_refinement = condition_of_maybe_sentinel
      cx type_params_map ~sense ~strict expr val_t in
    let out = empty_result (BoolT.at loc) in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  let eq_test loc ~sense ~strict left right =
    match left, right with
    (* typeof expr ==/=== string *)
    (* this must happen before the case below involving Literal.String in order
       to match anything. *)
    | (_, Expression.Unary { Unary.operator = Unary.Typeof; argument; _ }),
      (str_loc, Expression.Literal { Literal.value = Literal.String s; _ })
    | (str_loc, Expression.Literal { Literal.value = Literal.String s; _ }),
      (_, Expression.Unary { Unary.operator = Unary.Typeof; argument; _ })
      ->
        typeof_test loc sense argument s str_loc

    (* special case equality relations involving booleans *)
    | (_, Expression.Literal { Literal.value = Literal.Boolean lit; _})
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.Boolean lit; _})
      as value)
      ->
        let val_t = expression cx type_params_map value in
        literal_test loc ~sense ~strict expr val_t (SingletonBoolP lit)

    (* special case equality relations involving strings *)
    | (_, Expression.Literal { Literal.value = Literal.String lit; _})
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.String lit; _})
      as value)
      ->
        let val_t = expression cx type_params_map value in
        literal_test loc ~sense ~strict expr val_t (SingletonStrP lit)

    (* special case equality relations involving numbers *)
    | (_, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value)
      ->
        let val_t = expression cx type_params_map value in
        literal_test loc ~sense ~strict expr val_t (SingletonNumP (lit, raw))

    (* TODO: add Type.predicate variant that tests number equality *)

    (* expr op null *)
    | (_, Expression.Literal { Literal.value = Literal.Null; _ }), expr
    | expr, (_, Expression.Literal { Literal.value = Literal.Null; _ })
      ->
        null_test loc ~sense ~strict expr

    (* expr op undefined *)
    | (_, Identifier (_, { Identifier.name = "undefined"; _ })), expr
    | expr, (_, Identifier (_, { Identifier.name = "undefined"; _ }))
      ->
        undef_test loc ~sense ~strict expr

    (* expr op void(...) *)
    | (_, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg), expr
    | expr, (_, Unary ({ Unary.operator = Unary.Void; _ }) as void_arg)
      ->
        ignore (expression cx type_params_map void_arg);
        undef_test loc ~sense ~strict expr

    (* fallback case for equality relations involving sentinels (this should be
       lower priority since it refines the object but not the property) *)
    | (_, Expression.Member _ as expr), value
    | value, (_, Expression.Member _ as expr)
      ->
        let value_t = expression cx type_params_map value in
        sentinel_prop_test loc ~sense ~strict expr value_t

    (* for all other cases, walk the AST but always return bool *)
    | expr, value ->
        ignore (expression cx type_params_map expr);
        ignore (expression cx type_params_map value);
        empty_result (BoolT.at loc)
  in

  let mk_and map1 map2 = Scope.KeyMap.merge
    (fun _ p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> Some p
      | (Some p1, Some p2) -> Some (AndP(p1,p2))
    )
    map1 map2
  in

  let mk_or map1 map2 = Scope.KeyMap.merge
    (fun _ p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some _, None)
      | (None, Some _) -> None
      | (Some p1, Some p2) -> Some (OrP(p1,p2))
    )
    map1 map2
  in

  (* main *)
  match e with

  (* member expressions *)
  | loc, Member {
      Member._object;
      property = Member.PropertyIdentifier (prop_loc,
        { Identifier.name = prop_name; _ });
        _
      }
    ->
      let obj_t = match _object with
      | _, Identifier ( super_loc, { Ast.Identifier.name = "super"; _ }) ->
          super_ cx (mk_reason (spf "property `%s`" prop_name) super_loc)
      | _ ->
          (* use `expression` instead of `condition` because `_object` is the
             object in a member expression; if it itself is a member expression,
             it must exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here
             as `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
          expression cx type_params_map _object in

      let expr_reason = mk_reason (spf "property `%s`" prop_name) loc in
      let prop_reason = mk_reason (spf "property `%s`" prop_name) prop_loc in
      let t = match Refinement.get cx e expr_reason with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else get_prop ~is_cond:true cx expr_reason obj_t (prop_reason, prop_name)
      in

      let out = match Refinement.key e with
      | Some name -> result t name t ExistsP true
      | None -> empty_result t
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      begin match Refinement.key _object with
      | Some name ->
        out |> add_predicate name obj_t (PropExistsP prop_name) true
      | None ->
        out
      end

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let expr = expression cx type_params_map e in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result expr name expr ExistsP true
      | None, _ -> empty_result expr
    )

  (* expr instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      match refinable_lvalue left with
      | Some name, t ->
          let right_t = expression cx type_params_map right in
          let pred = LeftP (InstanceofTest, right_t) in
          result BoolT.t name t pred true
      | None, _ ->
          empty_result BoolT.t
    )

  (* expr op expr *)
  | loc, Binary { Binary.operator = Binary.Equal; left; right; } ->
      eq_test loc ~sense:true ~strict:false left right
  | loc, Binary { Binary.operator = Binary.StrictEqual; left; right; } ->
      eq_test loc ~sense:true ~strict:true left right
  | loc, Binary { Binary.operator = Binary.NotEqual; left; right; } ->
      eq_test loc ~sense:false ~strict:false left right
  | loc, Binary { Binary.operator = Binary.StrictNotEqual; left; right; } ->
      eq_test loc ~sense:false ~strict:true left right

  (* Array.isArray(expr) *)
  | _, Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_,
          { Identifier.name = "Array"; _ });
        property = Member.PropertyIdentifier (_,
          { Identifier.name = "isArray"; _ });
        _ };
      arguments = [Expression arg]
    } -> (
      match refinable_lvalue arg with
      | Some name, t ->
          result BoolT.t name t ArrP true
      | None, _ ->
          empty_result BoolT.t
    )

  (* test1 && test2 *)
  | loc, Logical {
      Logical.operator = Logical.And;
      left = (left_loc, _) as left;
      right = (right_loc, _) as right;
    } ->
      let reason = mk_reason "&&" loc in
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx type_params_map left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx reason map1 xts1
        (fun () -> predicates_of_condition cx type_params_map right)
      in
      (
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (t1, AndT (reason, (left_loc, right_loc), t2, t));
        ),
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        Scope.KeyMap.union xts1 xts2
      )

  (* test1 || test2 *)
  | loc, Logical {
      Logical.operator = Logical.Or;
      left = (left_loc, _) as left;
      right = (right_loc, _) as right;
    } ->
      let reason = mk_reason "||" loc in
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx type_params_map left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx reason not_map1 xts1
        (fun () -> predicates_of_condition cx type_params_map right)
      in
      (
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (t1, OrT (reason, (left_loc, right_loc), t2, t));
        ),
        mk_or map1 map2,
        mk_and not_map1 not_map2,
        Scope.KeyMap.union xts1 xts2
      )

  (* !test *)
  | loc, Unary { Unary.operator = Unary.Not; argument; _ } ->
      let (_, map, not_map, xts) =
        predicates_of_condition cx type_params_map argument in
      (BoolT.at loc, not_map, map, xts)

  (* ids *)
  | _, This
  | _, Identifier _
  | _, Member _ -> (
      match refinable_lvalue e with
      | Some name, t -> result t name t ExistsP true
      | None, t -> empty_result t
    )

  (* fallthrough case: evaluate test expr, no refinements *)
  | e ->
      empty_result (expression cx type_params_map e)
))

(* Conditional expressions are checked like expressions, except that property
   accesses are allowed even when such properties do not exist. This
   accommodates the common JavaScript idiom of testing for the existence of a
   property before using that property. *)
and condition cx type_params_map e =
  expression ~is_cond:true cx type_params_map e

(* Property lookups become non-strict when processing conditional expressions
   (see above).

   TODO: It should be possible to factor the processing of LHS / reference
   expressions out of `expression`, somewhat like what assignment_lhs does. That
   would make everything involving Refinement be in the same place.
*)
and get_prop ~is_cond cx reason tobj (prop_reason, name) =
  Flow.mk_tvar_where cx reason (fun t ->
    let get_prop_u =
      if is_cond
      then LookupT (reason, None, [], name, AnyWithUpperBoundT t)
      else GetPropT (reason, (prop_reason, name), t)
    in
    Flow.flow cx (tobj, get_prop_u)
  )

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx
  type_params_map loc prop_loc expr obj_t m args_ = Ast.Expression.(
  let reason = mk_reason (spf "Object.%s" m) loc in
  match (m, args_) with
  | ("create", [ Expression e ]) ->
    let proto = expression cx type_params_map e in
    Flow.mk_object_with_proto cx reason proto

  | ("create", [ Expression e;
                 Expression (_, Object { Object.properties }) ]) ->
    let proto = expression cx type_params_map e in
    let pmap = prop_map_of_object cx type_params_map properties in
    let map = pmap |> SMap.mapi (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      Flow.mk_tvar_where cx reason (fun tvar ->
        Flow.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
      )
    ) in
    Flow.mk_object_with_map_proto cx reason map proto

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let arr_reason = mk_reason "array type" loc in
    let o = expression cx type_params_map e in
    ArrT (arr_reason,
      Flow.mk_tvar_where cx arr_reason (fun tvar ->
        let keys_reason = prefix_reason "element of " reason in
        Flow.flow cx (o, GetKeysT (keys_reason, tvar));
      ),
      []
    )

  | ("defineProperty", [ Expression e;
                         Expression (ploc, Ast.Expression.Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ });
                         Expression config ]) ->
    let o = expression cx type_params_map e in
    let spec = expression cx type_params_map config in
    let tvar = Flow.mk_tvar cx reason in
    let prop_reason = mk_reason (spf "property `%s`" x) ploc in
    Flow.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
    Flow.flow cx (o, SetPropT (reason, (prop_reason, x), tvar));
    o

  | ("defineProperties", [ Expression e;
                         Expression (_, Object { Object.properties }) ]) ->
    let o = expression cx type_params_map e in
    let pmap = prop_map_of_object cx type_params_map properties in
    pmap |> SMap.iter (fun x spec ->
      let reason = prefix_reason (spf ".%s of " x) reason in
      let tvar = Flow.mk_tvar cx reason in
      Flow.flow cx (spec, GetPropT(reason, (reason, "value"), tvar));
      Flow.flow cx (o, SetPropT (reason, (reason, x), tvar));
    );
    o

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | ("freeze", [Expression ((arg_loc, Object _) as e)]) ->
    let arg_t = expression cx type_params_map e in

    let reason_arg = mk_reason "frozen object" arg_loc in
    let arg_t = Flow.mk_tvar_where cx reason_arg (fun tvar ->
      Flow.flow cx (arg_t, ObjFreezeT (reason_arg, tvar));
    ) in

    method_call cx loc prop_loc (expr, obj_t, m) [arg_t]

  (* TODO *)
  | (_, args) ->
    let argts = List.map (expression_or_spread cx type_params_map) args in
    method_call cx loc prop_loc (expr, obj_t, m) argts
)

and extract_setter_type = function
  | FunT (_, _, _, { params_tlist = [param_t]; _; }) -> param_t
  | _ ->  failwith "Setter property with unexpected type"

and extract_getter_type = function
  | FunT (_, _, _, { return_t; _; }) -> return_t
  | _ -> failwith "Getter property with unexpected type"

and extract_class_name class_loc  = Ast.Class.(function {id; _;} ->
  match id with
  | Some(name_loc, {Ast.Identifier.name; _;}) -> (name_loc, name)
  | None -> (class_loc, "<<anonymous class>>")
)

and mk_class cx type_params_map loc reason c =
  let self = Flow.mk_tvar cx reason in
  let class_sig =
    Cls.Class.mk_sig cx type_params_map expression loc reason self c in
  class_sig |> Cls.Class.generate_tests cx (fun test_class_sig ->
    Cls.Class.check_super cx test_class_sig;
    Cls.toplevels cx test_class_sig
      ~decls:toplevel_decls
      ~stmts:toplevels
      ~expr:expression
  );
  let class_t = Cls.Class.classtype cx class_sig in
  Flow.unify cx self class_t;
  class_t

(* Given a function declaration and types for `this` and `super`, extract a
   signature consisting of type parameters, parameter types, parameter names,
   and return type, check the body against that signature by adding `this`
   and super` to the environment, and return the signature. *)
and function_decl id cx type_params_map sig_this reason func body_this super =
  let func_sig = Func_sig.mk_function cx type_params_map reason sig_this func in

  let this, super =
    let new_entry t = Scope.Entry.new_var ~loc:(loc_of_t t) t in
    new_entry body_this, new_entry super
  in

  let save_return = Abnormal.clear_saved Abnormal.Return in
  let save_throw = Abnormal.clear_saved Abnormal.Throw in
  func_sig |> Func_sig.generate_tests cx (
    Func_sig.toplevels id cx this super
      ~decls:toplevel_decls
      ~stmts:toplevels
      ~expr:expression
  );
  ignore (Abnormal.swap_saved Abnormal.Return save_return);
  ignore (Abnormal.swap_saved Abnormal.Throw save_throw);

  func_sig

(* Switch back to the declared type for an internal name. *)
and define_internal cx reason x =
  let ix = internal_name x in
  let opt = Env.get_var_declared_type cx ix reason in
  ignore Env.(set_var cx ix (Flow.filter_optional cx reason opt) reason)

(* Process a function definition, returning a (polymorphic) function type. *)
and mk_function id cx type_params_map reason func =
  reject_explicit_this cx
    "`this` pseudo-parameters are not allowed on functions" func;
  let this = Flow.mk_tvar cx (replace_reason "this" reason) in
  (* Normally, functions do not have access to super. *)
  let super = MixedT (
    replace_reason "empty super object" reason,
    Mixed_everything
  ) in
  Func_sig.functiontype cx
    (function_decl id cx type_params_map this reason func this super)

(* Process an arrow function, returning a (polymorphic) function type. *)
and mk_arrow cx type_params_map reason func =
  reject_explicit_this cx
    "`this` pseudo-parameters are not allowed on arrow functions" func;
  let this = this_ cx reason in
  let super = super_ cx reason in
  let {Ast.Function.id; _} = func in
  (* Use `dummy_this` in the function's signature and, ultimately, type. The
     call to function_decl does the necessary checking of `this` in the body of
     the function. Since any `this`es in the arrow function are lexically bound,
     we want to avoid re-binding `this` to objects through which the function
     may be called, hence the `dummy_this`. *)
  Func_sig.functiontype cx
    (function_decl id cx type_params_map Flow.dummy_this reason func this super)

(* This function is around for the sole purpose of modeling some method-like
   behaviors of non-ES6 React classes. It is otherwise deprecated. *)
and mk_method cx type_params_map reason func this =
  let super = MixedT (reason, Mixed_everything) in
  let id = None in
  Func_sig.methodtype_DEPRECATED
    (function_decl id cx type_params_map this reason func this super)
