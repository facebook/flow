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

module Ast = Spider_monkey_ast
module Anno = Type_annotation
module Flow = Flow_js
module Iface_sig = Class_sig (* same thing, mo'less *)

open Utils_js
open Reason
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

let ident_name (_, name) = name

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

let function_desc ~async ~generator =
  match async, generator with
  | true, true -> RAsyncGenerator
  | true, false -> RAsync
  | false, true -> RGenerator
  | false, false -> RNormal

(************)
(* Visitors *)
(************)

(********************************************************************
 * local inference preliminary pass: traverse AST, collecting
 * declarations and populating variable environment (scope stack)
 * in prep for main pass
 ********************************************************************)

let rec variable_decl cx entry = Ast.Statement.(
  let value_kind, bind = match entry.VariableDeclaration.kind with
    | VariableDeclaration.Const ->
      Scope.Entry.(Const ConstVarBinding), Env.bind_const
    | VariableDeclaration.Let ->
      Scope.(Entry.Let Entry.LetVarBinding), Env.bind_let
    | VariableDeclaration.Var -> Scope.Entry.Var, Env.bind_var
  in

  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in

  let declarator = Ast.(function
    | (loc, Pattern.Identifier { Pattern.Identifier.name=(id_loc, name); _ }) ->
      let desc = RCustom (spf "%s `%s`" str_of_kind name) in
      let r = mk_reason desc id_loc in
      (* A variable declaration may have a type annotation, but trying to
         resolve the type annotation now may lead to errors, since in general it
         may contain types that will be declared later in this scope. So for
         now, we create a tvar that will serve as the declared type. Later, we
         will resolve the type annotation and unify it with this tvar. *)
      let t = Flow_js.mk_tvar cx r in
      Hashtbl.replace (Context.type_table cx) loc t;
      bind cx name t r
    | (loc, _) as p ->
      let pattern_name = internal_pattern_name loc in
      let desc = RCustom (spf "%s _" str_of_kind) in
      let r = mk_reason desc loc in
      let typeAnnotation = type_of_pattern p in
      let t = typeAnnotation |>
        (* TODO: delay resolution of type annotation like above? *)
        Anno.mk_type_annotation cx SMap.empty r in
      bind cx pattern_name t r;
      let expr _ _ = EmptyT.t in (* don't eval computed property keys *)
      destructuring cx ~expr t None None p ~f:(fun loc name _default t ->
        let t = match typeAnnotation with
        | None -> t
        | Some _ ->
          let r = repos_reason loc r in
          EvalT (t, DestructuringT (r, Become), mk_id())
        in
        Hashtbl.replace (Context.type_table cx) loc t;
        bind cx name t r
      )
  ) in

  VariableDeclaration.(entry.declarations |> List.iter (function
    | (_, { Declarator.id; _; }) -> declarator id
  ));
)

and toplevel_decls cx =
  List.iter (statement_decl cx)

(* TODO: detect structural misuses abnormal control flow constructs *)
and statement_decl cx = Ast.Statement.(

  let block_body cx { Block.body } =
    Env.in_lex_scope cx (fun () ->
      toplevel_decls cx body
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
      statement_decl cx consequent;
      (match alternate with
        | None -> ()
        | Some st -> statement_decl cx st
      )

  | (_, Labeled { Labeled.body; _ }) ->
      statement_decl cx body

  | (_, Break _) -> ()

  | (_, Continue _) -> ()

  | (_, With _) ->
      (* TODO disallow or push vars into env? *)
      ()
  | (_, TypeAlias { TypeAlias.id = (name_loc, name); _ } ) ->
      let r = DescFormat.type_reason name name_loc in
      let tvar = Flow.mk_tvar cx r in
      Env.bind_type cx name tvar r

  | (_, Switch { Switch.cases; _ }) ->
      Env.in_lex_scope cx (fun () ->
        cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
          toplevel_decls cx consequent
        )
      )

  | (_, Return _) -> ()

  | (_, Throw _) -> ()

  | (_, Try { Try.block = (_, b); handler; finalizer }) ->
      block_body cx b;

      (match handler with
        | None -> ()
        | Some (_, h) -> catch_clause cx h
      );

      (match finalizer with
        | None -> ()
        | Some (_, b) -> block_body cx b
      )

  | (_, While { While.body; _ }) ->
      statement_decl cx body

  | (_, DoWhile { DoWhile.body; _ }) ->
      statement_decl cx body

  | (_, For { For.init; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match init with
          | Some (For.InitDeclaration (_, decl)) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, ForIn { ForIn.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForIn.LeftDeclaration (_, decl) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, ForOf { ForOf.left; body; _ }) ->
      Env.in_lex_scope cx (fun () ->
        (match left with
          | ForOf.LeftDeclaration (_, decl) ->
              variable_decl cx decl
          | _ -> ()
        );
        statement_decl cx body
      )

  | (_, Debugger) -> ()

  | (loc, FunctionDeclaration { Ast.Function.id; async; generator; _ }) ->
      (match id with
      | Some (_, name) ->
        let desc = RFunction (function_desc ~async ~generator) in
        let r = mk_reason desc loc in
        let tvar = Flow.mk_tvar cx r in
        Env.bind_fun cx name tvar r
      | None ->
        failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
      )

  | (loc, DeclareVariable { DeclareVariable.
      id = (_, name);
      typeAnnotation;
    }) ->
      let r = mk_reason (RCustom (spf "declare %s" name)) loc in
      let t = Anno.mk_type_annotation cx SMap.empty r typeAnnotation in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env.bind_declare_var cx name t r

  | (loc, DeclareFunction { DeclareFunction.
      id = (_, name) as id;
      typeAnnotation;
      predicate}) ->
      (match declare_function_to_function_declaration
        cx id typeAnnotation predicate with
      | None ->
          let r = mk_reason (RCustom (spf "declare %s" name)) loc in
          let t =
            Anno.mk_type_annotation cx SMap.empty r (Some typeAnnotation) in
          Hashtbl.replace (Context.type_table cx) loc t;
          Env.bind_declare_fun cx name t r
      | Some func_decl ->
          statement_decl cx (loc, func_decl)
      )

  | (_, VariableDeclaration decl) ->
      variable_decl cx decl

  | (_, ClassDeclaration { Ast.Class.id; _ }) -> (
      match id with
      | Some (name_loc, name) ->
        let r = mk_reason (RCustom (spf "class `%s`" name)) name_loc in
        let tvar = Flow.mk_tvar cx r in
        Env.bind_implicit_let Scope.Entry.ClassNameBinding cx name tvar r
      | None -> ()
    )

  | (loc, DeclareClass { Interface.id = (_, name); _ })
  | (loc, InterfaceDeclaration { Interface.id = (_, name); _ }) as stmt ->
      let is_interface = match stmt with
      | (_, InterfaceDeclaration _) -> true
      | _ -> false in
      let r = mk_reason (RCustom (spf "class `%s`" name)) loc in
      let tvar = Flow.mk_tvar cx r in
      (* interface is a type alias, declare class is a var *)
      if is_interface
      then Env.bind_type cx name tvar r
      else Env.bind_declare_var cx name tvar r

  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, name)
      | DeclareModule.Literal (_, {
          Ast.Literal.value = Ast.Literal.String name; _;
        }) ->
        name
      | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
      let r = mk_reason (RCustom (spf "module `%s`" name)) loc in
      let t = Flow.mk_tvar cx r in
      Hashtbl.replace (Context.type_table cx) loc t;
      Env.bind_declare_var cx (internal_module_name name) t r

  | _,
    DeclareExportDeclaration {
      DeclareExportDeclaration.default; declaration; _
    } ->
        DeclareExportDeclaration.(match declaration with
        | Some (Variable (loc, v)) ->
            statement_decl cx (loc, DeclareVariable v)
        | Some (Function (loc, f)) ->
            statement_decl cx (loc, DeclareFunction f)
        | Some (Class (loc, c)) ->
            statement_decl cx (loc, DeclareClass c)
        | Some (DefaultType _) -> ()
        | Some (NamedType (loc, t)) ->
            statement_decl cx (loc, TypeAlias t)
        | Some (Interface (loc, i)) ->
            statement_decl cx (loc, InterfaceDeclaration i)
        | None ->
            if not default
            then ()
            else failwith (
              "Parser Error: declare export default must always have an " ^
              "associated declaration or type!"
            )
        )

  | (_, DeclareModuleExports _) -> ()

  | (_, ExportNamedDeclaration { ExportNamedDeclaration.declaration; _ }) -> (
      match declaration with
      | Some stmt -> statement_decl cx stmt
      | None -> ()
    )
  | _, ExportDefaultDeclaration { ExportDefaultDeclaration.declaration; _ } -> (
      match declaration with
      | ExportDefaultDeclaration.Declaration stmt ->
        statement_decl cx (nameify_default_export_decl stmt)
      | ExportDefaultDeclaration.Expression _ -> ()
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
                (local_name, mk_reason (RCustom reason_str) loc)
              | None ->
                let reason_str = spf "%s { %s }" import_str remote_name in
                (remote_name, mk_reason (RCustom reason_str) (fst remote))
            ) in
            (local_name, reason)
          | ImportDefaultSpecifier local ->
            let local_name = ident_name local in
            let reason_str =
              spf "%s %s from %S" import_str local_name module_name
            in
            let reason = mk_reason (RCustom reason_str) (fst local) in
            (local_name, reason)
          | ImportNamespaceSpecifier (_, local) ->
            let local_name = ident_name local in
            let reason_str =
              spf "%s * as %s from %S" import_str local_name module_name
            in
            let reason = mk_reason (RCustom reason_str) (fst local) in
            (local_name, reason)
        ) in
        let tvar = Flow.mk_tvar cx reason in
        let state = Scope.State.Initialized in
        if isType
        then Env.bind_type ~state cx local_name tvar reason
        else Env.bind_import cx local_name tvar reason
      )
)

(***************************************************************
 * local inference main pass: visit AST statement list, calling
 * flow to check types/create graphs for merge-time checking
 ***************************************************************)

and toplevels cx stmts =
  let stmts = List.filter Ast.Statement.(function
    | (_, Empty) -> false
    | _ -> true
  ) stmts
  in
  let n = ref 0 in
  match Abnormal.catch_control_flow_exception (fun () ->
    stmts |> List.iter (fun stmt ->
      statement cx stmt;
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
      let warn_unreachable loc = Flow_error.(
        add_output cx (EUnreachable loc)
      ) in
      let rec drop n lst = match (n, lst) with
        | (_, []) -> []
        | (0, l) -> l
        | (x, _ :: t) -> drop (pred x) t
      in
      let trailing = drop uc stmts in
      trailing |> List.iter Ast.Statement.(fun stmt ->
        match stmt with
        (* function declarations are hoisted, so not unreachable *)
        | (_, FunctionDeclaration _ ) -> statement cx stmt;
        (* variable declarations are hoisted, but associated assignments are
           not, so skip variable declarations with no assignments.
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

and statement cx = Ast.Statement.(

  let variables cx { VariableDeclaration.declarations; kind } =
    List.iter (variable cx kind) declarations
  in

  let interface cx loc structural i =
    let {Interface.id = (_, name); _} = i in
    let reason = DescFormat.instance_reason name loc in
    let self = Flow.mk_tvar cx reason in
    let iface_sig =
      Iface_sig.mk_interface cx loc reason structural self i
    in
    iface_sig |> Iface_sig.generate_tests cx (fun iface_sig ->
      Iface_sig.check_super cx iface_sig
    );
    let interface_t = Iface_sig.classtype ~check_polarity:false cx iface_sig in
    Flow.unify cx self interface_t;
    Hashtbl.replace (Context.type_table cx) loc interface_t;
    (* interface is a type alias, declare class is a var *)
    Env.(if structural then init_type else init_var ~has_anno:false)
      cx name interface_t reason
  in

  let catch_clause cx { Try.CatchClause.param; body = (_, b) } =
    Ast.Pattern.(match param with
      | loc, Identifier {
          Identifier.name = (_, name); typeAnnotation = None; _;
        } ->
          let r = mk_reason (RCustom "catch") loc in
          let t = Flow.mk_tvar cx r in

          Hashtbl.replace (Context.type_table cx) loc t;

          (match Env.in_lex_scope cx (fun () ->
            Scope.(Env.bind_implicit_let
              ~state:State.Initialized Entry.CatchParamBinding cx name t r);

            Abnormal.catch_control_flow_exception (fun () ->
              toplevel_decls cx b.Block.body;
              toplevels cx b.Block.body
            )
          ) with
          | Some exn -> Abnormal.throw_control_flow_exception exn
          | None -> ()
          )

      | loc, Identifier _ ->
          Flow_error.(add_output cx
            (EUnsupportedSyntax (loc, CatchParameterAnnotation)))

      | loc, _ ->
          Flow_error.(add_output cx
            (EUnsupportedSyntax (loc, CatchParameterDeclaration)))
    )
  in

  function

  | (_, Empty) -> ()

  | (_, Block { Block.body }) ->
      Env.in_lex_scope cx (fun () ->
        toplevel_decls cx body;
        toplevels cx body
      )

  | (_, Expression { Expression.expression = e }) ->
      ignore (expression cx e)

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let reason = mk_reason (RCustom "if") loc in

      let _, preds, not_preds, xts =
        predicates_of_condition cx test in

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
        (fun () -> statement cx consequent)
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
            (fun () -> statement cx st)
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

      | None, Some _
      | Some _, None
      | Some _, Some _ ->
        Env.merge_env cx reason (start_env, then_env, else_env) newset;
        start_env

      | None, None ->
        (* if neither branch has abnormal flow, then refinements that happen in
           the branches should be forgotten since the original type covers
           all of the options. *)
        Env.merge_env cx reason
          (start_env, then_env, else_env)
          (Changeset.exclude_refines newset);
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

  | (_, Labeled { Labeled.label = _, name; body }) ->
      (match body with
      | (loc, While _)
      | (loc, DoWhile _)
      | (loc, For _)
      | (loc, ForIn _)
        ->
        let reason = mk_reason (RCustom "label") loc in
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
              fun () -> statement cx body)));

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
              fun () -> statement cx body)));

        let newset = Changeset.merge oldset in
        if Abnormal.swap_saved (Abnormal.Break label) save_break <> None
        then Env.havoc_vars newset
      )

  | (loc, Break { Break.label }) ->
      (* save environment at unlabeled breaks, prior to activation clearing *)
      let label_opt, env = match label with
        | None -> None, Env.(clone_env (peek_env ()))
        | Some (_, name) -> Some name, []
      in
      Env.reset_current_activation (mk_reason (RCustom "break") loc);
      Abnormal.save_and_throw (Abnormal.Break label_opt) ~env

  | (loc, Continue { Continue.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, name) -> Some name
      in
      Env.reset_current_activation (mk_reason (RCustom "continue") loc);
      Abnormal.save_and_throw (Abnormal.Continue label_opt)

  | (_, With _) ->
      (* TODO or disallow? *)
      ()

  | (loc, TypeAlias {TypeAlias.id=(name_loc, name); typeParameters; right;}) ->
      let r = DescFormat.type_reason name name_loc in
      let typeparams, typeparams_map =
        Anno.mk_type_param_declarations cx typeParameters in
      let t = Anno.convert cx typeparams_map right in
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
    ignore (expression cx discriminant);

    (* switch body is a single lexical scope *)
    Env.in_lex_scope cx (fun () ->

      (* save incoming env state, clear changeset *)
      let incoming_changes = Changeset.clear () in
      let incoming_env = Env.peek_env () in
      let incoming_depth = List.length incoming_env in

      (* set up all bindings *)
      cases |> List.iter (fun (_, { Switch.Case.consequent; _ }) ->
        toplevel_decls cx consequent
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
          mk_reason (RCustom "default") loc,
          (EmptyT.at loc, Key_map.empty, Key_map.empty, Key_map.empty)
        | Some expr ->
          mk_reason (RCustom "case") loc,
          let fake_ast = loc, Ast.Expression.(Binary {
            Binary.operator = Binary.StrictEqual;
            left = discriminant; right = expr
          }) in
          predicates_of_condition cx fake_ast
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
          fun () -> toplevels cx consequent
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
            Flow_error.(add_output cx (EInternal (loc, BreakEnvMissingForCase)))
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
      let reason = mk_reason (RCustom "switch") switch_loc in
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
      let reason = mk_reason (RCustom "return") loc in
      let ret =
        Env.get_var cx (internal_name "return") reason
      in
      let t = match argument with
        | None -> VoidT.at loc
        | Some expr ->
          if Env.in_predicate_scope () then
            let (t, p_map, n_map, _) = predicates_of_condition cx expr in
            let pred_reason = replace_reason (fun desc ->
              RPredicateOf desc
            ) reason in
            OpenPredT (pred_reason, t, p_map, n_map)
          else
            expression cx expr
      in
      let t = match Env.var_scope_kind () with
      | Scope.Async ->
        (* Convert the return expression's type T to Promise<T>. If the
         * expression type is itself a Promise<T>, ensure we still return
         * a Promise<T> via Promise.resolve. *)
        let reason = mk_reason (RCustom "async return") loc in
        Flow.get_builtin_typeapp cx reason "Promise" [
          Flow.mk_tvar_derivable_where cx reason (fun tvar ->
            let funt = Flow.get_builtin cx "$await" reason in
            let callt = Flow.mk_functiontype [t] tvar in
            let reason = repos_reason (loc_of_reason (reason_of_t t)) reason in
            Flow.flow cx (funt, CallT (reason, callt))
          )
        ]
      | Scope.Generator ->
        (* Convert the return expression's type R to Generator<Y,R,N>, where
         * Y and R are internals, installed earlier. *)
        let reason = mk_reason (RCustom "generator return") loc in
        Flow.get_builtin_typeapp cx reason "Generator" [
          Env.get_var cx (internal_name "yield") reason;
          Flow.mk_tvar_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_var cx (internal_name "next") reason
        ]
      | Scope.AsyncGenerator ->
        let reason = mk_reason (RCustom "async generator return") loc in
        Flow.get_builtin_typeapp cx reason "AsyncGenerator" [
          Env.get_var cx (internal_name "yield") reason;
          Flow.mk_tvar_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_var cx (internal_name "next") reason
        ]
      | _ -> t
      in
      Flow.flow cx (t, UseT (FunReturn, ret));
      Env.reset_current_activation reason;
      Abnormal.save_and_throw Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      let reason = mk_reason (RCustom "throw") loc in
      ignore (expression cx argument);
      Env.reset_current_activation reason;
      Abnormal.save_and_throw Abnormal.Throw

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
  | (loc, Try { Try.block = (_, b); handler; finalizer }) ->
      let reason = mk_reason (RCustom "try") loc in
      let oldset = Changeset.clear () in

      (* save ref to initial env and swap in a clone *)
      let start_env = Env.peek_env () in
      Env.(update_env cx reason (clone_env start_env));

      let exception_try = Env.in_lex_scope cx (fun () ->
        Abnormal.catch_control_flow_exception (fun () ->
          toplevel_decls cx b.Block.body;
          toplevels cx b.Block.body
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
            toplevel_decls cx body;
            toplevels cx body
          )
        ) in

        (* 2. non-throwing finally case. *)
        Env.update_env cx reason nonthrow_finally_env;

        (* (exceptions will be the same in both cases) *)
        let _ = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx body;
            toplevels cx body
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

      let reason = mk_reason (RCustom "while") loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      (* generate loop test preds and their complements *)
      let _, preds, not_preds, orig_types =
        predicates_of_condition cx test in

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
        (fun () -> statement cx body));

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
      let reason = mk_reason (RCustom "do-while") loc in
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
          fun () -> statement cx body)
      ) in

      if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
      then Env.havoc_vars (Changeset.peek ());

      let _, preds, not_preds, xtypes =
        predicates_of_condition cx test in
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
        let reason = mk_reason (RCustom "for") loc in
        let save_break = Abnormal.clear_saved (Abnormal.Break None) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
        (match init with
          | None -> ()
          | Some (For.InitDeclaration (_, decl)) ->
              variable_decl cx decl;
              variables cx decl
          | Some (For.InitExpression expr) ->
              ignore (expression cx expr)
        );

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let do_env = Env.clone_env env in
        Env.update_env cx reason do_env;

        let _, preds, not_preds, xtypes = match test with
          | None ->
              EmptyT.at loc, Key_map.empty, Key_map.empty,
              Key_map.empty (* TODO: prune the "not" case *)
          | Some expr ->
              predicates_of_condition cx expr
        in

        let body_env = Env.clone_env do_env in
        Env.update_env cx reason body_env;
        let _ = Env.refine_with_preds cx reason preds xtypes in

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx body));

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars (Changeset.peek ());

        (match update with
          | None -> ()
          | Some expr ->
              ignore (expression cx expr)
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
      let reason = mk_reason (RCustom "for-in") loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      Flow.flow cx (expression cx right, AssertForInRHST reason);

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let body_env = Env.clone_env env in
        Env.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx right in
        let _ = Env.refine_with_preds cx reason preds xtypes in

        (match left with
          | ForIn.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              variable_decl cx decl;
              variable cx kind ~if_uninitialized:StrT.at vdecl

          | ForIn.LeftExpression (loc, Ast.Expression.Identifier ident) ->
              let name = ident_name ident in
              let reason = mk_reason (RCustom (spf "for..in `%s`" name)) loc in
              ignore Env.(set_var cx name (StrT.at loc) reason)

          | _ ->
              Flow_error.(add_output cx (EInternal (loc, ForInLHS)))
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx reason (env,body_env) newset;

        Env.update_env cx reason env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (loc, ForOf { ForOf.left; right; body; async; }) ->
      let reason = mk_reason (RCustom "for-of") loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let t = expression cx right in

      let element_tvar = Flow.mk_tvar cx reason in
      let o =
        let targs = [element_tvar; AnyT.at loc; AnyT.at loc] in
        if async then
          let reason = mk_reason
            (RCustom "async iteration expected on AsyncIterable") loc in
          Flow.get_builtin_typeapp cx reason "$AsyncIterable" targs
        else
          Flow.get_builtin_typeapp cx
            (mk_reason (RCustom "iteration expected on Iterable") loc)
            "$Iterable" targs
      in

      Flow.flow_t cx (t, o); (* null/undefined are NOT allowed *)

      Env.in_lex_scope cx (fun () ->

        let env =  Env.peek_env () in
        let oldset = Changeset.clear () in
        Env.widen_env cx reason;

        let body_env = Env.clone_env env in
        Env.update_env cx reason body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx right in
        let _ = Env.refine_with_preds cx reason preds xtypes in

        (match left with
          | ForOf.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              let repos_tvar loc =
                Flow.reposition cx (repos_reason loc reason) element_tvar
              in
              variable_decl cx decl;
              variable cx kind ~if_uninitialized:repos_tvar vdecl

          | ForOf.LeftExpression (loc, Ast.Expression.Identifier ident) ->
              let name = ident_name ident in
              let reason = mk_reason (RCustom (spf "for..of `%s`" name)) loc in
              ignore Env.(set_var cx name element_tvar reason)

          | _ ->
              Flow_error.(add_output cx (EInternal (loc, ForOfLHS)))
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx reason (env,body_env) newset;

        Env.update_env cx reason env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (_, Debugger) ->
      ()

  | (loc, FunctionDeclaration func) ->
      let {Ast.Function.id; _} = func in
      let reason = mk_reason (RFunction RNormal) loc in
      let fn_type = mk_function None cx reason func in
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
      | Some(_, name) ->
        Env.init_fun cx name fn_type reason
      | None -> ())

  | (_, DeclareVariable _) ->
      ()

  | (loc, DeclareFunction { DeclareFunction.
      id;
      typeAnnotation;
      predicate;
    }) ->
      (match declare_function_to_function_declaration
        cx id typeAnnotation predicate with
      | Some func_decl ->
          statement cx (loc, func_decl)
      | _ ->
          ())

  | (_, VariableDeclaration decl) ->
      variables cx decl

  | (class_loc, ClassDeclaration c) ->
      let (name_loc, name) = extract_class_name class_loc c in
      let reason = DescFormat.instance_reason name name_loc in
      Env.declare_implicit_let Scope.Entry.ClassNameBinding cx name reason;
      let class_t = mk_class cx class_loc reason c in
      Hashtbl.replace (Context.type_table cx) class_loc class_t;
      Env.init_implicit_let
        Scope.Entry.ClassNameBinding
        cx
        name
        ~has_anno:false
        class_t
        reason

  | (loc, DeclareClass decl) ->
    interface cx loc false decl

  | (loc, InterfaceDeclaration decl) ->
    interface cx loc true decl

  | (loc, DeclareModule { DeclareModule.id; body; kind; }) ->
    let name = match id with
    | DeclareModule.Identifier ident -> ident_name ident
    | DeclareModule.Literal (_, { Ast.Literal.
        value = Ast.Literal.String str;
        _;
      }) ->
        str
    | _ ->
        (* The only literals that we should see as module names are strings *)
        assert false in
    let _, { Ast.Statement.Block.body = elements } = body in

    let reason = mk_reason (RCustom (spf "module `%s`" name)) loc in
    let t = Env.get_var_declared_type cx (internal_module_name name) reason in

    let module_scope = Scope.fresh () in
    Env.push_var_scope cx module_scope;
    let outer_module_exports_kind = Context.module_kind cx in
    Context.set_module_kind cx (
      match kind with
      | DeclareModule.CommonJS loc -> Context.CommonJSModule (Some loc)
      | DeclareModule.ES _ -> Context.ESModule
    );
    Context.set_declare_module_t cx (Some t);

    toplevel_decls cx elements;
    toplevels cx elements;

    Context.set_declare_module_t cx None;
    Context.set_module_kind cx outer_module_exports_kind;
    Env.pop_var_scope ();

    (match kind with
      | DeclareModule.CommonJS kind_loc ->
        (**
         * TODO(jeffmo): `declare var exports` is deprecated (in favor of
         * `declare module.exports`). v0.25 retains support for it as
         * a transitionary grace period, but this will be removed as
         * early as v0.26.
         *)
        let legacy_exports = Scope.get_entry "exports" module_scope in
        let declared_module_exports =
          Scope.get_entry (internal_name "declare_module.exports") module_scope
        in

        let type_exports, cjs_module_exports =
          let open Scope in
          let open Entry in
          match legacy_exports, declared_module_exports with
          (* TODO: Eventually drop support for legacy "declare var exports" *)
          | Some (Value {specific=exports; _}), None
          | _, Some (Value {specific=exports; _}) ->
            let type_exports = SMap.fold (fun x entry acc ->
              match entry with
              | Value _ -> acc
              | Type {_type; _} -> SMap.add x _type acc
            ) module_scope.entries SMap.empty in
            type_exports, exports

          | _, Some (Type _) ->
            assert_false (
              "Internal Error: `declare module.exports` was created as a " ^
              "type binding. This should never happen!"
            )

          | Some (Type _), None
          | None, None ->
            let type_exports, value_exports = SMap.fold (fun x entry (ts, vs) ->
              match entry with
              | Value {specific; _} ->
                ts,
                SMap.add x (Property.field Neutral specific) vs
              | Type {_type; _} ->
                SMap.add x _type ts,
                vs
            ) module_scope.entries (SMap.empty, SMap.empty) in

            let reason = repos_reason kind_loc reason in
            let proto = ObjProtoT reason in

            type_exports,
            Flow.mk_object_with_map_proto cx reason value_exports proto
        in

        let module_t =
          mk_commonjs_module_t cx reason reason cjs_module_exports in
        let module_t = Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (module_t, ExportNamedT (reason, type_exports, t))
        ) in
        Flow.unify cx module_t t;
      | DeclareModule.ES _ ->
        Flow.flow_t cx (mk_module_t cx reason, t)
    )

  | (loc, DeclareExportDeclaration {
      DeclareExportDeclaration.default;
      DeclareExportDeclaration.declaration;
      DeclareExportDeclaration.specifiers;
      DeclareExportDeclaration.source;
    }) ->
      let open DeclareExportDeclaration in
      let export_info, export_kind =
        match declaration with
        | Some (Variable (loc, v)) ->
            let { DeclareVariable.id = (_, name); _; } = v in
            statement cx (loc, DeclareVariable v);
            [(spf "var %s" name, loc, name, None)], ExportValue
        | Some (Function (loc, f)) ->
            let { DeclareFunction.id = (_, name); _ } = f in
            statement cx (loc, DeclareFunction f);
            [(spf "function %s() {}" name, loc, name, None)], ExportValue
        | Some (Class (loc, c)) ->
            let { Interface.id = (name_loc, name); _; }
              = c in
            statement cx (loc, DeclareClass c);
            [(spf "class %s {}" name, name_loc, name, None)], ExportValue
        | Some (DefaultType (loc, t)) ->
            let _type = Anno.convert cx SMap.empty (loc, t) in
            [( "<<type>>", loc, "default", Some _type)], ExportValue
        | Some (NamedType (talias_loc, ({
            TypeAlias.
            id = (name_loc, name);
            _;
          } as talias))) ->
            statement cx (talias_loc, TypeAlias talias);
            [(spf "type %s = ..." name, name_loc, name, None)], ExportType
        | Some (Interface (loc, i)) ->
            let {Interface.id = (name_loc, name); _;} = i in
            statement cx (loc, InterfaceDeclaration i);
            [(spf "interface %s {}" name, name_loc, name, None)], ExportType
        | None ->
            [], ExportValue
      in

      export_statement cx loc ~default export_info specifiers source export_kind

  | (loc, DeclareModuleExports annot) ->
    let t = Anno.convert cx SMap.empty (snd annot) in

    if Context.declare_module_t cx <> None then (
      let name = internal_name "declare_module.exports" in
      let reason = mk_reason (RCustom "declare module.exports") loc in
      Env.bind_declare_var cx name t reason
    ) else (
      let reason = mk_reason (RCustom "declare module.exports") loc in
      set_module_kind cx reason (Context.CommonJSModule(Some loc));
      set_module_exports cx reason t
    )

  | (loc, ExportNamedDeclaration { ExportNamedDeclaration.
      declaration;
      specifiers;
      source;
      exportKind;
    }) ->
      let export_info = match declaration with
      | Some decl ->
          statement cx decl;
          (match decl with
          | _, FunctionDeclaration {Ast.Function.id = None; _} ->
            failwith (
              "Parser Error: Immediate exports of nameless functions can " ^
              "only exist for default exports!"
            )
          | loc, FunctionDeclaration {Ast.Function.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "function %s() {}" name, loc, name, None)]
          | _, ClassDeclaration {Ast.Class.id = None; _} ->
            failwith (
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

      | None -> [] in

      export_statement cx loc
        ~default:false export_info specifiers source exportKind

  | (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.
      declaration;
      exportKind;
    }) ->
      let export_info = match declaration with
      | ExportDefaultDeclaration.Declaration decl ->
          let decl = nameify_default_export_decl decl in
          statement cx decl;
          (match decl with
          | loc, FunctionDeclaration {Ast.Function.id = None; _} ->
            [("function() {}", loc, internal_name "*default*", None)]
          | loc, FunctionDeclaration {Ast.Function.id = Some ident; _} ->
            let name = ident_name ident in
            [(spf "function %s() {}" name, loc, name, None)]
          | loc, ClassDeclaration {Ast.Class.id = None; _} ->
            [("class {}", loc, internal_name "*default*", None)]
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

      | ExportDefaultDeclaration.Expression expr ->
          let expr_t = expression cx expr in
          [( "<<expression>>", fst expr, "default", Some expr_t)]
      in

      export_statement cx loc ~default:true export_info None None exportKind

  | (import_loc, ImportDeclaration import_decl) ->
    let open ImportDeclaration in
    Context.add_import_stmt cx import_decl;

    let module_name = (
      match (snd import_decl.source).Ast.Literal.value with
      | Ast.Literal.String value -> value
      | _ -> failwith (
          "Internal Parser Error: Invalid import source type! Must be a " ^
          "string literal."
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
        Context.add_imported_t cx local_name t;
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

          let (loc, local_name, import_reason, bind_reason) = (
            match local with
            | Some local ->
              let local_name = ident_name local in
              let import_reason =
                mk_reason (RCustom import_reason_str) (fst remote) in
              let bind_reason_str =
                spf "%s { %s as %s } from %S"
                  import_str
                  remote_name
                  local_name
                  module_name
              in
              let bind_loc = Loc.btwn (fst remote) (fst local) in
              let bind_reason = mk_reason (RCustom bind_reason_str) bind_loc in
              (bind_loc, local_name, import_reason, bind_reason)
            | None ->
              let import_reason =
                mk_reason (RCustom import_reason_str) (fst remote) in
              let bind_reason_str =
                spf "%s { %s } from %S"
                  import_str
                  remote_name
                  module_name
              in
              let bind_reason =
                mk_reason (RCustom bind_reason_str) (fst remote) in
              (fst remote, remote_name, import_reason, bind_reason)
          ) in
          let imported_t =
            if Type_inference_hooks_js.dispatch_member_hook
              cx remote_name loc module_t
            then AnyT.why import_reason
            else get_imported_t import_reason remote_name local_name
          in
          (bind_reason, local_name, imported_t)

        | ImportDefaultSpecifier local ->
          let local_name = ident_name local in
          let loc = fst local in

          let import_reason_str =
            spf "Default import from `%s`" module_name
          in
          let import_reason = mk_reason (RCustom import_reason_str) loc in

          let bind_reason_str =
            spf "%s %s from %S" import_str local_name module_name
          in
          let bind_reason = mk_reason (RCustom bind_reason_str) loc in

          let imported_t =
            if Type_inference_hooks_js.dispatch_member_hook
              cx "default" loc module_t
            then AnyT.why import_reason
            else get_imported_t import_reason "default" local_name
          in
          (bind_reason, local_name, imported_t)

        | ImportNamespaceSpecifier (ns_loc, local) ->
          let local_name = ident_name local in

          Type_inference_hooks_js.dispatch_import_hook cx module_name ns_loc;

          let import_reason_str = spf "%s * as %s" import_str local_name in
          let import_reason =
            mk_reason (RCustom import_reason_str) import_loc in

          (match import_kind with
            | Type.ImportType ->
              Flow_error.(add_output cx (EImportTypeofNamespace
                (import_reason, local_name, module_name)));
              (import_reason, local_name, AnyT.why import_reason)
            | Type.ImportTypeof ->
              let bind_reason = repos_reason (fst local) import_reason in
              let module_ns_t =
                import_ns cx import_reason module_name (fst import_decl.source)
              in
              let module_ns_typeof =
                Flow.mk_tvar_where cx bind_reason (fun t ->
                  Context.add_imported_t cx local_name t;
                  Flow.flow cx (module_ns_t,
                    ImportTypeofT (bind_reason, "*", t))
                )
              in
              (import_reason, local_name, module_ns_typeof)
            | Type.ImportValue ->
              let reason =
                mk_reason (RCustom (spf "exports of %S" module_name)) import_loc
              in
              let module_ns_t =
                import_ns cx reason module_name (fst import_decl.source)
              in
              Context.add_imported_t cx local_name module_ns_t;
              let bind_reason =
                mk_reason (RCustom import_reason_str) (fst local) in
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


and export_statement cx loc
  ~default declaration_export_info specifiers source exportKind =

  let open Ast.Statement in
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
      mk_reason (RCustom (spf "%s %s" export_reason_start export_reason)) loc
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
      set_module_kind cx reason Context.ESModule);

    let local_name = if default then "default" else local_name in
    set_module_t cx reason (fun t ->
      Flow.flow cx (
        module_t_of_cx cx,
        ExportNamedT(reason, SMap.singleton local_name local_tvar, t)
      )
    )
  ) in

  (match (declaration_export_info, specifiers) with
    (* [declare] export [type] {foo, bar} [from ...]; *)
    | ([], Some (ExportNamedDeclaration.ExportSpecifiers specifiers)) ->
      let export_specifier specifier = (
        let (reason, local_name, remote_name) = (
          match specifier with
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, id);
              exported = None;
            } ->
            let reason = mk_reason (RCustom (spf "export {%s}" id)) loc in
            (reason, id, id)
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, local);
              exported = Some (_, exported);
            } ->
            let reason =
              mk_reason (RCustom (spf "export {%s as %s}" local exported)) loc
            in
            (reason, local, exported)
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
                mk_reason (RCustom "ModuleNamespace for export {} from") src_loc
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
              Flow.flow cx (tvar, GetPropT (reason, Named (reason, local_name), t))
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
        then set_module_kind cx reason Context.ESModule);

        set_module_t cx reason (fun t ->
          Flow.flow cx (
            module_t_of_cx cx,
            ExportNamedT(reason, SMap.singleton remote_name local_tvar, t)
          )
        )
      ) in
      List.iter export_specifier specifiers

    (* [declare] export [type] * from "source"; *)
    | [],
      Some (ExportNamedDeclaration.ExportBatchSpecifier
        (batch_loc, star_as_name)
      ) ->
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
        let (_, name) = ident in
        let reason =
          mk_reason
            (RCustom (spf "export * as %s from %S" name source_module_name))
            loc
        in
        set_module_kind cx reason Context.ESModule;

        let remote_namespace_t =
          if parse_export_star_as = Options.ESPROPOSAL_ENABLE
          then import_ns cx reason source_module_name batch_loc
          else AnyT.why (
            let config_value =
              if parse_export_star_as = Options.ESPROPOSAL_IGNORE
              then "ignore"
              else "warn"
            in
            let reason =
              spf "flowconfig: esproposal.export_star_as=%s" config_value in
            mk_reason (RCustom reason) batch_loc
          )
        in
        set_module_t cx reason (fun t ->
          Flow.flow cx (
            module_t_of_cx cx,
            ExportNamedT(reason, SMap.singleton name remote_namespace_t, t)
          )
        )
      | None ->
        let reason =
          mk_reason (RCustom (spf "export * from %S" source_module_name)) loc
        in
        set_module_kind cx reason Context.ESModule;

        set_module_t cx reason (fun t ->
          Flow.flow cx (
            import ~reason cx source_module_name loc,
            CopyNamedExportsT(reason, module_t_of_cx cx, t)
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

and object_prop cx map = Ast.Expression.Object.(function
  (* name = function expr *)
  | Property (_, { Property.
      kind = Property.Init;
      key = Property.Identifier (_, name);
      value = (vloc, Ast.Expression.Function func);
      _method;
      _
    }) ->
    let {Ast.Function.id; async; generator; _} = func in
    let desc = RFunction (function_desc ~async ~generator) in
    let reason = mk_reason desc vloc in
    let ft = mk_function id cx reason func in
    Hashtbl.replace (Context.type_table cx) vloc ft;
    let polarity = if _method then Positive else Neutral in
    Properties.add_field name polarity ft map

  (* name = non-function expr *)
  | Property (_, { Property.kind = Property.Init;
      key =
        Property.Identifier (_, name) |
        Property.Literal (_, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
      value = v; _ }) ->
    let t = expression cx v in
    Properties.add_field name Neutral t map

  (* literal LHS *)
  | Property (loc, { Property.key = Property.Literal _; _ }) ->
    Flow_error.(add_output cx
      (EUnsupportedSyntax (loc, ObjectPropertyLiteralNonString)));
    map

  (* With the enable_unsafe_getters_and_setters option set, we enable some
   * unsafe support for getters and setters. The main unsafe bit is that we
   * don't properly havok refinements when getter and setter methods are called.
   *)

  (* unsafe getter property *)
  | Property (_, {
      Property.kind = Property.Get;
      key = Property.Identifier (_, name);
      value = (vloc, Ast.Expression.Function func);
      _ })
    when Context.enable_unsafe_getters_and_setters cx ->
      let reason = mk_reason RGetterFunction vloc in
      let function_type = mk_function None cx reason func in
      let return_t = extract_getter_type function_type in
      Properties.add_getter name return_t map

  (* unsafe setter property *)
  | Property (_, {
    Property.kind = Property.Set;
      key = Property.Identifier (_, name);
      value = (vloc, Ast.Expression.Function func);
      _ })
    when Context.enable_unsafe_getters_and_setters cx ->
      let reason = mk_reason RSetterFunction vloc in
      let function_type = mk_function None cx reason func in
      let param_t = extract_setter_type function_type in
      Properties.add_setter name param_t map

  | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
    Flow_error.(add_output cx (EUnsupportedSyntax (loc, ObjectPropertyGetSet)));
    map

  (* computed LHS *)
  | Property (_, { Property.key = Property.Computed _; _ }) ->
    map

  (* spread prop *)
  | SpreadProperty _ ->
    map
)

and prop_map_of_object cx props =
  List.fold_left (object_prop cx) SMap.empty props

and object_ cx reason ?(allow_sealed=true) props =
  Ast.Expression.Object.(
  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let proto = ObjProtoT reason in
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
        let spread = expression cx argument in
        let obj = eval_object (map, result) in
        let result = mk_spread spread obj in
        false, SMap.empty, Some result
    | Property (_, { Property.key = Property.Computed k; value = v; _ }) ->
        let k = expression cx k in
        let v = expression cx v in
        let obj = eval_object (map, result) in
        Flow.flow cx (obj, SetElemT (reason, k, v));
        (* TODO: vulnerable to race conditions? *)
        let result = obj in
        sealed, SMap.empty, Some result
    | t ->
        sealed, object_prop cx map t, result
  ) (allow_sealed, SMap.empty, None) props in

  let sealed = match result with
    | Some _ -> sealed
    | None -> sealed && not (SMap.is_empty map)
  in
  eval_object ~sealed (map, result)
)

and variable cx kind
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
    | (loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
          name = (id_loc, name); typeAnnotation; optional
        }) ->
        (* simple lvalue *)
        let desc = RCustom (spf "%s `%s`" str_of_kind name) in
        let reason = mk_reason desc id_loc in
        (* make annotation, unify with declared type created in variable_decl *)
        let t =
          let anno_reason = mk_reason desc loc in
          Anno.mk_type_annotation cx SMap.empty anno_reason typeAnnotation in
        Env.unify_declared_type cx name t;
        let has_anno = not (typeAnnotation = None) in
        (match init with
          | Some ((rhs_loc, _) as expr) ->
            let rhs_reason =
              let desc = RCustom (spf "assignment of var `%s`" name) in
              mk_reason desc rhs_loc
            in
            let rhs = expression cx expr in
            (**
             * Const and let variables are not declared during evaluation of
             * their initializer expressions.
             *)
            declare_var cx name reason;
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
        let reason = mk_reason (RCustom (spf "%s _" str_of_kind)) loc in
        let typeAnnotation = type_of_pattern id in
        let has_anno = not (typeAnnotation = None) in
        let t = match init with
          | Some expr -> expression cx expr
          | None -> (
            match if_uninitialized with
            | Some f -> f loc
            | None -> VoidT.at loc
          )
        in
        init_var cx pattern_name ~has_anno t reason;
        destructuring cx ~expr:expression ~f:(fun loc name default t ->
          let reason = mk_reason (RCustom (spf "%s %s" str_of_kind name)) loc in
          Option.iter default (fun d ->
            let default_t = Flow.mk_default cx reason d ~expr:expression in
            Flow.flow_t cx (default_t, t)
          );
          init_var cx name ~has_anno t reason
        ) t init None id
)

and array_element cx undef_loc el = Ast.Expression.(
  match el with
  | Some (Expression e) -> expression cx e
  | Some (Spread (_, { SpreadElement.argument })) ->
      array_element_spread cx argument
  | None -> EmptyT.at undef_loc
)

and expression_or_spread cx = Ast.Expression.(function
  | Expression e -> expression cx e
  | Spread (_, { SpreadElement.argument }) -> spread cx argument
)

and array_element_spread cx (loc, e) =
  let arr = expression cx (loc, e) in
  let reason = mk_reason (RCustom "spread operand") loc in
  Flow.mk_tvar_where cx reason (fun tvar ->
    Flow.flow_t cx (arr, ArrT (reason, tvar, []));
  )

and spread cx (loc, e) =
  RestT (array_element_spread cx (loc, e))

and expression ?(is_cond=false) cx (loc, e) =
  let t = expression_ ~is_cond cx loc e in
  Hashtbl.replace (Context.type_table cx) loc t;
  t

and this_ cx r = Ast.Expression.(
  match Refinement.get cx (loc_of_reason r, This) r with
  | Some t -> t
  | None -> Env.var_ref cx (internal_name "this") r
)

and super_ cx reason =
  Env.var_ref cx (internal_name "super") reason

and expression_ ~is_cond cx loc e = Ast.Expression.(match e with

  | Ast.Expression.Literal lit ->
      literal cx loc lit

  | Identifier (_, name) ->
      identifier cx name loc

  | This ->
      this_ cx (mk_reason RThis loc)

  | Super ->
      identifier cx "super" loc

  | Unary u ->
      unary cx loc u

  | Update u ->
      update cx loc u

  | Binary b ->
      binary cx loc b

  | Logical l ->
      logical cx loc l

  | TypeCast {
        TypeCast.expression = e;
        typeAnnotation } ->
      let r = mk_reason (RCustom "typecast") loc in
      let t = Anno.mk_type_annotation cx SMap.empty r (Some typeAnnotation)
      in Hashtbl.replace (Context.type_table cx) loc t;
      let infer_t = expression cx e in
      Flow.flow_t cx (infer_t, t);
      t

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      _
    } ->
      let reason =
        mk_reason (RCustom "access of computed property/element") loc in
      (match Refinement.get cx (loc, e) reason with
      | Some t -> t
      | None ->
        let tobj = expression cx _object in
        let tind = expression cx index in
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (tobj, GetElemT(reason, tind, t))
        )
      )

  | Member {
      Member._object = _, Identifier (_, "module");
      property = Member.PropertyIdentifier (_, "exports");
      _
    } ->
      let reason = mk_reason (RCustom "module.exports") loc in
      get_module_exports cx reason

  | Member {
      Member._object =
        _, Identifier (_, ("ReactGraphQL" | "ReactGraphQLLegacy"));
      property = Member.PropertyIdentifier (_, "Mixin");
      _
    } ->
      let reason = mk_reason (RCustom "ReactGraphQLMixin") loc in
      Flow.get_builtin cx "ReactGraphQLMixin" reason

  | Member {
      Member._object = super_loc, Super;
      property = Member.PropertyIdentifier (ploc, name);
      _
    } ->
      let expr_reason = mk_reason (RProperty (Some name)) loc in
      (match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (RProperty (Some name)) ploc in

        let super = super_ cx (mk_reason RSuper super_loc) in

        if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
        then AnyT.at ploc
        else (
          Flow.mk_tvar_where cx expr_reason (fun tvar ->
            Flow.flow cx (
              super, GetPropT (expr_reason, Named (prop_reason, name), tvar)
            )
          )
        )
      )

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, name);
      _
    } -> (
      let expr_reason = mk_reason (RProperty (Some name)) loc in
      match Refinement.get cx (loc, e) expr_reason with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (RProperty (Some name)) ploc in
        let tobj = expression cx _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else get_prop ~is_cond cx expr_reason tobj (prop_reason, name)
    )

  | Object { Object.properties } ->
    let reason = mk_reason RObjectLit loc in
    object_ cx reason properties

  | Array { Array.elements } -> (
    let reason = mk_reason RArrayLit loc in
    match elements with
    | [] ->
        (* empty array, analogous to object with implicit properties *)
        let element_reason =
          let desc = RCustom "unknown element type of empty array" in
          mk_reason desc loc
        in
        let elemt = Flow.mk_tvar cx element_reason in
        let reason = replace_reason_const REmptyArrayLit reason in
        ArrT (reason, elemt, [])
    | elems ->
        (* tup is true if no spreads *)
        (* tset is set of distinct (mod reason) elem types *)
        (* tlist is reverse list of element types if tup, else [] *)
        let _, tset, tlist = List.fold_left (fun (tup, tset, tlist) elem ->
          let elemt = array_element cx loc elem in

          let tup = match elem with Some (Spread _) -> false | _ -> tup in
          let elemt = if tup then elemt else summarize cx elemt in
          tup,
          TypeExSet.add elemt tset,
          if tup then elemt :: tlist else []
        ) (true, TypeExSet.empty, []) elems
        in
        (* composite elem type is an upper bound of all element types *)
        let elemt = match TypeExSet.elements tset with
        | [t] -> t
        | list ->
          let element_reason =
            let desc = RCustom ("inferred union of array element types \
(alternatively, provide an annotation to summarize the array element type)") in
            mk_reason desc loc
          in
          (* Should the element type of the array be the union of its element
             types?

             No. Instead of using a union, we use an unresolved tvar to
             represent the least upper bound of each element type. Effectively,
             this keeps the element type "open," at least locally.[*]

             Using a union pins down the element type prematurely, and moreover,
             might lead to speculative matching when setting elements or caling
             contravariant methods (`push`, `concat`, etc.) on the array.

             In any case, using a union doesn't quite work as intended today
             when the element types themselves could be unresolved tvars. For
             example, the following code would work even with unions:

             declare var o: { x: number; }
             // no error, but is an error if o.x is replaced by 42
             var a = ["hey", o.x];
             declare var i: number;
             a[i] = false;

             [*] Eventually, the element type does get pinned down to a union
             when it is part of the module's exports. In the future we might
             have to do that pinning more carefully, and using an unresolved
             tvar instead of a union here doesn't conflict with those plans.
          *)
          Flow_js.mk_tvar_where cx element_reason (fun tvar ->
            list |> List.iter (fun t ->
              Flow_js.flow cx (t, UseT (UnknownUse, tvar)))
          )
        in
        ArrT (reason, elemt, List.rev tlist)
    )

  | Call {
      Call.callee = _, Identifier (_, "require");
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
        if not ignore_non_literals
        then
          Flow_error.(add_output cx
            (EUnsupportedSyntax (loc, RequireDynamicArgument)));
        AnyT.at loc
    )

  | Call {
      Call.callee = _, Identifier (_, "requireLazy");
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
              Flow_error.(add_output cx
                (EUnsupportedSyntax (loc, RequireLazyDynamicArgument)));
              tvars
        ) in
        let module_tvars = List.fold_left element_to_module_tvar [] elements in
        let module_tvars = List.rev module_tvars in

        let callback_expr_t = expression cx callback_expr in
        let reason = mk_reason (RCustom "requireLazy() callback") loc in
        let _ = func_call cx reason callback_expr_t module_tvars in

        NullT.at loc

      | _ ->
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, RequireLazyDynamicArgument)));
        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, "Function");
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      List.iter (fun t ->
        Flow.flow_t cx (t, StrT.at loc)
      ) argts;
      let reason = mk_reason (RCustom "new Function(..)") loc in
      let proto = ObjProtoT reason in
      FunT (
        reason,
        Flow.dummy_static reason,
        Flow.dummy_prototype,
        Flow.mk_functiontype [] ~params_names:[] proto
      )
    )

  | New {
      New.callee = _, Identifier (_, "Array");
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      (match argts with
      | [argt] ->
        let reason = mk_reason (RCustom "new Array(..)") loc in
        let length_reason =
          replace_reason_const (RCustom "array length") reason in
        Flow.flow_t cx (argt, NumT (length_reason, AnyLiteral));
        let element_reason =
          replace_reason_const (RCustom "array element") reason in
        let t = Flow.mk_tvar cx element_reason in
        ArrT (reason, t, [])
      | _ ->
        Flow_error.(add_output cx (EUseArrayLiteral loc));
        EmptyT.at loc
      )
    )

  | New { New.callee; arguments } ->
      let class_ = expression cx callee in
      let argts = List.map (expression_or_spread cx) arguments in
      new_call cx loc class_ argts

  | Call {
      Call.callee = (_, Member {
        Member._object = (_, Identifier (_, "Object") as obj);
        property = Member.PropertyIdentifier (prop_loc, name);
        _
      } as expr);
      arguments
    } ->
      let obj_t = expression cx obj in
      static_method_call_Object cx loc prop_loc expr obj_t name arguments

  | Call {
      Call.callee = _, Member {
        Member._object = _, Identifier (_, "React");
        property = Member.PropertyIdentifier (_, "createClass");
        _
      };
      arguments = [ Expression (_, Object { Object.properties = class_props }) ]
    } ->
      react_create_class cx loc class_props

  | Call {
      Call.callee = callee_loc, Member {
        Member._object = super_loc, Super;
        property = Member.PropertyIdentifier (ploc, name);
        _
      };
      arguments
    } ->
      let reason = mk_reason (RMethodCall (Some name)) loc in
      let reason_lookup = mk_reason (RProperty (Some name)) callee_loc in
      let reason_prop = mk_reason (RProperty (Some name)) ploc in
      let super = super_ cx (mk_reason RSuper super_loc) in
      let argts = List.map (expression_or_spread cx) arguments in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
      Flow.mk_tvar_where cx reason (fun t ->
        let funtype = Flow.mk_methodtype super argts t in
        Flow.flow cx (
          super,
          MethodT (reason, reason_lookup, Named (reason_prop, name), funtype)
        )
      )

  | Call { Call.
      callee = (lookup_loc, Member { Member.
        _object;
        property; _
      }) as callee;
      arguments
    } ->
      (* method call *)
      let ot = expression cx _object in
      let argts = List.map (expression_or_spread cx) arguments in
      (match property with
      | Member.PropertyIdentifier (prop_loc, name) ->
        let reason_call = mk_reason (RMethodCall (Some name)) loc in
        method_call cx reason_call prop_loc (callee, ot, name) argts
      | Member.PropertyExpression expr ->
        let reason_call = mk_reason (RMethodCall None) loc in
        let reason_lookup = mk_reason (RProperty None) lookup_loc in
        Flow.mk_tvar_where cx reason_call (fun t ->
          let elem_t = expression cx expr in
          let frame = Env.peek_frame () in
          let funtype = Flow.mk_methodtype ot argts t ~frame in
          Flow.flow cx (ot,
            CallElemT (reason_call, reason_lookup, elem_t, funtype))
        ))

  | Call {
      Call.callee = ploc, Super;
      arguments
    } ->
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason RFunctionCall loc in
      let super_reason = mk_reason RSuper ploc in

      (* switch back env entries for this and super from undefined *)
      define_internal cx reason "this";
      define_internal cx reason "super";

      let this = this_ cx reason in
      let super = super_ cx super_reason in
      Flow.mk_tvar_where cx reason (fun t ->
        let funtype = Flow.mk_methodtype this argts t in
        let propref = Named (super_reason, "constructor") in
        Flow.flow cx (super, MethodT(reason, super_reason, propref, funtype)))

  (******************************************)
  (* See ~/www/static_upstream/core/ *)

  | Call {
      Call.callee = (_, Identifier (_, "invariant")) as callee;
      arguments
    } ->
      (* TODO: require *)
      let reason = mk_reason (RCustom "invariant") loc in
      ignore (expression cx callee);
      (match arguments with
      | [] ->
        (* invariant() is treated like a throw *)
        Env.reset_current_activation reason;
        Abnormal.save_and_throw Abnormal.Throw
      | (Expression (_, Ast.Expression.Literal {
          Ast.Literal.value = Ast.Literal.Boolean false; _;
        }))::arguments ->
        (* invariant(false, ...) is treated like a throw *)
        ignore (List.map (expression_or_spread cx) arguments);
        Env.reset_current_activation reason;
        Abnormal.save_and_throw Abnormal.Throw
      | (Expression cond)::arguments ->
        ignore (List.map (expression_or_spread cx) arguments);
        let _, preds, _, xtypes = predicates_of_condition cx cond in
        let _ = Env.refine_with_preds cx reason preds xtypes in
        ()
      | (Spread _)::_ ->
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, InvariantSpreadArgument)))
      );
      VoidT.at loc

  | Call { Call.callee; arguments } ->
      let f = expression cx callee in
      let reason = mk_reason RFunctionCall loc in
      let argts =
        List.map (expression_or_spread cx) arguments in
      func_call cx reason f argts

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason RConditional loc in
      let _, preds, not_preds, xtypes = predicates_of_condition cx test in
      let env =  Env.peek_env () in
      let oldset = Changeset.clear () in

      let then_env = Env.clone_env env in
      Env.update_env cx reason then_env;
      let _ = Env.refine_with_preds cx reason preds xtypes in
      let t1 = expression cx consequent in

      let else_env = Env.clone_env env in
      Env.update_env cx reason else_env;
      let _ = Env.refine_with_preds cx reason not_preds xtypes in
      let t2 = expression cx alternate in

      let newset = Changeset.merge oldset in
      Env.merge_env cx reason (env, then_env, else_env) newset;
      Env.update_env cx reason env;
      (* TODO call loc_of_predicate on some pred?
         t1 is wrong but hopefully close *)

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
      UnionT (reason, UnionRep.make t1 t2 [])

  | Assignment { Assignment.operator; left; right } ->
      assignment cx loc (left, operator, right)

  | Sequence { Sequence.expressions } ->
      List.fold_left
        (fun _ e -> expression cx e)
        (VoidT.at loc)
        expressions

  | Function func ->
      let {Ast.Function.id; async; generator; predicate; _} = func in
      let desc = RFunction (function_desc ~async ~generator) in
      let reason = mk_reason desc loc in

      (match predicate with
      | Some (_, Ast.Type.Predicate.Inferred) ->
          Flow_error.(add_output cx
            (EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)))
      | _ -> ());

      mk_function id cx reason func

  | ArrowFunction func ->
      let {Ast.Function.async; generator; _} = func in
      let desc = RArrowFunction (function_desc ~async ~generator) in
      let reason = mk_reason desc loc in
      mk_arrow cx reason func

  | TaggedTemplate {
      TaggedTemplate.tag = _, Identifier (_, "query");
      (* TODO: walk quasis? *)
      quasi = _, { TemplateLiteral.quasis = _; expressions }
    } ->
    List.iter (fun e -> ignore (expression cx e)) expressions;
    (*parse_graphql cx encaps;*)
    VoidT.at loc

  | TaggedTemplate {
      TaggedTemplate.tag;
      (* TODO: walk quasis? *)
      quasi = _, { TemplateLiteral.quasis = _; expressions }
    } ->
      List.iter (fun e -> ignore (expression cx e)) expressions;
      let t = expression cx tag in
      let reason = mk_reason (RCustom "encaps tag") loc in
      let reason_array = replace_reason_const RArray reason in
      let ret = Flow.mk_tvar cx reason in
      let ft = Flow.mk_functiontype
        [ ArrT (reason_array, StrT.why reason, []);
          RestT (AnyT.why reason) ]
        ret
      in
      Flow.flow cx (t, CallT (reason, ft));
      ret

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      let strt_of_quasi = function
      | (elem_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked;
            _
          };
          _
        }) -> StrT (mk_reason RString elem_loc, Type.Literal cooked)
      in
      begin match quasis with
      | head::[] ->
          strt_of_quasi head
      | _ ->
          let t_out = StrT.at loc in
          List.iter (fun expr ->
            let e = expression cx expr in
            Flow.flow cx (e, UseT (Coercion, t_out));
          ) expressions;
          t_out
      end

  | JSXElement e ->
      jsx cx e

  | Class c ->
      let (name_loc, name) = extract_class_name loc c in
      let reason = mk_reason (RCustom (spf "class expr `%s`" name)) loc in
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
          let class_t = mk_class cx loc reason c in
          Env.pop_var_scope ();
          Flow.flow_t cx (class_t, tvar);
          class_t;
      | None -> mk_class cx loc reason c)

  | Yield { Yield.argument; delegate = false } ->
      let reason = mk_reason (RCustom "yield") loc in
      let yield = Env.get_var cx (internal_name "yield") reason in
      let t = match argument with
      | Some expr -> expression cx expr
      | None -> VoidT.at loc in
      Flow.flow_t cx (t, yield);
      Env.get_var cx (internal_name "next") reason

  | Yield { Yield.argument; delegate = true } ->
      let reason = mk_reason (RCustom "yield* delegate") loc in
      let next_reason = replace_reason (fun desc -> RCustom (
        spf "next of parent generator in %s" (string_of_desc desc)
      )) reason in
      let yield_reason = replace_reason (fun desc -> RCustom (
        spf "yield of parent generator in %s" (string_of_desc desc)
      )) reason in
      let next = Env.get_var cx (internal_name "next") next_reason in
      let yield = Env.get_var cx (internal_name "yield") yield_reason in
      let t = match argument with
      | Some expr -> expression cx expr
      | None -> assert_false "delegate yield without argument" in

      let ret_reason = replace_reason (fun desc -> RCustom (
        spf "return of child generator in %s" (string_of_desc desc)
      )) reason in
      let ret = Flow.mk_tvar cx ret_reason in

      (* widen yield with the element type of the delegated-to iterable *)
      let iterable =
        let targs = [yield; ret; next] in
        if Env.in_async_scope () then
          let reason =
            mk_reason (RCustom "async iteration expected on AsyncIterable") loc
          in
          Flow.get_builtin_typeapp cx reason "$AsyncIterable" targs
        else
          Flow.get_builtin_typeapp cx
            (mk_reason (RCustom "iteration expected on Iterable") loc)
            "$Iterable" targs
      in
      Flow.flow_t cx (t, iterable);

      ret

  (* TODO *)
  | Comprehension _ ->
    Flow_error.(add_output cx
      (EUnsupportedSyntax (loc, ComprehensionExpression)));
    EmptyT.at loc

  | Generator _ ->
    Flow_error.(add_output cx
      (EUnsupportedSyntax (loc, GeneratorExpression)));
    EmptyT.at loc

  | MetaProperty _->
    Flow_error.(add_output cx
      (EUnsupportedSyntax (loc, MetaPropertyExpression)));
    EmptyT.at loc
)

(* Handles function calls that appear in conditional contexts. The main
   distinction from the case handled in `expression_` is that we also return
   the inferred types for the call receiver and the passed arguments, and
   potenially the keys that correspond to the supplied arguments.
*)
and predicated_call_expression cx (loc, callee, arguments) =
  let (f, argks, argts, t) =
    predicated_call_expression_ cx loc callee arguments in
  Hashtbl.replace (Context.type_table cx) loc t;
  (f, argks, argts, t)

(* Returns a quadruple containing:
   - the function type
   - argument keys
   - the arguments types
   - the returned type
*)
and predicated_call_expression_ cx loc callee arguments =
  let f = expression cx callee in
  let reason = mk_reason RFunctionCall loc in
  let argts = List.map (expression cx) arguments in
  let argks = List.map Refinement.key arguments in
  let t = func_call cx reason f argts in
  (f, argks, argts, t)

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx tok class_ argts =
  let reason = mk_reason RConstructorCall tok in
  Flow.mk_tvar_where cx reason (fun t ->
    Flow.flow cx (class_, ConstructorT (reason, argts, t));
  )

and func_call cx reason func_t argts =
  Env.havoc_heap_refinements ();
  Flow.mk_tvar_where cx reason (fun t ->
    let frame = Env.peek_frame () in
    let app = Flow.mk_functiontype argts t ~frame in
    Flow.flow cx (func_t, CallT(reason, app))
  )

and method_call cx reason prop_loc (expr, obj_t, name) argts =
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
        let app = Flow.mk_methodtype obj_t argts t ~frame in
        Flow.flow cx (f, CallT (reason, app));
      )
  | None ->
      Env.havoc_heap_refinements ();
      Flow.mk_tvar_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let expr_loc, _ = expr in
        let reason_expr = mk_reason (RProperty (Some name)) expr_loc in
        let reason_prop = mk_reason (RProperty (Some name)) prop_loc in
        let app = Flow.mk_methodtype obj_t argts t ~frame in
        let propref = Named (reason_prop, name) in
        Flow.flow cx (obj_t, MethodT(reason, reason_expr, propref, app))
      )
  )

and identifier cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    let reason = mk_reason (RIdentifier name) loc in
    Env.var_ref ~lookup_mode:ForValue cx name reason
  )

(* traverse a literal expression, return result type *)
and literal cx loc lit = Ast.Literal.(match lit.Ast.Literal.value with
  | String s ->
      StrT (mk_reason RString loc, Literal s)

  | Boolean b ->
      BoolT (mk_reason RBoolean loc, Some b)

  | Null ->
      NullT.at loc

  | Number f ->
      NumT (mk_reason RNumber loc, Literal (f, lit.raw))

  | RegExp _ ->
      Flow.get_builtin_type cx (mk_reason RRegExp loc) "RegExp"
)

(* traverse a unary expression, return result type *)
and unary cx loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; _ } ->
      let arg = expression cx argument in
      let reason = mk_reason (RCustom "not operator") loc in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (arg, NotT (reason, t));
      )

  | { operator = Plus; argument; _ } ->
      ignore (expression cx argument);
      NumT.at loc

  | { operator = Minus; argument; _ } ->
      let arg = expression cx argument in
      let reason = mk_reason (RCustom "unary minus operator") loc in
      Flow.mk_tvar_derivable_where cx reason (fun t ->
        Flow.flow cx (arg, UnaryMinusT (reason, t));
      )

  | { operator = BitNot; argument; _ } ->
      let t = NumT.at loc in
      Flow.flow_t cx (expression cx argument, t);
      t

  | { operator = Typeof; argument; _ } ->
      ignore (expression cx argument);
      StrT.at loc

  | { operator = Void; argument; _ } ->
      ignore (expression cx argument);
      VoidT.at loc

  | { operator = Delete; argument; _ } ->
      ignore (expression cx argument);
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
    let reason = mk_reason (RCustom "await") loc in
    let await = Flow.get_builtin cx "$await" reason in
    let arg = expression cx argument in
    func_call cx reason await [arg]
)

(* numeric pre/post inc/dec *)
and update cx loc expr = Ast.Expression.Update.(
  let reason = mk_reason (RCustom "update") loc in
  let result_t = NumT.at loc in
  (match expr.argument with
  | _, Ast.Expression.Identifier (id_loc, name) ->
    Flow.flow cx (identifier cx name id_loc, AssertArithmeticOperandT reason);
    (* enforce state-based guards for binding update, e.g., const *)
    let id_reason = mk_reason (RCustom name) id_loc in
    ignore (Env.set_var cx name result_t id_reason)
  | expr ->
    Flow.flow cx (expression cx expr, AssertArithmeticOperandT reason)
  );
  result_t
)

(* traverse a binary expression, return result type *)
and binary cx loc = Ast.Expression.Binary.(function
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let reason = mk_reason (RCustom "non-strict equality comparison") loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow.flow cx (t1, EqT (reason,t2));
      BoolT.at loc

  | { operator = In; left = (loc1, _) as left; right = (loc2, _) as right } ->
      let t1 = expression cx left in
      let t2 = expression cx right in
      let reason_lhs = mk_reason (RCustom "LHS of `in` operator") loc1 in
      let reason_rhs = mk_reason (RCustom "RHS of `in` operator") loc2 in
      Flow.flow cx (t1, AssertBinaryInLHST reason_lhs);
      Flow.flow cx (t2, AssertBinaryInRHST reason_rhs);
      BoolT.at loc

  | { operator = StrictEqual; left; right }
  | { operator = StrictNotEqual; left; right }
  | { operator = Instanceof; left; right } ->
      ignore (expression cx left);
      ignore (expression cx right);
      BoolT.at loc

  | { operator = LessThan; left; right }
  | { operator = LessThanEqual; left; right }
  | { operator = GreaterThan; left; right }
  | { operator = GreaterThanEqual; left; right } ->
      let reason = mk_reason (RCustom "relational comparison") loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
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
      let reason = mk_reason (RCustom "arithmetic operation") loc in
      Flow.flow cx (expression cx left, AssertArithmeticOperandT reason);
      Flow.flow cx (expression cx right, AssertArithmeticOperandT reason);
      NumT.at loc

  | { operator = Plus; left; right } ->
      let reason = mk_reason (RCustom "+") loc in
      let t1 = expression cx left in
      let t2 = expression cx right in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, AdderT (reason, t2, t));
      )
)

and logical cx loc = Ast.Expression.Logical.(function
  | { operator = Or; left; right } ->
      let t1, _, not_map, xtypes = predicates_of_condition cx left in
      let reason = mk_reason (RCustom "||") loc in
      let t2 = Env.in_refined_env cx reason not_map xtypes
        (fun () -> expression cx right)
      in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, OrT (reason, t2, t));
      )

  | { operator = And; left; right } ->
      let t1, map, _, xtypes = predicates_of_condition cx left in
      let reason = mk_reason (RCustom "&&") loc in
      let t2 = Env.in_refined_env cx reason map xtypes
        (fun () -> expression cx right)
      in
      Flow.mk_tvar_where cx reason (fun t ->
        Flow.flow cx (t1, AndT (reason, t2, t));
      )
)

and assignment_lhs cx = Ast.Pattern.(function
  | loc, Object _
  | loc, Array _ ->
      Flow_error.(add_output cx (EInvalidLHSInAssignment loc));
      AnyT.at loc

  | _, Identifier { Ast.Pattern.Identifier.name = (loc, name); _; } ->
      identifier cx name loc

  | _, Expression ((_, Ast.Expression.Member _) as m) ->
      expression cx m

  (* parser will error before we get here *)
  | _ -> assert false
)

(* traverse assignment expressions *)
and assignment cx loc = Ast.Expression.(function

  (* r = e *)
  | (r, Assignment.Assign, e) ->

      (* compute the type of the RHS. this is what we return *)
      let t = expression cx e in

      (* update env, add constraints arising from LHS structure,
         handle special cases, etc. *)
      (match r with

        (* module.exports = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Ast.Expression.Identifier (_, "module");
            property = Member.PropertyIdentifier (_, "exports");
            _
          }) ->
            let reason =
              mk_reason (RCustom "assignment of module.exports") lhs_loc in
            set_module_kind cx reason (Context.CommonJSModule(Some(lhs_loc)));
            set_module_exports cx reason t

        (* super.name = e *)
        | lhs_loc, Ast.Pattern.Expression (_, Member {
            Member._object = _, Super;
            property = Member.PropertyIdentifier (ploc, name);
            _
          }) ->
            let reason =
              mk_reason (RPropertyAssignment name) lhs_loc in
            let prop_reason = mk_reason (RProperty (Some name)) ploc in
            let super = super_ cx reason in
            Flow.flow cx (super, SetPropT (reason, Named (prop_reason, name), t))

        (* _object.name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyIdentifier (ploc, name);
            _
          }) as expr) ->
            let o = expression cx _object in
            (* if we fire this hook, it means the assignment is a sham. *)
            if not (Type_inference_hooks_js.dispatch_member_hook cx name ploc o)
            then (
              let reason = mk_reason (RPropertyAssignment name) lhs_loc in
              let prop_reason = mk_reason (RProperty (Some name)) ploc in

              (* flow type to object property itself *)
              Flow.flow cx (o, SetPropT (reason, Named (prop_reason, name), t));

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
              let desc = RCustom "assignment of computed property/element" in
              mk_reason desc lhs_loc
            in
            let a = expression cx _object in
            let i = expression cx index in
            Flow.flow cx (a, SetElemT (reason, i, t));

            (* types involved in the assignment itself are computed
               in pre-havoc environment. it's the assignment itself
               which clears refis *)
            Env.havoc_heap_refinements ();

        (* other r structures are handled as destructuring assignments *)
        | _ ->
            destructuring_assignment cx ~expr:expression t e r
      );
      t

  | (lhs, Assignment.PlusAssign, rhs) ->
      (* lhs += rhs *)
      let reason = mk_reason (RCustom "+=") loc in
      let lhs_t = assignment_lhs cx lhs in
      let rhs_t = expression cx rhs in
      let result_t = Flow.mk_tvar cx reason in
      (* lhs = lhs + rhs *)
      Flow.flow cx (lhs_t, AdderT (reason, rhs_t, result_t));
      Flow.flow cx (rhs_t, AdderT (reason, lhs_t, result_t));
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = id_loc, name;
        _;
      } ->
        let id_reason = mk_reason (RCustom name) id_loc in
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
      let reason = mk_reason (RCustom "(numop)=") loc in
      let lhs_t = assignment_lhs cx lhs in
      let rhs_t = expression cx rhs in
      (* lhs = lhs (numop) rhs *)
      Flow.flow cx (lhs_t, AssertArithmeticOperandT reason);
      Flow.flow cx (rhs_t, AssertArithmeticOperandT reason);
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = id_loc, name;
        _;
      } ->
        let id_reason = mk_reason (RCustom name) id_loc in
        ignore Env.(set_var cx name (NumT.at loc) id_reason)
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

and jsx cx = Ast.JSX.(
  function { openingElement; children; _ } ->
  jsx_title cx openingElement (List.map (jsx_body cx) children)
)

and jsx_title cx openingElement children = Ast.JSX.(
  let eloc, { Opening.name; attributes; _ } = openingElement in
  let facebook_fbt = Context.facebook_fbt cx in

  let is_react = Context.jsx cx = None in

  let mk_props reason c name =
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
                expression cx (loc, e)
            | _ ->
                (* empty or nonexistent attribute values *)
                EmptyT.at aloc
          ) in

          if not (is_react && react_ignore_attribute aname)
          then
            let p = Field (atype, Neutral) in
            map := SMap.add aname p !map

      | Opening.Attribute _ ->
          () (* TODO: attributes with namespaced names *)

      | Opening.SpreadAttribute (_, { SpreadAttribute.argument }) ->
          let ex_t = expression cx argument in
          spread := Some (ex_t)
    );
    let reason_props = replace_reason_const
      (if is_react then RReactElementProps name else RJSXElementProps name)
      reason in
    (* TODO: put children in the props for react, so that we can seal props for
     * react *)
    let sealed = !spread=None && not is_react in
    let o = Flow.mk_object_with_map_proto cx reason_props ~sealed
      !map (ObjProtoT reason_props)
    in
    match !spread with
      | None -> o
      | Some ex_t ->
          let reason_prop = replace_reason (fun desc ->
            RSpreadOf desc
          ) (reason_of_t ex_t) in
          let ignored_attributes =
            if is_react then react_ignored_attributes else [] in
          clone_object_with_excludes cx
            reason_prop o ex_t ignored_attributes
  in

  match (name, facebook_fbt) with
  | (Identifier (_, { Identifier.name }), Some facebook_fbt)
      when name = "fbt" ->
    let fbt_reason = mk_reason (RCustom "<fbt />") eloc in
    Flow.get_builtin_type cx fbt_reason facebook_fbt

  | Identifier (loc, { Identifier.name }), _
      when name = String.capitalize name ->
    if Type_inference_hooks_js.dispatch_id_hook cx name loc
    then AnyT.at eloc
    else begin
      let reason = mk_reason
        (if is_react then RReactElement(Some name) else RJSXElement(Some name))
        eloc in
      let c = Env.get_var cx name reason in
      let o = mk_props reason c name in
      jsx_desugar cx name c o attributes children eloc
    end

  | (Identifier (loc, { Identifier.name }), _) ->
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
        Flow.get_builtin_type
          cx
          (mk_reason (RCustom "JSX Intrinsics lookup") eloc)
          "$JSXIntrinsics"
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
       let desc = RCustom (spf "JSX Intrinsic: `%s`" name) in
       mk_reason desc eloc
     in
     let component_t =
       if is_react
       then Flow.mk_tvar_where cx component_t_reason (fun t ->
        let prop_t =
          if Type_inference_hooks_js.dispatch_member_hook
            cx name loc jsx_intrinsics
          then AnyT.at eloc
          else get_prop
            ~is_cond:false
            cx
            component_t_reason
            jsx_intrinsics
            (component_t_reason, name)
        in
        Flow.flow_t cx (prop_t, t)
      )
      else StrT (component_t_reason, Literal name) in
      let o = mk_props component_t_reason component_t name in
      jsx_desugar cx name component_t o attributes children eloc

  | _ ->
      (* TODO? covers namespaced names, member expressions as element names *)
      AnyT.at eloc
)

and jsx_desugar cx name component_t props attributes children eloc =
  match Context.jsx cx with
  | None ->
      (* TODO: children *)
      let reason = mk_reason (RReactElement (Some name)) eloc in
      let react = require cx ~internal:true "react" eloc in
      Flow.mk_tvar_where cx reason (fun tvar ->
        let reason_createElement =
          mk_reason (RProperty (Some "createElement")) eloc in
        Flow.flow cx (react, MethodT (
          reason,
          reason_createElement,
          Named (reason_createElement, "createElement"),
          Flow.mk_methodtype react [component_t;props] tvar
        ))
      )
  | Some (raw_jsx_expr, jsx_expr) ->
      let reason = mk_reason (RJSXFunctionCall raw_jsx_expr) eloc in

      (* A JSX element with no attributes should pass in null as the second
       * arg *)
      let props = match attributes with
      | [] -> NullT.at eloc
      | _ -> props in
      let argts = [component_t;props] @ children in
      Ast.Expression.(match jsx_expr with
      | _, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, name);
          _;
        } ->
          let ot = jsx_pragma_expression cx raw_jsx_expr eloc _object in
          method_call cx reason prop_loc (jsx_expr, ot, name) argts
      | _ ->
          let f = jsx_pragma_expression cx raw_jsx_expr eloc jsx_expr in
          func_call cx reason f argts
      )

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
and jsx_pragma_expression cx raw_jsx_expr loc = Ast.Expression.(function
  | _, Identifier (_, name) ->
      let reason = mk_reason (RJSXIdentifier(raw_jsx_expr, name)) loc in
      Env.var_ref ~lookup_mode:ForValue cx name reason
  | expr ->
      (* Oh well, we tried *)
      expression cx expr
)

and jsx_body cx = Ast.JSX.(function
  | _, Element e -> jsx cx e
  | _, ExpressionContainer ec -> (
      let open ExpressionContainer in
      let { expression = ex } = ec in
      match ex with
        | Expression (loc, e) -> expression cx (loc, e)
        | EmptyExpression loc ->
          EmptyT (mk_reason (RCustom "empty jsx body") loc)
    )
  | loc, Text _ -> StrT.at loc (* TODO: create StrT (..., Literal ...)) *)
)

(* Native support for React.PropTypes validation functions, which are
   interpreted as type annotations for React props. This strategy is reasonable
   because the validation functions enforce types at run time (during
   development), and we can always insist on them because they are turned off in
   production. *)

and mk_proptype cx = Ast.Expression.(function
  | _, Member { Member.
      property = Member.PropertyIdentifier
        (_, "isRequired");
      _object = e;
      _
    } ->
      mk_proptype cx e

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "number");
      _
    } ->
      NumT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "string");
      _
    } ->
      StrT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "bool");
      _
    } ->
      BoolT.at vloc

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "array");
      _
    } ->
      ArrT (mk_reason RPropTypeArray vloc, AnyT.at vloc, [])

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "func");
      _
    } ->
      AnyFunT (mk_reason RPropTypeFunc vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "object");
      _
    } ->
      AnyObjT (mk_reason RPropTypeObject vloc)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "node");
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "element");
      _
    } ->
      AnyT.at vloc (* TODO *)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "arrayOf");
         _
      };
      arguments = [Expression e];
    } ->
      ArrT (mk_reason RPropTypeArrayOf vloc, mk_proptype cx e, [])

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "instanceOf");
         _
      };
      arguments = [Expression e];
    } ->
      Flow.mk_instance cx ~for_type:false (mk_reason RPropTypeInstanceOf vloc)
        (expression cx e)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "objectOf");
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
        value = mk_proptype cx e;
        dict_polarity = Neutral;
      } in
      let pmap = Context.make_property_map cx SMap.empty in
      let proto = ObjProtoT (locationless_reason RObjectClassName) in
      let reason = mk_reason RPropTypeObjectOf vloc in
      ObjT (reason, Flow.mk_objecttype ~flags dict pmap proto)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "oneOf");
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec string_literals acc = function
        | Some (Expression (_, Ast.Expression.Literal { Ast.Literal.
            value = Ast.Literal.String lit; _
          })) :: tl ->
          string_literals (lit :: acc) tl
        | [] -> Some acc
        | _ -> None
      in
      let reason = mk_reason RPropTypeOneOf vloc in
      (match string_literals [] elements with
      | Some keys -> Anno.mk_keys_type reason keys
      | None -> AnyT reason)

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "oneOfType");
         _
      };
      arguments = [Expression (_, Array { Array.elements })]
    } ->
      let rec proptype_elements acc = function
        | Some (Expression e) :: tl ->
            proptype_elements (mk_proptype cx e :: acc) tl
        | _ -> acc
      in
      let reason = mk_reason RPropTypeOneOfType vloc in
      (match proptype_elements [] elements with
      | [] -> EmptyT reason
      | [t] -> t
      | t0::t1::ts -> UnionT (reason, UnionRep.make t0 t1 ts))

  | vloc, Call { Call.
      callee = _, Member { Member.
         property = Member.PropertyIdentifier
          (_, "shape");
         _
      };
      arguments = [Expression (_, Object { Object.properties })];
    } ->
      let reason = mk_reason RPropTypeShape vloc in
      let amap, omap, dict = mk_proptypes cx properties in
      let omap = Properties.map_t (fun t -> OptionalT t) omap in
      let map = SMap.union amap omap in
      let proto = ObjProtoT reason in
      Flow.mk_object_with_map_proto cx reason ?dict map proto

  (* Support for FB-specific ReactPropTypes validators. *)
  (** TODO: instead, route to custom lib defs, somehow...details of which have
      not been set up or even worked out yet. **)
  | vloc, Member { Member.
      property = Member.PropertyIdentifier
        (_, "Fbt");
      _
    } ->
      (* We assume that there is a Fbt type defined in the global scope. *)
      Flow.get_builtin_type cx (mk_reason RPropTypeFbt vloc) "Fbt"

  | vloc, _ -> AnyT.at vloc
)

and mk_proptypes cx props = Ast.Expression.Object.(
  List.fold_left (fun (amap, omap, dict) -> function

    (* required prop *)
    | Property (_, { Property.
        kind = Property.Init;
        key = Property.Identifier (_, name);
        value = (_, Ast.Expression.Member {
          Ast.Expression.Member.
          property = Ast.Expression.Member.PropertyIdentifier (_, "isRequired");
          _object = e;
          _
        });
        _ }) ->
        let p = Field (mk_proptype cx e, Neutral) in
        SMap.add name p amap,
        omap,
        dict

    (* other prop *)
    | Property (_, { Property.kind = Property.Init;
        key =
          Property.Identifier (_, name) |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = v;
        _ }) ->
        let p = Field (mk_proptype cx v, Neutral) in
        amap,
        SMap.add name p omap,
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
      amap, omap, Some {
        dict_name = None;
        key = StrT.t;
        value = AnyT.t;
        dict_polarity = Neutral;
      }

    (* literal LHS *)
    | Property (loc, { Property.key = Property.Literal _; _ }) ->
      Flow_error.(add_output cx
        (EUnsupportedSyntax (loc, ReactPropTypesPropertyLiteralNonString)));
      amap, omap, dict

    (* get/set kind *)
    | Property (loc, { Property.kind = Property.Get | Property.Set; _ }) ->
      Flow_error.(add_output cx
        (EUnsupportedSyntax (loc, ReactPropTypesPropertyGetSet)));
      amap, omap, dict

    (* computed LHS *)
    | Property (loc, { Property.key = Property.Computed _; _ }) ->
      Flow_error.(add_output cx
        (EUnsupportedSyntax (loc, ReactPropTypesPropertyComputed)));
      amap, omap, dict

  ) (SMap.empty, SMap.empty, None) props
)

(* Legacy: generate React class from specification object. *)
and react_create_class cx loc class_props = Ast.Expression.(
  let reason_class = mk_reason RReactClass loc in
  let reason_component = mk_reason RReactComponent loc in
  let this = Flow.mk_tvar cx reason_component in
  let mixins = ref [] in
  let static_reason = replace_reason_const RReactStatics reason_class in
  let static = ref (Flow.mk_object cx static_reason) in
  let default_reason =
    replace_reason_const RReactDefaultProps reason_component in
  let default = ref (Flow.mk_object cx default_reason) in
  let reason_state = replace_reason_const RReactState reason_component in
  let state = ref (Flow.mk_object cx reason_state) in

  let props_reason =
    replace_reason_const RReactComponentProps reason_component in
  (* TODO - this probably should be the empty object AND we should enforce it *)
  let props = ref (AnyObjT props_reason) in

  let (fmap, mmap) =
    List.fold_left Ast.Expression.Object.(fun (fmap, mmap) -> function

      (* mixins *)
      | Property (_, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, "mixins");
          value = aloc, Array { Array.elements };
          _ }) ->
        mixins := List.map (array_element cx aloc) elements;
        fmap, mmap

      (* statics *)
      | Property (_, { Property.kind = Property.Init;
            key = Property.Identifier (nloc, "statics");
          value = _, Object { Object.properties };
          _ }) ->
        let reason = mk_reason RReactStatics nloc in
        static :=
          object_ cx reason ~allow_sealed:false properties;
        fmap, mmap

      (* propTypes *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (nloc, "propTypes");
          value = _, Object { Object.properties } as value;
          _ }) ->
        ignore (expression cx value);
        let reason = mk_reason RReactPropTypes nloc in
        let amap, omap, dict = mk_proptypes cx properties in
        let omap = Properties.map_t (fun t -> OptionalT t) omap in
        let map = SMap.union amap omap in
        let proto = ObjProtoT reason in
        props := Flow.mk_object_with_map_proto cx reason ?dict map proto;
        fmap, mmap

      (* getDefaultProps *)
      | Property (_, { Property.kind = Property.Init;
          key = Property.Identifier (_, "getDefaultProps");
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let reason = mk_reason RReactDefaultProps vloc in
          let t = mk_method cx reason func this in
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
          key = Property.Identifier (_, "getInitialState");
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let reason = mk_reason RReactState vloc in
          let t = mk_method cx reason func this in
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
          key = Property.Identifier (_, name);
          value = (vloc, Ast.Expression.Function func);
          _
        }) ->
          let {Ast.Function.async; generator; _ } = func in
          let desc = RFunction (function_desc ~async ~generator) in
          let reason = mk_reason desc vloc in
          let t = mk_method cx reason func this in
          let p = Field (t, Neutral) in
          fmap, SMap.add name p mmap

      (* name = non-function expr *)
      | Property (_, { Property.kind = Property.Init;
          key =
            Property.Identifier (_, name) |
            Property.Literal (_, {
              Ast.Literal.value = Ast.Literal.String name; _;
            });
          value = v;
          _ }) ->
        let t = expression cx v in
        let p = Field (t, Neutral) in
        SMap.add name p fmap, mmap

      | _ ->
        Flow_error.(add_output cx
          (EUnsupportedSyntax (loc, ReactCreateClassPropertyNonInit)));
        fmap, mmap

    ) (SMap.empty, SMap.empty) class_props in

  let type_args = [!default; !props; !state] in
  let super_reason =
    replace_reason (fun desc -> RSuperOf desc) reason_component in
  let super =
    Flow.get_builtin_typeapp cx super_reason
      "LegacyReactComponent" type_args
  in

  let extract_map (from_map,to_map) name =
    match SMap.get name from_map with
    | Some p -> SMap.remove name from_map, SMap.add name p to_map
    | None -> from_map, to_map
  in
  let fmap =
    let p = Field (!state, Neutral) in
    SMap.add "state" p fmap in
  let fmap, smap =
    List.fold_left extract_map (fmap, SMap.empty)
      ["contextTypes";"childContextTypes";"displayName"]
  in
  let override_statics =
    Flow.mk_object_with_map_proto cx
      static_reason smap (ObjProtoT static_reason)
  in
  let super_static = Flow.mk_tvar_where cx static_reason (fun t ->
    Flow.flow cx (super, GetStaticsT (static_reason, t));
  ) in
  Flow.flow_t cx (super_static, override_statics);
  static := clone_object cx static_reason !static super_static;

  let itype = {
    class_id = 0;
    type_args = SMap.empty;
    arg_polarities = SMap.empty;
    fields_tmap = Context.make_property_map cx fmap;
    initialized_field_names = SSet.empty;
    methods_tmap = Context.make_property_map cx mmap;
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

(* Given an expression found in a test position, notices certain
   type refinements which follow from the test's success or failure,
   and returns a quad:
   - result type of the test (not always bool)
   - map (lookup key -> type) of refinements which hold if
   the test is true
   - map of refinements which hold if the test is false
   - map of unrefined types for lvalues found in refinement maps
 *)
and predicates_of_condition cx e = Ast.(Expression.(

  (* refinement key if expr is eligible, along with unrefined type *)
  let refinable_lvalue e =
    Refinement.key e, condition cx e
  in

  (* package empty result (no refinements derived) from test type *)
  let empty_result test_t =
    (test_t, Key_map.empty, Key_map.empty, Key_map.empty)
  in

  let add_predicate key unrefined_t pred sense (test_t, ps, notps, tmap) =
    let p, notp = if sense
      then pred, NotP pred
      else NotP pred, pred
    in
    (test_t,
      Key_map.add key p ps,
      Key_map.add key notp notps,
      Key_map.add key unrefined_t tmap)
  in

  let flow_eqt ~strict loc (t1, t2) =
    if not strict then
      let reason = mk_reason (RCustom "non-strict equality comparison") loc in
      Flow_js.flow cx (t1, EqT (reason, t2))
  in

  (* package result quad from test type, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_t key unrefined_t pred sense =
    empty_result test_t |> add_predicate key unrefined_t pred sense
  in

  (* inspect a null equality test *)
  let null_test loc ~sense ~strict e null_t =
    let t, refinement = match refinable_lvalue e with
    | None, t -> t, None
    | Some name, t ->
        let pred = if strict then NullP else MaybeP in
        t, Some (name, t, pred, sense)
    in
    flow_eqt ~strict loc (t, null_t);
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  let void_test loc ~sense ~strict e void_t =
    let t, refinement = match refinable_lvalue e with
    | None, t -> t, None
    | Some name, t ->
        let pred = if strict then VoidP else MaybeP in
        t, Some (name, t, pred, sense)
    in
    flow_eqt ~strict loc (t, void_t);
    match refinement with
    | Some (name, t, p, sense) -> result (BoolT.at loc) name t p sense
    | None -> empty_result (BoolT.at loc)
  in

  (* inspect an undefined equality test *)
  let undef_test loc ~sense ~strict e void_t =
    if Env.is_global_var cx "undefined"
    then void_test loc ~sense ~strict e void_t
    else empty_result (BoolT.at loc)
  in

  (* a wrapper around `condition` (which is a wrapper around `expression`) that
     evaluates `expr`. if this is a sentinel property check (determined by
     a strict equality check against a member expression `_object.prop_name`),
     then also returns the refinement of `_object`.

     this is used by other tests such as `bool_test` such that if given
     `foo.bar === false`, `foo.bar` is refined to be `false` (by `bool_test`)
     and `foo` is refined to eliminate branches that don't have a `false` bar
     property (by this function). *)
  let condition_of_maybe_sentinel cx ~sense ~strict expr val_t =
    match strict, expr with
    | true,
      (expr_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, prop_name);
        _
      }) ->

      (* use `expression` instead of `condition` because `_object` is the object
         in a member expression; if it itself is a member expression, it must
         exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here as
         `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
      let obj_t = expression cx _object in

      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      Flow.flow cx (obj_t,
        HasPropT (prop_reason, None, TypeTerm.Literal prop_name));

      let expr_reason = mk_reason (RProperty (Some prop_name)) expr_loc in
      let prop_t = match Refinement.get cx expr expr_reason with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else get_prop ~is_cond:true cx
          expr_reason obj_t (prop_reason, prop_name)
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
      condition cx expr, None
  in

  let literal_test loc ~strict ~sense expr val_t pred =
    let t, sentinel_refinement = condition_of_maybe_sentinel cx
      ~sense ~strict expr val_t in
    flow_eqt ~strict loc (t, val_t);
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
          Flow_error.(add_output cx (EInvalidTypeof (str_loc, typename)));
          empty_result (BoolT.at loc)
        end
    | None, _ -> empty_result (BoolT.at loc)
  in

  let sentinel_prop_test loc ~sense ~strict expr val_t =
    let t, sentinel_refinement = condition_of_maybe_sentinel
      cx ~sense ~strict expr val_t in
    flow_eqt ~strict loc (t, val_t);
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
    | (_, Expression.Unary { Unary.operator = Unary.Typeof; argument; _ }),
      (str_loc, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = s; _
          }; _
        }]; _
      })
    | (str_loc, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = s; _
          }; _
        }]; _
      }),
      (_, Expression.Unary { Unary.operator = Unary.Typeof; argument; _ })
      ->
        typeof_test loc sense argument s str_loc

    (* special case equality relations involving booleans *)
    | (_, Expression.Literal { Literal.value = Literal.Boolean lit; _})
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.Boolean lit; _})
      as value)
      ->
        let val_t = expression cx value in
        literal_test loc ~sense ~strict expr val_t (SingletonBoolP lit)

    (* special case equality relations involving strings *)
    | (_, Expression.Literal { Literal.value = Literal.String lit; _})
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.String lit; _})
      as value)
    | expr, ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value)
    | ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value), expr
      ->
        let val_t = expression cx value in
        literal_test loc ~sense ~strict expr val_t (SingletonStrP lit)

    (* special case equality relations involving numbers *)
    | (_, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value, expr
    | expr, ((_, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value)
      ->
        let val_t = expression cx value in
        literal_test loc ~sense ~strict expr val_t (SingletonNumP (lit, raw))

    (* TODO: add Type.predicate variant that tests number equality *)

    (* expr op null *)
    | (_, Expression.Literal { Literal.value = Literal.Null; _ } as null), expr
    | expr, (_, Expression.Literal { Literal.value = Literal.Null; _ } as null)
      ->
        let null_t = expression cx null in
        null_test loc ~sense ~strict expr null_t

    (* expr op undefined *)
    | (_, Identifier (_, "undefined") as void), expr
    | expr, (_, Identifier (_, "undefined") as void)
      ->
        let void_t = expression cx void in
        undef_test loc ~sense ~strict expr void_t

    (* expr op void(...) *)
    | (_, Unary ({ Unary.operator = Unary.Void; _ }) as void), expr
    | expr, (_, Unary ({ Unary.operator = Unary.Void; _ }) as void)
      ->
        let void_t = expression cx void in
        void_test loc ~sense ~strict expr void_t

    (* fallback case for equality relations involving sentinels (this should be
       lower priority since it refines the object but not the property) *)
    | (_, Expression.Member _ as expr), value
    | value, (_, Expression.Member _ as expr)
      ->
        let value_t = expression cx value in
        sentinel_prop_test loc ~sense ~strict expr value_t

    (* for all other cases, walk the AST but always return bool *)
    | expr, value ->
        let t1 = expression cx expr in
        let t2 = expression cx value in
        flow_eqt ~strict loc (t1, t2);
        empty_result (BoolT.at loc)
  in

  let mk_and map1 map2 = Key_map.merge
    (fun _ p1 p2 -> match (p1,p2) with
      | (None, None) -> None
      | (Some p, None)
      | (None, Some p) -> Some p
      | (Some p1, Some p2) -> Some (AndP(p1,p2))
    )
    map1 map2
  in

  let mk_or map1 map2 = Key_map.merge
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
      property = Member.PropertyIdentifier (prop_loc, prop_name);
        _
      }
    ->
      let obj_t = match _object with
      | super_loc, Super ->
          super_ cx (mk_reason (RProperty (Some prop_name)) super_loc)
      | _ ->
          (* use `expression` instead of `condition` because `_object` is the
             object in a member expression; if it itself is a member expression,
             it must exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here
             as `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
          expression cx _object in
      let expr_reason = mk_reason (RProperty (Some prop_name)) loc in
      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let t = match Refinement.get cx e expr_reason with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else get_prop ~is_cond:true cx
          expr_reason obj_t (prop_reason, prop_name)
      in

      let out = match Refinement.key e with
      | Some name -> result t name t ExistsP true
      | None -> empty_result t
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      begin match Refinement.key _object with
      | Some name ->
        let predicate = PropExistsP (expr_reason, prop_name) in
        out |> add_predicate name obj_t predicate true
      | None ->
        out
      end

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let expr = expression cx e in
      let id = id.Ast.Pattern.Identifier.name in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result expr name expr ExistsP true
      | None, _ -> empty_result expr
    )

  (* expr instanceof t *)
  | _, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      match refinable_lvalue left with
      | Some name, t ->
          let right_t = expression cx right in
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
      Call.callee = callee_loc, Member {
        Member._object = (_, Identifier (_, "Array") as o);
        property = Member.PropertyIdentifier (prop_loc, "isArray");
        _ };
      arguments = [Expression arg]
    } -> (
      (* get Array.isArray in order to populate the type tables, but we don't
         care about the result. *)
      (* TODO: one day we can replace this with a call to `method_call`, and
         then discard the result. currently MethodT does not update type_table
         properly. *)
      let obj_t = expression cx o in
      let reason = mk_reason (RCustom "Array.isArray") callee_loc in
      let fn_t = Flow.mk_tvar_where cx reason (fun t ->
        let prop_reason = mk_reason (RProperty (Some "isArray")) prop_loc in
        Flow.flow cx (obj_t, GetPropT (reason, Named (prop_reason, "isArray"), t))
      ) in
      Hashtbl.replace (Context.type_table cx) prop_loc fn_t;

      match refinable_lvalue arg with
      | Some name, t ->
          result BoolT.t name t ArrP true
      | None, _ ->
          empty_result BoolT.t
    )

  (* test1 && test2 *)
  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let reason = mk_reason (RCustom "&&") loc in
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx reason map1 xts1
        (fun () -> predicates_of_condition cx right)
      in
      (
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (t1, AndT (reason, t2, t));
        ),
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        Key_map.union xts1 xts2
      )

  (* test1 || test2 *)
  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let reason = mk_reason (RCustom "||") loc in
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx reason not_map1 xts1
        (fun () -> predicates_of_condition cx right)
      in
      (
        Flow.mk_tvar_where cx reason (fun t ->
          Flow.flow cx (t1, OrT (reason, t2, t));
        ),
        mk_or map1 map2,
        mk_and not_map1 not_map2,
        Key_map.union xts1 xts2
      )

  (* !test *)
  | loc, Unary { Unary.operator = Unary.Not; argument; _ } ->
      let (_, map, not_map, xts) = predicates_of_condition cx argument in
      (BoolT.at loc, not_map, map, xts)

  (* ids *)
  | _, This
  | _, Identifier _
  | _, Member _ -> (
      match refinable_lvalue e with
      | Some name, t -> result t name t ExistsP true
      | None, t -> empty_result t
    )

  (* e.m(...) *)
  (* TODO: Don't trap method calls for now *)
  | _, Call { Call.callee = (_, Member _); _ } ->
      empty_result (expression cx e)

  (* f(...) *)
  (* The concrete predicate is not known at this point. We attach a "latent"
     predicate pointing to the type of the function that will supply this
     predicated when it is resolved. *)
  | loc, Call { Call.callee = c; arguments }
    ->
      let is_spread = function | Spread _ -> true | _ -> false in
      if List.exists is_spread arguments then
        empty_result (expression cx e)
      else
        let exp_args = arguments |> List.map (function
          | Expression e -> e
          | _ -> Utils_js.assert_false "No spreads should reach here"
        ) in
        let fun_t, keys, arg_ts, ret_t =
          predicated_call_expression cx (loc, c, exp_args) in
        let args_with_offset = Utils_js.zipi keys arg_ts in
        let emp_pred_map = empty_result ret_t in
        List.fold_left (fun pred_map arg_info -> match arg_info with
          | (idx, Some key, unrefined_t) ->
              let pred = LatentP (fun_t, idx+1) in
              add_predicate key unrefined_t pred true pred_map
          | _ ->
              pred_map
        ) emp_pred_map args_with_offset

  (* fallthrough case: evaluate test expr, no refinements *)
  | e ->
      empty_result (expression cx e)
))

(* Conditional expressions are checked like expressions, except that property
   accesses are provisionally allowed even when such properties do not exist.
   This accommodates the common JavaScript idiom of testing for the existence
   of a property before using that property. *)
and condition cx e =
  expression ~is_cond:true cx e

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
      then TestPropT (reason, Named (prop_reason, name), t)
      else GetPropT (reason, Named (prop_reason, name), t)
    in
    Flow.flow cx (tobj, get_prop_u)
  )

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx loc prop_loc expr obj_t m args_ =
  let open Ast.Expression in
  let reason = mk_reason (RCustom (spf "Object.%s" m)) loc in
  match (m, args_) with
  | ("create", [ Expression e ]) ->
    let proto = expression cx e in
    Flow.mk_object_with_proto cx reason proto

  | ("create", [ Expression e;
                 Expression (_, Object { Object.properties }) ]) ->
    let proto = expression cx e in
    let pmap = prop_map_of_object cx properties in
    let map = SMap.fold (fun x p acc ->
      match Property.read_t p with
      | None ->
        (* Since the properties object must be a literal, and literal objects
           can only ever contain neutral fields, this should not happen. *)
        Flow_error.(add_output cx
          (EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)));
        acc
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let t = Flow.mk_tvar_where cx reason (fun tvar ->
          Flow.flow cx (spec, GetPropT (reason, Named (reason, "value"), tvar))
        ) in
        let p = Field (t, Neutral) in
        SMap.add x p acc
    ) pmap SMap.empty in
    Flow.mk_object_with_map_proto cx reason map proto

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let arr_reason = mk_reason RArrayType loc in
    let o = expression cx e in
    ArrT (arr_reason,
      Flow.mk_tvar_where cx arr_reason (fun tvar ->
        let keys_reason = replace_reason (fun desc ->
          RCustom (spf "element of %s" (string_of_desc desc))
        ) reason in
        Flow.flow cx (o, GetKeysT (keys_reason, tvar));
      ),
      []
    )

  | ("defineProperty", [ Expression e;
                         Expression ((ploc, Ast.Expression.Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ })
                           as key);
                         Expression config ]) ->
    let o = expression cx e in
    let _ = expression cx key in
    let spec = expression cx config in
    let tvar = Flow.mk_tvar cx reason in
    let prop_reason = mk_reason (RProperty (Some x)) ploc in
    Flow.flow cx (spec, GetPropT (reason, Named (reason, "value"), tvar));
    Flow.flow cx (o, SetPropT (reason, Named (prop_reason, x), tvar));
    o

  | ("defineProperties", [ Expression e;
                         Expression (_, Object { Object.properties }) ]) ->
    let o = expression cx e in
    let pmap = prop_map_of_object cx properties in
    pmap |> SMap.iter (fun x p ->
      match Property.read_t p with
      | None ->
        (* Since the properties object must be a literal, and literal objects
           can only ever contain neutral fields, this should not happen. *)
        Flow_error.(add_output cx
          (EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)));
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let tvar = Flow.mk_tvar cx reason in
        Flow.flow cx (spec, GetPropT (reason, Named (reason, "value"), tvar));
        Flow.flow cx (o, SetPropT (reason, Named (reason, x), tvar));
    );
    o

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | ("freeze", [Expression ((arg_loc, Object _) as e)]) ->
    let arg_t = expression cx e in

    let reason_arg = mk_reason (RFrozen RObject) arg_loc in
    let arg_t = Flow.mk_tvar_where cx reason_arg (fun tvar ->
      Flow.flow cx (arg_t, ObjFreezeT (reason_arg, tvar));
    ) in

    let reason = mk_reason (RMethodCall (Some m)) loc in
    method_call cx reason prop_loc (expr, obj_t, m) [arg_t]

  (* TODO *)
  | (_, args) ->
    let argts = List.map (expression_or_spread cx) args in
    let reason = mk_reason (RMethodCall (Some m)) loc in
    method_call cx reason prop_loc (expr, obj_t, m) argts

and extract_setter_type = function
  | FunT (_, _, _, { params_tlist = [param_t]; _; }) -> param_t
  | _ ->  failwith "Setter property with unexpected type"

and extract_getter_type = function
  | FunT (_, _, _, { return_t; _; }) -> return_t
  | _ -> failwith "Getter property with unexpected type"

and extract_class_name class_loc  = Ast.Class.(function {id; _;} ->
  match id with
  | Some(name_loc, name) -> (name_loc, name)
  | None -> (class_loc, "<<anonymous class>>")
)

and mk_class cx loc reason c =
  let self = Flow.mk_tvar cx reason in
  let class_sig =
    Class_sig.mk cx loc reason self c ~expr:expression
  in
  class_sig |> Class_sig.generate_tests cx (fun class_sig ->
    Class_sig.check_super cx class_sig;
    Class_sig.toplevels cx class_sig
      ~decls:toplevel_decls
      ~stmts:toplevels
      ~expr:expression
  );
  let class_t = Class_sig.classtype cx class_sig in
  Flow.unify cx self class_t;
  class_t

(* Given a function declaration and types for `this` and `super`, extract a
   signature consisting of type parameters, parameter types, parameter names,
   and return type, check the body against that signature by adding `this`
   and super` to the environment, and return the signature. *)
and function_decl id cx reason func this super =
  let func_sig = Func_sig.mk cx SMap.empty ~expr:expression reason func in

  let this, super =
    let new_entry t = Scope.Entry.new_var ~loc:(loc_of_t t) t in
    new_entry this, new_entry super
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
and mk_function id cx reason func =
  let this = Flow.mk_tvar cx (replace_reason_const RThis reason) in
  (* Normally, functions do not have access to super. *)
  let super = ObjProtoT (replace_reason_const RNoSuper reason) in
  let func_sig = function_decl id cx reason func this super in
  Func_sig.functiontype cx this func_sig

(* Process an arrow function, returning a (polymorphic) function type. *)
and mk_arrow cx reason func =
  let this = this_ cx reason in
  let super = super_ cx reason in
  let {Ast.Function.id; _} = func in
  let func_sig = function_decl id cx reason func this super in
  (* Do not expose the type of `this` in the function's type. The call to
     function_decl above has already done the necessary checking of `this` in
     the body of the function. Now we want to avoid re-binding `this` to
     objects through which the function may be called. *)
  Func_sig.functiontype cx Flow.dummy_this func_sig

(* This function is around for the sole purpose of modeling some method-like
   behaviors of non-ES6 React classes. It is otherwise deprecated. *)
and mk_method cx reason func this =
  let super = ObjProtoT reason in
  let id = None in
  let func_sig = function_decl id cx reason func this super in
  Func_sig.methodtype_DEPRECATED func_sig

(* Transform predicate declare functions to functions whose body is the
   predicate declared for the funcion *)
and declare_function_to_function_declaration cx id typeAnnotation predicate =
  match predicate with
  | Some (loc, Ast.Type.Predicate.Inferred) ->
      Flow_error.(add_output cx
        (EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)));
      None

  | Some (loc, Ast.Type.Predicate.Declared e) -> begin
      match typeAnnotation with
      | (_, (_, Ast.Type.Function
        { Ast.Type.Function.params = (params, rest);
          Ast.Type.Function.returnType;
          Ast.Type.Function.typeParameters;
        })) ->
          let param_type_to_param = Ast.Type.Function.(
            fun (l, { Param.name; Param.typeAnnotation; _ }) ->
              let name = match name with
              | Some name -> name
              | None ->
                  let name_loc = fst typeAnnotation in
                  Flow_error.(add_output cx (EUnsupportedSyntax
                    (loc, PredicateDeclarationAnonymousParameters)));
                  (name_loc, "_")
              in
              let name' = ({ Ast.Pattern.Identifier.
                name;
                typeAnnotation = Some (fst typeAnnotation, typeAnnotation);
                optional = false;
              }) in
              (l, Ast.Pattern.Identifier name')
          ) in
          let params = List.map param_type_to_param params in
          let rest = Ast.Type.Function.(
            match rest with
            | Some (rest_loc, { RestParam.argument; }) ->
              let argument = param_type_to_param argument in
              Some (rest_loc, { Ast.Function.RestElement.argument; })
            | None -> None
          ) in
          let body = Ast.Function.BodyBlock (loc, {Ast.Statement.Block.body = [
              (loc, Ast.Statement.Return {
                Ast.Statement.Return.argument = Some e
              })
            ]}) in
          let returnType = Some (loc, returnType) in
          Some (Ast.Statement.FunctionDeclaration Ast.Function.({
            id = Some id;
            params = (params, rest);
            body = body;
            async = false;
            generator = false;
            predicate = Some (loc, Ast.Type.Predicate.Inferred);
            expression = false;
            returnType = returnType;
            typeParameters;
          }))

      | _ ->
        None
      end
  | _ ->
      None
