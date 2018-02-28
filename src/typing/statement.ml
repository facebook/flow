(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module contains the traversal functions which set up subtyping
   constraints for every expression, statement, and declaration form in a
   JavaScript AST; the subtyping constraints are themselves solved in module
   Flow_js. It also manages environments, including not only the maintenance of
   scope information for every function (pushing/popping scopes, looking up
   variables) but also flow-sensitive information about local variables at every
   point inside a function (and when to narrow or widen their types). *)

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

let ident_name (_, name) = name

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
      Scope.Entry.(Let LetVarBinding), Env.bind_let
    | VariableDeclaration.Var ->
      Scope.Entry.(Var VarBinding), Env.bind_var
  in

  let str_of_kind = Scope.Entry.string_of_value_kind value_kind in

  let declarator = Ast.(function
    | (loc, Pattern.Identifier { Pattern.Identifier.name=(id_loc, name); _ }) ->
      let desc = RIdentifier name in
      let r = mk_reason desc id_loc in
      (* A variable declaration may have a type annotation, but trying to
         resolve the type annotation now may lead to errors, since in general it
         may contain types that will be declared later in this scope. So for
         now, we create a tvar that will serve as the declared type. Later, we
         will resolve the type annotation and unify it with this tvar. *)
      let t = Tvar.mk cx r in
      Type_table.set (Context.type_table cx) loc t;
      bind cx name t id_loc
    | (loc, _) as p ->
      let pattern_name = internal_pattern_name loc in
      let desc = RCustom (spf "%s _" str_of_kind) in
      let r = mk_reason desc loc in
      let typeAnnotation = type_of_pattern p in
      let t = typeAnnotation |>
        (* TODO: delay resolution of type annotation like above? *)
        Anno.mk_type_annotation cx SMap.empty r in
      bind cx pattern_name t loc;
      let expr _ _ = EmptyT.at loc in (* don't eval computed property keys *)
      destructuring cx ~expr t None None p ~f:(fun ~use_op:_ loc name _default t ->
        let t = match typeAnnotation with
        | None -> t
        | Some _ ->
          let r = mk_reason (RIdentifier name) loc in
          EvalT (t, DestructuringT (r, Become), mk_id())
        in
        Type_table.set (Context.type_table cx) loc t;
        bind cx name t loc
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

  | (_, DeclareTypeAlias { TypeAlias.id = (name_loc, name); _ } )
  | (_, TypeAlias { TypeAlias.id = (name_loc, name); _ } ) ->
      let r = DescFormat.type_reason name name_loc in
      let tvar = Tvar.mk cx r in
      Env.bind_type cx name tvar name_loc

  | (_, DeclareOpaqueType { OpaqueType.id = (name_loc, name); _ } )
  | (_, OpaqueType { OpaqueType.id = (name_loc, name); _ } ) ->
      let r = DescFormat.type_reason name name_loc in
      let tvar = Tvar.mk cx r in
      Env.bind_type cx name tvar name_loc

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

  | (loc, FunctionDeclaration func) ->
      (match func.Ast.Function.id with
      | Some (_, name) ->
        let r = func_reason func loc in
        let tvar = Tvar.mk cx r in
        Env.bind_fun cx name tvar loc
      | None ->
        failwith (
          "Flow Error: Nameless function declarations should always be given " ^
          "an implicit name before they get hoisted!"
        )
      )

  | (loc, DeclareVariable { DeclareVariable.id = (id_loc, name); _ }) ->
      let r = mk_reason (RCustom (spf "declare %s" name)) loc in
      let t = Tvar.mk cx r in
      Type_table.set (Context.type_table cx) id_loc t;
      Env.bind_declare_var cx name t id_loc

  | (loc, DeclareFunction { DeclareFunction.
      id = (id_loc, name) as id;
      typeAnnotation;
      predicate}) ->
      (match declare_function_to_function_declaration
        cx id typeAnnotation predicate with
      | None ->
          let r = mk_reason (RCustom (spf "declare %s" name)) loc in
          let t =
            Anno.mk_type_annotation cx SMap.empty r (Some typeAnnotation) in
          Type_table.set (Context.type_table cx) id_loc t;
          let id_info = name, t, Type_table.Other in
          Type_table.set_info (Context.type_table cx) id_loc id_info;
          Env.bind_declare_fun cx name t id_loc
      | Some func_decl ->
          statement_decl cx (loc, func_decl)
      )

  | (_, VariableDeclaration decl) ->
      variable_decl cx decl

  | (_, ClassDeclaration { Ast.Class.id; _ }) -> (
      match id with
      | Some (name_loc, name) ->
        let r = mk_reason (RType name) name_loc in
        let tvar = Tvar.mk cx r in
        Env.bind_implicit_let Scope.Entry.ClassNameBinding cx name tvar name_loc
      | None -> ()
    )

  | (_, DeclareClass { DeclareClass.id = (name_loc, name); _ })
  | (_, DeclareInterface { Interface.id = (name_loc, name); _ })
  | (_, InterfaceDeclaration { Interface.id = (name_loc, name); _ }) as stmt ->
      let is_interface = match stmt with
      | (_, DeclareInterface _) -> true
      | (_, InterfaceDeclaration _) -> true
      | _ -> false in
      let r = mk_reason (RType name) name_loc in
      let tvar = Tvar.mk cx r in
      (* interface is a type alias, declare class is a var *)
      if is_interface
      then Env.bind_type cx name tvar name_loc
      else Env.bind_declare_var cx name tvar name_loc

  | (loc, DeclareModule { DeclareModule.id; _ }) ->
      let name = match id with
      | DeclareModule.Identifier (_, value)
      | DeclareModule.Literal (_, { Ast.StringLiteral.value; _ }) -> value
      in
      let r = mk_reason (RCustom (spf "module `%s`" name)) loc in
      let t = Tvar.mk cx r in
      Type_table.set (Context.type_table cx) loc t;
      Env.bind_declare_var cx (internal_module_name name) t loc

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
        | Some (NamedOpaqueType (loc, t)) ->
            statement_decl cx (loc, OpaqueType t)
        | Some (Interface (loc, i)) ->
            statement_decl cx (loc, InterfaceDeclaration i)
        | None ->
            if Option.is_none default
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
  | (_, ImportDeclaration { ImportDeclaration.importKind; specifiers; default; source = _ }) ->
      let isType =
        match importKind with
        | ImportDeclaration.ImportType -> true
        | ImportDeclaration.ImportTypeof -> true
        | ImportDeclaration.ImportValue -> false
      in

      let bind_import local_name loc isType =
        let reason = if isType
          then DescFormat.type_reason local_name loc
          else mk_reason (RIdentifier local_name) loc in
        let tvar = Tvar.mk cx reason in
        if isType
        then Env.bind_import_type cx local_name tvar loc
        else Env.bind_import cx local_name tvar loc
      in

      Option.iter ~f:(fun local ->
        bind_import (ident_name local) (fst local) isType
      ) default;

      Option.iter ~f:(function
        | ImportDeclaration.ImportNamespaceSpecifier (_, local) ->
          bind_import (ident_name local) (fst local) isType

        | ImportDeclaration.ImportNamedSpecifiers named_specifiers ->
          List.iter (fun { ImportDeclaration.local; remote; kind;} ->
            let remote_name = ident_name remote in
            let (local_name, loc) = (
              match local with
              | Some local ->
                (ident_name local, Loc.btwn (fst remote) (fst local))
              | None ->
                (remote_name, fst remote)
            ) in
            let isType = isType || (
              match kind with
              | None -> isType
              | Some kind ->
                kind = ImportDeclaration.ImportType
              || kind = ImportDeclaration.ImportTypeof
            ) in
            bind_import local_name loc isType
          ) named_specifiers
      ) specifiers
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
      let warn_unreachable loc =
        Flow.add_output cx (Flow_error.EUnreachable loc) in
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

  let interface_helper cx loc (iface_sig, self) =
    let def_reason = mk_reason (desc_of_t self) loc in
    iface_sig |> Iface_sig.generate_tests cx (fun iface_sig ->
      Iface_sig.check_super cx def_reason iface_sig;
      Iface_sig.check_implements cx def_reason iface_sig
    );
    let t = Iface_sig.classtype ~check_polarity:false cx iface_sig in
    Flow.unify cx self t;
    Type_table.set (Context.type_table cx) loc t;
    t
  in

  let interface cx loc decl =
    let { Interface.id = (name_loc, name); _ } = decl in
    let reason = DescFormat.instance_reason name name_loc in
    let iface_sig = Iface_sig.of_interface cx reason decl in
    let t = interface_helper cx loc iface_sig in
    Env.init_type cx name t loc
  in

  let declare_class cx loc decl =
    let { DeclareClass.id = (name_loc, name); _ } = decl in
    let reason = DescFormat.instance_reason name name_loc in
    let class_sig = Iface_sig.of_declare_class cx reason decl in
    let t = interface_helper cx loc class_sig in
    let use_op = Op (AssignVar {
      var = Some (mk_reason (RIdentifier name) loc);
      init = reason_of_t t;
    }) in
    Env.init_var ~has_anno:false cx ~use_op name t loc
  in

  let catch_clause cx { Try.CatchClause.param; body = (_, b) } =
    Ast.Pattern.(match param with
      | loc, Identifier {
          Identifier.name = (_, name); typeAnnotation = None; _;
        } ->
          let r = mk_reason (RCustom "catch") loc in
          let t = Tvar.mk cx r in

          Type_table.set (Context.type_table cx) loc t;

          (match Env.in_lex_scope cx (fun () ->
            Scope.(Env.bind_implicit_let
              ~state:State.Initialized Entry.CatchParamBinding cx name t loc);

            Abnormal.catch_control_flow_exception (fun () ->
              toplevel_decls cx b.Block.body;
              toplevels cx b.Block.body
            )
          ) with
          | Some exn -> Abnormal.throw_control_flow_exception exn
          | None -> ()
          )

      | loc, Identifier _ ->
          Flow.add_output cx
            Flow_error.(EUnsupportedSyntax (loc, CatchParameterAnnotation))

      | loc, _ ->
          Flow.add_output cx
            Flow_error.(EUnsupportedSyntax (loc, CatchParameterDeclaration))
    )
  in

  function

  | (_, Empty) -> ()

  | (_, Block { Block.body }) ->
      Env.in_lex_scope cx (fun () ->
        toplevel_decls cx body;
        toplevels cx body
      )

  | (_, Expression { Expression.expression = e; directive = _ }) ->
      ignore (expression cx e)

  (* Refinements for `if` are derived by the following Hoare logic rule:

     [Pre & c] S1 [Post1]
     [Pre & ~c] S2 [Post2]
     Post = Post1 | Post2
     ----------------------------
     [Pre] if c S1 else S2 [Post]
  *)
  | (loc, If { If.test; consequent; alternate }) ->
      let loc_test, _ = test in
      let _, preds, not_preds, xts =
        predicates_of_condition cx test in

      (* grab a reference to the incoming env -
         we'll restore it and merge branched envs later *)
      let start_env =  Env.peek_env () in
      let oldset = Changeset.clear () in

      (* swap in a refined clone of initial env for then *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc_test preds xts)
      );

      let exception_then = Abnormal.catch_control_flow_exception
        (fun () -> statement cx consequent)
      in

      (* grab a reference to env after then branch *)
      let then_env = Env.peek_env () in

      (* then swap in a refined clone of initial env for else *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc_test not_preds xts)
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
        Env.merge_env cx loc (start_env, then_env, else_env) newset;
        start_env

      | None, None ->
        (* if neither branch has abnormal flow, then refinements that happen in
           the branches should be forgotten since the original type covers
           all of the options. *)
        Env.merge_env cx loc
          (start_env, then_env, else_env)
          (Changeset.exclude_refines newset);
        start_env
      in
      Env.update_env cx loc end_env;

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
        let oldset = Changeset.clear () in
        let label = Some name in
        let save_break = Abnormal.clear_saved (Abnormal.Break label) in
        let save_continue = Abnormal.clear_saved (Abnormal.Continue label) in

        let env = Env.peek_env () in
        Env.widen_env cx loc;

        let loop_env = Env.clone_env env in
        Env.update_env cx loc loop_env;

        Abnormal.(
          check_control_flow_exception (
            ignore_break_or_continue_to_label label (
              fun () -> statement cx body)));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue label) save_continue <> None
        then Env.havoc_vars newset;

        Env.copy_env cx loc (env,loop_env) newset;

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
      Env.reset_current_activation loc;
      Abnormal.save_and_throw (Abnormal.Break label_opt) ~env

  | (loc, Continue { Continue.label }) ->
      let label_opt = match label with
        | None -> None
        | Some (_, name) -> Some name
      in
      Env.reset_current_activation loc;
      Abnormal.save_and_throw (Abnormal.Continue label_opt)

  | (_, With _) ->
      (* TODO or disallow? *)
      ()

  | (loc, DeclareTypeAlias {TypeAlias.id=(name_loc, name); typeParameters; right;})
  | (loc, TypeAlias {TypeAlias.id=(name_loc, name); typeParameters; right;}) ->
      let r = DescFormat.type_reason name name_loc in
      let typeparams, typeparams_map =
        Anno.mk_type_param_declarations cx typeParameters in
      let t = Anno.convert cx typeparams_map right in
      let t =
        let mod_reason = replace_reason ~keep_def_loc:true (fun desc -> RTypeAlias (name, desc)) in
        let rec loop = function
        | ExactT (r, t) -> ExactT (mod_reason r, loop t)
        | DefT (r, MaybeT t) -> DefT (mod_reason r, MaybeT (loop t))
        | t -> mod_reason_of_t mod_reason t
        in
        loop t
      in
      let type_ = poly_type (Context.make_nominal cx) typeparams (DefT (r, TypeT t)) in
      Flow.check_polarity cx Positive t;
      Type_table.set (Context.type_table cx) loc type_;
      let id_info = name, type_, Type_table.Other in
      Type_table.set_info (Context.type_table cx) name_loc id_info;
      Env.init_type cx name type_ name_loc

  | (loc, DeclareOpaqueType
    {OpaqueType.id=(name_loc, name); typeParameters; impltype; supertype})
  | (loc, OpaqueType {OpaqueType.id=(name_loc, name); typeParameters; impltype; supertype}) ->
      let r = DescFormat.type_reason name name_loc in
      let typeparams, typeparams_map =
        Anno.mk_type_param_declarations cx typeParameters in
      let underlying_t = Option.map ~f:(Anno.convert cx typeparams_map) impltype in
      let opaque_arg_polarities = List.fold_left (fun acc tparam ->
        SMap.add tparam.name tparam.polarity acc) SMap.empty typeparams in
      let super_t = Option.map supertype (Anno.convert cx typeparams_map) in
      let opaquetype = { underlying_t;
                         super_t;
                         opaque_id = Context.make_nominal cx;
                         opaque_arg_polarities;
                         opaque_type_args = SMap.map (fun t -> (reason_of_t t, t)) typeparams_map;
                         opaque_name = name} in
      let t = OpaqueT (mk_reason (ROpaqueType name) loc, opaquetype) in
      Flow.check_polarity cx Positive t;
      let type_ = poly_type (Context.make_nominal cx) typeparams (DefT (r, TypeT t)) in
      let open Flow in
      let () = match underlying_t, super_t with
      | Some l, Some u ->
        generate_tests cx r typeparams (fun map_ ->
          flow_t cx (subst cx map_ l, subst cx map_ u)
        )
      | _ -> ()
      in
      Type_table.set (Context.type_table cx) loc type_;
      let id_info = name, type_, Type_table.Other in
      Type_table.set_info (Context.type_table cx) name_loc id_info;
      Env.init_type cx name type_ name_loc

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
      let update_switch_state (case_env, case_writes, _test_refis, loc) =
        let case_env = ListUtils.last_n incoming_depth case_env in
        let state = match !switch_state with
        | None ->
          case_env, Changeset.empty, case_writes
        | Some (env, partial_writes, total_writes) ->
          let case_diff = Changeset.comp case_writes total_writes in
          let partial_writes = Changeset.union partial_writes case_diff in
          let total_writes = Changeset.inter case_writes total_writes in
          (* merge new case into switch env *)
          Env.merge_env cx loc (env, env, case_env) case_writes;
          env, partial_writes, total_writes
        in switch_state := Some state
      in

      (* traverse case list, get list of control flow exits *)
      let exits = cases |> List.map (
        fun (loc, { Switch.Case.test; consequent }) ->

        (* compute predicates implied by case expr or default *)
        let _, preds, not_preds, xtypes = match test with
        | None ->
          EmptyT.at loc, Key_map.empty, Key_map.empty, Key_map.empty
        | Some expr ->
          let fake_ast = loc, Ast.Expression.(Binary {
            Binary.operator = Binary.StrictEqual;
            left = discriminant; right = expr
          }) in
          predicates_of_condition cx fake_ast
        in

        (* swap in case's starting env and clear changeset *)
        let case_env = Env.clone_env case_start_env in
        Env.update_env cx loc case_env;
        let save_changes = Changeset.clear () in

        (* add test refinements - save changelist for later *)
        let test_refis = Env.refine_with_preds cx loc preds xtypes in

        (* merge env changes from fallthrough case, if present *)
        Option.iter !fallthrough_case ~f:(fun (env, writes, refis, _) ->
          let chg = Changeset.union writes refis in
          Env.merge_env cx loc (case_env, case_env, env) chg
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
          then Some (case_env, case_writes, test_refis, loc)
          else None;

        (* if we break to end, add effects to terminal state *)
        if breaks_to_end then begin match break_opt with
          | None ->
            Flow.add_output cx
              Flow_error.(EInternal (loc, BreakEnvMissingForCase))
          | Some break_env ->
            update_switch_state (break_env, case_writes, test_refis, loc)
        end;

        (* add negative refis of this case's test to common start env *)
        (* TODO add API to do this without having to swap in env *)
        Env.update_env cx loc case_start_env;
        let _ = Env.refine_with_preds cx loc not_preds xtypes in

        exit
      ) in

    (* if last case fell out, update terminal switch state with it *)
    Option.iter !fallthrough_case ~f:update_switch_state;

    (** env in switch_state has accumulated switch effects. now merge in
        original types for partially written values, and swap env in *)
    Option.iter !switch_state ~f:(fun (env, partial_writes, _) ->
      Env.merge_env cx switch_loc (env, env, incoming_env) partial_writes;
      Env.update_env cx switch_loc env);

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
      let ret = Env.get_internal_var cx "return" loc in
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
        let t' = Flow.get_builtin_typeapp cx reason "Promise" [
          Tvar.mk_derivable_where cx reason (fun tvar ->
            let funt = Flow.get_builtin cx "$await" reason in
            let callt = mk_functioncalltype reason [Arg t] tvar in
            let reason = repos_reason (loc_of_reason (reason_of_t t)) reason in
            Flow.flow cx (funt, CallT (unknown_use, reason, callt))
          )
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | Scope.Generator ->
        (* Convert the return expression's type R to Generator<Y,R,N>, where
         * Y and R are internals, installed earlier. *)
        let reason = mk_reason (RCustom "generator return") loc in
        let t' = Flow.get_builtin_typeapp cx reason "Generator" [
          Env.get_internal_var cx "yield" loc;
          Tvar.mk_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_internal_var cx "next" loc
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | Scope.AsyncGenerator ->
        let reason = mk_reason (RCustom "async generator return") loc in
        let t' = Flow.get_builtin_typeapp cx reason "AsyncGenerator" [
          Env.get_internal_var cx "yield" loc;
          Tvar.mk_derivable_where cx reason (fun tvar ->
            Flow.flow_t cx (t, tvar)
          );
          Env.get_internal_var cx "next" loc
        ] in
        Flow.reposition cx ~desc:(desc_of_t t) loc t'
      | _ -> t
      in
      let use_op = Op (FunReturnStatement {
        value = Option.value_map argument ~default:(reason_of_t t) ~f:mk_expression_reason;
      }) in
      Flow.flow cx (t, UseT (use_op, ret));
      Env.reset_current_activation loc;
      Abnormal.save_and_throw Abnormal.Return

  | (loc, Throw { Throw.argument }) ->
      ignore (expression cx argument);
      Env.reset_current_activation loc;
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
      let oldset = Changeset.clear () in

      (* save ref to initial env and swap in a clone *)
      let start_env = Env.peek_env () in
      Env.(update_env cx loc (clone_env start_env));

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
          merge_env cx loc (e, e, try_env) (Changeset.peek ());
          update_env cx loc e
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
        merge_env cx loc (e, e, catch_env) (Changeset.peek ());
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
        Env.update_env cx loc nonthrow_finally_env;
        None

      | Some (_, { Block.body }) ->
        (* analyze twice, with different start states *)

        (* 1. throwing-finally case. *)
        (* env may be in any state from start of try through end of catch *)
        Env.(
          let e = clone_env start_env in
          merge_env cx loc (e, e, catch_env) (Changeset.peek ());
          update_env cx loc e
        );

        let result = Env.in_lex_scope cx (fun () ->
          Abnormal.catch_control_flow_exception (fun () ->
            toplevel_decls cx body;
            toplevels cx body
          )
        ) in

        (* 2. non-throwing finally case. *)
        Env.update_env cx loc nonthrow_finally_env;

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
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in

      (* generate loop test preds and their complements *)
      let _, preds, not_preds, orig_types =
        predicates_of_condition cx test in

      (* save current changeset and install an empty one *)
      let oldset = Changeset.clear () in

      (* widen_env wraps specifics in tvars, anticipating widening inflows *)
      Env.widen_env cx loc;

      (* start_env is Pre above: env as of loop top *)
      let start_env = Env.peek_env () in

      (* swap in Pre & c *)
      Env.(
        update_env cx loc (clone_env start_env);
        ignore (refine_with_preds cx loc preds orig_types)
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
        copy_env cx loc (start_env, body_env) newset;
        update_env cx loc start_env;
        ignore (refine_with_preds cx loc not_preds orig_types)
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
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let env =  Env.peek_env () in
      let oldset = Changeset.clear () in
      (* env = Pre *)
      (* ENV = [env] *)

      Env.widen_env cx loc;
      (* env = Pre', Pre' > Pre *)

      let body_env = Env.clone_env env in
      Env.update_env cx loc body_env;
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

      let _ = Env.refine_with_preds cx loc preds xtypes in
      (* body_env = Post' & c *)

      let newset = Changeset.merge oldset in
      Env.copy_env cx loc (env, body_env) newset;
      (* Pre' > Post' & c *)

      Env.update_env cx loc done_env;
      let _ = Env.refine_with_preds cx loc not_preds xtypes in
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
        Env.widen_env cx loc;

        let do_env = Env.clone_env env in
        Env.update_env cx loc do_env;

        let _, preds, not_preds, xtypes = match test with
          | None ->
              EmptyT.at loc, Key_map.empty, Key_map.empty,
              Key_map.empty (* TODO: prune the "not" case *)
          | Some expr ->
              predicates_of_condition cx expr
        in

        let body_env = Env.clone_env do_env in
        Env.update_env cx loc body_env;
        let _ = Env.refine_with_preds cx loc preds xtypes in

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
        Env.copy_env cx loc (env, body_env) newset;

        Env.update_env cx loc do_env;
        let _ = Env.refine_with_preds cx loc not_preds xtypes in
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
        Env.widen_env cx loc;

        let body_env = Env.clone_env env in
        Env.update_env cx loc body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx right in
        let _ = Env.refine_with_preds cx loc preds xtypes in

        (match left with
          | ForIn.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              variable_decl cx decl;
              variable cx kind ~if_uninitialized:StrT.at vdecl

          | ForIn.LeftPattern (loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
              name; _
            }) ->
              let name = ident_name name in
              let t = StrT.at loc in
              let use_op = Op (AssignVar {
                var = Some (mk_reason (RIdentifier name) loc);
                init = reason_of_t t;
              }) in
              ignore Env.(set_var cx ~use_op name t loc)

          | _ ->
              Flow.add_output cx Flow_error.(EInternal (loc, ForInLHS))
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx loc (env,body_env) newset;

        Env.update_env cx loc env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (loc, ForOf { ForOf.left; right; body; async; }) ->
      let reason_desc = match left with
      | ForOf.LeftDeclaration (_, {VariableDeclaration.declarations =
          [(_, {VariableDeclaration.Declarator.id = (_, Ast.Pattern.Identifier
            {Ast.Pattern.Identifier.name=(_, x); _}); _})]; _}) -> RIdentifier x
      | ForOf.LeftPattern (_, Ast.Pattern.Identifier
          {Ast.Pattern.Identifier.name=(_, x); _}) -> RIdentifier x
      | _ -> RCustom "for-of element"
      in
      let reason = mk_reason reason_desc loc in
      let save_break = Abnormal.clear_saved (Abnormal.Break None) in
      let save_continue = Abnormal.clear_saved (Abnormal.Continue None) in
      let t = expression cx right in

      let element_tvar = Tvar.mk cx reason in
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
        Env.widen_env cx loc;

        let body_env = Env.clone_env env in
        Env.update_env cx loc body_env;

        let _, preds, _, xtypes =
          predicates_of_condition cx right in
        let _ = Env.refine_with_preds cx loc preds xtypes in

        (match left with
          | ForOf.LeftDeclaration (_, ({ VariableDeclaration.
              kind; declarations = [vdecl]
            } as decl)) ->
              let repos_tvar _ = Flow.reposition cx (loc_of_t t) element_tvar in
              variable_decl cx decl;
              variable cx kind ~if_uninitialized:repos_tvar vdecl

          | ForOf.LeftPattern (loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
              name; _
            }) ->
              let name = ident_name name in
              let use_op = Op (AssignVar {
                var = Some (mk_reason (RIdentifier name) loc);
                init = reason_of_t element_tvar;
              }) in
              ignore Env.(set_var cx ~use_op name element_tvar loc)

          | _ ->
              Flow.add_output cx Flow_error.(EInternal (loc, ForOfLHS))
        );

        ignore (Abnormal.catch_control_flow_exception
          (fun () -> statement cx body));

        let newset = Changeset.merge oldset in

        if Abnormal.swap_saved (Abnormal.Continue None) save_continue <> None
        then Env.havoc_vars newset;
        Env.copy_env cx loc (env,body_env) newset;

        Env.update_env cx loc env;
        if Abnormal.swap_saved (Abnormal.Break None) save_break <> None
        then Env.havoc_vars newset
      )

  | (_, Debugger) ->
      ()

  | (loc, FunctionDeclaration func) ->
      let {Ast.Function.id; params; returnType; _} = func in
      let sig_loc = match params, returnType with
      | _, Some (end_loc, _)
      | (end_loc, _), None
         -> Loc.btwn loc end_loc
      in
      let fn_type = mk_function None cx sig_loc func in
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
      Type_table.set (Context.type_table cx) type_table_loc fn_type;
      (match id with
      | Some(id_loc, name) ->
        let id_info = name, fn_type, Type_table.Other in
        Type_table.set_info (Context.type_table cx) id_loc id_info;
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) loc);
          init = reason_of_t fn_type
        }) in
        Env.init_fun cx ~use_op name fn_type loc
      | None -> ())

  | (loc, DeclareVariable { DeclareVariable.
      id = (id_loc, name);
      typeAnnotation;
    }) ->
      let r = mk_reason (RCustom (spf "declare %s" name)) loc in
      let t = Anno.mk_type_annotation cx SMap.empty r typeAnnotation in
      let id_info = name, t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) id_loc id_info;
      Env.unify_declared_type cx name t;

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
      Env.declare_implicit_let Scope.Entry.ClassNameBinding cx name name_loc;
      let class_t = mk_class cx class_loc reason c in
      Type_table.set (Context.type_table cx) class_loc class_t;
      let id_info = name, class_t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) name_loc id_info;
      Env.init_implicit_let
        Scope.Entry.ClassNameBinding
        cx
        ~use_op:(Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) name_loc);
          init = reason_of_t class_t;
        }))
        name
        ~has_anno:false
        class_t
        name_loc

  | (loc, DeclareClass decl) ->
    declare_class cx loc decl

  | (loc, DeclareInterface decl)
  | (loc, InterfaceDeclaration decl) ->
    interface cx loc decl

  | (loc, DeclareModule { DeclareModule.id; body; kind=_; }) ->
    let id_loc, name = match id with
    | DeclareModule.Identifier (id_loc, value)
    | DeclareModule.Literal (id_loc, { Ast.StringLiteral.value; _ }) ->
      id_loc, value
    in
    let _, { Ast.Statement.Block.body = elements } = body in

    let module_ref = Reason.internal_module_name name in

    let module_scope = Scope.fresh () in
    Scope.add_entry
      (Reason.internal_name "exports")
      (Scope.Entry.new_var
        ~loc:Loc.none
        ~specific:Locationless.EmptyT.t
        Locationless.MixedT.t)
      module_scope;

    Env.push_var_scope cx module_scope;
    let outer_module_exports_kind = Context.module_kind cx in
    Context.set_module_kind cx (Context.CommonJSModule None);
    Context.push_declare_module cx module_ref;

    let initial_module_t = module_t_of_cx cx in

    toplevel_decls cx elements;
    toplevels cx elements;

    let reason = mk_reason (RCustom (spf "module `%s`" name)) loc in
    let module_t = match Context.module_kind cx with
    | Context.ESModule -> mk_module_t cx reason
    | Context.CommonJSModule clobbered ->
      let open Scope in
      let open Entry in
      (* TODO: drop `declare var exports` in favor of `declare module.exports` *)
      let legacy_exports = Scope.get_entry "exports" module_scope in
      let cjs_exports = match clobbered, legacy_exports with
      | Some loc, _ -> get_module_exports cx loc
      | None, Some (Value {specific; value_declare_loc; _}) ->
        Flow.add_output cx (Flow_error.EDeprecatedDeclareExports value_declare_loc);
        specific
      | None, _ ->
        let props = SMap.fold (fun x entry acc ->
          match entry with
          | Value {specific; _} ->
            let loc = Some (entry_loc entry) in
            Properties.add_field x Neutral loc specific acc
          | Type _ | Class _ -> acc
        ) module_scope.entries SMap.empty in
        let proto = ObjProtoT reason in
        Obj_type.mk_with_proto cx reason ~props proto
      in
      let type_exports = SMap.fold (fun x entry acc ->
        match entry with
        | Type {_type; _} -> SMap.add x _type acc
        | Value _ | Class _ -> acc
      ) module_scope.entries SMap.empty in
      set_module_t cx reason (fun t ->
        Flow.flow cx (
          module_t_of_cx cx,
          ExportNamedT (reason, false, type_exports, t)
        )
      );
      mk_commonjs_module_t cx reason reason cjs_exports
    in
    let id_info = name, module_t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) id_loc id_info;

    Flow.flow_t cx (module_t, initial_module_t);

    let t = Env.get_var_declared_type cx module_ref loc in
    Flow.flow_t cx (initial_module_t, t);

    Context.pop_declare_module cx;
    Context.set_module_kind cx outer_module_exports_kind;
    Env.pop_var_scope ();



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
            let { DeclareClass.id = (name_loc, name); _; } = c in
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
        | Some (NamedOpaqueType (opaque_loc, ({
            OpaqueType.
            id = (name_loc, name);
            _;
          } as opaque_t))) ->
            statement cx (opaque_loc, OpaqueType opaque_t);
            [(spf "opauqe type %s = ..." name, name_loc, name, None)], ExportType
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
    set_module_kind cx loc (Context.CommonJSModule(Some loc));
    set_module_exports cx loc t

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
              List.rev (Ast_utils.bindings_of_pattern accum id)
            in
            let bound_names = List.fold_left decl_to_bindings [] declarations in
            bound_names |> List.map (fun (loc, name) ->
              (spf "var %s" name, loc, name, None)
            )
          | _, TypeAlias {TypeAlias.id; _} ->
            let name = ident_name id in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, OpaqueType {OpaqueType.id; _} ->
            let name = ident_name id in
            [(spf "opaque type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration {Interface.id; _} ->
            let name = ident_name id in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | None -> [] in

      export_statement cx loc
        ~default:None export_info specifiers source exportKind

  | (loc, ExportDefaultDeclaration { ExportDefaultDeclaration.default; declaration }) ->
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
              List.rev (Ast_utils.bindings_of_pattern accum id)
            in
            let bound_names = List.fold_left decl_to_bindings [] declarations in
            bound_names |> List.map (fun (loc, name) ->
              (spf "var %s" name, loc, name, None)
            )
          | _, TypeAlias {TypeAlias.id; _} ->
            let name = ident_name id in
            [(spf "type %s = ..." name, loc, name, None)]
          | _, OpaqueType {OpaqueType .id; _} ->
            let name = ident_name id in
            [(spf "opaque type %s = ..." name, loc, name, None)]
          | _, InterfaceDeclaration {Interface.id; _} ->
            let name = ident_name id in
            [(spf "interface %s = ..." name, loc, name, None)]
          | _ -> failwith "Parser Error: Invalid export-declaration type!")

      | ExportDefaultDeclaration.Expression expr ->
          let expr_t = expression cx expr in
          [( "<<expression>>", fst expr, "default", Some expr_t)]
      in

      (* export default is always a value *)
      let exportKind = Ast.Statement.ExportValue in

      export_statement cx loc ~default:(Some default) export_info None None exportKind

  | (import_loc, ImportDeclaration import_decl) ->
    Context.add_import_stmt cx import_decl;

    let { ImportDeclaration.source; specifiers; default; importKind } = import_decl in

    let source_loc, { Ast.StringLiteral.value = module_name; _ } = source in

    let type_kind_of_kind = function
      | ImportDeclaration.ImportType -> Type.ImportType
      | ImportDeclaration.ImportTypeof -> Type.ImportTypeof
      | ImportDeclaration.ImportValue -> Type.ImportValue
    in

    let module_t = import cx (source_loc, module_name) import_loc in

    let get_imported_t get_reason import_kind remote_export_name local_name =
      Tvar.mk_where cx get_reason (fun t ->
        let import_type =
          if remote_export_name = "default"
          then ImportDefaultT
            (get_reason, import_kind, (local_name, module_name), t, Context.is_strict cx)
          else ImportNamedT
            (get_reason, import_kind, remote_export_name, module_name, t, Context.is_strict cx)
        in
        Context.add_imported_t cx local_name t;
        Flow.flow cx (module_t, import_type)
      )
    in

    let specifiers = match specifiers with
      | Some (ImportDeclaration.ImportNamedSpecifiers named_specifiers) ->
        named_specifiers |> List.map (function { ImportDeclaration.local; remote; kind;} ->
          let remote_name = ident_name remote in
          let (loc, local_name) = (
            match local with
            | Some local ->
              (Loc.btwn (fst remote) (fst local), ident_name local)
            | None ->
              (fst remote, remote_name)
          ) in
          let imported_t =
            let import_reason =
              mk_reason (RNamedImportedType module_name) (fst remote)
            in
            if Type_inference_hooks_js.dispatch_member_hook
              cx remote_name loc module_t
            then AnyT.why import_reason
            else
              let import_kind = type_kind_of_kind (Option.value ~default:importKind kind) in
              get_imported_t import_reason import_kind remote_name local_name
          in
          let id_info = local_name, imported_t, Type_table.Import (remote_name, module_t) in
          Type_table.set_info (Context.type_table cx) loc id_info;
          (loc, local_name, imported_t, kind)
        )

      | Some (ImportDeclaration.ImportNamespaceSpecifier (ns_loc, local)) ->
        let local_name = ident_name local in

        Type_inference_hooks_js.dispatch_import_hook cx (source_loc, module_name) ns_loc;

        let import_reason =
          let import_str =
            match importKind with
            | ImportDeclaration.ImportType -> "import type"
            | ImportDeclaration.ImportTypeof -> "import typeof"
            | ImportDeclaration.ImportValue -> "import"
          in
          let import_reason_str = spf "%s * as %s" import_str local_name in
          mk_reason (RCustom import_reason_str) import_loc
        in

        (match importKind with
          | ImportDeclaration.ImportType ->
            assert_false "import type * is a parse error"
          | ImportDeclaration.ImportTypeof ->
            let bind_reason = repos_reason (fst local) import_reason in
            let module_ns_t =
              import_ns cx import_reason (fst source, module_name) import_loc
            in
            let module_ns_typeof =
              Tvar.mk_where cx bind_reason (fun t ->
                Context.add_imported_t cx local_name t;
                Flow.flow cx (module_ns_t,
                  ImportTypeofT (bind_reason, "*", t))
              )
            in
            [import_loc, local_name, module_ns_typeof, None]
          | ImportDeclaration.ImportValue ->
            let reason =
              mk_reason (RCustom (spf "module `%s`" module_name)) import_loc
            in
            let module_ns_t =
              import_ns cx reason (fst source, module_name) import_loc
            in
            Context.add_imported_t cx local_name module_ns_t;
            [fst local, local_name, module_ns_t, None]
        )
      | None -> []
    in

    let specifiers = match default with
      | Some local ->
          let local_name = ident_name local in
          let loc = fst local in

          let import_reason_str =
            spf "Default import from `%s`" module_name
          in
          let import_reason = mk_reason (RCustom import_reason_str) loc in

          let imported_t =
            if Type_inference_hooks_js.dispatch_member_hook
              cx "default" loc module_t
            then AnyT.why import_reason
            else
              let import_kind = type_kind_of_kind importKind in
              get_imported_t import_reason import_kind "default" local_name
          in
          let id_info = local_name, imported_t, Type_table.Import ("default", module_t) in
          Type_table.set_info (Context.type_table cx) loc id_info;
          (loc, local_name, imported_t, None) :: specifiers
      | None -> specifiers
    in

    List.iter (fun (loc, local_name, t, specifier_kind) ->
      let t_generic =
        let lookup_mode =
          match Option.value ~default:importKind specifier_kind with
          | ImportDeclaration.ImportType -> ForType
          | ImportDeclaration.ImportTypeof -> ForType
          | ImportDeclaration.ImportValue -> ForValue
        in
        Env.get_var_declared_type ~lookup_mode cx local_name loc
      in
      Flow.unify cx t t_generic
    ) specifiers;
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
    if (Option.is_some default) then " default" else ""
  ) in

  let export_from_local (export_reason, loc, local_name, local_tvar) = (
    let reason =
      mk_reason (RCustom (spf "%s %s" export_reason_start export_reason)) loc
    in
    let local_tvar = match local_tvar with
    | None -> Env.var_ref ~lookup_mode cx local_name loc
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
      set_module_kind cx loc Context.ESModule);

    let local_name = if (Option.is_some default) then "default" else local_name in
    set_module_t cx reason (fun t ->
      Flow.flow cx (
        module_t_of_cx cx,
        ExportNamedT(reason, false, SMap.singleton local_name local_tvar, t)
      )
    )
  ) in

  (match (declaration_export_info, specifiers) with
    (* [declare] export [type] {foo, bar} [from ...]; *)
    | ([], Some (ExportNamedDeclaration.ExportSpecifiers specifiers)) ->
      let export_specifier specifier = (
        let loc, reason, local_name, remote_name =
          match specifier with
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, id);
              exported = None;
            } ->
            let reason = mk_reason (RCustom (spf "export {%s}" id)) loc in
            (loc, reason, id, id)
          | loc, { ExportNamedDeclaration.ExportSpecifier.
              local = (_, local);
              exported = Some (_, exported);
            } ->
            let reason =
              mk_reason (RCustom (spf "export {%s as %s}" local exported)) loc
            in
            (loc, reason, local, exported)
        in

        (**
          * Determine if we're dealing with the `export {} from` form
          * (and if so, retrieve the ModuleNamespaceObject tvar for the
          *  source module)
          *)
        let source_module_tvar = (
          match source with
          | Some (src_loc, { Ast.StringLiteral.value = module_name; _ }) ->
            let reason =
              mk_reason (RCustom "ModuleNamespace for export {} from") src_loc
            in
            Some (import_ns cx reason (src_loc, module_name) loc)
          | None -> None
        ) in

        let local_tvar = (
          match source_module_tvar with
          | Some(tvar) ->
            Tvar.mk_where cx reason (fun t ->
              Flow.flow cx (tvar, GetPropT (unknown_use, reason, Named (reason, local_name), t))
            )
          | None ->
            Env.var_ref ~lookup_mode cx local_name loc
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
        then set_module_kind cx loc Context.ESModule);

        set_module_t cx reason (fun t ->
          Flow.flow cx (
            module_t_of_cx cx,
            ExportNamedT(reason, false, SMap.singleton remote_name local_tvar, t)
          )
        )
      ) in
      List.iter export_specifier specifiers

    (* [declare] export [type] * from "source"; *)
    | [],
      Some (ExportNamedDeclaration.ExportBatchSpecifier
        (batch_loc, star_as_name)
      ) ->
      let source_loc, source_module_name = (
        match source with
        | Some (loc, { Ast.StringLiteral.value; _ }) -> loc, value
        | None -> failwith (
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
        set_module_kind cx loc Context.ESModule;

        let remote_namespace_t =
          if parse_export_star_as = Options.ESPROPOSAL_ENABLE
          then import_ns cx reason (source_loc, source_module_name) loc
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
            ExportNamedT(reason, false, SMap.singleton name remote_namespace_t, t)
          )
        )
      | None ->
        let reason =
          mk_reason
            (RCustom (spf "%s * from %S" export_kind_start source_module_name))
            loc
        in

        (* It's legal to export types from a CommonJS module. *)
        if exportKind != ExportType
        then set_module_kind cx loc Context.ESModule;

        set_module_t cx reason (fun t -> Flow.flow cx (
          import cx (source_loc, source_module_name) loc,
          let module_t = module_t_of_cx cx in
          match exportKind with
          | ExportValue -> CopyNamedExportsT(reason, module_t, t)
          | ExportType -> CopyTypeExportsT(reason, module_t, t)
        ))
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
  (* named prop *)
  | Property (_, Property.Init {
      key =
        Property.Identifier (loc, name) |
        Property.Literal (loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
      value = v; _ }) ->
    let t = expression cx v in
    let id_info = name, t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) loc id_info;
    Properties.add_field name Neutral (Some loc) t map

  (* named method *)
  | Property (_, Property.Method {
      key =
        Property.Identifier (loc, name) |
        Property.Literal (loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
      value = (fn_loc, func);
    }) ->
    let t = expression cx (fn_loc, Ast.Expression.Function func) in
    let id_info = name, t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) loc id_info;
    Properties.add_field name Neutral (Some loc) t map

  (* We enable some unsafe support for getters and setters. The main unsafe bit
  *  is that we don't properly havok refinements when getter and setter methods
  *  are called. *)

  (* unsafe getter property *)
  | Property (loc, Property.Get {
      key =
        Property.Identifier (id_loc, name) |
        Property.Literal (id_loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
      value = (vloc, func);
    }) ->
    Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
    let function_type = mk_function None cx vloc func in
    let return_t = Type.extract_getter_type function_type in
    let id_info = name, return_t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) id_loc id_info;
    Properties.add_getter name (Some id_loc) return_t map

  (* unsafe setter property *)
  | Property (loc, Property.Set {
      key =
        Property.Identifier (id_loc, name) |
        Property.Literal (id_loc, {
          Ast.Literal.value = Ast.Literal.String name;
          _;
        });
      value = (vloc, func);
    }) ->
    Flow_js.add_output cx (Flow_error.EUnsafeGettersSetters loc);
    let function_type = mk_function None cx vloc func in
    let param_t = Type.extract_setter_type function_type in
    let id_info = name, param_t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) id_loc id_info;
    Properties.add_setter name (Some id_loc) param_t map

  (* literal LHS *)
  | Property (loc, Property.Init { key = Property.Literal _; _ })
  | Property (loc, Property.Method { key = Property.Literal _; _ })
  | Property (loc, Property.Get { key = Property.Literal _; _ })
  | Property (loc, Property.Set { key = Property.Literal _; _ }) ->
    Flow.add_output cx
      Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyLiteralNonString));
    map

  (* computed getters and setters aren't supported yet regardless of the
     `enable_getters_and_setters` config option *)
  | Property (loc, Property.Get { key = Property.Computed _; _ })
  | Property (loc, Property.Set { key = Property.Computed _; _ }) ->
    Flow.add_output cx
      Flow_error.(EUnsupportedSyntax (loc, ObjectPropertyComputedGetSet));
    map

  (* computed LHS silently ignored for now *)
  | Property (_, Property.Init { key = Property.Computed _; _ })
  | Property (_, Property.Method { key = Property.Computed _; _ }) ->
    map

  (* spread prop *)
  | SpreadProperty _ ->
    map

  | Property (_, Property.Init { key = Property.PrivateName _; _ })
  | Property (_, Property.Method { key = Property.PrivateName _; _ })
  | Property (_, Property.Get { key = Property.PrivateName _; _ })
  | Property (_, Property.Set { key = Property.PrivateName _; _ }) ->
    failwith "Internal Error: Non-private field with private name"
)

and prop_map_of_object cx props =
  List.fold_left (object_prop cx) SMap.empty props

and object_ cx reason ?(allow_sealed=true) props =
  Ast.Expression.Object.(

  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let obj_proto = ObjProtoT reason in

  (* Return an object with specified sealing. *)
  let mk_object ?(proto=obj_proto) ?(sealed=false) props =
    Obj_type.mk_with_proto cx reason ~sealed ~props proto
  in

  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj =
    Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (to_obj, ObjAssignToT(reason, from_obj, t, ObjAssign));
    )
  in

  (* Add property to object, using optional tout argument to SetElemT to wait
     for the write to happen. This defers any reads until writes have happened,
     to avoid race conditions. *)
  let mk_computed key value obj =
    Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (obj, SetElemT (unknown_use, reason, key, value, Some t))
    )
  in

  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_object ?(proto=obj_proto) ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~proto ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object ~proto map) result
        else result
      in
      if not sealed then result else
        Tvar.mk_where cx reason (fun t ->
          Flow.flow cx (result, ObjSealT (reason, t))
        )
  in

  let sealed, map, proto, result = List.fold_left (
    fun (sealed, map, proto, result) -> function
    | SpreadProperty (_, { SpreadProperty.argument }) ->
        let spread = expression cx argument in
        let obj = eval_object (map, result) in
        let result = mk_spread spread obj in
        false, SMap.empty, proto, Some result
    | Property (_, Property.Init {
        key = Property.Computed k;
        value = v;
        shorthand = _;
      }) ->
        let k = expression cx k in
        let v = expression cx v in
        let obj = eval_object (map, result) in
        let result = mk_computed k v obj in
        sealed, SMap.empty, proto, Some result
    | Property (_, Property.Method {
        key = Property.Computed k;
        value = fn_loc, fn;
      }) ->
        let k = expression cx k in
        let v = expression cx (fn_loc, Ast.Expression.Function fn) in
        let obj = eval_object (map, result) in
        let result = mk_computed k v obj in
        sealed, SMap.empty, proto, Some result
    | Property (_, Property.Init {
        key =
          Property.Identifier (_, "__proto__") |
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String "__proto__";
            _;
          });
        value = v;
        shorthand = false;
      }) ->
        let reason = mk_reason RPrototype (fst v) in
        let t = Tvar.mk_where cx reason (fun t ->
          Flow.flow cx (expression cx v, ObjTestProtoT (reason, t))
        ) in
        sealed, map, Some t, result
    | prop ->
        sealed, object_prop cx map prop, proto, result
  ) (allow_sealed, SMap.empty, None, None) props in

  let sealed = match result with
    | Some _ -> sealed
    | None -> sealed && not (SMap.is_empty map)
  in
  eval_object ?proto ~sealed (map, result)
)

and variable cx kind
  ?if_uninitialized (_, vdecl) = Ast.Statement.(
  let init_var, declare_var = Env.(match kind with
    | VariableDeclaration.Const -> init_const, declare_const
    | VariableDeclaration.Let -> init_let, declare_let
    | VariableDeclaration.Var -> init_var, (fun _ _ _ -> ())
  ) in
  let { VariableDeclaration.Declarator.id; init } = vdecl in
  Ast.Expression.(match init with
    | Some (call_loc, Call { Call.callee = _, Identifier (_, "require"); optional; _ })
        when not (Env.local_scope_entry_exists "require") ->
      warn_or_ignore_optional_chaining optional cx call_loc;
      let loc, _ = id in
      (* Record the loc of the pattern, which contains the locations of any
         local definitions introduced by the pattern. This information is used
         by commands to automatically "follow" such definitions to the actual
         definitions in the required module. *)
      Type_inference_hooks_js.dispatch_require_pattern_hook loc
    | _ -> ()
  );
  match id with
    | (loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
          name = (id_loc, name); typeAnnotation; optional
        }) ->
        (* simple lvalue *)
        (* make annotation, unify with declared type created in variable_decl *)
        let t =
          let desc = RIdentifier name in
          let anno_reason = mk_reason desc loc in
          Anno.mk_type_annotation cx SMap.empty anno_reason typeAnnotation in
        let id_info = name, t, Type_table.Other in
        Type_table.set_info (Context.type_table cx) id_loc id_info;
        Env.unify_declared_type cx name t;
        let has_anno = not (typeAnnotation = None) in
        Type_inference_hooks_js.(dispatch_lval_hook cx name loc (Val t));
        (match init with
          | Some ((rhs_loc, _) as expr) ->
            let rhs = expression cx expr in
            (**
             * Const and let variables are not declared during evaluation of
             * their initializer expressions.
             *)
            declare_var cx name id_loc;
            let rhs = Flow.reposition cx rhs_loc rhs in
            let use_op = Op (AssignVar {
              var = Some (mk_reason (RIdentifier name) id_loc);
              init = mk_expression_reason expr;
            }) in
            init_var cx ~use_op name ~has_anno rhs id_loc
          | None ->
            match if_uninitialized with
            | Some f ->
              if not optional then
                let t = f loc in
                let use_op = Op (AssignVar {
                  var = Some (mk_reason (RIdentifier name) id_loc);
                  init = reason_of_t t;
                }) in
                init_var cx ~use_op name ~has_anno t id_loc
            | None ->
              if has_anno
              then Env.pseudo_init_declared_type cx name id_loc
              else declare_var cx name id_loc;
        )
    | loc, _ ->
        (* compound lvalue *)
        let pattern_name = internal_pattern_name loc in
        let typeAnnotation = type_of_pattern id in
        let has_anno = not (typeAnnotation = None) in
        let (t, init_reason) = match init with
          | Some expr -> (expression cx expr, mk_expression_reason expr)
          | None -> (
            let t = match if_uninitialized with
            | Some f -> f loc
            | None -> VoidT.at loc in
            (t, reason_of_t t)
          )
        in
        let use_op = Op (AssignVar {
          var = None;
          init = init_reason;
        }) in
        init_var cx ~use_op pattern_name ~has_anno t loc;
        destructuring cx ~expr:expression ~f:(fun ~use_op loc name default t ->
          let reason = mk_reason (RIdentifier name) loc in
          Option.iter default (fun d ->
            let default_t = Flow.mk_default cx reason d ~expr:expression in
            Flow.flow_t cx (default_t, t)
          );
          init_var cx ~use_op name ~has_anno t loc
        ) t init None id
)

and mixin_element cx undef_loc el = Ast.Expression.(
  match el with
  | Some (Expression (loc, expr)) ->
      let t = expression cx (loc, expr) in
      Flow.reposition cx loc t
  | Some (Spread (loc, { SpreadElement.argument })) ->
      let t = mixin_element_spread cx argument in
      Flow.reposition cx loc t
  | None -> EmptyT.at undef_loc
)

and expression_or_spread cx = Ast.Expression.(function
  | Expression e -> Arg (expression cx e)
  | Spread (_, { SpreadElement.argument }) -> SpreadArg (expression cx argument)
)

and expression_or_spread_list cx undef_loc = Ast.Expression.(
  List.map (function
  | Some (Expression e) -> UnresolvedArg (expression cx e)
  | None -> UnresolvedArg (EmptyT.at undef_loc)
  | Some (Spread (_, { SpreadElement.argument })) ->
      UnresolvedSpreadArg (expression cx argument)
  )
)

and mixin_element_spread cx (loc, e) =
  let arr = expression cx (loc, e) in
  let reason = mk_reason (RCustom "spread operand") loc in
  Tvar.mk_where cx reason (fun tvar ->
    Flow.flow_t cx (arr, DefT (reason, ArrT (ArrayAT(tvar, None))));
  )

and expression ?(is_cond=false) cx (loc, e) =
  let t = expression_ ~is_cond cx loc e in
  Type_table.set (Context.type_table cx) loc t;
  t

and this_ cx loc = Ast.Expression.(
  match Refinement.get cx (loc, This) loc with
  | Some t -> t
  | None -> Env.var_ref cx (internal_name "this") loc
)

and super_ cx loc =
  Env.var_ref cx (internal_name "super") loc

and expression_ ~is_cond cx loc e = let ex = (loc, e) in Ast.Expression.(match e with

  | Ast.Expression.Literal lit ->
      literal cx loc lit

  (* Treat the identifier `undefined` as an annotation for error reporting
   * purposes. Like we do with other literals. Otherwise we end up pointing to
   * `void` in `core.js`. While possible to re-declare `undefined`, it is
   * unlikely. The tradeoff is worth it. *)
  | Identifier (_, "undefined") ->
      mod_reason_of_t annot_reason (identifier cx "undefined" loc)

  | Identifier (_, name) ->
      identifier cx name loc

  | This ->
      let t = this_ cx loc in
      let id_info = "this", t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) loc id_info;
      t

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
      in Type_table.set (Context.type_table cx) loc t;
      let infer_t = expression cx e in
      let use_op = Op (Cast {
        lower = mk_expression_reason e;
        upper = reason_of_t t;
      }) in
      Flow.flow cx (infer_t, UseT (use_op, t));
      t

  | Member {
      Member._object;
      property = Member.PropertyExpression index;
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      let reason = mk_reason (RProperty None) loc in
      (match Refinement.get cx (loc, e) loc with
      | Some t -> t
      | None ->
        let tobj = expression cx _object in
        let tind = expression cx index in
        Tvar.mk_where cx reason (fun t ->
          let use_op = Op (GetProperty (mk_expression_reason ex)) in
          Flow.flow cx (tobj, GetElemT (use_op, reason, tind, t))
        )
      )

  | Member {
      Member._object = _, Identifier (_, "module");
      property = Member.PropertyIdentifier (_, "exports");
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      get_module_exports cx loc

  | Member {
      Member._object =
        _, Identifier (_, ("ReactGraphQL" | "ReactGraphQLLegacy"));
      property = Member.PropertyIdentifier (_, "Mixin");
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      let reason = mk_reason (RCustom "ReactGraphQLMixin") loc in
      Flow.get_builtin cx "ReactGraphQLMixin" reason

  | Member {
      Member._object = super_loc, Super;
      property = Member.PropertyIdentifier (ploc, name);
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      let super = super_ cx super_loc in
      let id_info = "super", super, Type_table.Other in
      Type_table.set_info (Context.type_table cx) super_loc id_info;
      let expr_reason = mk_reason (RProperty (Some name)) loc in
      (match Refinement.get cx (loc, e) loc with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (RProperty (Some name)) ploc in

        if Type_inference_hooks_js.dispatch_member_hook cx name ploc super
        then AnyT.at ploc
        else (
          Tvar.mk_where cx expr_reason (fun tvar ->
            let use_op = Op (GetProperty (mk_expression_reason ex)) in
            Flow.flow cx (
              super, GetPropT (use_op, expr_reason, Named (prop_reason, name), tvar)
            )
          )
        )
      )
      |> begin fun t ->
        let id_info = name, t, Type_table.PropertyAccess super in
        Type_table.set_info (Context.type_table cx) ploc id_info;
        t
      end

  | Member {
      Member._object;
      property = Member.PropertyIdentifier (ploc, name);
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
    let tobj = expression cx _object in
    (
      let expr_reason = mk_reason (RProperty (Some name)) loc in
      if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
      then AnyT.at ploc
      else match Refinement.get cx (loc, e) loc with
      | Some t -> t
      | None ->
        let prop_reason = mk_reason (RProperty (Some name)) ploc in
        let use_op = Op (GetProperty (mk_expression_reason ex)) in
        get_prop ~is_cond cx expr_reason ~use_op tobj (prop_reason, name)
    )
    |> begin fun t ->
      let id_info = name, t, Type_table.PropertyAccess tobj in
      Type_table.set_info (Context.type_table cx) ploc id_info;
      t
    end

  | Member {
      Member._object;
      property = Member.PropertyPrivateName (ploc, (_, name));
      optional;
      _
    } -> (
      warn_or_ignore_optional_chaining optional cx loc;
      let expr_reason = mk_reason (RPrivateProperty name) loc in
      match Refinement.get cx (loc, e) loc with
      | Some t -> t
      | None ->
        let tobj = expression cx _object in
        if Type_inference_hooks_js.dispatch_member_hook cx name ploc tobj
        then AnyT.at ploc
        else
          let use_op = Op (GetProperty (mk_expression_reason ex)) in
          get_private_field cx expr_reason ~use_op tobj name
    )
    |> begin fun t ->
      (* TODO use PropertyAccess *)
      let id_info = name, t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) ploc id_info;
      t
    end

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
        let elemt = Tvar.mk cx element_reason in
        let reason = replace_reason_const REmptyArrayLit reason in
        DefT (reason, ArrT (ArrayAT (elemt, Some [])))
    | elems ->
        let elem_spread_list = expression_or_spread_list cx loc elems in
        Tvar.mk_where cx reason (fun tout ->
          let resolve_to = (ResolveSpreadsToArrayLiteral (mk_id (), tout)) in
          let reason_op = reason in
          Flow.resolve_spread_list cx ~use_op:unknown_use ~reason_op elem_spread_list resolve_to
        )
    )

  | Call {
      Call.callee = _, Identifier (_, "require");
      arguments;
      optional;
    } when not (Env.local_scope_entry_exists "require") -> (
      warn_or_ignore_optional_chaining optional cx loc;
      match arguments with
      | [ Expression (source_loc, Ast.Expression.Literal {
          Ast.Literal.value = Ast.Literal.String module_name; _;
        }) ]
      | [ Expression (source_loc, TemplateLiteral {
          TemplateLiteral.quasis = [_, {
            TemplateLiteral.Element.value = {
              TemplateLiteral.Element.cooked = module_name; _
            }; _
          }];
          expressions = [];
        }) ] ->
        require cx (source_loc, module_name) loc
      | _ ->
        let ignore_non_literals =
          Context.should_ignore_non_literal_requires cx in
        if not ignore_non_literals
        then
          Flow.add_output cx
            Flow_error.(EUnsupportedSyntax (loc, RequireDynamicArgument));
        AnyT.at loc
    )

  | Call {
      Call.callee = _, Identifier (_, "requireLazy");
      arguments;
      optional;
    } when not (Env.local_scope_entry_exists "requireLazy") -> (
      warn_or_ignore_optional_chaining optional cx loc;
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
          | Some(Expression(source_loc, Ast.Expression.Literal({
              Ast.Literal.value = Ast.Literal.String module_name;
              _;
            }))) ->
              let module_tvar = require cx (source_loc, module_name) loc in
              module_tvar::tvars
          | _ ->
              Flow.add_output cx Flow_error.(
                EUnsupportedSyntax (loc, RequireLazyDynamicArgument)
              );
              tvars
        ) in
        let module_tvars = elements
          |> List.fold_left element_to_module_tvar []
          |> List.rev_map (fun e -> Arg e) in

        let callback_expr_t = expression cx callback_expr in
        let reason = mk_reason (RCustom "requireLazy() callback") loc in
        let use_op = Op (FunCall {
          op = mk_expression_reason ex;
          fn = mk_expression_reason callback_expr;
          args = [];
        }) in
        let _ = func_call cx reason ~use_op callback_expr_t module_tvars in

        NullT.at loc

      | _ ->
        Flow.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, RequireLazyDynamicArgument));
        AnyT.at loc
    )

  | New {
      New.callee = _, Identifier (_, "Function");
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      List.iter (function Arg t | SpreadArg t ->
        Flow.flow_t cx (t, StrT.at loc)
      ) argts;
      let reason = mk_reason (RCustom "new Function(..)") loc in
      let proto = ObjProtoT reason in
      DefT (reason, FunT (
        dummy_static reason,
        dummy_prototype,
        mk_functiontype reason
          [] ~rest_param:None ~def_reason:reason ~params_names:[] proto
      ))
    )

  | New {
      New.callee = _, Identifier (_, "Array");
      arguments
    } -> (
      let argts = List.map (expression_or_spread cx) arguments in
      (match argts with
      | [Arg argt] ->
        let reason = mk_reason (RCustom "new Array(..)") loc in
        let length_reason =
          replace_reason_const (RCustom "array length") reason in
        Flow.flow_t cx (argt, DefT (length_reason, NumT AnyLiteral));
        let element_reason =
          replace_reason_const (RCustom "array element") reason in
        let t = Tvar.mk cx element_reason in
        (* TODO - tuple_types could be undefined x N if given a literal *)
        DefT (reason, ArrT (ArrayAT (t, None)))
      | _ ->
        Flow.add_output cx (Flow_error.EUseArrayLiteral loc);
        EmptyT.at loc
      )
    )

  | New { New.callee; arguments } ->
      let class_ = expression cx callee in
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason (RConstructorCall (desc_of_t class_)) loc in
      let use_op = Op (FunCall {
        op = mk_expression_reason ex;
        fn = mk_expression_reason callee;
        args = mk_initial_arguments_reason arguments;
      }) in
      new_call cx reason ~use_op class_ argts

  | Call {
      Call.callee = (callee_loc, Member {
        Member._object = (_, Identifier (_, "Object") as obj);
        property = Member.PropertyIdentifier (prop_loc, name);
        optional = member_optional;
        _
      } as expr);
      arguments;
      optional;
    } ->
      warn_or_ignore_optional_chaining member_optional cx callee_loc;
      warn_or_ignore_optional_chaining optional cx loc;
      let obj_t = expression cx obj in
      static_method_call_Object cx loc callee_loc prop_loc expr obj_t name arguments

  | Call {
      Call.callee = (callee_loc, Member {
        Member._object = super_loc, Super;
        property = Member.PropertyIdentifier (ploc, name);
        optional = member_optional;
        _
      }) as callee;
      arguments;
      optional;
      _
    } ->
      warn_or_ignore_optional_chaining member_optional cx callee_loc;
      warn_or_ignore_optional_chaining optional cx loc;
      let reason = mk_reason (RMethodCall (Some name)) loc in
      let reason_lookup = mk_reason (RProperty (Some name)) callee_loc in
      let reason_prop = mk_reason (RProperty (Some name)) ploc in
      let super = super_ cx super_loc in
      let id_info = "super", super, Type_table.Other in
      Type_table.set_info (Context.type_table cx) super_loc id_info;
      let argts = List.map (expression_or_spread cx) arguments in
      Type_inference_hooks_js.dispatch_call_hook cx name ploc super;
      Tvar.mk_where cx reason (fun t ->
        let funtype = mk_methodcalltype super argts t in
        let use_op = Op (FunCallMethod {
          op = mk_expression_reason ex;
          fn = mk_expression_reason callee;
          prop = reason_prop;
          args = mk_initial_arguments_reason arguments;
        }) in
        let prop_t = Tvar.mk cx reason_prop in
        let id_info = name, prop_t, Type_table.PropertyAccess super in
        Type_table.set_info (Context.type_table cx) ploc id_info;
        Flow.flow cx (
          super,
          MethodT (use_op, reason, reason_lookup, Named (reason_prop, name),
            funtype, Some prop_t)
        )
      )

  | Call { Call.
      callee = (lookup_loc, Member { Member.
        _object;
        property;
        optional = member_optional;
        _
      }) as callee;
      arguments;
      optional;
    } ->
      (* method call *)
      warn_or_ignore_optional_chaining member_optional cx lookup_loc;
      warn_or_ignore_optional_chaining optional cx loc;
      let ot = expression cx _object in
      let argts = List.map (expression_or_spread cx) arguments in
      (match property with
      | Member.PropertyPrivateName (prop_loc, (_, name))
      | Member.PropertyIdentifier (prop_loc, name) ->
        let reason_call = mk_reason (RMethodCall (Some name)) loc in
        let use_op = Op (FunCallMethod {
          op = mk_expression_reason ex;
          fn = mk_expression_reason callee;
          prop = mk_reason (RProperty (Some name)) prop_loc;
          args = mk_initial_arguments_reason arguments;
        }) in
        method_call cx reason_call ~use_op prop_loc (callee, ot, name) argts
      | Member.PropertyExpression expr ->
        let reason_call = mk_reason (RMethodCall None) loc in
        let reason_lookup = mk_reason (RProperty None) lookup_loc in
        Tvar.mk_where cx reason_call (fun t ->
          let elem_t = expression cx expr in
          let frame = Env.peek_frame () in
          let funtype = mk_methodcalltype ot argts t ~frame in
          Flow.flow cx (ot,
            CallElemT (reason_call, reason_lookup, elem_t, funtype))
        ))

  | Call {
      Call.callee = (ploc, Super) as callee;
      arguments;
      optional;
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      let argts = List.map (expression_or_spread cx) arguments in
      let reason = mk_reason (RFunctionCall RSuper) loc in

      (* switch back env entries for this and super from undefined *)
      define_internal cx reason "this";
      define_internal cx reason "super";

      let this = this_ cx loc in
      let super = super_ cx ploc in
      let id_info = "super", super, Type_table.Other in
      Type_table.set_info (Context.type_table cx) ploc id_info;
      let super_reason = reason_of_t super in
      Tvar.mk_where cx reason (fun t ->
        let funtype = mk_methodcalltype this argts t in
        let propref = Named (super_reason, "constructor") in
        let use_op = Op (FunCall {
          op = mk_expression_reason ex;
          fn = mk_expression_reason callee;
          args = mk_initial_arguments_reason arguments;
        }) in
        Flow.flow cx (super, MethodT (use_op, reason, super_reason, propref, funtype, None)))

  (******************************************)
  (* See ~/www/static_upstream/core/ *)

  | Call {
      Call.callee = (_, Identifier (_, "invariant")) as callee;
      arguments;
      optional;
    } ->
      warn_or_ignore_optional_chaining optional cx loc;
      (* TODO: require *)
      ignore (expression cx callee);
      (match arguments with
      | [] ->
        (* invariant() is treated like a throw *)
        Env.reset_current_activation loc;
        Abnormal.save_and_throw Abnormal.Throw
      | (Expression (_, Ast.Expression.Literal {
          Ast.Literal.value = Ast.Literal.Boolean false; _;
        }))::arguments ->
        (* invariant(false, ...) is treated like a throw *)
        ignore (List.map (expression_or_spread cx) arguments);
        Env.reset_current_activation loc;
        Abnormal.save_and_throw Abnormal.Throw
      | (Expression cond)::arguments ->
        ignore (List.map (expression_or_spread cx) arguments);
        let _, preds, _, xtypes = predicates_of_condition cx cond in
        let _ = Env.refine_with_preds cx loc preds xtypes in
        ()
      | (Spread _)::_ ->
        Flow.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, InvariantSpreadArgument))
      );
      VoidT.at loc

  | Call { Call.callee; arguments; optional } ->
      warn_or_ignore_optional_chaining optional cx loc;
      let f = expression cx callee in
      let reason = mk_reason (RFunctionCall (desc_of_t f)) loc in
      let argts =
        List.map (expression_or_spread cx) arguments in
      let use_op = Op (FunCall {
        op = mk_expression_reason ex;
        fn = mk_expression_reason callee;
        args = mk_initial_arguments_reason arguments;
      }) in
      func_call cx reason ~use_op f argts

  | Conditional { Conditional.test; consequent; alternate } ->
      let reason = mk_reason RConditional loc in
      let _, preds, not_preds, xtypes = predicates_of_condition cx test in
      let env =  Env.peek_env () in
      let oldset = Changeset.clear () in

      let then_env = Env.clone_env env in
      Env.update_env cx loc then_env;
      let _ = Env.refine_with_preds cx loc preds xtypes in
      let t1 = expression cx consequent in

      let else_env = Env.clone_env env in
      Env.update_env cx loc else_env;
      let _ = Env.refine_with_preds cx loc not_preds xtypes in
      let t2 = expression cx alternate in

      let newset = Changeset.merge oldset in
      Env.merge_env cx loc (env, then_env, else_env) newset;
      Env.update_env cx loc env;
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
      DefT (reason, UnionT (UnionRep.make t1 t2 []))

  | Assignment { Assignment.operator; left; right } ->
      assignment cx loc (left, operator, right)

  | Sequence { Sequence.expressions } ->
      List.fold_left
        (fun _ e -> expression cx e)
        (VoidT.at loc)
        expressions

  | Function func ->
      let {Ast.Function.id; params; returnType; predicate; _} = func in
      let sig_loc = match params, returnType with
      | _, Some (end_loc, _)
      | (end_loc, _), None
         -> Loc.btwn loc end_loc
      in

      (match predicate with
      | Some (_, Ast.Type.Predicate.Inferred) ->
          Flow.add_output cx Flow_error.(
            EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)
          )
      | _ -> ());

      let t = mk_function id cx sig_loc func in
      (match id with
      | Some (id_loc, name) ->
          let id_info = name, t, Type_table.Other in
          Type_table.set_info (Context.type_table cx) id_loc id_info
      | _ -> ());
      t

  | ArrowFunction func ->
      mk_arrow cx loc func

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
      let ret = Tvar.mk cx reason in
      let args =
        [ Arg (DefT (reason_array, ArrT (ArrayAT (StrT.why reason, None))));
          SpreadArg (AnyT.why reason) ] in
      let ft = mk_functioncalltype reason args ret in
      let use_op = Op (FunCall {
        op = mk_expression_reason ex;
        fn = mk_expression_reason tag;
        args = [];
      }) in
      Flow.flow cx (t, CallT (use_op, reason, ft));
      ret

  | TemplateLiteral {
      TemplateLiteral.quasis;
      expressions
    } ->
      let strt_of_quasi = function
      | (elem_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.raw; cooked;
          };
          _
        }) ->
          literal cx elem_loc { Ast.Literal.
            value = Ast.Literal.String cooked;
            raw;
          }
      in
      begin match quasis with
      | head::[] ->
          strt_of_quasi head
      | _ ->
          let t_out = StrT.at loc in
          List.iter (fun expr ->
            let e = expression cx expr in
            Flow.flow cx (e, UseT (Op (Coercion {
              from = mk_expression_reason expr;
              target = reason_of_t t_out;
            }), t_out));
          ) expressions;
          t_out
      end

  | JSXElement e ->
      jsx cx e

  | JSXFragment f ->
      jsx_fragment cx f

  | Class c ->
      let (name_loc, name) = extract_class_name loc c in
      let reason = mk_reason (RIdentifier name) loc in
      (match c.Ast.Class.id with
      | Some _ ->
          let tvar = Tvar.mk cx reason in
          let id_info = name, tvar, Type_table.Other in
          Type_table.set_info (Context.type_table cx) name_loc id_info;
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
      let yield = Env.get_internal_var cx "yield" loc in
      let t = match argument with
      | Some expr -> expression cx expr
      | None -> VoidT.at loc in
      Env.havoc_heap_refinements ();
      let use_op = Op (GeneratorYield {
        value = (match argument with
        | Some expr -> mk_expression_reason expr
        | None -> reason_of_t t);
      }) in
      Flow.flow cx (t, UseT (use_op, yield));
      Env.get_internal_var cx "next" loc

  | Yield { Yield.argument; delegate = true } ->
      let reason = mk_reason (RCustom "yield* delegate") loc in
      let next = Env.get_internal_var cx "next" loc in
      let yield = Env.get_internal_var cx "yield" loc in
      let t = match argument with
      | Some expr -> expression cx expr
      | None -> assert_false "delegate yield without argument" in

      let ret_reason = replace_reason (fun desc -> RCustom (
        spf "return of child generator in %s" (string_of_desc desc)
      )) reason in
      let ret = Tvar.mk cx ret_reason in

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
      Env.havoc_heap_refinements ();
      let use_op = Op (GeneratorYield {
        value = (match argument with
        | Some expr -> mk_expression_reason expr
        | None -> reason_of_t t);
      }) in
      Flow.flow cx (t, UseT (use_op, iterable));

      ret

  (* TODO *)
  | Comprehension _ ->
    Flow.add_output cx
      Flow_error.(EUnsupportedSyntax (loc, ComprehensionExpression));
    EmptyT.at loc

  | Generator _ ->
    Flow.add_output cx
      Flow_error.(EUnsupportedSyntax (loc, GeneratorExpression));
    EmptyT.at loc

  | MetaProperty _->
    Flow.add_output cx
      Flow_error.(EUnsupportedSyntax (loc, MetaPropertyExpression));
    EmptyT.at loc

  | Import arg -> (
    match arg with
    | source_loc, Ast.Expression.Literal {
        Ast.Literal.value = Ast.Literal.String module_name; _;
      }
    | source_loc, TemplateLiteral {
        TemplateLiteral.quasis = [_, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = module_name; _
          }; _
        }];
        expressions = [];
      } ->

      let imported_module_t =
        let import_reason = mk_reason (RCustom (
          spf "module `%s`" module_name
        )) loc in
        import_ns cx import_reason (source_loc, module_name) loc
      in

      let reason = annot_reason (mk_reason (RCustom "async import") loc) in
      Flow.get_builtin_typeapp cx reason "Promise" [imported_module_t]
    | _ ->
      let ignore_non_literals =
        Context.should_ignore_non_literal_requires cx in
      if not ignore_non_literals
      then
        Flow.add_output cx
          Flow_error.(EUnsupportedSyntax (loc, ImportDynamicArgument));
      AnyT.at loc
  )
)

(* Handles function calls that appear in conditional contexts. The main
   distinction from the case handled in `expression_` is that we also return
   the inferred types for the call receiver and the passed arguments, and
   potenially the keys that correspond to the supplied arguments.
*)
and predicated_call_expression cx (loc, callee, arguments) =
  let (f, argks, argts, t) =
    predicated_call_expression_ cx loc callee arguments in
  Type_table.set (Context.type_table cx) loc t;
  (f, argks, argts, t)

(* Returns a quadruple containing:
   - the function type
   - argument keys
   - the arguments types
   - the returned type
*)
and predicated_call_expression_ cx loc callee arguments =
  let args = arguments |> List.map (function
    | Ast.Expression.Expression e -> e
    | _ -> Utils_js.assert_false "No spreads should reach here"
  ) in
  let f = expression cx callee in
  let reason = mk_reason (RFunctionCall (desc_of_t f)) loc in
  let argts = List.map (expression cx) args in
  let argks = List.map Refinement.key args in
  let use_op = Op (FunCall {
    op = reason;
    fn = mk_expression_reason callee;
    args = mk_initial_arguments_reason arguments;
  }) in
  let t = func_call cx reason ~use_op f (List.map (fun e -> Arg e) argts) in
  (f, argks, argts, t)

(* We assume that constructor functions return void
   and constructions return objects.
   TODO: This assumption does not always hold.
   If construction functions return non-void values (e.g., functions),
   then those values are returned by constructions.
*)
and new_call cx reason ~use_op class_ argts =
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (class_, ConstructorT (use_op, reason, argts, t));
  )

and func_call cx reason ~use_op ?(call_strict_arity=true) func_t argts =
  Env.havoc_heap_refinements ();
  Tvar.mk_where cx reason (fun t ->
    let frame = Env.peek_frame () in
    let app = mk_functioncalltype reason argts t ~frame ~call_strict_arity in
    Flow.flow cx (func_t, CallT (use_op, reason, app))
  )

and method_call cx reason ~use_op ?(call_strict_arity=true) prop_loc
    (expr, obj_t, name) argts =
  Type_inference_hooks_js.dispatch_call_hook cx name prop_loc obj_t;
  (match Refinement.get cx expr (loc_of_reason reason) with
  | Some f ->
      (* note: the current state of affairs is that we understand
         member expressions as having refined types, rather than
         understanding receiver objects as carrying refined properties.
         generalizing this properly is a todo, and will deliver goodness.
         meanwhile, here we must hijack the property selection normally
         performed by the flow algorithm itself. *)
      Env.havoc_heap_refinements ();
      let id_info = name, f, Type_table.PropertyAccess obj_t in
      Type_table.set_info (Context.type_table cx) prop_loc id_info;
      Tvar.mk_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let app =
          mk_methodcalltype obj_t argts t ~frame ~call_strict_arity in
        Flow.flow cx (f, CallT (use_op, reason, app));
      )
  | None ->
      Env.havoc_heap_refinements ();
      Tvar.mk_where cx reason (fun t ->
        let frame = Env.peek_frame () in
        let expr_loc, _ = expr in
        let reason_expr = mk_reason (RProperty (Some name)) expr_loc in
        let reason_prop = mk_reason (RProperty (Some name)) prop_loc in
        let app =
          mk_methodcalltype obj_t argts t ~frame ~call_strict_arity in
        let propref = Named (reason_prop, name) in
        let prop_t = Tvar.mk cx reason_prop in
        let id_info = name, prop_t, Type_table.PropertyAccess obj_t in
        Type_table.set_info (Context.type_table cx) prop_loc id_info;
        Flow.flow cx (obj_t, MethodT (use_op, reason, reason_expr, propref, app, Some prop_t))
      )
  )

and identifier_ cx name loc =
  if Type_inference_hooks_js.dispatch_id_hook cx name loc
  then AnyT.at loc
  else (
    let t = Env.var_ref ~lookup_mode:ForValue cx name loc in
    (* We want to make sure that the reason description for the type we return
     * is always `RIdentifier name`. *)
    match desc_of_t t with
    | RIdentifier name' when name = name' -> t
    | _ ->
      (match t with
      (* If this is an `OpenT` we can change its reason description directly. *)
      | OpenT _ -> mod_reason_of_t (replace_reason_const (RIdentifier name)) t
      (* If this is not an `OpenT` then create a new type variable with our
       * desired reason and unify it with our type. This adds a level of
       * indirection so that we don't modify the underlying reason of our type. *)
      | _ ->
        let reason = mk_reason (RIdentifier name) loc in
        Tvar.mk_where cx reason (Flow.unify cx t)
      )
  )

and identifier cx name loc =
  let t = identifier_ cx name loc in
  let id_info = name, t, Type_table.Other in
  Type_table.set_info (Context.type_table cx) loc id_info;
  t

(* traverse a literal expression, return result type *)
and literal cx loc lit = Ast.Literal.(match lit.Ast.Literal.value with
  | String s ->
      (* It's too expensive to track literal information for large strings.*)
      let lit =
        if String.length s < 100
        then Literal (None, s)
        else AnyLiteral
      in
      DefT (annot_reason (mk_reason RString loc), StrT lit)

  | Boolean b ->
      DefT (annot_reason (mk_reason RBoolean loc), BoolT (Some b))

  | Null ->
      NullT.at loc

  | Number f ->
      DefT (annot_reason (mk_reason RNumber loc), NumT (Literal (None, (f, lit.raw))))

  | RegExp _ ->
      Flow.get_builtin_type cx (annot_reason (mk_reason RRegExp loc)) "RegExp"
)

(* traverse a unary expression, return result type *)
and unary cx loc = Ast.Expression.Unary.(function
  | { operator = Not; argument; _ } ->
      let arg = expression cx argument in
      let reason = mk_reason (RUnaryOperator ("not", desc_of_t arg)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (arg, NotT (reason, t));
      )

  | { operator = Plus; argument; _ } ->
      ignore (expression cx argument);
      NumT.at loc

  | { operator = Minus; argument; _ } ->
      let arg = expression cx argument in
      let reason = mk_reason (desc_of_t arg) loc in
      Tvar.mk_derivable_where cx reason (fun t ->
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
    let use_op = Op (FunCall {
      op = reason;
      fn = reason_of_t await;
      args = [mk_expression_reason argument];
    }) in
    func_call cx reason ~use_op await [Arg arg]
)

(* numeric pre/post inc/dec *)
and update cx loc expr = Ast.Expression.Update.(
  let reason = mk_reason (RCustom "update") loc in
  let result_t = NumT.at loc in
  (match expr.argument with
  | _, Ast.Expression.Identifier (id_loc, name) ->
    Flow.flow cx (identifier cx name id_loc, AssertArithmeticOperandT reason);
    (* enforce state-based guards for binding update, e.g., const *)
    let use_op = Op (AssignVar {
      var = Some (mk_reason (RIdentifier name) id_loc);
      init = reason_of_t result_t;
    }) in
    ignore (Env.set_var cx ~use_op name result_t id_loc)
  | expr ->
    Flow.flow cx (expression cx expr, AssertArithmeticOperandT reason)
  );
  result_t
)

(* traverse a binary expression, return result type *)
and binary cx loc expr = Ast.Expression.Binary.(
  let operator = expr.operator in
  match expr with
  | { operator = Equal; left; right }
  | { operator = NotEqual; left; right } ->
      let t1 = expression cx left in
      let t2 = expression cx right in
      let desc = RBinaryOperator (
        (match operator with
        | Equal -> "=="
        | NotEqual -> "!="
        | _ -> failwith "unreachable"),
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, EqT (reason,false,t2));
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
      let t1 = expression cx left in
      let t2 = expression cx right in
      let desc = RBinaryOperator (
        (match operator with
        | LessThan -> "<"
        | LessThanEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanEqual -> ">="
        | _ -> failwith "unreachable"),
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Flow.flow cx (t1, ComparatorT (reason,false,t2));
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
      let t1 = expression cx left in
      let t2 = expression cx right in
      let desc = RBinaryOperator (
        "+",
        desc_of_reason (reason_of_t t1),
        desc_of_reason (reason_of_t t2)
      ) in
      let reason = mk_reason desc loc in
      Tvar.mk_where cx reason (fun t ->
        let use_op = Op (Addition {
          op = reason;
          left = mk_expression_reason left;
          right = mk_expression_reason right;
        }) in
        Flow.flow cx (t1, AdderT (use_op, reason, false, t2, t));
      )
)

and logical cx loc = Ast.Expression.Logical.(function
  | { operator = Or; left; right } ->
      let () = check_default_pattern cx left right in
      let t1, _, not_map, xtypes = predicates_of_condition cx left in
      let t2 = Env.in_refined_env cx loc not_map xtypes
        (fun () -> expression cx right)
      in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (t1, OrT (reason, t2, t));
      )

  | { operator = And; left; right } ->
      let t1, map, _, xtypes = predicates_of_condition cx left in
      let t2 = Env.in_refined_env cx loc map xtypes
        (fun () -> expression cx right)
      in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (t1, AndT (reason, t2, t));
      )
)

and assignment_lhs cx = Ast.Pattern.(function
  | loc, Object _
  | loc, Array _ ->
      Flow.add_output cx (Flow_error.EInvalidLHSInAssignment loc);
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
            optional;
            _
          }) ->
            warn_or_ignore_optional_chaining optional cx lhs_loc;
            set_module_kind cx lhs_loc (Context.CommonJSModule(Some(lhs_loc)));
            set_module_exports cx lhs_loc t

        (* super.name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object = super_loc, Super;
            property = Member.PropertyIdentifier (ploc, name);
            optional;
            _
          }) as rx) ->
            warn_or_ignore_optional_chaining optional cx lhs_loc;
            let reason =
              mk_reason (RPropertyAssignment (Some name)) lhs_loc in
            let prop_reason = mk_reason (RProperty (Some name)) ploc in
            let super = super_ cx lhs_loc in
            let id_info = "super", super, Type_table.Other in
            Type_table.set_info (Context.type_table cx) super_loc id_info;
            let prop_t = Tvar.mk cx prop_reason in
            let id_info = name, prop_t, Type_table.PropertyAccess super in
            Type_table.set_info (Context.type_table cx) ploc id_info;
            let use_op = Op (SetProperty {
              lhs = reason;
              prop = mk_reason (desc_of_reason (mk_expression_reason rx)) ploc;
              value = mk_expression_reason e;
            }) in
            Flow.flow cx (super, SetPropT (
              use_op, reason, Named (prop_reason, name), Normal, t, Some prop_t
            ))

        (* _object.#name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyPrivateName (ploc, (_, name));
            optional;
            _
          }) as expr) ->
            warn_or_ignore_optional_chaining optional cx lhs_loc;
            let o = expression cx _object in
            (* if we fire this hook, it means the assignment is a sham. *)
            if not (Type_inference_hooks_js.dispatch_member_hook cx name ploc o)
            then (
              let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in

              (* flow type to object property itself *)
              let class_entries = Env.get_class_entries () in
              let prop_reason = mk_reason (RPrivateProperty name) ploc in
              let prop_t = Tvar.mk cx prop_reason in
              let id_info = name, prop_t, Type_table.PropertyAccess o in
              Type_table.set_info (Context.type_table cx) ploc id_info;
              let use_op = Op (SetProperty {
                lhs = reason;
                prop = mk_reason (desc_of_reason (mk_expression_reason expr)) ploc;
                value = mk_expression_reason e;
              }) in
              Flow.flow cx (o, SetPrivatePropT (
                use_op, reason, name, class_entries, false, t, Some prop_t
              ));
              post_assignment_havoc ~private_:true name expr lhs_loc t
            )

        (* _object.name = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyIdentifier (ploc, name);
            optional;
            _
          }) as expr) ->
            warn_or_ignore_optional_chaining optional cx lhs_loc;
            let o = expression cx _object in
            let wr_ctx = match _object, Env.var_scope_kind () with
              | (_, This), Scope.Ctor -> ThisInCtor
              | _ -> Normal
            in
            (* if we fire this hook, it means the assignment is a sham. *)
            if not (Type_inference_hooks_js.dispatch_member_hook cx name ploc o)
            then (
              let reason = mk_reason (RPropertyAssignment (Some name)) lhs_loc in
              let prop_reason = mk_reason (RProperty (Some name)) ploc in

              (* flow type to object property itself *)
              let prop_t = Tvar.mk cx prop_reason in
              let id_info = name, prop_t, Type_table.PropertyAccess o in
              Type_table.set_info (Context.type_table cx) ploc id_info;
              let use_op = Op (SetProperty {
                lhs = reason;
                prop = mk_reason (desc_of_reason (mk_expression_reason expr)) ploc;
                value = mk_expression_reason e;
              }) in
              Flow.flow cx (o, SetPropT (
                use_op, reason, Named (prop_reason, name), wr_ctx, t, Some prop_t
              ));
              post_assignment_havoc ~private_:false name expr lhs_loc t
            )

        (* _object[index] = e *)
        | lhs_loc, Ast.Pattern.Expression ((_, Member {
            Member._object;
            property = Member.PropertyExpression ((iloc, _) as index);
            optional;
            _
          }) as rx) ->
            warn_or_ignore_optional_chaining optional cx lhs_loc;
            let reason = mk_reason (RPropertyAssignment None) lhs_loc in
            let a = expression cx _object in
            let i = expression cx index in
            let use_op = Op (SetProperty {
              lhs = reason;
              prop = mk_reason (desc_of_reason (mk_expression_reason rx)) iloc;
              value = mk_expression_reason e;
            }) in
            Flow.flow cx (a, SetElemT (use_op, reason, i, t, None));

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
      let result_t = Tvar.mk cx reason in
      (* lhs = lhs + rhs *)
      let () =
        let use_op = Op (Addition {
          op = reason;
          left = (match lhs with
          | (_, Ast.Pattern.Expression lhs) -> mk_expression_reason lhs
          | _ -> reason_of_t lhs_t);
          right = mk_expression_reason rhs;
        }) in
        Flow.flow cx (lhs_t, AdderT (use_op, reason, false, rhs_t, result_t))
      in
      let () =
        let use_op = Op (Addition {
          op = reason;
          left = mk_expression_reason rhs;
          right = (match lhs with
          | (_, Ast.Pattern.Expression lhs) -> mk_expression_reason lhs
          | _ -> reason_of_t lhs_t);
        }) in
        Flow.flow cx (rhs_t, AdderT (use_op, reason, false, lhs_t, result_t))
      in
      (* enforce state-based guards for binding update, e.g., const *)
      (match lhs with
      | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.
        name = id_loc, name;
        _;
      } ->
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) id_loc);
          init = reason;
        }) in
        ignore Env.(set_var cx ~use_op name result_t id_loc)
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
        let t = NumT.at loc in
        let use_op = Op (AssignVar {
          var = Some (mk_reason (RIdentifier name) id_loc);
          init = reason_of_t t;
        }) in
        ignore Env.(set_var cx ~use_op name t id_loc)
      | _ -> ()
      );
      lhs_t
)

and clone_object cx reason this that =
  Tvar.mk_where cx reason (fun tvar ->
    let u = ObjRestT (reason, [], tvar) in
    let t = Flow.tvar_with_constraint cx u in
    Flow.flow cx (
      this,
      ObjAssignToT (reason, that, t, ObjAssign)
    )
  )

and collapse_children cx children = Ast.JSX.(
  children
  |> List.filter (ExpressionContainer.(function
    | (_, ExpressionContainer { expression = EmptyExpression _ }) -> false
    | _ -> true))
  |> List.map (jsx_body cx)
  |> List.fold_left (fun children -> function
    | None -> children
    | Some child -> child::children) []
  |> List.rev)

and jsx cx = Ast.JSX.(
  function { openingElement; children; closingElement } ->
  let locs =
    let open_, _ = openingElement in
    match closingElement with
    | Some (close, _) -> Loc.btwn open_ close, open_, Loc.btwn_exclusive open_ close
    | _ -> open_, open_, open_
  in
  let children = collapse_children cx children in
  jsx_title cx openingElement children locs
)

and jsx_fragment cx = Ast.JSX.(
  function { frag_openingElement; frag_children; frag_closingElement } ->
  let locs =
    let open_ = frag_openingElement in
    match frag_closingElement with
    | Some close -> Loc.btwn open_ close, open_, Loc.btwn_exclusive open_ close
    | _ -> open_, open_, open_
  in
  let _, loc_opening, _ = locs in
  let children = collapse_children cx frag_children in
  let fragment =
    let reason = mk_reason (RIdentifier "React.Fragment") loc_opening in
    let react = Env.var_ref ~lookup_mode:ForValue cx "React" loc_opening in
    let use_op = Op (GetProperty reason) in
    get_prop ~is_cond:false cx reason ~use_op react (reason, "Fragment")
  in
  jsx_desugar cx "React.Fragment" fragment (NullT.at loc_opening) [] children locs
)

and jsx_title cx openingElement children locs = Ast.JSX.(
  let loc_element, _, _ = locs in
  let _, { Opening.name; attributes; _ } = openingElement in
  let facebook_fbt = Context.facebook_fbt cx in
  let jsx_mode = Context.jsx cx in

  match (name, facebook_fbt, jsx_mode) with
  | (Identifier (_, { Identifier.name }), Some facebook_fbt, _)
      when name = "fbt" ->
    let fbt_reason = mk_reason RFbt loc_element in
    Flow.get_builtin_type cx fbt_reason facebook_fbt

  (**
   * It's a bummer to duplicate this case, but CSX does not want the
   * "when name = String.capitalize name" restriction.
   *)
  | (Identifier (loc, { Identifier.name }), _, Some Options.CSX) ->
    if Type_inference_hooks_js.dispatch_id_hook cx name loc
    then AnyT.at loc_element
    else begin
      let reason = mk_reason (RJSXElement (Some name)) loc_element in
      let c = identifier cx name loc in
      (* With CSX children are just a prop, so pass them to jsx_mk_props... *)
      let o = jsx_mk_props cx reason c name attributes children in
      (* Sucks to also pass children to jsx_desugar here, they're ignored *)
      jsx_desugar cx name c o attributes children locs
    end

  | Identifier (loc, { Identifier.name }), _, _
      when name = String.capitalize_ascii name ->
    if Type_inference_hooks_js.dispatch_id_hook cx name loc
    then AnyT.at loc_element
    else begin
      let el =
        if jsx_mode = None
        then RReactElement (Some name) else RJSXElement (Some name) in
      let reason = mk_reason el loc_element in
      let c = identifier cx name loc in
      let o = jsx_mk_props cx reason c name attributes children in
      jsx_desugar cx name c o attributes children locs
    end

  (* In React we use a string literal type instead of fetching a full component
   * type from $JSXIntrinsics. React.createElement() then handles intrinsic type
   * checking. *)
  | (Identifier (loc, { Identifier.name }), _, _) when jsx_mode = None ->
    let c =
      let reason = mk_reason (RIdentifier name) loc in
      DefT (reason, SingletonStrT name)
    in
    let reason = mk_reason (RReactElement (Some name)) loc_element in
    let o = jsx_mk_props cx reason c name attributes children in
    jsx_desugar cx name c o attributes children locs

  | (Identifier (loc, { Identifier.name }), _, _) ->
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
          (mk_reason (RCustom "JSX Intrinsics lookup") loc)
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
       let desc = RCustom (spf "`%s`" name) in
       mk_reason desc loc
     in
     let component_t =
       if jsx_mode = None
       then Tvar.mk_where cx component_t_reason (fun t ->
        let prop_t =
          if Type_inference_hooks_js.dispatch_member_hook
            cx name loc jsx_intrinsics
          then AnyT.at loc
          else
            let use_op = Op (GetProperty component_t_reason) in
            get_prop
              ~is_cond:false
              cx
              component_t_reason
              ~use_op
              jsx_intrinsics
              (component_t_reason, name)
        in
        Flow.flow_t cx (prop_t, t)
      )
      else DefT (component_t_reason, StrT (Literal (None, name))) in
      let o = jsx_mk_props cx component_t_reason
        component_t name attributes children in
      jsx_desugar cx name component_t o attributes children locs

  | MemberExpression member, _, None ->
    let name = jsx_title_member_to_string member in
    let el = RReactElement (Some name) in
    let reason = mk_reason el loc_element in
    let c = jsx_title_member_to_expression member in
    let c = mod_reason_of_t (replace_reason_const (RIdentifier name)) (expression cx c) in
    let o = jsx_mk_props cx reason c name attributes children in
    jsx_desugar cx name c o attributes children locs

  | _ ->
      (* TODO? covers namespaced names as element names *)
      AnyT.at loc_element
)

and jsx_mk_props cx reason c name attributes children = Ast.JSX.(
  let is_react = Context.jsx cx = None in
  let reason_props = replace_reason_const
    (if is_react then RReactProps else RJSXElementProps name)
    reason in
  (* Use the same reason for proto and the ObjT so we can walk the proto chain
     and use the root proto reason to build an error. *)
  let proto = (ObjProtoT reason_props) in
  (* Return an object with specified sealing. *)
  let mk_object ?(sealed=false) props =
    Obj_type.mk_with_proto cx reason_props ~sealed ~props proto
  in
  (* Copy properties from from_obj to to_obj. We should ensure that to_obj is
     not sealed. *)
  let mk_spread from_obj to_obj =
    Tvar.mk_where cx reason_props (fun t ->
      Flow.flow cx (to_obj,
        ObjAssignToT (reason_props, from_obj, t, ObjAssign));
    )
  in
  (* When there's no result, return a new object with specified sealing. When
     there's result, copy a new object into it, sealing the result when
     necessary.

     When building an object incrementally, only the final call to this function
     may be with sealed=true, so we will always have an unsealed object to copy
     properties to. *)
  let eval_props ?(sealed=false) (map, result) =
    match result with
    | None -> mk_object ~sealed map
    | Some result ->
      let result =
        if not (SMap.is_empty map)
        then mk_spread (mk_object map) result
        else result
      in
      if not sealed then result else
        Tvar.mk_where cx reason_props (fun t ->
          Flow.flow cx (result, ObjSealT (reason_props, t))
        )
  in

  let sealed, map, result = List.fold_left (fun (sealed, map, result) att ->
    match att with
    (* All attributes with a non-namespaced name that are not a react ignored
     * attribute. *)
    | Opening.Attribute (aloc, { Attribute.
        name = Attribute.Identifier (id_loc, { Identifier.name = aname });
        value
      }) ->
      (* Get the type for the attribute's value. *)
      let atype =
        if Type_inference_hooks_js.dispatch_jsx_hook cx aname aloc c
        then AnyT.at aloc
        else
          match value with
            (* <element name="literal" /> *)
            | Some (Attribute.Literal (loc, lit)) ->
                literal cx loc lit
            (* <element name={expression} /> *)
            | Some (Attribute.ExpressionContainer (_, {
                ExpressionContainer.expression =
                  ExpressionContainer.Expression (loc, e)
              })) ->
                expression cx (loc, e)
            (* <element name={} /> *)
            | Some (Attribute.ExpressionContainer _) ->
                EmptyT.at aloc
            (* <element name /> *)
            | None ->
                DefT (mk_reason RBoolean aloc, BoolT (Some true))
      in
      let p = Field (Some id_loc, atype, Neutral) in
      (sealed, SMap.add aname p map, result)
    (* Do nothing for namespaced attributes or ignored React attributes. *)
    | Opening.Attribute _ ->
        (* TODO: attributes with namespaced names *)
        (sealed, map, result)
    (* <element {...spread} /> *)
    | Opening.SpreadAttribute (_, { SpreadAttribute.argument }) ->
        let spread = expression cx argument in
        let obj = eval_props (map, result) in
        let result = mk_spread spread obj in
        false, SMap.empty, Some result
  ) (true, SMap.empty, None) attributes in

  let map =
    match children with
    | [] -> map
    (* We add children to the React.createElement() call for React. Not to the
     * props as other JSX users may support. *)
    | _ when is_react -> map
    | _ ->
        let arr = Tvar.mk_where cx reason (fun tout ->
          Flow.resolve_spread_list
            cx
            ~use_op:unknown_use
            ~reason_op:reason
            children
            (ResolveSpreadsToArrayLiteral (mk_id (), tout))
        ) in
        let p = Field (None, arr, Neutral) in
        SMap.add "children" p map
  in
  eval_props ~sealed (map, result)
)

and jsx_desugar cx name component_t props attributes children locs =
  let loc_element, loc_opening, loc_children = locs in
  match Context.jsx cx with
  | None ->
      let reason = mk_reason (RReactElement (Some name)) loc_element in
      let react = Env.var_ref ~lookup_mode:ForValue cx "React" loc_opening in
      let children = List.map (function
        | UnresolvedArg a -> a
        | UnresolvedSpreadArg a ->
            Flow.add_output cx Flow_error.(EUnsupportedSyntax (loc_children, SpreadArgument));
            AnyT.why (reason_of_t a)
      ) children in
      Tvar.mk_where cx reason (fun tvar ->
        let reason_createElement =
          mk_reason (RProperty (Some "createElement")) loc_element in
        let use_op = Op (ReactCreateElementCall {
          op = reason_createElement;
          component = reason_of_t component_t;
          children = loc_children;
        }) in
        Flow.flow cx (react, MethodT (
          use_op,
          reason,
          reason_createElement,
          Named (reason_createElement, "createElement"),
          mk_methodcalltype
            react
            ([Arg component_t; Arg props] @ List.map (fun c -> Arg c) children)
            tvar,
          None
        ))
      )
  | Some Options.JSXPragma (raw_jsx_expr, jsx_expr) ->
      let reason = mk_reason (RJSXFunctionCall raw_jsx_expr) loc_element in

      (* A JSX element with no attributes should pass in null as the second
       * arg *)
      let props = match attributes with
      | [] -> NullT.at loc_opening
      | _ -> props in
      let argts =
        [Arg component_t; Arg props] @
        (List.map (function
          | UnresolvedArg c -> Arg c
          | UnresolvedSpreadArg c -> SpreadArg c
        ) children) in
      let use_op = Op (JSXCreateElement {
        op = reason;
        component = reason_of_t component_t;
      }) in
      Ast.Expression.(match jsx_expr with
      | jsx_loc, Member {
        Member._object;
        property = Member.PropertyIdentifier (prop_loc, name);
        optional;
          _;
        } ->
          warn_or_ignore_optional_chaining optional cx jsx_loc;
          let ot = jsx_pragma_expression cx raw_jsx_expr loc_element _object in
          method_call cx reason ~use_op ~call_strict_arity:false prop_loc
            (jsx_expr, ot, name) argts
      | _ ->
          let f = jsx_pragma_expression cx raw_jsx_expr loc_element jsx_expr in
          func_call cx reason ~use_op ~call_strict_arity:false f argts
      )
  | Some Options.CSX ->
      let reason = mk_reason (RJSXFunctionCall name) loc_element in
      let use_op = Op (JSXCreateElement {
        op = reason;
        component = reason_of_t component_t;
      }) in
      func_call cx reason ~use_op ~call_strict_arity:false component_t [Arg props]

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
      let desc = RJSXIdentifier (raw_jsx_expr, name) in
      Env.var_ref ~lookup_mode:ForValue cx name loc ~desc
  | expr ->
      (* Oh well, we tried *)
      expression cx expr
)

and jsx_body cx = Ast.JSX.(function
  | _, Element e -> Some (UnresolvedArg (jsx cx e))
  | _, Fragment f -> Some (UnresolvedArg (jsx_fragment cx f))
  | _, ExpressionContainer ec -> (
      let open ExpressionContainer in
      let { expression = ex } = ec in
      Some (UnresolvedArg (match ex with
        | Expression (loc, e) -> expression cx (loc, e)
        | EmptyExpression loc ->
          DefT (mk_reason (RCustom "empty jsx body") loc, EmptyT)))
    )
  | _, SpreadChild expr -> Some (UnresolvedSpreadArg (expression cx expr))
  | loc, Text { Text.value; raw=_; } ->
      Option.map (jsx_trim_text loc value) (fun c -> UnresolvedArg c)
)

and jsx_trim_text loc value =
  match (Utils_jsx.trim_jsx_text loc value) with
  | Some (loc, trimmed) ->
    Some (DefT (mk_reason RJSXText loc, StrT (Type.Literal (None, trimmed))))
  | None -> None

and jsx_title_member_to_string (_, member) = Ast.JSX.MemberExpression.(
  let (_, { Ast.JSX.Identifier.name }) = member.property in
  match member._object with
  | MemberExpression member -> (jsx_title_member_to_string member) ^ "." ^ name
  | Identifier (_, { Ast.JSX.Identifier.name = obj }) -> obj ^ "." ^ name
)

and jsx_title_member_to_expression member =
  let (mloc, member) = member in
  let _object = Ast.JSX.MemberExpression.(
    match member._object with
    | MemberExpression member -> jsx_title_member_to_expression member
    | Identifier (loc, { Ast.JSX.Identifier.name }) ->
      (loc, Ast.Expression.Identifier (loc, name))
  ) in
  let property = Ast.JSX.MemberExpression.(
    let (loc, { Ast.JSX.Identifier.name }) = member.property in
    (loc, name)
  ) in
  Ast.Expression.Member.(
    (mloc, Ast.Expression.Member {
      _object;
      property = PropertyIdentifier property;
      computed = false;
      optional = false;
    })
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
      Flow.flow cx (t1, EqT (reason, false, t2))
  in

  (* package result quad from test type, refi key, unrefined type,
     predicate, and predicate's truth sense *)
  let result test_t key unrefined_t pred sense =
    empty_result test_t |> add_predicate key unrefined_t pred sense
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
        optional;
        _
      }) ->
      warn_or_ignore_optional_chaining optional cx expr_loc;

      (* use `expression` instead of `condition` because `_object` is the object
         in a member expression; if it itself is a member expression, it must
         exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here as
         `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
      let obj_t = expression cx _object in

      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let expr_reason = mk_reason (RProperty (Some prop_name)) expr_loc in
      let prop_t = match Refinement.get cx expr expr_loc with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else
          let use_op = Op (GetProperty prop_reason) in
          get_prop ~is_cond:true cx
            expr_reason ~use_op obj_t (prop_reason, prop_name)
      in
      let id_info = prop_name, prop_t, Type_table.PropertyAccess obj_t in
      Type_table.set_info (Context.type_table cx) prop_loc id_info;

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      let refinement = match Refinement.key _object with
      | None -> None
      | Some name ->
          let pred = LeftP (SentinelProp prop_name, val_t) in
          Some (name, obj_t, pred, sense)
      in

      (* since we never called `expression cx expr`, we have to add to the
         type table ourselves *)
      Type_table.set (Context.type_table cx) expr_loc prop_t;

      prop_t, refinement
    | _ ->
      condition cx expr, None
  in

  (* inspect a null equality test *)
  let null_test loc ~sense ~strict e null_t =
    let t, sentinel_refinement = condition_of_maybe_sentinel cx
      ~sense ~strict e null_t in
    flow_eqt ~strict loc (t, null_t);
    let out = match Refinement.key e with
    | None -> empty_result (BoolT.at loc)
    | Some name ->
        let pred = if strict then NullP else MaybeP in
        result (BoolT.at loc) name t pred sense
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  let void_test loc ~sense ~strict e void_t =
    (* if `void_t` is not a VoidT, make it one so that the sentinel test has a
       literal type to test against. It's not appropriate to call `void_test`
       with a `void_t` that you don't want to treat like an actual `void`! *)
    let void_t = match void_t with
    | DefT (_, VoidT) -> void_t
    | _ -> VoidT.why (reason_of_t void_t)
    in
    let t, sentinel_refinement = condition_of_maybe_sentinel cx
      ~sense ~strict e void_t in
    flow_eqt ~strict loc (t, void_t);
    let out = match Refinement.key e with
    | None -> empty_result (BoolT.at loc)
    | Some name ->
        let pred = if strict then VoidP else MaybeP in
        result (BoolT.at loc) name t pred sense
    in
    match sentinel_refinement with
    | Some (name, obj_t, p, sense) -> out |> add_predicate name obj_t p sense
    | None -> out
  in

  (* inspect an undefined equality test *)
  let undef_test loc ~sense ~strict e void_t =
    (* if `undefined` isn't redefined in scope, then we assume it is `void` *)
    if Env.is_global_var cx "undefined"
    then void_test loc ~sense ~strict e void_t
    else empty_result (BoolT.at loc)
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
    let bool = BoolT.at loc in
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
        | Some pred -> result bool name t pred sense
        | None ->
          Flow.add_output cx Flow_error.(EInvalidTypeof (str_loc, typename));
          empty_result bool
        end
    | None, _ -> empty_result bool
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
    | ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _})
      as value), expr
    | expr, ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _})
      as value)
    | expr, ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [lit_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value)
    | ((_, Expression.TemplateLiteral {
        TemplateLiteral.quasis = [lit_loc, {
          TemplateLiteral.Element.value = {
            TemplateLiteral.Element.cooked = lit; _
          }; _
        }]; _
      }) as value), expr
      ->
        let val_t = expression cx value in
        literal_test loc ~sense ~strict expr val_t
          (SingletonStrP (lit_loc, sense, lit))

    (* special case equality relations involving numbers *)
    | ((lit_loc, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value), expr
    | expr, ((lit_loc, Expression.Literal { Literal.value = Literal.Number lit; raw })
      as value)
      ->
        let val_t = expression cx value in
        literal_test loc ~sense ~strict expr val_t
          (SingletonNumP (lit_loc, sense, (lit, raw)))

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
      optional;
        _
      }
    ->
      warn_or_ignore_optional_chaining optional cx loc;
      let obj_t = match _object with
      | super_loc, Super ->
          let t = super_ cx super_loc in
          let id_info = "super", t, Type_table.Other in
          Type_table.set_info (Context.type_table cx) super_loc id_info;
          t

      | _ ->
          (* use `expression` instead of `condition` because `_object` is the
             object in a member expression; if it itself is a member expression,
             it must exist (so ~is_cond:false). e.g. `foo.bar.baz` shows up here
             as `_object = foo.bar`, `prop_name = baz`, and `bar` must exist. *)
          expression cx _object in
      let expr_reason = mk_reason (RProperty (Some prop_name)) loc in
      let prop_reason = mk_reason (RProperty (Some prop_name)) prop_loc in
      let t = match Refinement.get cx e loc with
      | Some t -> t
      | None ->
        if Type_inference_hooks_js.dispatch_member_hook cx
          prop_name prop_loc obj_t
        then AnyT.at prop_loc
        else
          let use_op = Op (GetProperty (mk_expression_reason e)) in
          get_prop ~is_cond:true cx
            expr_reason ~use_op obj_t (prop_reason, prop_name)
      in

      (* since we never called `expression cx e`, we have to add to the
         type table ourselves *)
      Type_table.set (Context.type_table cx) loc t;
      let id_info = prop_name, t, Type_table.PropertyAccess obj_t in
      Type_table.set_info (Context.type_table cx) prop_loc id_info;

      let out = match Refinement.key e with
      | Some name -> result t name t (ExistsP (Some loc)) true
      | None -> empty_result t
      in

      (* refine the object (`foo.bar` in the example) based on the prop. *)
      begin match Refinement.key _object with
      | Some name ->
        let predicate = PropExistsP (expr_reason, prop_name, Some prop_loc) in
        out |> add_predicate name obj_t predicate true
      | None ->
        out
      end

  (* assignments *)
  | _, Assignment { Assignment.left = loc, Ast.Pattern.Identifier id; _ } -> (
      let expr = expression cx e in
      let id = id.Ast.Pattern.Identifier.name in
      match refinable_lvalue (loc, Ast.Expression.Identifier id) with
      | Some name, _ -> result expr name expr (ExistsP (Some loc)) true
      | None, _ -> empty_result expr
    )

  (* expr instanceof t *)
  | loc, Binary { Binary.operator = Binary.Instanceof; left; right } -> (
      let bool = BoolT.at loc in
      match refinable_lvalue left with
      | Some name, t ->
          let right_t = expression cx right in
          let pred = LeftP (InstanceofTest, right_t) in
          result bool name t pred true
      | None, _ ->
          empty_result bool
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
  | loc, Call {
      Call.callee = callee_loc, Member {
        Member._object = (_, Identifier (_, "Array") as o);
        property = Member.PropertyIdentifier (prop_loc, "isArray");
        optional = member_optional;
        _ };
      arguments = [Expression arg];
      optional;
    } -> (
      warn_or_ignore_optional_chaining member_optional cx callee_loc;
      warn_or_ignore_optional_chaining optional cx loc;

      (* get Array.isArray in order to populate the type tables, but we don't
         care about the result. *)
      (* TODO: one day we can replace this with a call to `method_call`, and
         then discard the result. currently MethodT does not update type_table
         properly. *)
      let obj_t = expression cx o in
      let reason = mk_reason (RCustom "`Array.isArray(...)`") callee_loc in
      let fn_t = Tvar.mk_where cx reason (fun t ->
        let prop_reason = mk_reason (RProperty (Some "isArray")) prop_loc in
        let use_op = Op (GetProperty (mk_expression_reason e)) in
        Flow.flow cx (obj_t, GetPropT (use_op, reason, Named (prop_reason, "isArray"), t))
      ) in
      Type_table.set (Context.type_table cx) prop_loc fn_t;
      let id_info = "isArray", fn_t, Type_table.Other in
      Type_table.set_info (Context.type_table cx) prop_loc id_info;
      let bool = BoolT.at loc in

      match refinable_lvalue arg with
      | Some name, t ->
          result bool name t ArrP true
      | None, _ ->
          empty_result bool
    )

  (* test1 && test2 *)
  | loc, Logical { Logical.operator = Logical.And; left; right } ->
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx loc map1 xts1
        (fun () -> predicates_of_condition cx right) in
      let reason = mk_reason (RLogical ("&&", desc_of_t t1, desc_of_t t2)) loc in
      (
        Tvar.mk_where cx reason (fun t ->
          Flow.flow cx (t1, AndT (reason, t2, t));
        ),
        mk_and map1 map2,
        mk_or not_map1 not_map2,
        Key_map.union xts1 xts2
      )

  (* test1 || test2 *)
  | loc, Logical { Logical.operator = Logical.Or; left; right } ->
      let () = check_default_pattern cx left right in
      let t1, map1, not_map1, xts1 =
        predicates_of_condition cx left in
      let t2, map2, not_map2, xts2 = Env.in_refined_env cx loc not_map1 xts1
        (fun () -> predicates_of_condition cx right) in
      let reason = mk_reason (RLogical ("||", desc_of_t t1, desc_of_t t2)) loc in
      (
        Tvar.mk_where cx reason (fun t ->
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
  | loc, This
  | loc, Identifier _
  | loc, Member _ -> (
      match refinable_lvalue e with
      | Some name, t -> result t name t (ExistsP (Some loc)) true
      | None, t -> empty_result t
    )

  (* e.m(...) *)
  (* TODO: Don't trap method calls for now *)
  | loc, Call { Call.callee = (_, Member _); optional; _ } ->
      warn_or_ignore_optional_chaining optional cx loc;
      empty_result (expression cx e)

  (* f(...) *)
  (* The concrete predicate is not known at this point. We attach a "latent"
     predicate pointing to the type of the function that will supply this
     predicated when it is resolved. *)
  | loc, Call { Call.callee = c; arguments; optional }
    ->
      warn_or_ignore_optional_chaining optional cx loc;
      let is_spread = function | Spread _ -> true | _ -> false in
      if List.exists is_spread arguments then
        empty_result (expression cx e)
      else
        let fun_t, keys, arg_ts, ret_t =
          predicated_call_expression cx (loc, c, arguments) in
        let args_with_offset = ListUtils.zipi keys arg_ts in
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

and get_private_field cx reason ~use_op tobj name =
  Tvar.mk_where cx reason (fun t ->
    let class_entries = Env.get_class_entries () in
    let get_prop_u = GetPrivatePropT (use_op, reason, name, class_entries, false, t) in
    Flow.flow cx (tobj, get_prop_u)
  )

(* Property lookups become non-strict when processing conditional expressions
   (see above).

   TODO: It should be possible to factor the processing of LHS / reference
   expressions out of `expression`, somewhat like what assignment_lhs does. That
   would make everything involving Refinement be in the same place.
*)
and get_prop ~is_cond cx reason ~use_op tobj (prop_reason, name) =
  Tvar.mk_where cx reason (fun t ->
    let get_prop_u =
      if is_cond
      then TestPropT (reason, Named (prop_reason, name), t)
      else GetPropT (use_op, reason, Named (prop_reason, name), t)
    in
    Flow.flow cx (tobj, get_prop_u)
  )

(* TODO: switch to TypeScript specification of Object *)
and static_method_call_Object cx loc callee_loc prop_loc expr obj_t m args_ =
  let open Ast.Expression in
  let reason = mk_reason (RCustom (spf "`Object.%s`" m)) loc in
  match (m, args_) with
  | ("create", [ Expression e ]) ->
    let proto =
      let reason = mk_reason RPrototype (fst e) in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (expression cx e, ObjTestProtoT (reason, t))
      )
    in
    Obj_type.mk_with_proto cx reason proto

  | ("create", [ Expression e;
                 Expression (_, Object { Object.properties }) ]) ->
    let proto =
      let reason = mk_reason RPrototype (fst e) in
      Tvar.mk_where cx reason (fun t ->
        Flow.flow cx (expression cx e, ObjTestProtoT (reason, t))
      )
    in
    let pmap = prop_map_of_object cx properties in
    let props = SMap.fold (fun x p acc ->
      let loc = Property.read_loc p in
      match Property.read_t p with
      | None ->
        (* Since the properties object must be a literal, and literal objects
           can only ever contain neutral fields, this should not happen. *)
        Flow.add_output cx Flow_error.(
          EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)
        );
        acc
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let t = Tvar.mk_where cx reason (fun tvar ->
          Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar))
        ) in
        let p = Field (loc, t, Neutral) in
        SMap.add x p acc
    ) pmap SMap.empty in
    Obj_type.mk_with_proto cx reason ~props proto

  | (("getOwnPropertyNames" | "keys"), [ Expression e ]) ->
    let arr_reason = mk_reason RArrayType loc in
    let o = expression cx e in
    DefT (arr_reason, ArrT (
      ArrayAT (
        Tvar.mk_where cx arr_reason (fun tvar ->
          let keys_reason = replace_reason (fun desc ->
            RCustom (spf "element of %s" (string_of_desc desc))
          ) reason in
          Flow.flow cx (o, GetKeysT (keys_reason, UseT (unknown_use, tvar)));
        ),
        None
      )
    ))

  | ("defineProperty", [ Expression e;
                         Expression ((ploc, Ast.Expression.Literal
                           { Ast.Literal.value = Ast.Literal.String x; _ })
                           as key);
                         Expression config ]) ->
    let o = expression cx e in
    let _ = expression cx key in
    let spec = expression cx config in
    let tvar = Tvar.mk cx reason in
    let prop_reason = mk_reason (RProperty (Some x)) ploc in
    Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar));
    let prop_t = Tvar.mk cx prop_reason in
    let id_info = x, prop_t, Type_table.Other in
    Type_table.set_info (Context.type_table cx) ploc id_info;
    Flow.flow cx (o, SetPropT (
      unknown_use, reason, Named (prop_reason, x), Normal, tvar, Some prop_t
    ));
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
        Flow.add_output cx Flow_error.(
          EInternal (prop_loc, PropertyDescriptorPropertyCannotBeRead)
        );
      | Some spec ->
        let reason = replace_reason (fun desc ->
          RCustom (spf ".%s of %s" x (string_of_desc desc))
        ) reason in
        let tvar = Tvar.mk cx reason in
        Flow.flow cx (spec, GetPropT (unknown_use, reason, Named (reason, "value"), tvar));
        Flow.flow cx (o, SetPropT (
          unknown_use, reason, Named (reason, x), Normal, tvar, None
        ));
    );
    o

  (* Freezing an object literal is supported since there's no way it could
     have been mutated elsewhere *)
  | ("freeze", [Expression ((arg_loc, Object _) as e)]) ->
    let arg_t = expression cx e in

    let reason_arg = mk_reason (RFrozen RObject) arg_loc in
    let arg_t = Tvar.mk_where cx reason_arg (fun tvar ->
      Flow.flow cx (arg_t, ObjFreezeT (reason_arg, tvar));
    ) in

    let reason = mk_reason (RMethodCall (Some m)) loc in
    method_call cx reason prop_loc ~use_op:unknown_use (expr, obj_t, m) [Arg arg_t]

  (* TODO *)
  | (_, args) ->
    let argts = List.map (expression_or_spread cx) args in
    let reason = mk_reason (RMethodCall (Some m)) loc in
    let use_op = Op (FunCallMethod {
      op = reason;
      fn = mk_reason (RMethod (Some m)) callee_loc;
      prop = mk_reason (RProperty (Some m)) prop_loc;
      args = mk_initial_arguments_reason args;
    }) in
    method_call cx reason ~use_op prop_loc (expr, obj_t, m) argts

and extract_class_name class_loc  = Ast.Class.(function {id; _;} ->
  match id with
  | Some(name_loc, name) -> (name_loc, name)
  | None -> (class_loc, "<<anonymous class>>")
)

and mk_class cx loc reason c =
  let def_reason = repos_reason loc reason in
  let this_in_class = Class_sig.This.in_class c in
  let self = Tvar.mk cx reason in
  let class_sig =
    Class_sig.mk cx loc reason self c ~expr:expression
  in
    class_sig |> Class_sig.generate_tests cx (fun class_sig ->
      Class_sig.check_super cx def_reason class_sig;
      Class_sig.check_implements cx def_reason class_sig;
      if this_in_class || not (Class_sig.This.is_bound_to_empty class_sig) then
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
and function_decl id cx loc func this super =
  let func_sig = Func_sig.mk cx SMap.empty ~expr:expression loc func in

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
  let loc = loc_of_reason reason in
  let opt = Env.get_var_declared_type cx ix loc in
  ignore Env.(set_var cx ~use_op:unknown_use ix (Flow.filter_optional cx reason opt) loc)

(* Process a function definition, returning a (polymorphic) function type. *)
and mk_function id cx loc func =
  let this = Tvar.mk cx (mk_reason RThis loc) in
  (* Normally, functions do not have access to super. *)
  let super = ObjProtoT (mk_reason RNoSuper loc) in
  let func_sig = function_decl id cx loc func this super in
  Func_sig.functiontype cx this func_sig

(* Process an arrow function, returning a (polymorphic) function type. *)
and mk_arrow cx loc func =
  let this = this_ cx loc in
  let super = super_ cx loc in
  let {Ast.Function.id; _} = func in
  let func_sig = function_decl id cx loc func this super in
  (* Do not expose the type of `this` in the function's type. The call to
     function_decl above has already done the necessary checking of `this` in
     the body of the function. Now we want to avoid re-binding `this` to
     objects through which the function may be called. *)
  Func_sig.functiontype cx dummy_this func_sig

(* Transform predicate declare functions to functions whose body is the
   predicate declared for the funcion *)
and declare_function_to_function_declaration cx id typeAnnotation predicate =
  match predicate with
  | Some (loc, Ast.Type.Predicate.Inferred) ->
      Flow.add_output cx Flow_error.(
        EUnsupportedSyntax (loc, PredicateDeclarationWithoutExpression)
      );
      None

  | Some (loc, Ast.Type.Predicate.Declared e) -> begin
      match typeAnnotation with
      | (_, (_, Ast.Type.Function
        { Ast.Type.Function.params = (params_loc, { Ast.Type.Function.Params.params; rest });
          Ast.Type.Function.returnType;
          Ast.Type.Function.typeParameters;
        })) ->
          let param_type_to_param = Ast.Type.Function.(
            fun (l, { Param.name; Param.typeAnnotation; _ }) ->
              let name = match name with
              | Some name -> name
              | None ->
                  let name_loc = fst typeAnnotation in
                  Flow.add_output cx Flow_error.(EUnsupportedSyntax
                    (loc, PredicateDeclarationAnonymousParameters));
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
          Some (Ast.Statement.FunctionDeclaration { Ast.Function.
            id = Some id;
            params = (params_loc, { Ast.Function.Params.params; rest });
            body = body;
            async = false;
            generator = false;
            predicate = Some (loc, Ast.Type.Predicate.Inferred);
            expression = false;
            returnType = returnType;
            typeParameters;
          })

      | _ ->
        None
      end
  | _ ->
      None

and check_default_pattern cx left right =
  let left_loc = fst left in
  let right_loc = fst right in

  let update_excuses update_fun =
    let exists_excuses = Context.exists_excuses cx in
    let exists_excuse = Utils_js.LocMap.get left_loc exists_excuses
      |> Option.value ~default:ExistsCheck.empty
      |> update_fun in
    let exists_excuses = Utils_js.LocMap.add left_loc exists_excuse exists_excuses in
    Context.set_exists_excuses cx exists_excuses
  in

  match snd right with
    | Ast.Expression.Literal literal ->
      let open ExistsCheck in
      begin match literal.Ast.Literal.value with
        | Ast.Literal.String "" ->
          update_excuses (fun excuse -> {excuse with string_loc = Some right_loc})
        | Ast.Literal.Boolean false ->
          update_excuses (fun excuse -> {excuse with bool_loc = Some right_loc})
        | Ast.Literal.Number 0. ->
          update_excuses (fun excuse -> {excuse with number_loc = Some right_loc})
        (* There's no valid default value for mixed to create an excuse. *)
        | _ -> ()
      end
    | _ -> ()

and post_assignment_havoc ~private_ name expr lhs_loc t =
  (* types involved in the assignment are computed
     in pre-havoc environment. it's the assignment itself
     which clears refis *)
  Env.havoc_heap_refinements_with_propname ~private_ name;

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
    ignore Env.(set_expr key lhs_loc t t)
  | None ->
    ()

and mk_initial_arguments_reason = Ast.Expression.(function
| [] -> []
| Expression x :: args -> mk_expression_reason x :: mk_initial_arguments_reason args
| Spread _ :: _ -> []
)

and warn_or_ignore_optional_chaining optional cx loc =
  if optional
  then match Context.esproposal_optional_chaining cx with
  | Options.ESPROPOSAL_ENABLE
  | Options.ESPROPOSAL_IGNORE -> ()
  | Options.ESPROPOSAL_WARN -> Flow.add_output cx (Flow_error.EExperimentalOptionalChaining loc)
  else ()
