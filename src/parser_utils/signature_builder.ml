(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor
module Entry = Signature_builder_entry
module Env = Signature_builder_env
module V = Signature_builder_verify.Verifier
module G = Signature_builder_generate.Generator
module Signature_builder_deps = Signature_builder_deps.With_Loc
module File_sig = File_sig.With_Loc

module Signature = struct
  type t = Env.t * File_sig.exports_info File_sig.t'

  let add_env env entry = Env.add entry env

  let add_env_list env entries = Env.push entries env

  let add_variable_declaration env loc variable_declaration =
    add_env_list env (Entry.variable_declaration loc variable_declaration)

  let add_function_declaration env loc function_declaration =
    add_env env (Entry.function_declaration loc function_declaration)

  let add_function_expression env loc function_expression =
    add_env env (Entry.function_expression loc function_expression)

  let add_class env loc class_ = add_env env (Entry.class_ loc class_)

  let add_declare_variable env loc declare_variable =
    add_env env (Entry.declare_variable loc declare_variable)

  let add_declare_function env loc declare_function =
    add_env env (Entry.declare_function loc declare_function)

  let add_declare_class env loc declare_class = add_env env (Entry.declare_class loc declare_class)

  let add_type_alias env loc type_alias = add_env env (Entry.type_alias loc type_alias)

  let add_opaque_type env loc opaque_type = add_env env (Entry.opaque_type loc opaque_type)

  let add_interface env loc interface = add_env env (Entry.interface loc interface)

  let add_enum env loc enum = add_env env (Entry.enum loc enum)

  let add_declare_export_declaration env =
    let open Ast.Statement.DeclareExportDeclaration in
    function
    | Variable (loc, declare_variable) -> add_declare_variable env loc declare_variable
    | Function (loc, declare_function) -> add_declare_function env loc declare_function
    | Class (loc, declare_class) -> add_declare_class env loc declare_class
    | NamedType (loc, type_alias) -> add_type_alias env loc type_alias
    | NamedOpaqueType (loc, opaque_type) -> add_opaque_type env loc opaque_type
    | Interface (loc, interface) -> add_interface env loc interface
    | DefaultType _ -> assert false

  let add_export_default_declaration env =
    let open Ast.Statement.ExportDefaultDeclaration in
    function
    | Declaration
        ( loc,
          Ast.Statement.FunctionDeclaration ({ Ast.Function.id = Some _; _ } as function_declaration)
        ) ->
      add_function_declaration env loc function_declaration
    | Declaration (loc, Ast.Statement.ClassDeclaration ({ Ast.Class.id = Some _; _ } as class_)) ->
      add_class env loc class_
    | Declaration (loc, Ast.Statement.EnumDeclaration enum) -> add_enum env loc enum
    | Declaration _ -> assert false
    | Expression (loc, Ast.Expression.Function ({ Ast.Function.id = Some _; _ } as function_)) ->
      add_function_expression env loc function_
    | Expression _ -> assert false

  (* TODO: class? *)

  let add_stmt env =
    let open Ast.Statement in
    function
    | (loc, VariableDeclaration variable_declaration) ->
      add_variable_declaration env loc variable_declaration
    | (loc, DeclareVariable declare_variable) -> add_declare_variable env loc declare_variable
    | (loc, FunctionDeclaration function_declaration) ->
      add_function_declaration env loc function_declaration
    | (loc, DeclareFunction declare_function) -> add_declare_function env loc declare_function
    | (loc, ClassDeclaration class_) -> add_class env loc class_
    | (loc, DeclareClass declare_class) -> add_declare_class env loc declare_class
    | (loc, TypeAlias type_alias) -> add_type_alias env loc type_alias
    | (loc, DeclareTypeAlias type_alias) -> add_type_alias env loc type_alias
    | (loc, OpaqueType opaque_type) -> add_opaque_type env loc opaque_type
    | (loc, DeclareOpaqueType opaque_type) -> add_opaque_type env loc opaque_type
    | (loc, InterfaceDeclaration interface) -> add_interface env loc interface
    | (loc, DeclareInterface interface) -> add_interface env loc interface
    | (loc, EnumDeclaration enum) -> add_enum env loc enum
    | (_, Block _)
    | (_, DoWhile _)
    | (_, For _)
    | (_, ForIn _)
    | (_, ForOf _)
    | (_, If _)
    | (_, Labeled _)
    | (_, Switch _)
    | (_, Try _)
    | (_, While _)
    | (_, DeclareExportDeclaration _)
    | (_, ExportDefaultDeclaration _)
    | (_, ExportNamedDeclaration _)
    | (_, ImportDeclaration _)
    | (_, DeclareModule _)
    | (_, DeclareModuleExports _)
    | (_, Empty)
    | (_, Expression _)
    | (_, Break _)
    | (_, Continue _)
    | (_, Throw _)
    | (_, Return _)
    | (_, Debugger)
    | (_, With _) ->
      assert false

  let add_export_value_bindings named named_infos env =
    File_sig.(
      let named =
        List.filter
          (function
            | (_, (_, ExportNamed { kind = NamedSpecifier _; _ }))
            | (_, (_, ExportNs _)) ->
              false
            | (_, (_, _)) -> true)
          named
      in
      List.fold_left2
        (fun env (_n, (_, export)) export_def ->
          match (export, export_def) with
          | (ExportDefault { local; _ }, DeclareExportDef declare_export_declaration) ->
            begin
              match local with
              | Some _id -> add_declare_export_declaration env declare_export_declaration
              | None -> env
            end
          | (ExportNamed { kind; _ }, DeclareExportDef declare_export_declaration) ->
            begin
              match kind with
              | NamedDeclaration -> add_declare_export_declaration env declare_export_declaration
              | NamedSpecifier _ -> assert false
            end
          | (ExportDefault { local; _ }, ExportDefaultDef export_default_declaration) ->
            begin
              match local with
              | Some _id -> add_export_default_declaration env export_default_declaration
              | None -> env
            end
          | (ExportNamed { kind; _ }, ExportNamedDef stmt) ->
            begin
              match kind with
              | NamedDeclaration -> add_stmt env stmt
              | NamedSpecifier _ -> assert false
            end
          | _ -> assert false)
        env
        named
        named_infos)

  let add_export_type_bindings type_named type_named_infos env =
    File_sig.(
      let type_named =
        List.filter
          (function
            | (_, (_, TypeExportNamed { kind = NamedSpecifier _; _ })) -> false
            | (_, (_, _)) -> true)
          type_named
      in
      List.fold_left2
        (fun env (_n, (_, export)) export_def ->
          match (export, export_def) with
          | (TypeExportNamed { kind; _ }, DeclareExportDef declare_export_declaration) ->
            begin
              match kind with
              | NamedDeclaration -> add_declare_export_declaration env declare_export_declaration
              | NamedSpecifier _ -> assert false
            end
          | (TypeExportNamed { kind; _ }, ExportNamedDef stmt) ->
            begin
              match kind with
              | NamedDeclaration -> add_stmt env stmt
              | NamedSpecifier _ -> assert false
            end
          | _ -> assert false)
        env
        type_named
        type_named_infos)

  let add_named_imports import_loc source kind named_imports env =
    SMap.fold
      (fun remote ids env ->
        SMap.fold
          (fun local locs env ->
            Nel.fold_left
              (fun env { File_sig.remote_loc; local_loc } ->
                let id = Flow_ast_utils.ident_of_source (local_loc, local) in
                let name = (remote_loc, remote) in
                add_env env (Entry.import_named import_loc id name kind source))
              env
              locs)
          ids
          env)
      named_imports
      env

  let rec add_require_bindings toplevel_names require_loc source ?name require_bindings env =
    let filter (_, x) = SSet.mem x toplevel_names in
    File_sig.(
      match require_bindings with
      | BindIdent id ->
        if filter id then
          add_env env (Entry.require require_loc (Flow_ast_utils.ident_of_source id) ?name source)
        else
          env
      | BindNamed named_requires ->
        List.fold_left
          (fun env (remote, require_bindings) ->
            let name =
              match name with
              | None -> Nel.one remote
              | Some name -> Nel.cons remote name
            in
            add_require_bindings toplevel_names require_loc source ~name require_bindings env)
          env
          named_requires)

  let add_ns_imports import_loc source kind ns_imports env =
    match ns_imports with
    | None -> env
    | Some id -> add_env env (Entry.import_star import_loc id kind source)

  let mk env toplevel_names file_sig =
    File_sig.(
      let module_sig = file_sig.module_sig in
      let { requires = imports_info; info = exports_info; module_kind; type_exports_named; _ } =
        module_sig
      in
      let env =
        let { module_kind_info; type_exports_named_info } = exports_info in
        let env =
          match (module_kind, module_kind_info) with
          | (CommonJS _, CommonJSInfo _) -> env
          | (ES { named; _ }, ESInfo named_infos) -> add_export_value_bindings named named_infos env
          | _ -> assert false
        in
        add_export_type_bindings type_exports_named type_exports_named_info env
      in
      let env =
        List.fold_left
          (fun env -> function
            | Require { source; bindings = Some require_bindings; require_loc } ->
              add_require_bindings toplevel_names require_loc source require_bindings env
            | Import { import_loc; source; named; ns; types; typesof; typesof_ns } ->
              let open Ast.Statement.ImportDeclaration in
              let env = add_named_imports import_loc source ImportValue named env in
              let env =
                add_ns_imports
                  import_loc
                  source
                  ImportValue
                  (Option.map ~f:Flow_ast_utils.ident_of_source ns)
                  env
              in
              let env = add_named_imports import_loc source ImportType types env in
              let env = add_named_imports import_loc source ImportTypeof typesof env in
              add_ns_imports
                import_loc
                source
                ImportTypeof
                (Option.map ~f:Flow_ast_utils.ident_of_source typesof_ns)
                env
            | _ -> env)
          env
          imports_info
      in
      (env, file_sig))

  let verify
      ?(prevent_munge = false)
      ?(facebook_fbt = None)
      ?(ignore_static_propTypes = false)
      ?(facebook_keyMirror = false)
      (env, file_sig) =
    let module Verify = V (struct
      let prevent_munge = prevent_munge

      let facebook_fbt = facebook_fbt

      let ignore_static_propTypes = ignore_static_propTypes

      let facebook_keyMirror = facebook_keyMirror
    end) in
    Verify.check env file_sig @@ Verify.exports file_sig

  let generate
      ?(prevent_munge = false)
      ?(facebook_fbt = None)
      ?(ignore_static_propTypes = false)
      ?(facebook_keyMirror = false)
      (env, file_sig)
      program =
    let module Generate = G (struct
      let prevent_munge = prevent_munge

      let facebook_fbt = facebook_fbt

      let ignore_static_propTypes = ignore_static_propTypes

      let facebook_keyMirror = facebook_keyMirror
    end) in
    Generate.make env file_sig program

  (* Returns a triplet containing
     - a set of signature verification errors
     - an environment of local bindings reachable from the exports
     - a signature AST
  *)
  let verify_and_generate
      ?(prevent_munge = false)
      ?(facebook_fbt = None)
      ?(ignore_static_propTypes = false)
      ?(facebook_keyMirror = false)
      (env, file_sig)
      program =
    let (errors, _, pruned_env) =
      verify
        ~prevent_munge
        ~facebook_fbt
        ~ignore_static_propTypes
        ~facebook_keyMirror
        (env, file_sig)
    in
    let env =
      if Signature_builder_deps.PrintableErrorSet.is_empty errors then
        pruned_env
      else
        env
    in
    ( errors,
      pruned_env,
      generate
        ~prevent_munge
        ~facebook_fbt
        ~ignore_static_propTypes
        ~facebook_keyMirror
        (env, file_sig)
        program )
end

class type_hoister =
  object (this)
    inherit [Env.t, Loc.t] visitor ~init:Env.empty as super

    (* tracks the current block scope level; for now, this can only take on values 0 and 1 *)
    val mutable level = 0

    method private next f =
      level <- level + 1;
      Lazy.force f;
      level <- level - 1

    method private is_toplevel = level = 0

    method private add_binding entry =
      let entry =
        if this#is_toplevel then
          entry
        else
          let (id, (loc, _)) = entry in
          Entry.sketchy_toplevel loc id
      in
      this#update_acc (Env.add entry)

    method private update_binding (x, id, expr) =
      this#update_acc (fun env ->
          match SMap.find_opt x env with
          | None -> env
          | Some u ->
            SMap.add
              x
              (Loc_collections.LocMap.map
                 (function
                   | (loc, Signature_builder_kind.WithPropertiesDef def) ->
                     ( loc,
                       Signature_builder_kind.WithPropertiesDef
                         { def with properties = (id, expr) :: def.properties } )
                   | (loc, base) ->
                     ( loc,
                       Signature_builder_kind.WithPropertiesDef { base; properties = [(id, expr)] }
                     ))
                 u)
              env)

    method private add_binding_opt =
      function
      | (None, _) -> ()
      | (Some id, kind) -> this#add_binding (id, kind)

    method private add_binding_list = List.iter (fun entry -> this#add_binding entry)

    (* Process local declarations. Ignore import declarations and export declarations since they are
     handled in File_sig, although it is likely there is still some overlap, in which case we
     arrange things so that whatever File_sig does wins.  *)
    method! toplevel_statement_list (stmts : (Loc.t, Loc.t) Ast.Statement.t list) =
      stmts
      |> ListUtils.ident_map (fun stmt ->
             let open Ast.Statement in
             match stmt with
             (* process bindings *)
             | (_, VariableDeclaration _)
             | (_, DeclareVariable _)
             | (_, FunctionDeclaration _)
             | (_, DeclareFunction _)
             | (_, ClassDeclaration _)
             | (_, DeclareClass _)
             | (_, EnumDeclaration _)
             | (_, TypeAlias _)
             | (_, DeclareTypeAlias _)
             | (_, OpaqueType _)
             | (_, DeclareOpaqueType _)
             | (_, InterfaceDeclaration _)
             | (_, DeclareInterface _) ->
               super#statement stmt
             (* recurse through control-flow *)
             | (_, Block _)
             | (_, DoWhile _)
             | (_, For _)
             | (_, ForIn _)
             | (_, ForOf _)
             | (_, If _)
             | (_, Labeled _)
             | (_, Switch _)
             | (_, Try _)
             | (_, While _) ->
               this#next (lazy (ignore @@ super#statement stmt));
               stmt
             | ( _,
                 Expression
                   {
                     Expression.expression =
                       ( _,
                         Ast.Expression.Assignment
                           {
                             Ast.Expression.Assignment.operator = None;
                             left =
                               ( _,
                                 Ast.Pattern.Expression
                                   ( _,
                                     Ast.Expression.Member
                                       {
                                         Ast.Expression.Member._object =
                                           ( _,
                                             Ast.Expression.Identifier
                                               (_, { Ast.Identifier.name = x; _ }) );
                                         property = Ast.Expression.Member.PropertyIdentifier id;
                                       } ) );
                             right = expr;
                           } );
                     _;
                   } ) ->
               this#update_binding (x, id, expr);
               stmt
             (* shortcut *)
             | (_, DeclareExportDeclaration _)
             | (_, ExportDefaultDeclaration _)
             | (_, ExportNamedDeclaration _)
             | (_, ImportDeclaration _)
             | (_, DeclareModule _)
             | (_, DeclareModuleExports _)
             | (_, Empty)
             | (_, Expression _)
             | (_, Break _)
             | (_, Continue _)
             | (_, Throw _)
             | (_, Return _)
             | (_, Debugger)
             | (_, With _) ->
               stmt)

    method! statement (stmt : (Loc.t, Loc.t) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      (* ignore block-scoped bindings and type bindings *)
      | (_, ClassDeclaration _)
      | (_, DeclareClass _)
      | (_, EnumDeclaration _)
      | (_, TypeAlias _)
      | (_, DeclareTypeAlias _)
      | (_, OpaqueType _)
      | (_, DeclareOpaqueType _)
      | (_, InterfaceDeclaration _)
      | (_, DeclareInterface _) ->
        stmt
      (* process function-scoped bindings *)
      | (_, VariableDeclaration decl) ->
        let open Ast.Statement.VariableDeclaration in
        let { kind; _ } = decl in
        begin
          match kind with
          | Ast.Statement.VariableDeclaration.Var -> super#statement stmt
          | Ast.Statement.VariableDeclaration.Let
          | Ast.Statement.VariableDeclaration.Const ->
            stmt
        end
      | (_, DeclareVariable _)
      | (_, FunctionDeclaration _)
      | (_, DeclareFunction _) ->
        super#statement stmt
      (* recurse through control flow *)
      | (_, Block _)
      | (_, DoWhile _)
      | (_, For _)
      | (_, ForIn _)
      | (_, ForOf _)
      | (_, If _)
      | (_, Labeled _)
      | (_, Switch _)
      | (_, Try _)
      | (_, While _) ->
        super#statement stmt
      (* shortcut *)
      | (_, DeclareExportDeclaration _)
      | (_, ExportDefaultDeclaration _)
      | (_, ExportNamedDeclaration _)
      | (_, ImportDeclaration _)
      | (_, DeclareModule _)
      | (_, DeclareModuleExports _)
      | (_, Empty)
      | (_, Expression _)
      | (_, Break _)
      | (_, Continue _)
      | (_, Throw _)
      | (_, Return _)
      | (_, Debugger)
      | (_, With _) ->
        stmt

    method! variable_declaration loc (decl : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
      this#add_binding_list (Entry.variable_declaration loc decl);
      decl

    method! declare_variable loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareVariable.t) =
      this#add_binding (Entry.declare_variable loc decl);
      decl

    method! function_declaration loc (expr : (Loc.t, Loc.t) Ast.Function.t) =
      this#add_binding (Entry.function_declaration loc expr);
      expr

    method! declare_function loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareFunction.t) =
      this#add_binding (Entry.declare_function loc decl);
      decl

    method! class_ loc (cls : (Loc.t, Loc.t) Ast.Class.t) =
      this#add_binding (Entry.class_ loc cls);
      cls

    method! declare_class loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareClass.t) =
      this#add_binding (Entry.declare_class loc decl);
      decl

    method! enum_declaration loc (enum : (Loc.t, Loc.t) Ast.Statement.EnumDeclaration.t) =
      this#add_binding (Entry.enum loc enum);
      enum

    method! type_alias loc (stuff : (Loc.t, Loc.t) Ast.Statement.TypeAlias.t) =
      this#add_binding (Entry.type_alias loc stuff);
      stuff

    method! opaque_type loc (otype : (Loc.t, Loc.t) Ast.Statement.OpaqueType.t) =
      this#add_binding (Entry.opaque_type loc otype);
      otype

    method! interface loc (interface : (Loc.t, Loc.t) Ast.Statement.Interface.t) =
      this#add_binding (Entry.interface loc interface);
      interface

    (* Ignore expressions *)
    method! expression (expr : (Loc.t, Loc.t) Ast.Expression.t) = expr
  end

let program ast ~exports_info ~toplevel_names =
  let env =
    let hoist = new type_hoister in
    hoist#eval hoist#program ast
  in
  Signature.mk env toplevel_names exports_info
