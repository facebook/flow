(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

module Entry = Signature_builder_entry
module Env = Signature_builder_env
module Verify = Signature_builder_verify

module Signature = struct
  type t = Env.t * File_sig.exports_info
  let init = Env.empty, []

  let add_env env entry =
    Env.add entry env

  let add_env_opt env = function
    | None, _ -> env
    | Some id, kind -> add_env env (id, kind)

  let add_env_list env entries =
    Env.push entries env

  let add_variable_declaration env variable_declaration =
    add_env_list env (Entry.variable_declaration variable_declaration)

  let add_function_declaration env function_declaration =
    add_env_opt env (Entry.function_declaration function_declaration)

  let add_class env class_ =
    add_env_opt env (Entry.class_ class_)

  let add_declare_variable env declare_variable =
    add_env env (Entry.declare_variable declare_variable)

  let add_declare_function env declare_function =
    add_env env (Entry.declare_function declare_function)

  let add_declare_class env declare_class =
    add_env env (Entry.declare_class declare_class)

  let add_type_alias env type_alias =
    add_env env (Entry.type_alias type_alias)

  let add_opaque_type env opaque_type =
    add_env env (Entry.opaque_type opaque_type)

  let add_interface env interface =
    add_env env (Entry.interface interface)

  let add_declare_export_declaration env = Ast.Statement.DeclareExportDeclaration.(function
    | Variable (_, declare_variable) -> add_declare_variable env declare_variable
    | Function (_, declare_function) -> add_declare_function env declare_function
    | Class (_, declare_class) -> add_declare_class env declare_class
    | NamedType (_, type_alias) -> add_type_alias env type_alias
    | NamedOpaqueType (_, opaque_type) -> add_opaque_type env opaque_type
    | Interface (_, interface) -> add_interface env interface
    | DefaultType _ -> assert false
  )

  let add_export_default_declaration env = Ast.Statement.ExportDefaultDeclaration.(function
    | Declaration (_, Ast.Statement.FunctionDeclaration
        ({ Ast.Function.id = Some _; _ } as function_declaration)
      ) ->
      add_function_declaration env function_declaration
    | Declaration (_, Ast.Statement.ClassDeclaration ({ Ast.Class.id = Some _; _ } as class_)) ->
      add_class env class_
    | Declaration _ -> assert false
    | Expression (_, Ast.Expression.Function ({ Ast.Function.id = Some _; _ } as function_)) ->
      add_function_declaration env function_
    | Expression _ -> assert false
  )

  let add_stmt env = Ast.Statement.(function
    | _, VariableDeclaration variable_declaration -> add_variable_declaration env variable_declaration
    | _, DeclareVariable declare_variable -> add_declare_variable env declare_variable
    | _, FunctionDeclaration function_declaration -> add_function_declaration env function_declaration
    | _, DeclareFunction declare_function -> add_declare_function env declare_function
    | _, ClassDeclaration class_ -> add_class env class_
    | _, DeclareClass declare_class -> add_declare_class env declare_class
    | _, TypeAlias type_alias -> add_type_alias env type_alias
    | _, DeclareTypeAlias type_alias -> add_type_alias env type_alias
    | _, OpaqueType opaque_type -> add_opaque_type env opaque_type
    | _, DeclareOpaqueType opaque_type -> add_opaque_type env opaque_type
    | _, InterfaceDeclaration interface -> add_interface env interface
    | _, DeclareInterface interface -> add_interface env interface

    | _, Block _
    | _, DoWhile _
    | _, For _
    | _, ForIn _
    | _, ForOf _
    | _, If _
    | _, Labeled _
    | _, Switch _
    | _, Try _
    | _, While _
    | _, DeclareExportDeclaration _
    | _, ExportDefaultDeclaration _
    | _, ExportNamedDeclaration _
    | _, ImportDeclaration _
    | _, DeclareModule _
    | _, DeclareModuleExports _
    | _, Empty
    | _, Expression _
    | _, Break _
    | _, Continue _
    | _, Throw _
    | _, Return _
    | _, Debugger
    | _, With _
      -> assert false
  )

  let add_export_value_bindings named named_infos env =
    let open File_sig in
    SMap.fold (fun n export_def env ->
      let export = SMap.find n named in
      match export, export_def with
        | ExportDefault { local; _ }, DeclareExportDef declare_export_declaration ->
          begin match local with
            | Some _id -> add_declare_export_declaration env declare_export_declaration
            | None -> env
          end
        | ExportNamed { kind; _ }, DeclareExportDef declare_export_declaration ->
          begin match kind with
            | NamedDeclaration -> add_declare_export_declaration env declare_export_declaration
            | NamedSpecifier _ -> assert false
          end
        | ExportDefault { local; _ }, ExportDefaultDef export_default_declaration ->
          begin match local with
            | Some _id -> add_export_default_declaration env export_default_declaration
            | None -> env
          end
        | ExportNamed { kind; _ }, ExportNamedDef stmt ->
          begin match kind with
            | NamedDeclaration -> add_stmt env stmt
            | NamedSpecifier _ -> assert false
          end
        | _ -> assert false
    ) named_infos env

  let add_export_type_bindings type_named type_named_infos env =
    let open File_sig in
    SMap.fold (fun n export_def env ->
      let export = SMap.find n type_named in
      match export, export_def with
        | TypeExportNamed { kind; _ }, DeclareExportDef declare_export_declaration ->
          begin match kind with
            | NamedDeclaration -> add_declare_export_declaration env declare_export_declaration
            | NamedSpecifier _ -> assert false
          end
        | TypeExportNamed { kind; _ }, ExportNamedDef stmt ->
          begin match kind with
            | NamedDeclaration -> add_stmt env stmt
            | NamedSpecifier _ -> assert false
          end
        | _ -> assert false
    ) type_named_infos env

  let add_named_imports ?(filter=(fun _ -> true)) source kind named_imports env =
    SMap.fold (fun remote ids env ->
      SMap.fold (fun local locs env ->
        Nel.fold_left (fun env { File_sig.remote_loc; local_loc } ->
          let id = local_loc, local in
          let name = remote_loc, remote in
          if filter id then add_env env (Entry.import_named id name kind source) else env
        ) env locs
      ) ids env
    ) named_imports env

  let add_require_bindings toplevel_names source require_bindings env =
    let filter (_, x) = SSet.mem x toplevel_names in
    let open File_sig in
    match require_bindings with
      | BindIdent id -> if filter id then add_env env (Entry.require id source) else env
      | BindNamed named_imports ->
        let kind = Ast.Statement.ImportDeclaration.ImportValue in
        add_named_imports ~filter source kind named_imports env

  let add_ns_imports source kind ns_imports env =
    match ns_imports with
      | None -> env
      | Some id -> add_env env (Entry.import_star id kind source)

  let mk env toplevel_names file_sig =
    let open File_sig in
    let module_sig = file_sig.module_sig in
    let {
      requires = imports_info;
      info = exports_info;
      module_kind;
      type_exports_named;
      _
    } = module_sig in
    let env =
      let { module_kind_info; type_exports_named_info } = exports_info in
      let env = match module_kind, module_kind_info with
        | CommonJS _, CommonJSInfo _ -> env
        | ES { named; _ }, ESInfo named_infos ->
          add_export_value_bindings named named_infos env
        | _ -> assert false
      in
      add_export_type_bindings type_exports_named type_exports_named_info env
    in
    let env = List.fold_left (fun env -> function
      | Require { source; bindings = Some require_bindings; _ } ->
        add_require_bindings toplevel_names source require_bindings env
      | Import { source; named; ns; types; typesof; typesof_ns } ->
        let env = add_named_imports source Ast.Statement.ImportDeclaration.ImportValue named env in
        let env = add_ns_imports source Ast.Statement.ImportDeclaration.ImportValue ns env in
        let env = add_named_imports source Ast.Statement.ImportDeclaration.ImportType types env in
        let env = add_named_imports source Ast.Statement.ImportDeclaration.ImportTypeof typesof env in
        add_ns_imports source Ast.Statement.ImportDeclaration.ImportTypeof typesof_ns env
      | _ -> env
    ) env imports_info in
    env, file_sig

  let verify (env, file_sig) =
    Verify.check env @@ Verify.exports file_sig
end

class type_hoister = object(this)
  inherit [Env.t] visitor ~init:Env.empty as super

  method private add_binding entry =
    this#update_acc (Env.add entry)

  method private add_binding_opt = function
    | None, _ -> ()
    | Some id, kind -> this#add_binding (id, kind)

  method private add_binding_list =
    List.iter (fun entry -> this#add_binding entry)

  (* Process local declarations. Ignore import declarations and export declarations since they are
     handled in File_sig, although it is likely there is still some overlap, in which case we
     arrange things so that whatever File_sig does wins.  *)
  method! toplevel_statement_list (stmts: (Loc.t, Loc.t) Ast.Statement.t list) =
    stmts |> ListUtils.ident_map (fun stmt ->
      let open Ast.Statement in
      match stmt with
      (* process bindings *)
      | _, VariableDeclaration _
      | _, DeclareVariable _
      | _, FunctionDeclaration _
      | _, DeclareFunction _
      | _, ClassDeclaration _
      | _, DeclareClass _
      | _, TypeAlias _
      | _, DeclareTypeAlias _
      | _, OpaqueType _
      | _, DeclareOpaqueType _
      | _, InterfaceDeclaration _
      | _, DeclareInterface _
        -> super#statement stmt

      (* recurse through control-flow *)
      | _, Block _
      | _, DoWhile _
      | _, For _
      | _, ForIn _
      | _, ForOf _
      | _, If _
      | _, Labeled _
      | _, Switch _
      | _, Try _
      | _, While _
        -> super#statement stmt

      (* shortcut *)
      | _, DeclareExportDeclaration _
      | _, ExportDefaultDeclaration _
      | _, ExportNamedDeclaration _
      | _, ImportDeclaration _
      | _, DeclareModule _
      | _, DeclareModuleExports _
      | _, Empty
      | _, Expression _
      | _, Break _
      | _, Continue _
      | _, Throw _
      | _, Return _
      | _, Debugger
      | _, With _
        -> stmt
    )

  method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    (* process non-lexical bindings *)
    | _, VariableDeclaration decl ->
      let open Ast.Statement.VariableDeclaration in
      let { kind; _ } = decl in
      begin match kind with
        | Ast.Statement.VariableDeclaration.Var -> super#statement stmt
        | Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const -> stmt
      end
    | _, DeclareVariable _ -> super#statement stmt

    (* ignore lexical bindings *)
    | _, FunctionDeclaration _
    | _, DeclareFunction _
    | _, ClassDeclaration _
    | _, DeclareClass _
    | _, TypeAlias _
    | _, DeclareTypeAlias _
    | _, OpaqueType _
    | _, DeclareOpaqueType _
    | _, InterfaceDeclaration _
    | _, DeclareInterface _
      -> stmt

    (* recurse through control flow *)
    | _, Block _
    | _, DoWhile _
    | _, For _
    | _, ForIn _
    | _, ForOf _
    | _, If _
    | _, Labeled _
    | _, Switch _
    | _, Try _
    | _, While _
      -> super#statement stmt

    (* shortcut *)
    | _, DeclareExportDeclaration _
    | _, ExportDefaultDeclaration _
    | _, ExportNamedDeclaration _
    | _, ImportDeclaration _
    | _, DeclareModule _
    | _, DeclareModuleExports _
    | _, Empty
    | _, Expression _
    | _, Break _
    | _, Continue _
    | _, Throw _
    | _, Return _
    | _, Debugger
    | _, With _
      -> stmt

  method! variable_declaration (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) =
    this#add_binding_list (Entry.variable_declaration decl);
    decl

  method! declare_variable (decl: (Loc.t, Loc.t) Ast.Statement.DeclareVariable.t) =
    this#add_binding (Entry.declare_variable decl);
    decl

  method! function_declaration (expr: (Loc.t, Loc.t) Ast.Function.t) =
    this#add_binding_opt (Entry.function_declaration expr);
    expr

  method! declare_function (decl: (Loc.t, Loc.t) Ast.Statement.DeclareFunction.t) =
    this#add_binding (Entry.declare_function decl);
    decl

  method! class_ (cls: (Loc.t, Loc.t) Ast.Class.t) =
    this#add_binding_opt (Entry.class_ cls);
    cls

  method! declare_class (decl: (Loc.t, Loc.t) Ast.Statement.DeclareClass.t) =
    this#add_binding (Entry.declare_class decl);
    decl

  method! type_alias (stuff: (Loc.t, Loc.t) Ast.Statement.TypeAlias.t) =
    this#add_binding (Entry.type_alias stuff);
    stuff

  method! opaque_type (otype: (Loc.t, Loc.t) Ast.Statement.OpaqueType.t) =
    this#add_binding (Entry.opaque_type otype);
    otype

  method! interface (interface: (Loc.t, Loc.t) Ast.Statement.Interface.t) =
    this#add_binding (Entry.interface interface);
    interface

  (* Ignore expressions *)
  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    expr

end

let program program =
  let env =
    let hoist = new type_hoister in
    hoist#eval hoist#program program in
  let { File_sig.toplevel_names; exports_info } =
    File_sig.program_with_toplevel_names_and_exports_info program in
  Core_result.map ~f:(Signature.mk env toplevel_names) exports_info
