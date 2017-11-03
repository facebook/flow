(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor

type t = {
  module_sig: module_sig;
  declare_modules: (Loc.t * module_sig) SMap.t
}

and module_sig = {
  requires: require SMap.t;
  module_kind: module_kind;
  type_exports: Loc.t SMap.t;
}

and require = {
  loc: Loc.t;
  cjs_requires: Loc.t list;
  es_imports: Loc.t list;
  named: Loc.t Nel.t SMap.t SMap.t;
  ns: Loc.t Nel.t SMap.t;
  types: Loc.t Nel.t SMap.t SMap.t;
  types_ns: Loc.t Nel.t SMap.t;
  typesof: Loc.t Nel.t SMap.t SMap.t;
  typesof_ns: Loc.t Nel.t SMap.t;
}

and module_kind =
  | CommonJS of { clobbered: Loc.t option }
  | ES of { named: Loc.t option SMap.t; batch: Loc.t SMap.t }

let empty_module_sig = {
  requires = SMap.empty;
  module_kind = CommonJS { clobbered = None };
  type_exports = SMap.empty;
}

let empty_file_sig = {
  module_sig = empty_module_sig;
  declare_modules = SMap.empty;
}

let mk_require
  ?(cjs_requires = []) ?(es_imports = [])
  ?(named = SMap.empty)
  ?(ns = SMap.empty)
  ?(types = SMap.empty)
  ?(types_ns = SMap.empty)
  ?(typesof = SMap.empty)
  ?(typesof_ns = SMap.empty)
  loc =
  { loc; cjs_requires; es_imports; named; ns; types; types_ns; typesof; typesof_ns }

let combine_nel _ a b = Some (Nel.concat (a, [b]))

let merge_requires =
  let nel_smap_union _ a b = Some (SMap.union a b ~combine:combine_nel) in
  let nel_append _ a b = Some (Nel.rev_append a b) in
  fun r1 r2 -> {
    loc = r2.loc;
    cjs_requires = List.rev_append r2.cjs_requires r1.cjs_requires;
    es_imports = List.rev_append r2.es_imports r2.es_imports;
    named = SMap.union r1.named r2.named ~combine:nel_smap_union;
    ns = SMap.union r1.ns r2.ns ~combine:nel_append;
    types = SMap.union r1.types r2.types ~combine:nel_smap_union;
    types_ns = SMap.union r1.types_ns r2.types_ns ~combine:nel_append;
    typesof = SMap.union r1.typesof r2.typesof ~combine:nel_smap_union;
    typesof_ns = SMap.union r1.typesof_ns r2.typesof_ns ~combine:nel_append;
  }

let require_loc_map msig =
  SMap.fold (fun name {loc; _} acc ->
    SMap.add name loc acc
  ) msig.requires SMap.empty

let add_declare_module name m loc fsig = {
  fsig with
  declare_modules = SMap.add name (loc, m) fsig.declare_modules;
}

let update_sig f fsig = { fsig with module_sig = f fsig.module_sig }

let set_module_kind module_kind msig = { msig with module_kind }

let add_cjs_require name loc msig =
  let require = mk_require loc ~cjs_requires:[loc] in
  let requires = SMap.add name require msig.requires ~combine:merge_requires in
  { msig with requires }

let add_es_import name ?named ?ns ?types ?types_ns ?typesof ?typesof_ns loc msig =
  let require = mk_require loc ~es_imports:[loc] ?named ?ns ?types ?types_ns ?typesof ?typesof_ns in
  let requires = SMap.add name require msig.requires ~combine:merge_requires in
  { msig with requires }

let add_type_export name loc msig = {
  msig with
  type_exports = SMap.add name loc msig.type_exports;
}

let add_es_exports (named_bindings: (Loc.t option * string) list) batch_bindings msig =
  let named, batch = match msig.module_kind with
  | CommonJS _ -> SMap.empty, SMap.empty
  | ES { named; batch } -> named, batch
  in
  let named = List.fold_left (fun acc (loc, x) ->
    SMap.add x loc acc
  ) named named_bindings in
  let batch = List.fold_left (fun acc (loc, x) ->
    SMap.add x loc acc
  ) batch batch_bindings in
  set_module_kind (ES { named; batch }) msig

(* Subclass of the AST visitor class that calculates requires. Initializes with
   the scope builder class.
*)
class requires_calculator ~ast = object(this)
  inherit [t] visitor ~init:empty_file_sig as super

  val scope_info = Scope_builder.program ast

  val mutable curr_declare_module: module_sig option = None;

  method private update_module_sig f =
    match curr_declare_module with
    | Some m ->
      curr_declare_module <- Some (f m)
    | None ->
      this#update_acc (update_sig f)

  method private add_cjs_require r loc =
    this#update_module_sig (add_cjs_require r loc)

  method private add_es_import r ?named ?ns ?types ?types_ns ?typesof ?typesof_ns loc =
    this#update_module_sig (add_es_import r ?named ?ns ?types ?types_ns ?typesof ?typesof_ns loc)

  method private add_type_export name loc =
    this#update_module_sig (add_type_export name loc)

  method private add_es_exports named_bindings batch_bindings =
    this#update_module_sig (add_es_exports named_bindings batch_bindings)

  method private set_module_kind module_kind =
    this#update_module_sig (set_module_kind module_kind)

  method! call (expr: Loc.t Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; arguments } = expr in
    begin match callee, arguments with
    | ((_, Identifier (loc, "require")),
       [Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })])
      ->
      if not (Scope_api.is_local_use scope_info loc)
      then this#add_cjs_require v require_loc
    | ((_, Identifier (loc, "requireLazy")),
       [Expression (_, Array ({ Array.elements })); Expression (_);])
      ->
      let element = function
        | Some (Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })) ->
          if not (Scope_api.is_local_use scope_info loc)
          then this#add_cjs_require v require_loc
        | _ -> () in
      List.iter element elements
    | _ -> ()
    end;
    super#call expr

  method! import (expr: Loc.t Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | import_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      this#add_es_import v import_loc
    (* TODO: match statement.ml support for template literals *)
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration (decl: Loc.t Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind; source; specifiers; default } = decl in
    let loc, name =  match source with
    | loc, { Ast.Literal.value = Ast.Literal.String name; _ } -> loc, name
    | _ -> failwith "import declaration source must be a string literal"
    in
    let named: Loc.t Nel.t SMap.t SMap.t ref = ref SMap.empty in
    let ns = ref SMap.empty in
    let types = ref SMap.empty in
    let types_ns = ref SMap.empty in
    let typesof = ref SMap.empty in
    let typesof_ns = ref SMap.empty in
    let ref_of_kind = function
      | ImportType -> types
      | ImportTypeof -> typesof
      | ImportValue -> named
    in
    let add_named remote local loc ref =
      let locals = SMap.singleton local (Nel.one loc) in
      let combine_nel_smap a b = SMap.union a b ~combine:combine_nel in
      ref := SMap.add remote locals !ref ~combine:combine_nel_smap
    in
    let add_ns local loc ref =
      let locs = Nel.one loc in
      ref := SMap.add local locs !ref ~combine:Nel.rev_append
    in
    Option.iter ~f:(fun (loc, local) ->
      add_named "default" local loc (ref_of_kind importKind)
    ) default;
    Option.iter ~f:(function
      | ImportNamespaceSpecifier (loc, (_, local)) ->
        add_ns local loc (
          match importKind with
          | ImportType -> types_ns
          | ImportTypeof -> typesof_ns
          | ImportValue -> ns)
      | ImportNamedSpecifiers named_specifiers ->
        List.iter (function {local; remote; kind} ->
          let importKind = match kind with Some k -> k | None -> importKind in
          let loc, local_name = match local with Some x -> x | None -> remote in
          let _, remote_name = remote in
          add_named remote_name local_name loc (ref_of_kind importKind)
        ) named_specifiers
    ) specifiers;
    this#add_es_import name loc
      ~named:!named ~ns:!ns ~types:!types ~types_ns:!types_ns ~typesof:!typesof ~typesof_ns:!typesof_ns;
    super#import_declaration decl

  method! export_default_declaration_decl (decl: Loc.t Ast.Statement.ExportDefaultDeclaration.declaration) =
    let open Ast.Statement.ExportDefaultDeclaration in
    begin match decl with
    | Declaration _
    | Expression _ ->
      this#add_es_exports [None, "default"] []
    end;
    super#export_default_declaration_decl  decl

  method! export_named_declaration (decl: Loc.t Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind = _; source; specifiers; declaration} = decl in
    begin match source with
    | Some (import_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some (loc, stmt) ->
      let open Ast.Statement in
      match stmt with
      | FunctionDeclaration { Ast.Function.id = Some (id_loc, name); _ }
      | ClassDeclaration { Ast.Class.id = Some (id_loc, name); _ } ->
        this#add_es_exports [Some id_loc, name] []
      | VariableDeclaration { VariableDeclaration.declarations = decls; _ } ->
        let bindings = Ast_utils.bindings_of_variable_declarations decls in
        let bindings =
          List.map (fun (loc, name) -> (Some loc, name)) bindings
        in
        this#add_es_exports bindings []
      | TypeAlias { TypeAlias.id; _ }
      | OpaqueType { OpaqueType.id; _ }
      | InterfaceDeclaration { Interface.id; _ } ->
        this#add_type_export (snd id) loc;
      | _ -> failwith "unsupported declaration"
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      this#export_specifiers source specifiers
    end;
    super#export_named_declaration decl

  method! declare_module_exports loc (annot: Loc.t Ast.Type.annotation) =
    this#set_module_kind (CommonJS { clobbered = Some loc });
    super#declare_module_exports loc annot

  method! declare_export_declaration (decl: Loc.t Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    begin match source with
    | Some (import_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some declaration ->
      let open Ast.Statement in
      match declaration with
      | Variable (_, { DeclareVariable.id=(id_loc, name); _ })
      | Function (_, { DeclareFunction.id=(id_loc, name); _ })
      | Class (_, { DeclareClass.id=(id_loc, name); _ }) ->
        let name = if default then "default" else name in
        this#add_es_exports [Some id_loc, name] []
      | DefaultType (_, _) ->
        this#add_es_exports [None, "default"] []
      | NamedType (_, { TypeAlias.id; _ })
      | NamedOpaqueType (_, { OpaqueType.id; _ })
      | Interface (_, { Interface.id; _ }) ->
        let name = if default then "default" else snd id in
        this#add_type_export name (fst id)
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      assert (not default);
      this#export_specifiers source specifiers
    end;
    super#declare_export_declaration decl

  method! assignment (expr: Loc.t Ast.Expression.Assignment.t) =
    let open Ast.Expression in
    let open Ast.Expression.Assignment in
    (* module.exports = e *)
    let { operator; left; _ } = expr in
    begin match operator, left with
    | Assign, (assign_loc, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Ast.Expression.Identifier (_, "module");
        property = Member.PropertyIdentifier (_, "exports"); _
      })) ->
      (* expressions not allowed in declare module body *)
      assert (curr_declare_module = None);
      if not (Scope_api.is_local_use scope_info module_loc)
      then this#set_module_kind (CommonJS { clobbered = Some assign_loc })
    | _ -> ()
    end;
    super#assignment expr

  method! declare_module loc (m: Loc.t Ast.Statement.DeclareModule.t) =
    let name = Ast.Statement.DeclareModule.(match m.id with
    | Identifier (_, name) -> name
    | Literal (_, { Ast.Literal.value = Ast.Literal.String name; _ }) -> name
    | Literal _ -> failwith "declare module literal id must be a string"
    ) in
    curr_declare_module <- Some (empty_module_sig);
    let ret = super#declare_module loc m in
    begin match curr_declare_module with
    | None -> failwith "lost curr_declare_module"
    | Some m -> this#update_acc (add_declare_module name m loc)
    end;
    curr_declare_module <- None;
    ret

  method private export_specifiers source =
    let open Ast.Statement.ExportNamedDeclaration in
    function
    | ExportBatchSpecifier (_, Some (id_loc, name)) ->
      this#add_es_exports [Some id_loc, name] []
    | ExportBatchSpecifier (loc, None) ->
      let require = match source with
      | Some (_, { Ast.Literal.value = Ast.Literal.String v; _ }) -> v
      | _ -> failwith "batch export missing source"
      in
      this#add_es_exports [] [loc, require]
    | ExportSpecifiers specs ->
      let bindings = Ast_utils.bindings_of_export_specifiers specs in
      let bindings =
        List.map (fun (loc, name) -> (Some loc, name)) bindings
      in
      this#add_es_exports bindings []
end

let program ~ast =
  let walk = new requires_calculator ~ast in
  walk#eval walk#program ast
