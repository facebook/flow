(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module Result = Core_result
open Flow_ast_visitor

type 'info t' = {
  module_sig: 'info module_sig';
  declare_modules: (Loc.t * 'info module_sig') SMap.t;
  tolerable_errors: tolerable_error list;
}

and 'info module_sig' = {
  requires: require list;
  module_kind: module_kind;
  type_exports_named: type_export SMap.t;
  type_exports_star: export_star list;
  info: 'info;
}

and require =
  | Require of {
      source: Ast_utils.source;
      require_loc: Loc.t;
      bindings: require_bindings option;
    }
  | ImportDynamic of { source: Ast_utils.source; import_loc: Loc.t }
  | Import0 of { source: Ast_utils.source }
  | Import of {
      source: Ast_utils.source;
      named: imported_locs Nel.t SMap.t SMap.t;
      ns: Ast_utils.ident option;
      types: imported_locs Nel.t SMap.t SMap.t;
      typesof: imported_locs Nel.t SMap.t SMap.t;
      typesof_ns: Ast_utils.ident option;
    }

and imported_locs = {
  remote_loc: Loc.t;
  local_loc: Loc.t;
}

and require_bindings =
  | BindIdent of Ast_utils.ident
  | BindNamed of imported_locs Nel.t SMap.t SMap.t

and module_kind =
  | CommonJS of {
      mod_exp_loc: Loc.t option;
    }
  | ES of {
      named: export SMap.t;
      star: export_star list;
    }

and export =
  | ExportDefault of {
      default_loc: Loc.t;
      local: Ast_utils.ident option;
    }
  | ExportNamed of {
      loc: Loc.t;
      kind: named_export_kind;
    }
  | ExportNs of {
      loc: Loc.t;
      source: Ast_utils.source;
    }

and named_export_kind =
  | NamedDeclaration
  | NamedSpecifier of { local: Ast_utils.ident; source: Ast_utils.source option }

and export_star =
  | ExportStar of { star_loc: Loc.t; source: Ast_utils.source }

and type_export =
  | TypeExportNamed of {
      loc: Loc.t;
      kind: named_export_kind
    }

and tolerable_error =
  (* e.g. `module.exports.foo = 4` when not at the top level *)
  | BadExportPosition of Loc.t
  (* e.g. `foo(module)`, dangerous because `module` is aliased *)
  | BadExportContext of string (* offending identifier *) * Loc.t
  | SignatureVerificationError of Signature_builder_deps.Error.t

type exports_info = {
  module_kind_info: module_kind_info;
  type_exports_named_info: es_export_def SMap.t;
}

and module_kind_info =
  | CommonJSInfo of cjs_exports_def list
  | ESInfo of es_export_def SMap.t

and cjs_exports_def =
  | DeclareModuleExportsDef of (Loc.t, Loc.t) Ast.Type.annotation
  | SetModuleExportsDef of (Loc.t, Loc.t) Ast.Expression.t
  | AddModuleExportsDef of Ast_utils.ident * (Loc.t, Loc.t) Ast.Expression.t

and es_export_def =
  | DeclareExportDef of (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.declaration
  | ExportDefaultDef of (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.declaration
  | ExportNamedDef of (Loc.t, Loc.t) Ast.Statement.t

type error =
  | IndeterminateModuleType of Loc.t

let mk_module_sig info = {
  requires = [];
  module_kind = CommonJS { mod_exp_loc = None };
  type_exports_named = SMap.empty;
  type_exports_star = [];
  info;
}

let mk_file_sig info = {
  module_sig = mk_module_sig info;
  declare_modules = SMap.empty;
  tolerable_errors = [];
}

let init_exports_info =  {
  module_kind_info = CommonJSInfo [];
  type_exports_named_info = SMap.empty;
}

module PP = struct
  let string_of_option f = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some (%s)" (f x)

  let items_to_collection_string indent open_ close items =
    let indent_str = String.make (indent * 2) ' ' in
    let items_str =
      items
    |> List.map (Printf.sprintf "%s%s;\n" (indent_str ^ "  "))
    |> String.concat ""
    in
    Printf.sprintf "%s\n%s%s%s" open_ items_str indent_str close

  let items_to_list_string indent items =
    items_to_collection_string indent "[" "]" items

  let items_to_record_string indent items =
    let items = items |> List.map (fun (label, value) ->
      Printf.sprintf "%s: %s" label value
    ) in
    items_to_collection_string indent "{" "}" items
end

let exports_info_to_string exports_info =
  let string_of_es_export_def = function
    | DeclareExportDef _ -> "DeclareExportDef"
    | ExportDefaultDef _ -> "ExportDefaultDef"
    | ExportNamedDef _ -> "ExportNamedDef"
  in
  let string_of_module_kind_info = function
    | CommonJSInfo _ -> "CommonJSInfo"
    | ESInfo named ->
      PP.items_to_record_string 2 @@ SMap.bindings @@ SMap.map string_of_es_export_def named
  in
  PP.items_to_record_string 1 [
    "module_kind_info", string_of_module_kind_info exports_info.module_kind_info;
    "type_exports_named_info", PP.items_to_record_string 2 @@ SMap.bindings @@ SMap.map string_of_es_export_def exports_info.type_exports_named_info;
  ]

(* Applications may not care about the info carried by signatures. *)
type module_sig = unit module_sig'
type t = unit t'

let init = mk_file_sig ()

let to_string t =
  let string_of_module_sig module_sig =
    let string_of_require_list require_list =
      let string_of_require_bindings = function
      | BindIdent (_, name) -> "BindIdent: " ^ name
      | BindNamed _ -> "BindNamed"
      in
      let string_of_require = function
      | Require {source=(_, name); bindings; _} ->
        Printf.sprintf "Require (%s, %s)"
          name
          (PP.string_of_option string_of_require_bindings bindings)
      | ImportDynamic _ -> "ImportDynamic"
      | Import0 _ -> "Import0"
      | Import _ -> "Import"
      in
      PP.items_to_list_string 2 (List.map string_of_require require_list)
    in
    let string_of_named_export_kind = function
      | NamedDeclaration -> "NamedDeclaration"
      | NamedSpecifier { local; _ } ->
        let _, x = local in
        Printf.sprintf "NamedSpecifier(%s)" x
    in
    let string_of_export = function
      | ExportDefault { local; _ } ->
        Printf.sprintf "ExportDefault (%s)" @@
          PP.string_of_option (fun (_, x) -> x) local
      | ExportNamed { kind; _ } ->
        Printf.sprintf "ExportNamed (%s)" @@
          string_of_named_export_kind kind
      | ExportNs _ -> "ExportNs"
    in
    let string_of_type_export = function
      | TypeExportNamed { kind; _ } ->
        Printf.sprintf "TypeExportNamed (%s)" @@
          string_of_named_export_kind kind
    in
    let string_of_export_star = function
      | ExportStar _ -> "ExportStar"
    in
    let string_of_module_kind = function
      | CommonJS _ -> "CommonJS"
      | ES { named; star } ->
        PP.items_to_record_string 2 [
          "named", PP.items_to_record_string 3 @@ SMap.bindings @@ SMap.map string_of_export named;
          "star", PP.items_to_list_string 3 @@ List.map string_of_export_star star;
        ]
    in
    PP.items_to_record_string 1 [
      "requires", string_of_require_list module_sig.requires;
      "module_kind", string_of_module_kind module_sig.module_kind;
      "type_exports_named", PP.items_to_record_string 2 @@ SMap.bindings @@ SMap.map string_of_type_export module_sig.type_exports_named;
      "type_exports_star", PP.items_to_list_string 2 @@ List.map string_of_export_star module_sig.type_exports_star;
    ]
  in
  let string_of_declare_modules _ = "TODO" in
  let string_of_tolerable_errors _ = "TODO" in
  PP.items_to_record_string 0 [
    "module_sig", string_of_module_sig t.module_sig;
    "declare_modules", string_of_declare_modules t.declare_modules;
    "tolerable_errors", string_of_tolerable_errors t.tolerable_errors;
  ]

let combine_nel _ a b = Some (Nel.concat (a, [b]))

let require_loc_map msig =
  let acc = SMap.empty in
  (* requires *)
  let acc = List.fold_left (fun acc require ->
    match require with
    | Require { source = (loc, mref); _ }
    | ImportDynamic { source = (loc, mref); _ }
    | Import0 { source = (loc, mref) }
    | Import { source = (loc, mref); _ } ->
      SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
  ) acc msig.requires in
  (* export type {...} from 'foo' *)
  let acc = SMap.fold (fun _ type_export acc ->
    match type_export with
    | TypeExportNamed { kind = NamedSpecifier ({source = Some (loc, mref); _ }); _ } ->
      SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
    | _ -> acc
  ) msig.type_exports_named acc in
  (* export type * from 'foo' *)
  let acc = List.fold_left (fun acc export_star ->
    match export_star with
    | ExportStar { source = (source_loc, mref); _ } ->
      SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append
  ) acc msig.type_exports_star in
  let acc = match msig.module_kind with
  | CommonJS _ -> acc
  | ES { named; star } ->
    (* export {...} from 'foo' *)
    let acc = SMap.fold (fun _ export acc ->
      match export with
      | ExportNamed { kind = NamedSpecifier ({source = Some (loc, mref); _ }); _ }
      | ExportNs { source = (loc, mref); _ } ->
        SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
      | _ -> acc
    ) named acc in
    (* export * from 'foo' *)
    let acc = List.fold_left (fun acc export_star ->
      match export_star with
      | ExportStar { source = (source_loc, mref); _ } ->
        SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append
    ) acc star in
    acc
  in
  acc

let add_declare_module name m loc fsig = {
  fsig with
  declare_modules = SMap.add name (loc, m) fsig.declare_modules;
}

let add_require require msig =
  let requires = require :: msig.requires in
  Ok ({ msig with requires })

let add_type_exports named named_info star msig =
  let type_exports_named = List.fold_left (fun acc (export, name) ->
    let type_export = match export with
    | ExportNamed { loc; kind; } -> TypeExportNamed { loc; kind; }
    | ExportDefault _ -> failwith "export default type"
    | ExportNs _ -> failwith "export type * as X"
    in
    SMap.add name type_export acc
  ) msig.type_exports_named named in
  let info = msig.info in
  let type_exports_named_info = List.fold_left (fun acc (export_info, name) ->
    SMap.add name export_info acc
  ) info.type_exports_named_info named_info in
  let type_exports_star = Option.fold ~f:(fun acc export_star ->
    export_star :: acc
  ) ~init:msig.type_exports_star star in
  Ok { msig with
    type_exports_named;
    type_exports_star;
    info = { info with
      type_exports_named_info
    }
  }

let add_es_exports loc named named_info star msig =
  let info = msig.info in
  let result = match msig.module_kind, info.module_kind_info with
  | CommonJS { mod_exp_loc = Some _ }, CommonJSInfo _ -> Error (IndeterminateModuleType loc)
  | CommonJS { mod_exp_loc = None }, CommonJSInfo _ -> Ok (SMap.empty, SMap.empty, [])
  | ES { named; star }, ESInfo named_info -> Ok (named, named_info, star)
  | _ -> failwith "unreachable"
  in
  match result with
  | Error e -> Error e
  | Ok (named0, named_info0, star0) ->
    let named = List.fold_left (fun acc (export, name) ->
      SMap.add name export acc
    ) named0 named in
    let named_info = List.fold_left (fun acc (export, name) ->
      SMap.add name export acc
    ) named_info0 named_info in
    let star = Option.fold ~f:(fun acc export_star ->
      export_star :: acc
    ) ~init:star0 star in
    let module_kind = ES { named; star } in
    let module_kind_info = ESInfo named_info in
    Ok ({ msig with module_kind; info = { info with module_kind_info } })

let set_cjs_exports mod_exp_loc cjs_exports_def msig =
  let info = msig.info in
  match msig.module_kind, info.module_kind_info with
  | CommonJS { mod_exp_loc = original_mod_exp_loc }, CommonJSInfo def ->
    let mod_exp_loc = Option.first_some original_mod_exp_loc (Some mod_exp_loc) in
    let module_kind = CommonJS { mod_exp_loc } in
    let module_kind_info = CommonJSInfo (cjs_exports_def::def) in
    Ok { msig with module_kind; info = { info with module_kind_info } }
  | ES _, ESInfo _ -> Error (IndeterminateModuleType mod_exp_loc)
  | _ -> failwith "unreachable"

(* Subclass of the AST visitor class that calculates requires and exports. Initializes with the
   scope builder class.
*)
class requires_exports_calculator ~ast = object(this)
  inherit [(exports_info t', error) result] visitor ~init:(Ok (mk_file_sig init_exports_info)) as super

  val scope_info = Scope_builder.program ast
  method toplevel_names =
    Scope_api.toplevel_names scope_info

  val mutable curr_declare_module: exports_info module_sig' option = None;

  (* This ensures that we do not add `require`s to `module_sig.requires` twice:
   * once in `variable_declarator`/`assignment` and once in `call`. *)
  val mutable visited_requires: Utils_js.LocSet.t = Utils_js.LocSet.empty;

  method private update_module_sig f =
    match curr_declare_module with
    | Some m ->
      (match f m with
      | Error e -> this#set_acc (Error e)
      | Ok msig -> curr_declare_module <- Some msig)
    | None ->
      this#update_acc (function
        | Error _ as acc -> acc
        | Ok fsig ->
          match f fsig.module_sig with
          | Error e -> Error e
          | Ok module_sig -> Ok ({ fsig with module_sig })
      )

  method private add_require require =
    this#update_module_sig (add_require require)

  method private add_exports loc kind named named_info batch =
    let add = Ast.Statement.(match kind with
    | ExportType -> add_type_exports
    | ExportValue -> (add_es_exports loc)
    ) in
    this#update_module_sig (add named named_info batch)

  method private set_cjs_exports mod_exp_loc cjs_exports_def =
    this#update_module_sig (set_cjs_exports mod_exp_loc cjs_exports_def)

  method private add_cjs_export mod_exp_loc cjs_exports_def =
    this#update_module_sig (set_cjs_exports mod_exp_loc cjs_exports_def)

  method private add_tolerable_error (err: tolerable_error) =
    this#update_acc (Result.map ~f:(fun fsig ->
        { fsig with tolerable_errors=err::fsig.tolerable_errors }
    ))

  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    (* Disallow expressions consisting of `module` or `exports`. These are dangerous because they
     * can allow aliasing and mutation. *)
    | _, Identifier (loc, (("module" | "exports") as name))
        when not (Scope_api.is_local_use scope_info loc) ->
      this#add_tolerable_error (BadExportContext (name, loc))
    | _ -> ()
    end;
    super#expression expr

  method! binary (expr: (Loc.t, Loc.t) Ast.Expression.Binary.t) =
    let open Ast.Expression in
    let open Ast.Expression.Binary in
    let is_module_or_exports = function
      | _, Identifier (_, ("module" | "exports")) -> true
      | _ -> false
    in
    let is_legal_operator = function
      | StrictEqual | StrictNotEqual -> true
      | _ -> false
    in
    let identify_or_recurse subexpr =
      if not (is_module_or_exports subexpr) then
        ignore (this#expression subexpr)
    in
    let { operator; left; right } = expr in
    (* Whitelist e.g. `require.main === module` by avoiding the recursive calls (where the errors
     * are generated) if the AST matches specific patterns. *)
    if is_legal_operator operator then begin
      identify_or_recurse left;
      identify_or_recurse right;
      expr
    end else
      super#binary expr

  method! member (expr: (Loc.t, Loc.t) Ast.Expression.Member.t) =
    let open Ast.Expression in
    let open Ast.Expression.Member in
    let { _object; property; computed = _ } = expr in
    (* Strip the loc to simplify the patterns *)
    let _, _object = _object in
    (* This gets called when patterns like `module.id` appear on the LHS of an
     * assignment, in addition to when they appear in ordinary expression
     * locations. Therefore we have to prevent anything that would be dangerous
     * if it appeared on the LHS side of an assignment. Ordinary export
     * statements are handled by handle_assignment, which stops recursion so we
     * don't arrive here in those cases. *)
    begin match _object, property with
      (* Allow `module.anythingButExports` *)
      | Identifier (_, "module"), PropertyIdentifier (_, prop) when prop <> "exports" -> ()
      (* Allow `module.exports.whatever` -- this is safe because handle_assignment has already
       * looked for assignments to it before recursing down here. *)
      | Member {
          _object=(_, Identifier (_, "module"));
          property = PropertyIdentifier (_, "exports");
          _;
        },
        PropertyIdentifier _
      (* Allow `exports.whatever`, for the same reason as above *)
      | Identifier (_, "exports"), PropertyIdentifier _ ->
        (* In these cases we don't know much about the property so we should recurse *)
        ignore (this#member_property property)
      | _ -> ignore (super#member expr)
    end;
    expr

  method! call call_loc (expr: (Loc.t, Loc.t) Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; targs = _; arguments } = expr in
    this#handle_call call_loc callee arguments None;
    super#call call_loc expr

  method! import import_loc (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | loc, (
        Literal { Ast.Literal.value = Ast.Literal.String name; _ } |
        TemplateLiteral { TemplateLiteral.
          quasis = [_, { TemplateLiteral.Element.
            value = { TemplateLiteral.Element.cooked = name; _ }; _
          }]; _
        }
      ) ->
      this#add_require (ImportDynamic {
        source = (loc, name);
        import_loc;
      })
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration stmt_loc (decl: (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind; source; specifiers; default } = decl in
    let source = match source with
    | loc, { Ast.StringLiteral.value = name; _ } -> loc, name
    in
    let import = match default, specifiers with
    | None, None -> Import0 { source }
    | _ ->
      let named = ref SMap.empty in
      let ns = ref None in
      let types = ref SMap.empty in
      let typesof = ref SMap.empty in
      let typesof_ns = ref None in
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
      let set_ns local loc ref =
        if !ref = None then ref := Some (loc, local)
        else failwith "unreachable"
      in
      Option.iter ~f:(fun (loc, local) ->
        add_named "default" local {remote_loc=loc; local_loc=loc} (ref_of_kind importKind)
      ) default;
      Option.iter ~f:(function
        | ImportNamespaceSpecifier (loc, (_, local)) ->
          (match importKind with
          | ImportType -> failwith "import type * is a parse error"
          | ImportTypeof -> set_ns local loc typesof_ns
          | ImportValue -> set_ns local loc ns)
        | ImportNamedSpecifiers named_specifiers ->
          List.iter (function {local; remote; kind} ->
            let importKind = match kind with Some k -> k | None -> importKind in
            let local_loc, local_name = match local with Some x -> x | None -> remote in
            let remote_loc, remote_name = remote in
            add_named remote_name local_name {remote_loc; local_loc} (ref_of_kind importKind)
          ) named_specifiers
      ) specifiers;
      Import {
        source;
        named = !named;
        ns = !ns;
        types = !types;
        typesof = !typesof;
        typesof_ns = !typesof_ns;
      }
    in
    this#add_require import;
    super#import_declaration stmt_loc decl

  method! export_default_declaration stmt_loc (decl: (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t) =
    let open Ast.Statement in
    let open Ast.Statement.ExportDefaultDeclaration in
    let { default = default_loc; declaration } = decl in
    let local = match declaration with
    | Declaration (_, FunctionDeclaration { Ast.Function.id; _ }) -> id
    | Declaration (_, ClassDeclaration { Ast.Class.id; _ }) -> id
    (* There's some ambiguity about the name Expression. This satisfies the compiler. *)
    | ExportDefaultDeclaration.Expression (_, Ast.Expression.Function { Ast.Function.id; _ }) -> id
    | _ -> None
    in
    let export = ExportDefault { default_loc; local } in
    let export_info = ExportDefaultDef declaration in
    this#add_exports stmt_loc ExportValue [export, "default"] [export_info, "default"] None;
    super#export_default_declaration stmt_loc decl

  method! export_named_declaration stmt_loc (decl: (Loc.t, Loc.t) Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind; source; specifiers; declaration} = decl in
    let source = match source with
    | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) -> Some (loc, mref)
    | None -> None
    in
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some (loc, stmt) ->
      let open Ast.Statement in
      assert (source = None);
      let kind = NamedDeclaration in
      let export_info = ExportNamedDef (loc, stmt) in
      match stmt with
      | FunctionDeclaration { Ast.Function.id = Some (loc, name); _ }
      | ClassDeclaration { Ast.Class.id = Some (loc, name); _ } ->
        let export = ExportNamed { loc; kind; } in
        this#add_exports stmt_loc ExportValue [export, name] [export_info, name] None
      | VariableDeclaration { VariableDeclaration.declarations = decls; _ } ->
        let bindings = Ast_utils.bindings_of_variable_declarations decls in
        let bindings = List.map (fun (loc, name) ->
          let export = ExportNamed { loc; kind } in
          (export, name), (export_info, name)
        ) bindings in
        let bindings1, bindings2 = List.split bindings in
        this#add_exports stmt_loc ExportValue bindings1 bindings2 None
      | TypeAlias { TypeAlias.id; _ }
      | OpaqueType { OpaqueType.id; _ }
      | InterfaceDeclaration { Interface.id; _ } ->
        let export = ExportNamed { loc; kind } in
        this#add_exports stmt_loc ExportType [export, snd id] [export_info, snd id] None;
      | _ -> failwith "unsupported declaration"
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      this#export_specifiers stmt_loc exportKind source specifiers
    end;
    super#export_named_declaration stmt_loc decl

  method! declare_module_exports loc (annot: (Loc.t, Loc.t) Ast.Type.annotation) =
    this#set_cjs_exports loc (DeclareModuleExportsDef annot);
    super#declare_module_exports loc annot

  method! declare_export_declaration stmt_loc (decl: (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    let source = match source with
    | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) ->
      assert (Option.is_none default); (* declare export default from not supported *)
      Some (loc, mref)
    | _ -> None
    in
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some declaration ->
      let open Ast.Statement in
      assert (source = None);
      let kind = NamedDeclaration in
      let export_info = DeclareExportDef declaration in
      match declaration with
      | Variable (_, { DeclareVariable.id; _ })
      | Function (_, { DeclareFunction.id; _ })
      | Class (_, { DeclareClass.id; _ }) ->
        let name, export = match default with
          | Some default_loc -> "default", ExportDefault { default_loc; local = Some id }
          | None -> snd id, ExportNamed { loc = fst id; kind }
        in
        this#add_exports stmt_loc ExportValue [export, name] [export_info, name] None
      | DefaultType _ ->
        let default_loc = match default with
        | Some loc -> loc
        | None -> failwith "declare export default must have a default loc"
        in
        let export = ExportDefault { default_loc; local = None } in
        this#add_exports stmt_loc ExportValue [export, "default"] [export_info, "default"] None
      | NamedType (_, { TypeAlias.id; _ })
      | NamedOpaqueType (_, { OpaqueType.id; _ })
      | Interface (_, { Interface.id; _ }) ->
        assert (Option.is_none default);
        let export = ExportNamed { loc = fst id; kind } in
        this#add_exports stmt_loc ExportType [export, snd id] [export_info, snd id] None
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      assert (Option.is_none default);
      (* declare export type unsupported *)
      let exportKind = Ast.Statement.ExportValue in
      this#export_specifiers stmt_loc exportKind source specifiers
    end;
    super#declare_export_declaration stmt_loc decl

  method! assignment (expr: (Loc.t, Loc.t) Ast.Expression.Assignment.t) =
    this#handle_assignment ~is_toplevel:false expr;
    expr

  method handle_assignment ~(is_toplevel: bool) (expr: (Loc.t, Loc.t) Ast.Expression.Assignment.t) =
    let open Ast.Expression in
    let open Ast.Expression.Assignment in
    let { operator; left; right } = expr in

    (* Handle exports *)
    begin match operator, left with
    (* module.exports = ... *)
    | Assign, (mod_exp_loc, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Identifier (_, "module");
        property = Member.PropertyIdentifier (_, "exports"); _
      })) when not (Scope_api.is_local_use scope_info module_loc) ->
      this#handle_cjs_default_export module_loc mod_exp_loc (SetModuleExportsDef right);
      ignore (this#expression right);
      if not is_toplevel then
        this#add_tolerable_error (BadExportPosition mod_exp_loc)
    (* exports.foo = ... *)
    | Assign, (_, Ast.Pattern.Expression (_, Member { Member.
        _object = mod_exp_loc as module_loc, Identifier (_, "exports");
        property = Member.PropertyIdentifier id; _
      }))
    (* module.exports.foo = ... *)
    | Assign, (_, Ast.Pattern.Expression (_, Member { Member.
        _object = mod_exp_loc, Member { Member.
          _object = module_loc, Identifier (_, "module");
          property = Member.PropertyIdentifier (_, "exports"); _
        };
        property = Member.PropertyIdentifier id; _
      })) when not (Scope_api.is_local_use scope_info module_loc) ->
      (* expressions not allowed in declare module body *)
      assert (curr_declare_module = None);
      this#add_cjs_export mod_exp_loc (AddModuleExportsDef (id, right));
      ignore (this#expression right);
      if not is_toplevel then
        this#add_tolerable_error (BadExportPosition mod_exp_loc)
    (* module = ... *)
    | Assign, (_, Ast.Pattern.Identifier {
        Ast.Pattern.Identifier.name=(loc, ("exports" | "module" as id)); _
      }) when not (Scope_api.is_local_use scope_info loc) ->
      ignore (this#expression right);
      this#add_tolerable_error (BadExportContext (id, loc))
    | _ ->
      ignore (super#assignment expr)
    end;

    (* Handle imports *)
    begin match operator with
    | Assign -> this#handle_require left right
    | _ -> ()
    end

  method private handle_cjs_default_export module_loc mod_exp_loc cjs_exports_def =
    (* expressions not allowed in declare module body *)
    assert (curr_declare_module = None);
    if not (Scope_api.is_local_use scope_info module_loc)
    then this#set_cjs_exports mod_exp_loc cjs_exports_def

  method! variable_declarator ~kind (decl: (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
    begin match decl with
    | _, { Ast.Statement.VariableDeclaration.Declarator.id; init = Some init } ->
      this#handle_require id init
    | _ -> ()
    end;
    super#variable_declarator ~kind decl

  method private handle_require (left: (Loc.t, Loc.t) Ast.Pattern.t) (right: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    let bindings = begin match left with
    | _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ } -> Some (BindIdent name)
    | _, Ast.Pattern.Object { Ast.Pattern.Object.properties; _ } ->
      let add_named remote local loc acc =
        let locals = SMap.singleton local (Nel.one loc) in
        let combine_nel_smap a b = SMap.union a b ~combine:combine_nel in
        SMap.add remote locals acc ~combine:combine_nel_smap
      in
      Some (BindNamed (List.fold_left (fun acc prop ->
        match prop with
        | Ast.Pattern.Object.Property (_, {
            Ast.Pattern.Object.Property.key = Ast.Pattern.Object.Property.Identifier (remote_loc, remote_name);
            pattern = _, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = (local_loc, local_name); _ };
            _
          }) ->
          add_named remote_name local_name { local_loc; remote_loc } acc
        | _ -> acc
      ) SMap.empty properties))
    | _ -> None
    end in
    begin match right with
    | call_loc, Call { Call.callee; targs = _; arguments } ->
      this#handle_call call_loc callee arguments bindings
    | _ -> ()
    end

  method private handle_call call_loc callee arguments bindings =
    let open Ast.Expression in
    if not (Utils_js.LocSet.mem call_loc visited_requires) then begin
      visited_requires <- Utils_js.LocSet.add call_loc visited_requires;
      match callee, arguments with
      | ((_, Identifier (loc, "require")), [Expression (source_loc, (
          Literal { Ast.Literal.value = Ast.Literal.String name; _ } |
          TemplateLiteral { TemplateLiteral.
            quasis = [_, { TemplateLiteral.Element.
              value = { TemplateLiteral.Element.cooked = name; _ }; _
            }]; _
          }
        ))]) ->
        if not (Scope_api.is_local_use scope_info loc)
        then
          this#add_require (Require {
            source = (source_loc, name);
            require_loc = call_loc;
            bindings;
          })
      | ((_, Identifier (loc, "requireLazy")),
         [Expression (_, Array ({ Array.elements })); Expression (_);])
        ->
        let element = function
          | Some (Expression (source_loc, Literal { Ast.Literal.value = Ast.Literal.String name; _ })) ->
            if not (Scope_api.is_local_use scope_info loc)
            then
              this#add_require (Require {
                source = (source_loc, name);
                require_loc = call_loc;
                bindings;
              })
          | _ -> () in
        List.iter element elements
      | _ -> ()
    end

  method! declare_module loc (m: (Loc.t, Loc.t) Ast.Statement.DeclareModule.t) =
    let name = Ast.Statement.DeclareModule.(match m.id with
    | Identifier (_, name) -> name
    | Literal (_, { Ast.StringLiteral.value; _ }) -> value
    ) in
    curr_declare_module <- Some (mk_module_sig init_exports_info);
    let ret = super#declare_module loc m in
    begin match curr_declare_module with
    | None -> failwith "lost curr_declare_module"
    | Some m ->
      this#update_acc (function
        | Error _ as acc -> acc
        | Ok fsig -> Ok (add_declare_module name m loc fsig)
      )
    end;
    curr_declare_module <- None;
    ret

  method private export_specifiers stmt_loc kind source =
    let open Ast.Statement.ExportNamedDeclaration in
    function
    | ExportBatchSpecifier (_, Some (loc, name)) ->
      (* export type * as X from "foo" unsupported *)
      assert (kind = Ast.Statement.ExportValue);
      let mref = match source with
      | Some mref -> mref
      | None -> failwith "export batch without source"
      in
      this#add_exports stmt_loc kind [ExportNs { loc; source = mref }, name] [] None
    | ExportBatchSpecifier (star_loc, None) ->
      let mref = match source with
      | Some mref -> mref
      | _ -> failwith "batch export missing source"
      in
      this#add_exports stmt_loc kind [] [] (Some (ExportStar { star_loc; source = mref }))
    | ExportSpecifiers specs ->
      let bindings = List.fold_left ExportSpecifier.(fun acc (_, spec) ->
        let name, loc = match spec.exported with
        | None -> snd spec.local, fst spec.local
        | Some remote -> snd remote, fst remote
        in
        let export = ExportNamed { loc; kind = NamedSpecifier { local = spec.local; source } } in
        (export, name) :: acc
      ) [] specs in
      this#add_exports stmt_loc kind bindings [] None

  method! toplevel_statement_list (stmts: (Loc.t, Loc.t) Ast.Statement.t list) =
    let open Ast in
    let id = Flow_ast_mapper.id in
    let map_expression (expr: (Loc.t, Loc.t) Expression.t) =
      let open Expression in
      match expr with
      | _, Assignment assg ->
          this#handle_assignment ~is_toplevel:true assg;
          expr
      | _ -> this#expression expr
    in
    let map_expression_statement (stmt: (Loc.t, Loc.t) Statement.Expression.t) =
      let open Statement.Expression in
      let {expression; _} = stmt in
      id map_expression expression stmt (fun expr -> { stmt with expression=expr })
    in
    let map_statement (stmt: (Loc.t, Loc.t) Statement.t) =
      let open Statement in
      match stmt with
      | loc, Expression expr ->
        id map_expression_statement expr stmt (fun expr -> loc, Expression expr)
      | _ -> this#statement stmt
    in
    ListUtils.ident_map map_statement stmts
end

type toplevel_names_and_exports_info = {
  toplevel_names: SSet.t;
  exports_info: (exports_info t', error) result
}

let program_with_toplevel_names_and_exports_info ~ast =
  let walk = new requires_exports_calculator ~ast in
  {
    toplevel_names = walk#toplevel_names;
    exports_info = walk#eval walk#program ast
  }

let map_unit_file_sig =
  let map_unit_module_sig module_sig =
    { module_sig with info = () }
  in fun file_sig ->
    let { module_sig; declare_modules; _ } = file_sig in
    let module_sig' = map_unit_module_sig module_sig in
    let declare_modules' = SMap.map (fun (loc, module_sig) ->
      let module_sig' = map_unit_module_sig module_sig in
      (loc, module_sig')
    ) declare_modules in
    { file_sig with
      module_sig = module_sig';
      declare_modules = declare_modules';
    }

let program ~ast =
  match program_with_toplevel_names_and_exports_info ~ast with
    | { exports_info = Ok file_sig; _ } -> Ok (map_unit_file_sig file_sig)
    | { exports_info = Error e; _ } -> Error e

let verified errors file_sig =
  let file_sig = map_unit_file_sig file_sig in
  { file_sig with
    tolerable_errors = Signature_builder_deps.ErrorSet.fold (fun error acc ->
      (SignatureVerificationError error):: acc
    ) errors file_sig.tolerable_errors
  }

class mapper = object(this)
  method file_sig (file_sig: t) =
    let { module_sig; declare_modules; tolerable_errors } = file_sig in
    let module_sig' = this#module_sig module_sig in
    let declare_modules' = SMapUtils.ident_map (fun (loc, module_sig) ->
      let loc = this#loc loc in
      let module_sig = this#module_sig module_sig in
      (loc, module_sig)
    ) declare_modules in
    let tolerable_errors' = ListUtils.ident_map this#tolerable_error tolerable_errors in
    if module_sig == module_sig' &&
      declare_modules == declare_modules' &&
      tolerable_errors == tolerable_errors'
    then file_sig
    else {
      module_sig = module_sig';
      declare_modules = declare_modules';
      tolerable_errors = tolerable_errors';
    }

  method module_sig (module_sig: module_sig) =
    let { requires; module_kind; type_exports_named; type_exports_star; info = () } = module_sig in
    let requires' = ListUtils.ident_map this#require requires in
    let module_kind' = this#module_kind module_kind in
    let type_exports_named' = SMapUtils.ident_map this#type_export type_exports_named in
    let type_exports_star' = ListUtils.ident_map this#export_star type_exports_star in
    if requires == requires' &&
      module_kind == module_kind' &&
      type_exports_named == type_exports_named' &&
      type_exports_star == type_exports_star'
    then module_sig
    else { module_sig with
      requires = requires';
      module_kind = module_kind';
      type_exports_named = type_exports_named';
      type_exports_star = type_exports_star';
    }

  method require (require: require) =
    match require with
    | Require { source; require_loc; bindings; } ->
      let source' = this#source source in
      let require_loc' = this#loc require_loc in
      let bindings' = OptionUtils.ident_map this#require_bindings bindings in
      if source == source' && require_loc == require_loc' && bindings == bindings'
      then require
      else Require { source = source'; require_loc = require_loc'; bindings = bindings'; }
    | ImportDynamic { source; import_loc } ->
      let source' = this#source source in
      let import_loc' = this#loc import_loc in
      if source == source' && import_loc == import_loc'
      then require
      else ImportDynamic { source = source'; import_loc = import_loc'; }
    | Import0 { source } ->
      let source' = this#source source in
      if source == source'
      then require
      else Import0 { source = source' }
    | Import { source; named; ns; types; typesof; typesof_ns; } ->
      let source' = this#source source in
      let named' = SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) named in
      let ns' = OptionUtils.ident_map this#ident ns in
      let types' = SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) types in
      let typesof' = SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) typesof in
      let typesof_ns' = OptionUtils.ident_map this#ident typesof_ns in
      if source == source' &&
        named == named' &&
        ns == ns' &&
        types == types' &&
        typesof == typesof' &&
        typesof_ns == typesof_ns'
      then require
      else Import {
        source = source';
        named = named';
        ns = ns';
        types = types';
        typesof = typesof';
        typesof_ns = typesof_ns';
      }

  method imported_locs (imported_locs: imported_locs) =
    let {remote_loc; local_loc} = imported_locs in
    let remote_loc' = this#loc remote_loc in
    let local_loc' = this#loc local_loc in
    if remote_loc == remote_loc' && local_loc == local_loc'
    then imported_locs
    else {remote_loc = remote_loc'; local_loc = local_loc'}

  method require_bindings (require_bindings: require_bindings) =
    match require_bindings with
    | BindIdent ident ->
      let ident' = this#ident ident in
      if ident == ident'
      then require_bindings
      else BindIdent ident'
    | BindNamed named ->
      let named' = SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) named in
      if named == named'
      then require_bindings
      else BindNamed named'

  method module_kind (module_kind: module_kind) =
    match module_kind with
    | CommonJS { mod_exp_loc } ->
      let mod_exp_loc' = OptionUtils.ident_map this#loc mod_exp_loc in
      if mod_exp_loc == mod_exp_loc'
      then module_kind
      else CommonJS { mod_exp_loc = mod_exp_loc' }
    | ES { named; star } ->
      let named' = SMapUtils.ident_map this#export named in
      let star' = ListUtils.ident_map this#export_star star in
      if named == named' && star == star'
      then module_kind
      else ES { named = named'; star = star' }

  method named_export_kind (kind: named_export_kind) =
    match kind with
      | NamedDeclaration -> kind
      | NamedSpecifier { local; source } ->
        let local' = this#ident local in
        let source' = OptionUtils.ident_map this#source source in
        if local == local' && source == source'
        then kind
        else NamedSpecifier { local = local'; source = source' }

  method export (export: export) =
    match export with
    | ExportDefault { default_loc; local } ->
      let default_loc' = this#loc default_loc in
      let local' = OptionUtils.ident_map this#ident local in
      if default_loc == default_loc' && local == local'
      then export
      else ExportDefault { default_loc = default_loc'; local = local' }
    | ExportNamed { loc; kind } ->
      let loc' = this#loc loc in
      let kind' = this#named_export_kind kind in
      if loc == loc' && kind == kind'
      then export
      else ExportNamed { loc = loc'; kind = kind' }
    | ExportNs { loc; source; } ->
      let loc' = this#loc loc in
      let source' = this#source source in
      if loc == loc' && source == source'
      then export
      else ExportNs { loc = loc'; source = source'; }

  method export_star (export_star: export_star) =
    match export_star with
    | ExportStar { star_loc; source } ->
      let star_loc' = this#loc star_loc in
      let source' = this#source source in
      if star_loc == star_loc' && source == source'
      then export_star
      else ExportStar { star_loc = star_loc'; source = source'; }

  method type_export (type_export: type_export) =
    match type_export with
    | TypeExportNamed { loc; kind } ->
      let loc' = this#loc loc in
      let kind' = this#named_export_kind kind in
      if loc == loc' && kind == kind'
      then type_export
      else TypeExportNamed { loc = loc'; kind = kind' }

  method ident (ident: Ast_utils.ident) =
    let (loc, str) = ident in
    let loc' = this#loc loc in
    if loc == loc'
    then ident
    else (loc', str)

  method source (source: Ast_utils.source) =
    let (loc, str) = source in
    let loc' = this#loc loc in
    if loc == loc'
    then source
    else (loc', str)

  method tolerable_error (tolerable_error: tolerable_error) =
    match tolerable_error with
    | BadExportPosition loc ->
      let loc' = this#loc loc in
      if loc == loc'
      then tolerable_error
      else BadExportPosition loc'
    | BadExportContext (str, loc) ->
      let loc' = this#loc loc in
      if loc == loc'
      then tolerable_error
      else BadExportContext (str, loc')
    | SignatureVerificationError sve ->
      let open Signature_builder_deps.Error in
      begin match sve with
        | ExpectedSort (sort, x, loc) ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (ExpectedSort (sort, x, loc'))
        | ExpectedAnnotation loc ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (ExpectedAnnotation loc')
        | InvalidTypeParamUse loc ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (InvalidTypeParamUse loc')
        | UnexpectedObjectKey loc ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (UnexpectedObjectKey loc')
        | UnexpectedExpression loc ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (UnexpectedExpression loc')
        | TODO (msg, loc) ->
          let loc' = this#loc loc in
          if loc == loc'
          then tolerable_error
          else SignatureVerificationError (TODO (msg, loc'))
      end

  method error (error: error) =
    match error with
    | IndeterminateModuleType loc ->
      let loc' = this#loc loc in
      if loc == loc'
      then error
      else IndeterminateModuleType loc'

  method loc (loc: Loc.t) = loc
end
