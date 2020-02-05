(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
module Result = Base.Result
open Flow_ast_visitor

module Make
    (L : Loc_sig.S)
    (Scope_api : Scope_api_sig.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L and module Api = Scope_api)
    (Signature_builder_deps : Signature_builder_deps_sig.S with module L = L) =
struct
  module L = L
  module Signature_builder_deps = Signature_builder_deps

  type 'info t' = {
    module_sig: 'info module_sig';
    declare_modules: (L.t * 'info module_sig') SMap.t;
    tolerable_errors: tolerable_error list;
    (* Some map in types-first, None in classic *)
    exported_locals: L.LSet.t SMap.t option;
  }

  and 'info module_sig' = {
    requires: require list;
    module_kind: module_kind;
    type_exports_named: (string * (L.t * type_export)) list;
    type_exports_star: (L.t * export_star) list;
    info: 'info;
  }

  and require =
    | Require of {
        source: L.t Ast_utils.source;
        require_loc: L.t;
        bindings: require_bindings option;
      }
    | ImportDynamic of {
        source: L.t Ast_utils.source;
        import_loc: L.t;
      }
    | Import0 of { source: L.t Ast_utils.source }
    | Import of {
        import_loc: L.t;
        source: L.t Ast_utils.source;
        named: imported_locs Nel.t SMap.t SMap.t;
        ns: L.t Ast_utils.ident option;
        types: imported_locs Nel.t SMap.t SMap.t;
        typesof: imported_locs Nel.t SMap.t SMap.t;
        typesof_ns: L.t Ast_utils.ident option;
      }

  and imported_locs = {
    remote_loc: L.t;
    local_loc: L.t;
  }

  and require_bindings =
    | BindIdent of L.t Ast_utils.ident
    | BindNamed of (L.t Ast_utils.ident * require_bindings) list

  and module_kind =
    | CommonJS of { mod_exp_loc: L.t option }
    | ES of {
        named: (string * (L.t * export)) list;
        star: (L.t * export_star) list;
      }

  and export =
    | ExportDefault of {
        default_loc: L.t;
        local: L.t Ast_utils.ident option;
      }
    | ExportNamed of {
        loc: L.t;
        kind: named_export_kind;
      }
    | ExportNs of {
        loc: L.t;
        star_loc: L.t;
        source: L.t Ast_utils.source;
      }

  and named_export_kind =
    | NamedDeclaration
    | NamedSpecifier of {
        local: L.t Ast_utils.ident;
        source: L.t Ast_utils.source option;
      }

  and export_star =
    | ExportStar of {
        star_loc: L.t;
        source: L.t Ast_utils.source;
      }

  and type_export =
    | TypeExportNamed of {
        loc: L.t;
        kind: named_export_kind;
      }

  and tolerable_error =
    (* e.g. `module.exports.foo = 4` when not at the top level *)
    | BadExportPosition of L.t
    (* e.g. `foo(module)`, dangerous because `module` is aliased *)
    | BadExportContext of string (* offending identifier *) * L.t
    | SignatureVerificationError of Signature_builder_deps.Error.t

  type exports_info = {
    module_kind_info: module_kind_info;
    type_exports_named_info: es_export_def list;
  }

  and module_kind_info =
    | CommonJSInfo of cjs_exports_def list
    | ESInfo of es_export_def list

  and cjs_exports_def =
    | DeclareModuleExportsDef of (L.t, L.t) Ast.Type.annotation
    | SetModuleExportsDef of (L.t, L.t) Ast.Expression.t
    | AddModuleExportsDef of L.t Ast_utils.ident * (L.t, L.t) Ast.Expression.t

  and es_export_def =
    | DeclareExportDef of (L.t, L.t) Ast.Statement.DeclareExportDeclaration.declaration
    | ExportDefaultDef of (L.t, L.t) Ast.Statement.ExportDefaultDeclaration.declaration
    | ExportNamedDef of (L.t, L.t) Ast.Statement.t

  type error = IndeterminateModuleType of L.t

  let mk_module_sig info =
    {
      requires = [];
      module_kind = CommonJS { mod_exp_loc = None };
      type_exports_named = [];
      type_exports_star = [];
      info;
    }

  let mk_file_sig info =
    {
      module_sig = mk_module_sig info;
      declare_modules = SMap.empty;
      tolerable_errors = [];
      exported_locals = None;
    }

  let init_exports_info = { module_kind_info = CommonJSInfo []; type_exports_named_info = [] }

  module PP = struct
    let string_of_option f = function
      | None -> "None"
      | Some x -> Printf.sprintf "Some (%s)" (f x)

    let items_to_collection_string indent open_ close items =
      let indent_str = String.make (indent * 2) ' ' in
      let items_str =
        items |> Base.List.map ~f:(Printf.sprintf "%s%s;\n" (indent_str ^ "  ")) |> String.concat ""
      in
      Printf.sprintf "%s\n%s%s%s" open_ items_str indent_str close

    let items_to_list_string indent items = items_to_collection_string indent "[" "]" items

    let items_to_record_string indent items =
      let items =
        items |> Base.List.map ~f:(fun (label, value) -> Printf.sprintf "%s: %s" label value)
      in
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
      | ESInfo named -> PP.items_to_list_string 2 @@ Base.List.map ~f:string_of_es_export_def named
    in
    PP.items_to_record_string
      1
      [
        ("module_kind_info", string_of_module_kind_info exports_info.module_kind_info);
        ( "type_exports_named_info",
          PP.items_to_list_string 2
          @@ Base.List.map ~f:string_of_es_export_def exports_info.type_exports_named_info );
      ]

  (* Applications may not care about the info carried by signatures. *)
  type module_sig = unit module_sig'

  type t = unit t'

  let init = mk_file_sig ()

  let to_string t =
    let string_of_module_sig module_sig =
      let string_of_require_list require_list =
        let string_of_require_bindings = function
          | BindIdent (_, name) -> Printf.sprintf "BindIdent: %s" name
          | BindNamed named ->
            Printf.sprintf
              "BindNamed: %s"
              (String.concat ", " @@ Base.List.map ~f:(fun ((_, name), _) -> name) named)
        in
        let string_of_require = function
          | Require { source = (_, name); bindings; _ } ->
            Printf.sprintf
              "Require (%s, %s)"
              name
              (PP.string_of_option string_of_require_bindings bindings)
          | ImportDynamic _ -> "ImportDynamic"
          | Import0 _ -> "Import0"
          | Import _ -> "Import"
        in
        PP.items_to_list_string 2 (Base.List.map ~f:string_of_require require_list)
      in
      let string_of_named_export_kind = function
        | NamedDeclaration -> "NamedDeclaration"
        | NamedSpecifier { local; _ } ->
          let (_, x) = local in
          Printf.sprintf "NamedSpecifier(%s)" x
      in
      let string_of_export (n, export) =
        ( n,
          match export with
          | (_, ExportDefault { local; _ }) ->
            Printf.sprintf "ExportDefault (%s)" @@ PP.string_of_option (fun (_, x) -> x) local
          | (_, ExportNamed { kind; _ }) ->
            Printf.sprintf "ExportNamed (%s)" @@ string_of_named_export_kind kind
          | (_, ExportNs _) -> "ExportNs" )
      in
      let string_of_type_export (n, type_export) =
        ( n,
          match type_export with
          | (_, TypeExportNamed { kind; _ }) ->
            Printf.sprintf "TypeExportNamed (%s)" @@ string_of_named_export_kind kind )
      in
      let string_of_export_star = function
        | (_, ExportStar _) -> "ExportStar"
      in
      let string_of_module_kind = function
        | CommonJS _ -> "CommonJS"
        | ES { named; star } ->
          PP.items_to_record_string
            2
            [
              ("named", PP.items_to_record_string 3 @@ Base.List.map ~f:string_of_export named);
              ("star", PP.items_to_list_string 3 @@ Base.List.map ~f:string_of_export_star star);
            ]
      in
      PP.items_to_record_string
        1
        [
          ("requires", string_of_require_list module_sig.requires);
          ("module_kind", string_of_module_kind module_sig.module_kind);
          ( "type_exports_named",
            PP.items_to_record_string 2
            @@ Base.List.map ~f:string_of_type_export module_sig.type_exports_named );
          ( "type_exports_star",
            PP.items_to_list_string 2
            @@ Base.List.map ~f:string_of_export_star module_sig.type_exports_star );
        ]
    in
    PP.items_to_record_string 0 [("module_sig", string_of_module_sig t.module_sig)]

  let combine_nel _ a b = Some (Nel.concat (a, [b]))

  let require_loc_map msig =
    let acc = SMap.empty in
    (* requires *)
    let acc =
      List.fold_left
        (fun acc require ->
          match require with
          | Require { source = (loc, mref); _ }
          | ImportDynamic { source = (loc, mref); _ }
          | Import0 { source = (loc, mref) }
          | Import { source = (loc, mref); _ } ->
            SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append)
        acc
        msig.requires
    in
    (* export type {...} from 'foo' *)
    let acc =
      List.fold_left
        (fun acc (_, type_export) ->
          match type_export with
          | (_, TypeExportNamed { kind = NamedSpecifier { source = Some (loc, mref); _ }; _ }) ->
            SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
          | _ -> acc)
        acc
        msig.type_exports_named
    in
    (* export type * from 'foo' *)
    let acc =
      List.fold_left
        (fun acc export_star ->
          match export_star with
          | (_, ExportStar { source = (source_loc, mref); _ }) ->
            SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append)
        acc
        msig.type_exports_star
    in
    let acc =
      match msig.module_kind with
      | CommonJS _ -> acc
      | ES { named; star } ->
        (* export {...} from 'foo' *)
        let acc =
          List.fold_left
            (fun acc (_, export) ->
              match export with
              | (_, ExportNamed { kind = NamedSpecifier { source = Some (loc, mref); _ }; _ })
              | (_, ExportNs { source = (loc, mref); _ }) ->
                SMap.add mref (Nel.one loc) acc ~combine:Nel.rev_append
              | _ -> acc)
            acc
            named
        in
        (* export * from 'foo' *)
        let acc =
          List.fold_left
            (fun acc export_star ->
              match export_star with
              | (_, ExportStar { source = (source_loc, mref); _ }) ->
                SMap.add mref (Nel.one source_loc) acc ~combine:Nel.rev_append)
            acc
            star
        in
        acc
    in
    acc

  let require_set msig =
    let map = require_loc_map msig in
    SMap.fold (fun key _ acc -> SSet.add key acc) map SSet.empty

  let add_declare_module name m loc fsig =
    { fsig with declare_modules = SMap.add name (loc, m) fsig.declare_modules }

  let add_require require msig =
    let requires = require :: msig.requires in
    Ok { msig with requires }

  let add_type_exports named named_info star msig =
    let named =
      Base.List.map
        ~f:(fun (name, export) ->
          let type_export =
            match export with
            | (export_loc, ExportNamed { loc; kind }) -> (export_loc, TypeExportNamed { loc; kind })
            | (_, ExportDefault _) -> failwith "export default type"
            | (_, ExportNs _) -> failwith "export type * as X"
          in
          (name, type_export))
        named
    in
    let type_exports_named = List.rev_append named msig.type_exports_named in
    let info = msig.info in
    let type_exports_named_info = List.rev_append named_info info.type_exports_named_info in
    let type_exports_star =
      Option.fold ~f:(fun acc export_star -> export_star :: acc) ~init:msig.type_exports_star star
    in
    Ok
      {
        msig with
        type_exports_named;
        type_exports_star;
        info = { info with type_exports_named_info };
      }

  let add_es_exports loc named named_info star msig =
    let info = msig.info in
    let result =
      match (msig.module_kind, info.module_kind_info) with
      | (CommonJS { mod_exp_loc = Some _ }, CommonJSInfo _) -> Error (IndeterminateModuleType loc)
      | (CommonJS { mod_exp_loc = None }, CommonJSInfo _) -> Ok ([], [], [])
      | (ES { named; star }, ESInfo named_info) -> Ok (named, named_info, star)
      | _ -> failwith "unreachable"
    in
    match result with
    | Error e -> Error e
    | Ok (named0, named_info0, star0) ->
      let named = List.rev_append named named0 in
      let named_info = List.rev_append named_info named_info0 in
      let star = Option.fold ~f:(fun acc export_star -> export_star :: acc) ~init:star0 star in
      let module_kind = ES { named; star } in
      let module_kind_info = ESInfo named_info in
      Ok { msig with module_kind; info = { info with module_kind_info } }

  let set_cjs_exports mod_exp_loc cjs_exports_def msig =
    let info = msig.info in
    match (msig.module_kind, info.module_kind_info) with
    | (CommonJS { mod_exp_loc = original_mod_exp_loc }, CommonJSInfo def) ->
      let mod_exp_loc = Option.first_some original_mod_exp_loc (Some mod_exp_loc) in
      let module_kind = CommonJS { mod_exp_loc } in
      let module_kind_info = CommonJSInfo (cjs_exports_def :: def) in
      Ok { msig with module_kind; info = { info with module_kind_info } }
    | (ES _, ESInfo _) -> Error (IndeterminateModuleType mod_exp_loc)
    | _ -> failwith "unreachable"

  (* Subclass of the AST visitor class that calculates requires and exports. Initializes with the
     scope builder class.
  *)
  class requires_exports_calculator ~ast ~module_ref_prefix =
    object (this)
      inherit
        [(exports_info t', error) result, L.t] visitor ~init:(Ok (mk_file_sig init_exports_info)) as super

      val scope_info = Scope_builder.program ast

      method toplevel_names = Scope_api.toplevel_names scope_info

      val mutable curr_declare_module : exports_info module_sig' option = None

      (* This ensures that we do not add a `require` with no bindings to `module_sig.requires` (when
       * processing a `call`) when we have already added that `require` with bindings (when processing
       * a `variable_declarator`). *)
      val mutable visited_requires_with_bindings : L.LSet.t = L.LSet.empty

      method private visited_requires_with_bindings loc bindings =
        bindings = None && L.LSet.mem loc visited_requires_with_bindings

      method private visit_requires_with_bindings loc bindings =
        if bindings <> None then
          visited_requires_with_bindings <- L.LSet.add loc visited_requires_with_bindings

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
                (match f fsig.module_sig with
                | Error e -> Error e
                | Ok module_sig -> Ok { fsig with module_sig }))

      method private add_require require = this#update_module_sig (add_require require)

      method private add_exports loc kind named named_info batch =
        let add =
          let open Ast.Statement in
          match kind with
          | ExportType -> add_type_exports
          | ExportValue -> add_es_exports loc
        in
        this#update_module_sig (add named named_info batch)

      method private set_cjs_exports mod_exp_loc cjs_exports_def =
        this#update_module_sig (set_cjs_exports mod_exp_loc cjs_exports_def)

      method private add_cjs_export mod_exp_loc cjs_exports_def =
        this#update_module_sig (set_cjs_exports mod_exp_loc cjs_exports_def)

      method private add_tolerable_error (err : tolerable_error) =
        this#update_acc
          (Result.map ~f:(fun fsig -> { fsig with tolerable_errors = err :: fsig.tolerable_errors }))

      method! expression (expr : (L.t, L.t) Ast.Expression.t) =
        let open Ast.Expression in
        begin
          match expr with
          (* Disallow expressions consisting of `module` or `exports`. These are dangerous because they
           * can allow aliasing and mutation. *)
          | ( _,
              Identifier
                (loc, { Ast.Identifier.name = ("module" | "exports") as name; comments = _ }) )
            when not (Scope_api.is_local_use scope_info loc) ->
            this#add_tolerable_error (BadExportContext (name, loc))
          | _ -> ()
        end;
        super#expression expr

      method! binary loc (expr : (L.t, L.t) Ast.Expression.Binary.t) =
        let open Ast.Expression in
        let open Ast.Expression.Binary in
        let is_module_or_exports = function
          | (_, Identifier (_, { Ast.Identifier.name = "module" | "exports"; comments = _ })) ->
            true
          | _ -> false
        in
        let is_legal_operator = function
          | StrictEqual
          | StrictNotEqual ->
            true
          | _ -> false
        in
        let identify_or_recurse subexpr =
          if not (is_module_or_exports subexpr) then ignore (this#expression subexpr)
        in
        let { operator; left; right } = expr in
        (* Whitelist e.g. `require.main === module` by avoiding the recursive calls (where the errors
         * are generated) if the AST matches specific patterns. *)
        if is_legal_operator operator then (
          identify_or_recurse left;
          identify_or_recurse right;
          expr
        ) else
          super#binary loc expr

      method! member loc (expr : (L.t, L.t) Ast.Expression.Member.t) =
        let open Ast.Expression in
        let open Ast.Expression.Member in
        let { _object; property } = expr in
        (* Strip the loc to simplify the patterns *)
        let (_, _object) = _object in
        (* This gets called when patterns like `module.id` appear on the LHS of an
         * assignment, in addition to when they appear in ordinary expression
         * locations. Therefore we have to prevent anything that would be dangerous
         * if it appeared on the LHS side of an assignment. Ordinary export
         * statements are handled by handle_assignment, which stops recursion so we
         * don't arrive here in those cases. *)
        begin
          match (_object, property) with
          (* Allow `module.anythingButExports` *)
          | ( Identifier (_, { Ast.Identifier.name = "module"; comments = _ }),
              PropertyIdentifier (_, { Ast.Identifier.name = prop; comments = _ }) )
            when prop <> "exports" ->
            ()
          (* Allow `module.exports.whatever` -- this is safe because handle_assignment has already
           * looked for assignments to it before recursing down here. *)
          | ( Member
                {
                  _object = (_, Identifier (_, { Ast.Identifier.name = "module"; comments = _ }));
                  property =
                    PropertyIdentifier (_, { Ast.Identifier.name = "exports"; comments = _ });
                  _;
                },
              PropertyIdentifier _ )
          (* Allow `exports.whatever`, for the same reason as above *)
          | (Identifier (_, { Ast.Identifier.name = "exports"; comments = _ }), PropertyIdentifier _)
            ->
            (* In these cases we don't know much about the property so we should recurse *)
            ignore (this#member_property property)
          | _ -> ignore (super#member loc expr)
        end;
        expr

      method! call call_loc (expr : (L.t, L.t) Ast.Expression.Call.t) =
        let open Ast.Expression in
        let { Call.callee; targs = _; arguments } = expr in
        this#handle_call call_loc callee arguments None;
        super#call call_loc expr

      method! literal loc (expr : L.t Ast.Literal.t) =
        let open Ast.Literal in
        this#handle_literal loc expr.value;
        super#literal loc expr

      method! import import_loc (expr : (L.t, L.t) Ast.Expression.t) =
        let open Ast.Expression in
        begin
          match expr with
          | ( loc,
              ( Literal { Ast.Literal.value = Ast.Literal.String name; _ }
              | TemplateLiteral
                  {
                    TemplateLiteral.quasis =
                      [
                        ( _,
                          {
                            TemplateLiteral.Element.value =
                              { TemplateLiteral.Element.cooked = name; _ };
                            _;
                          } );
                      ];
                    _;
                  } ) ) ->
            this#add_require (ImportDynamic { source = (loc, name); import_loc })
          | _ -> ()
        end;
        super#expression expr

      method! import_declaration import_loc (decl : (L.t, L.t) Ast.Statement.ImportDeclaration.t) =
        let open Ast.Statement.ImportDeclaration in
        let { importKind; source; specifiers; default } = decl in
        let source =
          match source with
          | (loc, { Ast.StringLiteral.value = name; _ }) -> (loc, name)
        in
        let import =
          match (default, specifiers) with
          | (None, None) -> Import0 { source }
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
              if !ref = None then
                ref := Some (loc, local)
              else
                failwith "unreachable"
            in
            Option.iter
              ~f:(fun (loc, { Ast.Identifier.name = local; comments = _ }) ->
                add_named
                  "default"
                  local
                  { remote_loc = loc; local_loc = loc }
                  (ref_of_kind importKind))
              default;
            Option.iter
              ~f:(function
                | ImportNamespaceSpecifier (loc, (_, { Ast.Identifier.name = local; comments = _ }))
                  ->
                  (match importKind with
                  | ImportType -> failwith "import type * is a parse error"
                  | ImportTypeof -> set_ns local loc typesof_ns
                  | ImportValue -> set_ns local loc ns)
                | ImportNamedSpecifiers named_specifiers ->
                  List.iter
                    (function
                      | { local; remote; kind } ->
                        let importKind =
                          match kind with
                          | Some k -> k
                          | None -> importKind
                        in
                        let (local_loc, { Ast.Identifier.name = local_name; comments = _ }) =
                          match local with
                          | Some x -> x
                          | None -> remote
                        in
                        let (remote_loc, { Ast.Identifier.name = remote_name; comments = _ }) =
                          remote
                        in
                        add_named
                          remote_name
                          local_name
                          { remote_loc; local_loc }
                          (ref_of_kind importKind))
                    named_specifiers)
              specifiers;
            Import
              {
                import_loc;
                source;
                named = !named;
                ns = !ns;
                types = !types;
                typesof = !typesof;
                typesof_ns = !typesof_ns;
              }
        in
        this#add_require import;
        super#import_declaration import_loc decl

      method! export_default_declaration
          stmt_loc (decl : (L.t, L.t) Ast.Statement.ExportDefaultDeclaration.t) =
        let open Ast.Statement.ExportDefaultDeclaration in
        let { default = default_loc; declaration } = decl in
        let local =
          match declaration with
          | Declaration (_, Ast.Statement.FunctionDeclaration { Ast.Function.id; _ }) -> id
          | Declaration (_, Ast.Statement.ClassDeclaration { Ast.Class.id; _ }) -> id
          | Declaration (_, Ast.Statement.EnumDeclaration { Ast.Statement.EnumDeclaration.id; _ })
            ->
            Some id
          | Expression (_, Ast.Expression.Function { Ast.Function.id; _ }) -> id
          | _ -> None
        in
        let local = Option.map ~f:Flow_ast_utils.source_of_ident local in
        let export = (stmt_loc, ExportDefault { default_loc; local }) in
        let export_info = ExportDefaultDef declaration in
        this#add_exports stmt_loc Ast.Statement.ExportValue [("default", export)] [export_info] None;
        super#export_default_declaration stmt_loc decl

      method! export_named_declaration
          stmt_loc (decl : (L.t, L.t) Ast.Statement.ExportNamedDeclaration.t) =
        let open Ast.Statement.ExportNamedDeclaration in
        let { exportKind; source; specifiers; declaration } = decl in
        let source =
          match source with
          | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) -> Some (loc, mref)
          | None -> None
        in
        begin
          match declaration with
          | None -> () (* assert specifiers <> None *)
          | Some (loc, stmt) ->
            let open Ast.Statement in
            assert (source = None);
            let kind = NamedDeclaration in
            let export_info = ExportNamedDef (loc, stmt) in
            (match stmt with
            | FunctionDeclaration
                { Ast.Function.id = Some (loc, { Ast.Identifier.name; comments = _ }); _ }
            | ClassDeclaration
                { Ast.Class.id = Some (loc, { Ast.Identifier.name; comments = _ }); _ }
            | EnumDeclaration
                { Ast.Statement.EnumDeclaration.id = (loc, { Ast.Identifier.name; _ }); _ } ->
              let export = (stmt_loc, ExportNamed { loc; kind }) in
              this#add_exports stmt_loc ExportValue [(name, export)] [export_info] None
            | VariableDeclaration { VariableDeclaration.declarations = decls; _ } ->
              let (rev_named, rev_info) =
                Ast_utils.fold_bindings_of_variable_declarations
                  (fun (named, infos) (loc, { Ast.Identifier.name; comments = _ }) ->
                    let export = (stmt_loc, ExportNamed { loc; kind }) in
                    ((name, export) :: named, export_info :: infos))
                  ([], [])
                  decls
              in
              this#add_exports stmt_loc ExportValue (List.rev rev_named) (List.rev rev_info) None
            | TypeAlias { TypeAlias.id; _ }
            | OpaqueType { OpaqueType.id; _ }
            | InterfaceDeclaration { Interface.id; _ } ->
              let export = (stmt_loc, ExportNamed { loc; kind }) in
              this#add_exports
                stmt_loc
                ExportType
                [(Flow_ast_utils.name_of_ident id, export)]
                [export_info]
                None
            | _ -> failwith "unsupported declaration")
        end;
        begin
          match specifiers with
          | None -> () (* assert declaration <> None *)
          | Some specifiers -> this#export_specifiers stmt_loc exportKind source specifiers
        end;
        super#export_named_declaration stmt_loc decl

      method! declare_module_exports loc (annot : (L.t, L.t) Ast.Type.annotation) =
        this#set_cjs_exports loc (DeclareModuleExportsDef annot);
        super#declare_module_exports loc annot

      method! declare_export_declaration
          stmt_loc (decl : (L.t, L.t) Ast.Statement.DeclareExportDeclaration.t) =
        let open Ast.Statement.DeclareExportDeclaration in
        let { default; source; specifiers; declaration } = decl in
        let source =
          match source with
          | Some (loc, { Ast.StringLiteral.value = mref; raw = _ }) ->
            assert (Option.is_none default);

            (* declare export default from not supported *)
            Some (loc, mref)
          | _ -> None
        in
        begin
          match declaration with
          | None -> () (* assert specifiers <> None *)
          | Some declaration ->
            let open Ast.Statement in
            assert (source = None);
            let kind = NamedDeclaration in
            let export_info = DeclareExportDef declaration in
            (match declaration with
            | Variable (_, { DeclareVariable.id; _ })
            | Function (_, { DeclareFunction.id; _ })
            | Class (_, { DeclareClass.id; _ }) ->
              let (name, export) =
                match default with
                | Some default_loc ->
                  ( "default",
                    ( stmt_loc,
                      ExportDefault
                        { default_loc; local = Some (Flow_ast_utils.source_of_ident id) } ) )
                | None ->
                  (Flow_ast_utils.name_of_ident id, (stmt_loc, ExportNamed { loc = fst id; kind }))
              in
              this#add_exports stmt_loc ExportValue [(name, export)] [export_info] None
            | DefaultType _ ->
              let default_loc =
                match default with
                | Some loc -> loc
                | None -> failwith "declare export default must have a default loc"
              in
              let export = (stmt_loc, ExportDefault { default_loc; local = None }) in
              this#add_exports stmt_loc ExportValue [("default", export)] [export_info] None
            | NamedType (_, { TypeAlias.id; _ })
            | NamedOpaqueType (_, { OpaqueType.id; _ })
            | Interface (_, { Interface.id; _ }) ->
              assert (Option.is_none default);
              let export = (stmt_loc, ExportNamed { loc = fst id; kind }) in
              this#add_exports
                stmt_loc
                ExportType
                [(Flow_ast_utils.name_of_ident id, export)]
                [export_info]
                None)
        end;
        begin
          match specifiers with
          | None -> () (* assert declaration <> None *)
          | Some specifiers ->
            assert (Option.is_none default);

            (* declare export type unsupported *)
            let exportKind = Ast.Statement.ExportValue in
            this#export_specifiers stmt_loc exportKind source specifiers
        end;
        super#declare_export_declaration stmt_loc decl

      method! assignment loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
        this#handle_assignment ~is_toplevel:false loc expr;
        expr

      method handle_assignment
          ~(is_toplevel : bool) loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression in
        let open Ast.Expression.Assignment in
        let { operator; left; right } = expr in
        (* Handle exports *)
        match (operator, left) with
        (* module.exports = ... *)
        | ( None,
            ( mod_exp_loc,
              Ast.Pattern.Expression
                ( _,
                  Member
                    {
                      Member._object =
                        ( module_loc,
                          Identifier (_, { Ast.Identifier.name = "module"; comments = _ }) );
                      property =
                        Member.PropertyIdentifier
                          (_, { Ast.Identifier.name = "exports"; comments = _ });
                      _;
                    } ) ) )
          when not (Scope_api.is_local_use scope_info module_loc) ->
          this#handle_cjs_default_export module_loc mod_exp_loc (SetModuleExportsDef right);
          ignore (this#expression right);
          if not is_toplevel then this#add_tolerable_error (BadExportPosition mod_exp_loc)
        (* exports.foo = ... *)
        | ( None,
            ( _,
              Ast.Pattern.Expression
                ( _,
                  Member
                    {
                      Member._object =
                        ( (mod_exp_loc as module_loc),
                          Identifier (_, { Ast.Identifier.name = "exports"; comments = _ }) );
                      property = Member.PropertyIdentifier id;
                      _;
                    } ) ) )
        (* module.exports.foo = ... *)
        | ( None,
            ( _,
              Ast.Pattern.Expression
                ( _,
                  Member
                    {
                      Member._object =
                        ( mod_exp_loc,
                          Member
                            {
                              Member._object =
                                ( module_loc,
                                  Identifier (_, { Ast.Identifier.name = "module"; comments = _ })
                                );
                              property =
                                Member.PropertyIdentifier
                                  (_, { Ast.Identifier.name = "exports"; comments = _ });
                              _;
                            } );
                      property = Member.PropertyIdentifier id;
                      _;
                    } ) ) )
          when not (Scope_api.is_local_use scope_info module_loc) ->
          (* expressions not allowed in declare module body *)
          assert (curr_declare_module = None);
          this#add_cjs_export
            mod_exp_loc
            (AddModuleExportsDef (Flow_ast_utils.source_of_ident id, right));
          ignore (this#expression right);
          if not is_toplevel then this#add_tolerable_error (BadExportPosition mod_exp_loc)
        (* module = ... *)
        | ( None,
            ( _,
              Ast.Pattern.Identifier
                {
                  Ast.Pattern.Identifier.name =
                    (loc, { Ast.Identifier.name = ("exports" | "module") as id; comments = _ });
                  _;
                } ) )
          when not (Scope_api.is_local_use scope_info loc) ->
          ignore (this#expression right);
          this#add_tolerable_error (BadExportContext (id, loc))
        | _ -> ignore (super#assignment loc expr)

      method private handle_cjs_default_export module_loc mod_exp_loc cjs_exports_def =
        (* expressions not allowed in declare module body *)
        assert (curr_declare_module = None);
        if not (Scope_api.is_local_use scope_info module_loc) then
          this#set_cjs_exports mod_exp_loc cjs_exports_def

      method! variable_declarator
          ~kind (decl : (L.t, L.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        begin
          match decl with
          | (_, { Ast.Statement.VariableDeclaration.Declarator.id; init = Some init }) ->
            this#handle_require id init
          | _ -> ()
        end;
        super#variable_declarator ~kind decl

      method private require_pattern (pattern : (L.t, L.t) Ast.Pattern.t) =
        match pattern with
        | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
          Some (BindIdent (Flow_ast_utils.source_of_ident name))
        | (_, Ast.Pattern.Object { Ast.Pattern.Object.properties; _ }) ->
          let named_opt =
            ListUtils.fold_left_opt
              (fun named prop ->
                match prop with
                | Ast.Pattern.Object.Property
                    ( _,
                      {
                        Ast.Pattern.Object.Property.key =
                          Ast.Pattern.Object.Property.Identifier remote;
                        pattern;
                        _;
                      } ) ->
                  let bindings = this#require_pattern pattern in
                  Option.map bindings (fun bindings -> (remote, bindings) :: named)
                | _ -> None)
              []
              properties
          in
          Option.map named_opt (fun named ->
              let named_bind =
                List.map (fun (id, bind) -> (Flow_ast_utils.source_of_ident id, bind)) named
              in
              BindNamed named_bind)
        | _ -> None

      method private handle_require
          (left : (L.t, L.t) Ast.Pattern.t) (right : (L.t, L.t) Ast.Expression.t) =
        let open Ast.Expression in
        let bindings = this#require_pattern left in
        match right with
        | (call_loc, Call { Call.callee; targs = _; arguments }) ->
          this#handle_call call_loc callee arguments bindings
        | _ -> ()

      method private handle_call call_loc callee arguments bindings =
        let open Ast.Expression in
        if not (this#visited_requires_with_bindings call_loc bindings) then (
          this#visit_requires_with_bindings call_loc bindings;
          match (callee, arguments) with
          | ( (_, Identifier (loc, { Ast.Identifier.name = "require"; comments = _ })),
              ( _,
                [
                  Expression
                    ( source_loc,
                      ( Literal { Ast.Literal.value = Ast.Literal.String name; _ }
                      | TemplateLiteral
                          {
                            TemplateLiteral.quasis =
                              [
                                ( _,
                                  {
                                    TemplateLiteral.Element.value =
                                      { TemplateLiteral.Element.cooked = name; _ };
                                    _;
                                  } );
                              ];
                            _;
                          } ) );
                ] ) ) ->
            if not (Scope_api.is_local_use scope_info loc) then
              this#add_require
                (Require { source = (source_loc, name); require_loc = call_loc; bindings })
          | ( (_, Identifier (loc, { Ast.Identifier.name = "requireLazy"; comments = _ })),
              (_, [Expression (_, Array { Array.elements; comments = _ }); Expression _]) ) ->
            let element = function
              | Some
                  (Expression
                    (source_loc, Literal { Ast.Literal.value = Ast.Literal.String name; _ })) ->
                if not (Scope_api.is_local_use scope_info loc) then
                  this#add_require
                    (Require { source = (source_loc, name); require_loc = call_loc; bindings })
              | _ -> ()
            in
            List.iter element elements
          | _ -> ()
        )

      method private handle_literal loc lit =
        let open Ast.Literal in
        match module_ref_prefix with
        | Some prefix ->
          begin
            match lit with
            | String s when String_utils.string_starts_with s prefix ->
              this#add_require
                (Require
                   {
                     source = (loc, String_utils.lstrip s prefix);
                     require_loc = loc;
                     bindings = None;
                   })
            | _ -> ()
          end
        | None -> ()

      method! declare_module loc (m : (L.t, L.t) Ast.Statement.DeclareModule.t) =
        let name =
          let open Ast.Statement.DeclareModule in
          match m.id with
          | Identifier (_, { Ast.Identifier.name; comments = _ }) -> name
          | Literal (_, { Ast.StringLiteral.value; _ }) -> value
        in
        curr_declare_module <- Some (mk_module_sig init_exports_info);
        let ret = super#declare_module loc m in
        begin
          match curr_declare_module with
          | None -> failwith "lost curr_declare_module"
          | Some m ->
            this#update_acc (function
                | Error _ as acc -> acc
                | Ok fsig -> Ok (add_declare_module name m loc fsig))
        end;
        curr_declare_module <- None;
        ret

      method private export_specifiers stmt_loc kind source =
        let open Ast.Statement.ExportNamedDeclaration in
        function
        | ExportBatchSpecifier (star_loc, Some (loc, { Ast.Identifier.name; comments = _ })) ->
          (* export type * as X from "foo" unsupported *)
          assert (kind = Ast.Statement.ExportValue);
          let mref =
            match source with
            | Some mref -> mref
            | None -> failwith "export batch without source"
          in
          let export = (stmt_loc, ExportNs { loc; star_loc; source = mref }) in
          this#add_exports stmt_loc kind [(name, export)] [] None
        | ExportBatchSpecifier (star_loc, None) ->
          let mref =
            match source with
            | Some mref -> mref
            | _ -> failwith "batch export missing source"
          in
          let export = (stmt_loc, ExportStar { star_loc; source = mref }) in
          this#add_exports stmt_loc kind [] [] (Some export)
        | ExportSpecifiers specs ->
          let bindings =
            List.fold_left
              ExportSpecifier.(
                fun acc (_, spec) ->
                  let ({ Ast.Identifier.name; comments = _ }, loc) =
                    match spec.exported with
                    | None -> (snd spec.local, fst spec.local)
                    | Some remote -> (snd remote, fst remote)
                  in
                  let export =
                    ( stmt_loc,
                      ExportNamed
                        {
                          loc;
                          kind =
                            NamedSpecifier
                              { local = Flow_ast_utils.source_of_ident spec.local; source };
                        } )
                  in
                  (name, export) :: acc)
              []
              specs
          in
          this#add_exports stmt_loc kind bindings [] None

      method! toplevel_statement_list (stmts : (L.t, L.t) Ast.Statement.t list) =
        let open Ast in
        let id = Flow_ast_mapper.id in
        let map_expression (expr : (L.t, L.t) Expression.t) =
          let open Expression in
          match expr with
          | (loc, Assignment assg) ->
            this#handle_assignment ~is_toplevel:true loc assg;
            expr
          | _ -> this#expression expr
        in
        let map_expression_statement (stmt : (L.t, L.t) Statement.Expression.t) =
          Statement.Expression.(
            let { expression; _ } = stmt in
            id map_expression expression stmt (fun expr -> { stmt with expression = expr }))
        in
        let map_statement (stmt : (L.t, L.t) Statement.t) =
          let open Statement in
          match stmt with
          | (loc, Expression expr) ->
            id map_expression_statement expr stmt (fun expr -> (loc, Expression expr))
          | _ -> this#statement stmt
        in
        ListUtils.ident_map map_statement stmts
    end

  type toplevel_names_and_exports_info = {
    toplevel_names: SSet.t;
    exports_info: (exports_info t', error) result;
  }

  let program_with_toplevel_names_and_exports_info ~ast ~module_ref_prefix =
    let walk = new requires_exports_calculator ~ast ~module_ref_prefix in
    { toplevel_names = walk#toplevel_names; exports_info = walk#eval walk#program ast }

  let map_unit_file_sig =
    let map_unit_module_sig module_sig = { module_sig with info = () } in
    fun file_sig ->
      let { module_sig; declare_modules; _ } = file_sig in
      let module_sig' = map_unit_module_sig module_sig in
      let declare_modules' =
        SMap.map
          (fun (loc, module_sig) ->
            let module_sig' = map_unit_module_sig module_sig in
            (loc, module_sig'))
          declare_modules
      in
      { file_sig with module_sig = module_sig'; declare_modules = declare_modules' }

  let program ~ast ~module_ref_prefix =
    match program_with_toplevel_names_and_exports_info ~ast ~module_ref_prefix with
    | { exports_info = Ok file_sig; _ } -> Ok (map_unit_file_sig file_sig)
    | { exports_info = Error e; _ } -> Error e

  let verified errors env file_sig =
    let file_sig = map_unit_file_sig file_sig in
    {
      file_sig with
      tolerable_errors =
        Signature_builder_deps.PrintableErrorSet.fold
          (fun error acc -> SignatureVerificationError error :: acc)
          errors
          file_sig.tolerable_errors;
      exported_locals = env;
    }

  class mapper =
    object (this)
      method file_sig (file_sig : t) =
        let { module_sig; declare_modules; tolerable_errors; exported_locals } = file_sig in
        let module_sig' = this#module_sig module_sig in
        let declare_modules' =
          SMapUtils.ident_map
            (fun (loc, module_sig) ->
              let loc = this#loc loc in
              let module_sig = this#module_sig module_sig in
              (loc, module_sig))
            declare_modules
        in
        let tolerable_errors' = ListUtils.ident_map this#tolerable_error tolerable_errors in
        let exported_locals' =
          OptionUtils.ident_map
            (SMapUtils.ident_map (L.LSetUtils.ident_map this#loc))
            exported_locals
        in
        if
          module_sig == module_sig'
          && declare_modules == declare_modules'
          && tolerable_errors == tolerable_errors'
        then
          file_sig
        else
          {
            module_sig = module_sig';
            declare_modules = declare_modules';
            tolerable_errors = tolerable_errors';
            exported_locals = exported_locals';
          }

      method module_sig (module_sig : module_sig) =
        let { requires; module_kind; type_exports_named; type_exports_star; info = () } =
          module_sig
        in
        let requires' = ListUtils.ident_map this#require requires in
        let module_kind' = this#module_kind module_kind in
        let type_exports_named' = ListUtils.ident_map this#type_export type_exports_named in
        let type_exports_star' = ListUtils.ident_map this#export_star type_exports_star in
        if
          requires == requires'
          && module_kind == module_kind'
          && type_exports_named == type_exports_named'
          && type_exports_star == type_exports_star'
        then
          module_sig
        else
          {
            module_sig with
            requires = requires';
            module_kind = module_kind';
            type_exports_named = type_exports_named';
            type_exports_star = type_exports_star';
          }

      method require (require : require) =
        match require with
        | Require { source; require_loc; bindings } ->
          let source' = this#source source in
          let require_loc' = this#loc require_loc in
          let bindings' = OptionUtils.ident_map this#require_bindings bindings in
          if source == source' && require_loc == require_loc' && bindings == bindings' then
            require
          else
            Require { source = source'; require_loc = require_loc'; bindings = bindings' }
        | ImportDynamic { source; import_loc } ->
          let source' = this#source source in
          let import_loc' = this#loc import_loc in
          if source == source' && import_loc == import_loc' then
            require
          else
            ImportDynamic { source = source'; import_loc = import_loc' }
        | Import0 { source } ->
          let source' = this#source source in
          if source == source' then
            require
          else
            Import0 { source = source' }
        | Import { import_loc; source; named; ns; types; typesof; typesof_ns } ->
          let import_loc' = this#loc import_loc in
          let source' = this#source source in
          let named' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) named
          in
          let ns' = OptionUtils.ident_map this#ident ns in
          let types' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) types
          in
          let typesof' =
            SMapUtils.ident_map (SMapUtils.ident_map (Nel.ident_map this#imported_locs)) typesof
          in
          let typesof_ns' = OptionUtils.ident_map this#ident typesof_ns in
          if
            import_loc == import_loc'
            && source == source'
            && named == named'
            && ns == ns'
            && types == types'
            && typesof == typesof'
            && typesof_ns == typesof_ns'
          then
            require
          else
            Import
              {
                import_loc = import_loc';
                source = source';
                named = named';
                ns = ns';
                types = types';
                typesof = typesof';
                typesof_ns = typesof_ns';
              }

      method imported_locs (imported_locs : imported_locs) =
        let { remote_loc; local_loc } = imported_locs in
        let remote_loc' = this#loc remote_loc in
        let local_loc' = this#loc local_loc in
        if remote_loc == remote_loc' && local_loc == local_loc' then
          imported_locs
        else
          { remote_loc = remote_loc'; local_loc = local_loc' }

      method require_bindings (require_bindings : require_bindings) =
        match require_bindings with
        | BindIdent ident ->
          let ident' = this#ident ident in
          if ident == ident' then
            require_bindings
          else
            BindIdent ident'
        | BindNamed named ->
          let named' =
            ListUtils.ident_map
              (fun ((remote, require_bindings) as x) ->
                let remote' = this#ident remote in
                let require_bindings' = this#require_bindings require_bindings in
                if remote == remote' && require_bindings == require_bindings' then
                  x
                else
                  (remote', require_bindings'))
              named
          in
          if named == named' then
            require_bindings
          else
            BindNamed named'

      method module_kind (module_kind : module_kind) =
        match module_kind with
        | CommonJS { mod_exp_loc } ->
          let mod_exp_loc' = OptionUtils.ident_map this#loc mod_exp_loc in
          if mod_exp_loc == mod_exp_loc' then
            module_kind
          else
            CommonJS { mod_exp_loc = mod_exp_loc' }
        | ES { named; star } ->
          let named' = ListUtils.ident_map this#export named in
          let star' = ListUtils.ident_map this#export_star star in
          if named == named' && star == star' then
            module_kind
          else
            ES { named = named'; star = star' }

      method named_export_kind (kind : named_export_kind) =
        match kind with
        | NamedDeclaration -> kind
        | NamedSpecifier { local; source } ->
          let local' = this#ident local in
          let source' = OptionUtils.ident_map this#source source in
          if local == local' && source == source' then
            kind
          else
            NamedSpecifier { local = local'; source = source' }

      method export (export : string * (L.t * export)) =
        match export with
        | (n, (export_loc, ExportDefault { default_loc; local })) ->
          let export_loc' = this#loc export_loc in
          let default_loc' = this#loc default_loc in
          let local' = OptionUtils.ident_map this#ident local in
          if export_loc == export_loc' && default_loc == default_loc' && local == local' then
            export
          else
            (n, (export_loc', ExportDefault { default_loc = default_loc'; local = local' }))
        | (n, (export_loc, ExportNamed { loc; kind })) ->
          let export_loc' = this#loc export_loc in
          let loc' = this#loc loc in
          let kind' = this#named_export_kind kind in
          if export_loc == export_loc' && loc == loc' && kind == kind' then
            export
          else
            (n, (export_loc', ExportNamed { loc = loc'; kind = kind' }))
        | (n, (export_loc, ExportNs { loc; star_loc; source })) ->
          let export_loc' = this#loc export_loc in
          let loc' = this#loc loc in
          let star_loc' = this#loc star_loc in
          let source' = this#source source in
          if export_loc == export_loc' && loc == loc' && star_loc == star_loc' && source == source'
          then
            export
          else
            (n, (export_loc', ExportNs { loc = loc'; star_loc = star_loc'; source = source' }))

      method export_star (export_star : L.t * export_star) =
        match export_star with
        | (export_loc, ExportStar { star_loc; source }) ->
          let export_loc' = this#loc export_loc in
          let star_loc' = this#loc star_loc in
          let source' = this#source source in
          if export_loc == export_loc' && star_loc == star_loc' && source == source' then
            export_star
          else
            (export_loc', ExportStar { star_loc = star_loc'; source = source' })

      method type_export (type_export : string * (L.t * type_export)) =
        match type_export with
        | (n, (export_loc, TypeExportNamed { loc; kind })) ->
          let export_loc' = this#loc export_loc in
          let loc' = this#loc loc in
          let kind' = this#named_export_kind kind in
          if export_loc == export_loc' && loc == loc' && kind == kind' then
            type_export
          else
            (n, (export_loc', TypeExportNamed { loc = loc'; kind = kind' }))

      method ident (ident : L.t Ast_utils.ident) =
        let (loc, str) = ident in
        let loc' = this#loc loc in
        if loc == loc' then
          ident
        else
          (loc', str)

      method source (source : L.t Ast_utils.source) =
        let (loc, str) = source in
        let loc' = this#loc loc in
        if loc == loc' then
          source
        else
          (loc', str)

      method tolerable_error (tolerable_error : tolerable_error) =
        match tolerable_error with
        | BadExportPosition loc ->
          let loc' = this#loc loc in
          if loc == loc' then
            tolerable_error
          else
            BadExportPosition loc'
        | BadExportContext (str, loc) ->
          let loc' = this#loc loc in
          if loc == loc' then
            tolerable_error
          else
            BadExportContext (str, loc')
        | SignatureVerificationError sve ->
          Signature_error.(
            begin
              match sve with
              | ExpectedSort (sort, x, loc) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (ExpectedSort (sort, x, loc'))
              | ExpectedAnnotation (loc, sort) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (ExpectedAnnotation (loc', sort))
              | InvalidTypeParamUse loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (InvalidTypeParamUse loc')
              | UnexpectedObjectKey (loc, key_loc) ->
                let loc' = this#loc loc in
                let key_loc' = this#loc key_loc in
                if loc == loc' && key_loc == key_loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedObjectKey (loc', key_loc'))
              | UnexpectedObjectSpread (loc, spread_loc) ->
                let loc' = this#loc loc in
                let spread_loc' = this#loc spread_loc in
                if loc == loc' && spread_loc == spread_loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedObjectSpread (loc', spread_loc'))
              | UnexpectedArraySpread (loc, spread_loc) ->
                let loc' = this#loc loc in
                let spread_loc' = this#loc spread_loc in
                if loc == loc' && spread_loc == spread_loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedArraySpread (loc', spread_loc'))
              | UnexpectedArrayHole loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedArrayHole loc')
              | EmptyArray loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (EmptyArray loc')
              | EmptyObject loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (EmptyObject loc')
              | UnexpectedExpression (loc, esort) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnexpectedExpression (loc', esort))
              | SketchyToplevelDef loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (SketchyToplevelDef loc')
              | UnsupportedPredicateExpression loc ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (UnsupportedPredicateExpression loc')
              | TODO (msg, loc) ->
                let loc' = this#loc loc in
                if loc == loc' then
                  tolerable_error
                else
                  SignatureVerificationError (TODO (msg, loc'))
            end)

      method error (error : error) =
        match error with
        | IndeterminateModuleType loc ->
          let loc' = this#loc loc in
          if loc == loc' then
            error
          else
            IndeterminateModuleType loc'

      method loc (loc : L.t) = loc
    end
end

module With_Loc =
  Make (Loc_sig.LocS) (Scope_api.With_Loc) (Scope_builder.With_Loc)
    (Signature_builder_deps.With_Loc)
module With_ALoc =
  Make (Loc_sig.ALocS) (Scope_api.With_ALoc) (Scope_builder.With_ALoc)
    (Signature_builder_deps.With_ALoc)

let abstractify_tolerable_errors =
  let module WL = With_Loc in
  let module WA = With_ALoc in
  let abstractify_tolerable_error = function
    | WL.BadExportPosition loc -> WA.BadExportPosition (ALoc.of_loc loc)
    | WL.BadExportContext (name, loc) -> WA.BadExportContext (name, ALoc.of_loc loc)
    | WL.SignatureVerificationError err ->
      WA.SignatureVerificationError (Signature_error.map_locs ~f:ALoc.of_loc err)
  in
  Base.List.map ~f:abstractify_tolerable_error

let abstractify_locs : With_Loc.t -> With_ALoc.t =
  let module WL = With_Loc in
  let module WA = With_ALoc in
  let abstractify_fst (loc, x) = (ALoc.of_loc loc, x) in
  let abstractify_imported_locs { WL.remote_loc; local_loc } =
    { WA.remote_loc = ALoc.of_loc remote_loc; local_loc = ALoc.of_loc local_loc }
  in
  let abstractify_imported_locs_map = SMap.map (SMap.map (Nel.map abstractify_imported_locs)) in
  let rec abstractify_require_bindings = function
    | WL.BindIdent x -> WA.BindIdent (abstractify_fst x)
    | WL.BindNamed named ->
      WA.BindNamed
        (Base.List.map
           ~f:(fun (remote, require_bindings) ->
             (abstractify_fst remote, abstractify_require_bindings require_bindings))
           named)
  in
  let abstractify_require = function
    | WL.Require { source; require_loc; bindings } ->
      WA.Require
        {
          source = abstractify_fst source;
          require_loc = ALoc.of_loc require_loc;
          bindings = Option.map ~f:abstractify_require_bindings bindings;
        }
    | WL.ImportDynamic { source; import_loc } ->
      WA.ImportDynamic { source = abstractify_fst source; import_loc = ALoc.of_loc import_loc }
    | WL.Import0 { source } -> WA.Import0 { source = abstractify_fst source }
    | WL.Import { import_loc; source; named; ns; types; typesof; typesof_ns } ->
      WA.Import
        {
          import_loc = ALoc.of_loc import_loc;
          source = abstractify_fst source;
          named = abstractify_imported_locs_map named;
          ns = Option.map ~f:abstractify_fst ns;
          types = abstractify_imported_locs_map types;
          typesof = abstractify_imported_locs_map typesof;
          typesof_ns = Option.map ~f:abstractify_fst typesof_ns;
        }
  in
  let abstractify_requires = Base.List.map ~f:abstractify_require in
  let abstractify_named_export_kind = function
    | WL.NamedDeclaration -> WA.NamedDeclaration
    | WL.NamedSpecifier { local; source } ->
      WA.NamedSpecifier
        { local = abstractify_fst local; source = Option.map ~f:abstractify_fst source }
  in
  let abstractify_export = function
    | WL.ExportDefault { default_loc; local } ->
      WA.ExportDefault
        { default_loc = ALoc.of_loc default_loc; local = Option.map ~f:abstractify_fst local }
    | WL.ExportNamed { loc; kind } ->
      WA.ExportNamed { loc = ALoc.of_loc loc; kind = abstractify_named_export_kind kind }
    | WL.ExportNs { loc; star_loc; source } ->
      WA.ExportNs
        { loc = ALoc.of_loc loc; star_loc = ALoc.of_loc star_loc; source = abstractify_fst source }
  in
  let abstractify_named_export (name, (loc, export)) =
    (name, (ALoc.of_loc loc, abstractify_export export))
  in
  let abstractify_named_exports = Base.List.map ~f:abstractify_named_export in
  let abstractify_export_star = function
    | WL.ExportStar { star_loc; source } ->
      WA.ExportStar { star_loc = ALoc.of_loc star_loc; source = abstractify_fst source }
  in
  let abstractify_es_star =
    Base.List.map ~f:(fun (loc, export_star) ->
        (ALoc.of_loc loc, abstractify_export_star export_star))
  in
  let abstractify_module_kind = function
    | WL.CommonJS { mod_exp_loc } ->
      WA.CommonJS { mod_exp_loc = Option.map ~f:ALoc.of_loc mod_exp_loc }
    | WL.ES { named; star } ->
      WA.ES { named = abstractify_named_exports named; star = abstractify_es_star star }
  in
  let abstractify_type_export = function
    | WL.TypeExportNamed { loc; kind } ->
      WA.TypeExportNamed { loc = ALoc.of_loc loc; kind = abstractify_named_export_kind kind }
  in
  let abstractify_type_exports_named =
    Base.List.map ~f:(fun (name, (loc, type_export)) ->
        (name, (ALoc.of_loc loc, abstractify_type_export type_export)))
  in
  let abstractify_type_exports_star = abstractify_es_star in
  let abstractify_module_sig
      { WL.requires; module_kind; type_exports_named; type_exports_star; info } =
    {
      WA.requires = abstractify_requires requires;
      module_kind = abstractify_module_kind module_kind;
      type_exports_named = abstractify_type_exports_named type_exports_named;
      type_exports_star = abstractify_type_exports_star type_exports_star;
      info;
    }
  in
  let abstractify_declare_modules =
    SMap.map (fun (loc, module_sig) -> (ALoc.of_loc loc, abstractify_module_sig module_sig))
  in
  let abstractify_local_env = function
    | Some exported_locals ->
      Some
        (SMap.map
           (fun loc_set ->
             Loc_collections.LocSet.fold
               (fun loc acc -> Loc_collections.ALocSet.add (ALoc.of_loc loc) acc)
               loc_set
               Loc_collections.ALocSet.empty)
           exported_locals)
    | None -> None
  in
  fun { WL.module_sig; declare_modules; tolerable_errors; exported_locals } ->
    {
      WA.module_sig = abstractify_module_sig module_sig;
      declare_modules = abstractify_declare_modules declare_modules;
      tolerable_errors = abstractify_tolerable_errors tolerable_errors;
      exported_locals = abstractify_local_env exported_locals;
    }
