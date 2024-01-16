(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
open Flow_ast_visitor
module Scope_api = Scope_api.With_Loc
module Scope_builder = Scope_builder.With_Loc
module Loc = Loc_sig.LocS

type options = {
  enable_enums: bool;
  enable_relay_integration: bool;
  explicit_available_platforms: string list option;
  file_options: Files.options;
  haste_module_ref_prefix: string option;
  haste_module_ref_prefix_LEGACY_INTEROP: string option;
  relay_integration_module_prefix: string option;
}

type t = require list

and require =
  | Require of {
      source: Loc.t Ast_utils.source;
      require_loc: Loc.t;
      bindings: require_bindings option;
      prefix: string option;
    }
  | ImportDynamic of {
      source: Loc.t Ast_utils.source;
      import_loc: Loc.t;
    }
  | Import0 of { source: Loc.t Ast_utils.source }
  | ImportSynthetic of { source: string }
  | Import of {
      import_loc: Loc.t;
      source: Loc.t Ast_utils.source;
      named: imported_locs Nel.t SMap.t SMap.t;
      ns: Loc.t Ast_utils.ident option;
      types: imported_locs Nel.t SMap.t SMap.t;
      typesof: imported_locs Nel.t SMap.t SMap.t;
      typesof_ns: Loc.t Ast_utils.ident option;
    }
  | ExportFrom of { source: Loc.t Flow_ast_utils.source }

and imported_locs = {
  remote_loc: Loc.t;
  local_loc: Loc.t;
}

and require_bindings =
  | BindIdent of Loc.t Ast_utils.ident
  | BindNamed of (Loc.t Ast_utils.ident * require_bindings) list
[@@deriving show]

type tolerable_error = SignatureVerificationError of Loc.t Signature_error.t [@@deriving show]

type tolerable_t = t * tolerable_error list

let empty = []

let default_opts =
  {
    enable_relay_integration = false;
    enable_enums = false;
    explicit_available_platforms = None;
    file_options = Files.default_options;
    haste_module_ref_prefix = None;
    haste_module_ref_prefix_LEGACY_INTEROP = None;
    relay_integration_module_prefix = None;
  }

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
end

let to_string require_list =
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
    | ImportSynthetic _ -> "ImportSynthetic"
    | Import _ -> "Import"
    | ExportFrom _ -> "ExportFrom"
  in
  PP.items_to_list_string 2 (Base.List.map ~f:string_of_require require_list)

let combine_nel _ a b = Some (Nel.concat (a, [b]))

let require_loc_map msig =
  List.fold_left
    (fun acc require ->
      match require with
      | ImportSynthetic { source } -> SMap.add source [] acc ~combine:List.rev_append
      | Require { source = (loc, mref); _ }
      | ImportDynamic { source = (loc, mref); _ }
      | Import0 { source = (loc, mref) }
      | Import { source = (loc, mref); _ }
      | ExportFrom { source = (loc, mref) } ->
        SMap.add mref [loc] acc ~combine:List.rev_append)
    SMap.empty
    msig

let require_set msig =
  let map = require_loc_map msig in
  SMap.fold (fun key _ acc -> SSet.add key acc) map SSet.empty

let requires msig = msig

let add_require require requires = require :: requires

let add_es_export source requires =
  match source with
  | None -> requires
  | Some source -> ExportFrom { source } :: requires

(* Subclass of the AST visitor class that calculates requires and exports. Initializes with the
   scope builder class.
*)
class requires_calculator ~file_key ~ast ~opts =
  object (this)
    inherit [t, Loc.t] visitor ~init:empty as super

    val scope_info : Scope_api.info =
      Scope_builder.program ~with_types:true ~enable_enums:opts.enable_enums ast

    (* This ensures that we do not add a `require` with no bindings to `requires` (when processing a
     * `call`) when we have already added that `require` with bindings (when processing a
     * `variable_declarator`). *)
    val mutable visited_requires_with_bindings : Loc.LSet.t = Loc.LSet.empty

    method private visited_requires_with_bindings loc bindings =
      bindings = None && Loc.LSet.mem loc visited_requires_with_bindings

    method private visit_requires_with_bindings loc bindings =
      if bindings <> None then
        visited_requires_with_bindings <- Loc.LSet.add loc visited_requires_with_bindings

    method private update_file_sig f = this#update_acc f

    method private add_require require = this#update_file_sig (add_require require)

    method private add_exports kind source =
      let open Ast.Statement in
      let add =
        match (kind, source) with
        | (ExportValue, _) -> add_es_export source
        | (ExportType, None) -> (fun msig -> msig)
        | (ExportType, Some source) -> add_require (ExportFrom { source })
      in
      this#update_file_sig add

    method add_multiplatform_synthetic_imports =
      match
        Platform_set.platform_specific_implementation_mrefs_of_possibly_interface_file
          ~file_options:opts.file_options
          ~platform_set:
            (Platform_set.available_platforms
               ~file_options:opts.file_options
               ~filename:(File_key.to_string file_key)
               ~explicit_available_platforms:opts.explicit_available_platforms
            )
          ~file:file_key
      with
      | Some sources ->
        Base.List.iter sources ~f:(fun source -> this#add_require (ImportSynthetic { source }))
      | None ->
        (match
           Files.relative_interface_mref_of_possibly_platform_specific_file
             ~options:opts.file_options
             file_key
         with
        | Some imported_interface_module_name ->
          this#add_require (ImportSynthetic { source = imported_interface_module_name })
        | None -> ())

    method! call call_loc (expr : (Loc.t, Loc.t) Ast.Expression.Call.t) =
      let open Ast.Expression in
      let { Call.callee; targs; arguments; comments = _ } = expr in
      this#handle_call call_loc callee targs arguments None;
      super#call call_loc expr

    method! module_ref_literal loc lit =
      let { Ast.ModuleRefLiteral.value; prefix_len; _ } = lit in
      let mref = Base.String.drop_prefix value prefix_len in
      let prefix =
        if prefix_len > 0 then
          Some (Base.String.prefix value prefix_len)
        else
          None
      in
      this#add_require (Require { source = (loc, mref); require_loc = loc; bindings = None; prefix });
      super#module_ref_literal loc lit

    method! tagged_template loc (expr : ('loc, 'loc) Ast.Expression.TaggedTemplate.t) =
      let open Ast.Expression.TaggedTemplate in
      let { tag; quasi; comments = _ } = expr in
      match tag with
      | (_, Ast.Expression.Identifier (_, { Ast.Identifier.name = "graphql"; _ }))
        when opts.enable_relay_integration ->
        (match
           Graphql.extract_module_name ~module_prefix:opts.relay_integration_module_prefix quasi
         with
        | Ok module_name ->
          this#add_require
            (Require
               {
                 source = (loc, module_name);
                 require_loc = loc;
                 bindings = None;
                 prefix = opts.relay_integration_module_prefix;
               }
            );
          expr
        | Error _ -> expr)
      | _ -> super#tagged_template loc expr

    method! import import_loc (expr : (Loc.t, Loc.t) Ast.Expression.Import.t) =
      let open Ast.Expression in
      let { Import.argument; comments = _ } = expr in
      begin
        match argument with
        | ( loc,
            ( StringLiteral { Ast.StringLiteral.value = name; _ }
            | TemplateLiteral
                {
                  TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          TemplateLiteral.Element.value =
                            { TemplateLiteral.Element.cooked = name; _ };
                          _;
                        }
                      );
                    ];
                  _;
                } )
          ) ->
          this#add_require (ImportDynamic { source = (loc, name); import_loc })
        | _ -> ()
      end;
      super#import import_loc expr

    method! import_declaration import_loc (decl : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t)
        =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default; comments = _ } = decl in
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
          Base.Option.iter
            ~f:
              (fun {
                     identifier = (loc, { Ast.Identifier.name = local; comments = _ });
                     remote_default_name_def_loc = _;
                   } ->
              add_named
                "default"
                local
                { remote_loc = loc; local_loc = loc }
                (ref_of_kind import_kind))
            default;
          Base.Option.iter
            ~f:(function
              | ImportNamespaceSpecifier (_, (loc, { Ast.Identifier.name = local; comments = _ }))
                ->
                (match import_kind with
                | ImportType ->
                  (* this is a parse error. ignore it. *)
                  ()
                | ImportTypeof -> set_ns local loc typesof_ns
                | ImportValue -> set_ns local loc ns)
              | ImportNamedSpecifiers named_specifiers ->
                List.iter
                  (function
                    | { local; remote; remote_name_def_loc = _; kind } ->
                      let import_kind =
                        match kind with
                        | Some k -> k
                        | None -> import_kind
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
                        (ref_of_kind import_kind))
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
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t) =
      let open Ast.Statement.ExportDefaultDeclaration in
      let { default = _; declaration = _; comments = _ } = decl in
      this#add_exports Ast.Statement.ExportValue None;
      super#export_default_declaration stmt_loc decl

    method! export_named_declaration
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.ExportNamedDeclaration.t) =
      let open Ast.Statement.ExportNamedDeclaration in
      let { export_kind; source; specifiers; declaration; comments = _ } = decl in
      let source =
        match source with
        | Some (loc, { Ast.StringLiteral.value = mref; raw = _; comments = _ }) -> Some (loc, mref)
        | None -> None
      in
      begin
        match declaration with
        | None -> ()
        | Some _ -> this#add_exports export_kind source
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports export_kind source
      end;
      super#export_named_declaration stmt_loc decl

    method! declare_export_declaration
        stmt_loc (decl : (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.t) =
      let open Ast.Statement.DeclareExportDeclaration in
      let { default = _; source; specifiers; declaration; comments = _ } = decl in
      let source =
        match source with
        | Some (loc, { Ast.StringLiteral.value = mref; raw = _; comments = _ }) -> Some (loc, mref)
        | _ -> None
      in
      begin
        match declaration with
        | None -> () (* assert specifiers <> None *)
        | Some declaration ->
          let open Ast.Statement in
          let export_kind =
            match declaration with
            | Variable _
            | Function _
            | Class _
            | Component _
            | DefaultType _
            | Enum _ ->
              ExportValue
            | NamedType _
            | NamedOpaqueType _
            | Interface _ ->
              ExportType
          in
          this#add_exports export_kind source
      end;
      begin
        match specifiers with
        | None -> ()
        | Some _ -> this#add_exports Ast.Statement.ExportValue source
      end;
      super#declare_export_declaration stmt_loc decl

    method! variable_declarator
        ~kind (decl : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
      begin
        match decl with
        | (_, { Ast.Statement.VariableDeclaration.Declarator.id; init = Some init }) ->
          this#handle_require id init
        | _ -> ()
      end;
      super#variable_declarator ~kind decl

    method private require_pattern (pattern : (Loc.t, Loc.t) Ast.Pattern.t) =
      match pattern with
      | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
        Some (BindIdent (Flow_ast_utils.source_of_ident name))
      | (_, Ast.Pattern.Object { Ast.Pattern.Object.properties; _ }) ->
        let named_bind =
          Base.List.fold_result properties ~init:[] ~f:(fun named prop ->
              match prop with
              | Ast.Pattern.Object.Property
                  ( _,
                    {
                      Ast.Pattern.Object.Property.key =
                        Ast.Pattern.Object.Property.Identifier remote;
                      pattern;
                      _;
                    }
                  ) ->
                (match this#require_pattern pattern with
                | Some bindings -> Ok ((Flow_ast_utils.source_of_ident remote, bindings) :: named)
                | None -> Error ())
              | _ -> Error ()
          )
        in
        (match named_bind with
        | Ok named_bind -> Some (BindNamed named_bind)
        | Error () -> None)
      | _ -> None

    method private handle_require
        (left : (Loc.t, Loc.t) Ast.Pattern.t) (right : (Loc.t, Loc.t) Ast.Expression.t) =
      let open Ast.Expression in
      let bindings = this#require_pattern left in
      match right with
      | (call_loc, Call { Call.callee; targs; arguments; comments = _ }) ->
        this#handle_call call_loc callee targs arguments bindings
      | _ -> ()

    method private handle_call call_loc callee targs arguments bindings =
      let open Ast.Expression in
      if not (this#visited_requires_with_bindings call_loc bindings) then (
        this#visit_requires_with_bindings call_loc bindings;
        match (callee, targs, arguments) with
        | ( (_, Identifier (loc, { Ast.Identifier.name = "require"; comments = _ })),
            None,
            ( _,
              {
                Ast.Expression.ArgList.arguments =
                  [
                    Expression
                      ( source_loc,
                        ( StringLiteral { Ast.StringLiteral.value = name; _ }
                        | TemplateLiteral
                            {
                              TemplateLiteral.quasis =
                                [
                                  ( _,
                                    {
                                      TemplateLiteral.Element.value =
                                        { TemplateLiteral.Element.cooked = name; _ };
                                      _;
                                    }
                                  );
                                ];
                              _;
                            } )
                      );
                  ];
                comments = _;
              }
            )
          ) ->
          if not (Scope_api.is_local_use scope_info loc) then
            this#add_require
              (Require
                 { source = (source_loc, name); require_loc = call_loc; bindings; prefix = None }
              )
        | _ -> ()
      )

    (* skip declare module *)
    method! declare_module _loc (m : (Loc.t, Loc.t) Ast.Statement.DeclareModule.t) = m
  end

let program ~file_key ~ast ~opts =
  let walk = new requires_calculator ~file_key ~ast ~opts in
  walk#add_multiplatform_synthetic_imports;
  walk#eval walk#program ast
