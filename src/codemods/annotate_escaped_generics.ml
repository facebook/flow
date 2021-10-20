(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections

module SymbolMap = WrappedMap.Make (struct
  type t = Ty_symbol.symbol

  let compare = Stdlib.compare
end)

let norm_opts = Ty_normalizer_env.default_options

(* Change the name of the symbol to avoid local aliases *)
let localize_str (str : string) = Printf.sprintf "$IMPORTED$_%s" str

(* TODO consider just excluding internal names here. *)
let localize =
  Reason.(
    function
    | OrdinaryName str -> OrdinaryName (localize_str str)
    | InternalName str -> InternalName (localize_str str)
    | InternalModuleName str -> InternalModuleName (localize_str str)
  )

let localize_type =
  let remote_syms = ref SymbolMap.empty in
  let localize_symbol symbol =
    match symbol.Ty.sym_provenance with
    | Ty.Remote { Ty.imported_as = None } ->
      let local_name = localize symbol.Ty.sym_name in
      Utils_js.print_endlinef "local_name: %s" (Reason.display_string_of_name local_name);
      let sym_provenance =
        Ty.Remote
          {
            Ty.imported_as =
              Some
                ( ALoc.none,
                  (* TODO this use of display_string_of_name is a bit sketchy *)
                  Reason.display_string_of_name local_name,
                  Ty.TypeMode
                );
          }
      in
      let imported = { symbol with Ty.sym_provenance; sym_name = local_name } in
      remote_syms := SymbolMap.add symbol imported !remote_syms;
      imported
    | _ -> symbol
  in
  let o =
    object (_self)
      inherit [_] Ty.endo_ty

      method! on_symbol _env s =
        Utils_js.print_endlinef "Localizing symbol: %s" (Ty_debug.dump_symbol s);
        localize_symbol s
    end
  in
  fun ty ->
    remote_syms := SymbolMap.empty;
    let ty' = o#on_t () ty in
    (ty', !remote_syms)

let remote_symbols_map tys =
  List.fold_left
    (fun acc ty ->
      Ty_utils.symbols_of_type ty
      |> List.fold_left
           (fun a symbol ->
             let { Ty.sym_provenance; sym_name; sym_anonymous; _ } = symbol in
             match sym_provenance with
             | Ty.Remote { Ty.imported_as = None } when not sym_anonymous ->
               (* Anonymous symbols will cause errors to the serializer.
                * Discard them here as well. *)
               let local_alias =
                 {
                   Ty.sym_provenance = Ty.Local;
                   sym_def_loc = ALoc.none;
                   sym_anonymous;
                   sym_name = localize sym_name;
                 }
               in
               Utils_js.print_endlinef
                 "Localizing: %s"
                 (Reason.display_string_of_name local_alias.Ty.sym_name);
               SymbolMap.add symbol local_alias a
             | _ -> a)
           acc)
    SymbolMap.empty
    tys

let gen_import_statements file (symbols : Ty_symbol.symbol SymbolMap.t) =
  let dummy_loc = Loc.none in
  let raw_from_string (str : string) = Printf.sprintf "%S" str in
  let gen_import_statement remote_symbol local_symbol =
    let { Ty.sym_def_loc = remote_loc; sym_name = remote_name; _ } = remote_symbol in
    let { Ty.sym_name = local_name; _ } = local_symbol in
    let remote_source = ALoc.source remote_loc in
    let remote_source =
      match remote_source with
      | Some remote_source -> remote_source
      | None -> failwith "No source"
    in
    Hh_logger.debug "remote source %s" (File_key.to_string file);

    let { Module_heaps.module_name; _ } =
      let reader = State_reader.create () in
      Module_heaps.Reader.get_info_unsafe ~reader ~audit:Expensive.warn remote_source
    in
    (* Relativize module name *)
    let module_name =
      match module_name with
      | Modulename.String s -> s
      | Modulename.Filename f ->
        let f = File_key.to_string f in
        let dir = Filename.dirname (File_key.to_string file) in
        Filename.concat "./" (Files.relative_path dir f)
    in
    (* TODO we should probably abort if we encounter an internal name here, rather than
     * constructing AST nodes with stringified internal names. *)
    let remote_name =
      Flow_ast_utils.ident_of_source (dummy_loc, Reason.display_string_of_name remote_name)
    in
    let local_name =
      Flow_ast_utils.ident_of_source (dummy_loc, Reason.display_string_of_name local_name)
    in
    Ast.Statement.
      ( dummy_loc,
        ImportDeclaration
          {
            ImportDeclaration.import_kind = ImportDeclaration.ImportType;
            source =
              ( dummy_loc,
                {
                  Ast.StringLiteral.value = module_name;
                  raw = raw_from_string module_name;
                  comments = None;
                }
              );
            default = None;
            specifiers =
              Some
                ImportDeclaration.(
                  ImportNamedSpecifiers
                    [{ kind = None; local = Some local_name; remote = remote_name }]
                );
            comments = None;
          }
      )
    
  in

  SymbolMap.fold (fun remote local acc -> gen_import_statement remote local :: acc) symbols []

(* The mapper *)

module UnitStats : Insert_type_utils.BASE_STATS with type t = unit = struct
  type t = unit

  let empty = ()

  let combine _ _ = ()

  let serialize _s = []

  let report _s = []
end

module Accumulator = Insert_type_utils.Acc (UnitStats)
module Unit_Codemod_annotator = Codemod_annotator.Make (UnitStats)

let reporter =
  {
    Codemod_report.report = Codemod_report.StringReporter Accumulator.report;
    combine = Accumulator.combine;
    empty = Accumulator.empty;
  }

type accumulator = Accumulator.t

let mapper ~default_any ~preserve_literals ~max_type_size (ask : Codemod_context.Typed.t) =
  let imports_react =
    Insert_type_imports.ImportsHelper.imports_react ask.Codemod_context.Typed.file_sig
  in
  let options = ask.Codemod_context.Typed.options in
  let exact_by_default = Options.exact_by_default options in
  let metadata =
    Context.docblock_overrides ask.Codemod_context.Typed.docblock ask.Codemod_context.Typed.metadata
  in
  let { Context.strict; strict_local; _ } = metadata in
  let lint_severities =
    if strict || strict_local then
      StrictModeSettings.fold
        (fun lint_kind lint_severities ->
          LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
        (Options.strict_mode options)
        (Options.lint_severities options)
    else
      Options.lint_severities options
  in
  let suppress_types = Options.suppress_types options in
  let escape_locs =
    let cx = Codemod_context.Typed.context ask in
    let errors = Context.errors cx in
    Flow_error.ErrorSet.fold
      (fun err locs ->
        match Flow_error.msg_of_error err with
        | Error_message.EEscapedGeneric { annot_reason = Some annot_reason; _ } ->
          ALocSet.add (Reason.aloc_of_reason annot_reason) locs
        | _ -> locs)
      errors
      ALocSet.empty
  in

  object (this)
    inherit
      Unit_Codemod_annotator.mapper
        ~max_type_size
        ~exact_by_default
        ~lint_severities
        ~suppress_types
        ~imports_react
        ~preserve_literals
        ~default_any
        ask as super

    val mutable remote_symbols_map = SymbolMap.empty

    method private register_remote_symbols syms =
      remote_symbols_map <- SymbolMap.fold SymbolMap.add syms remote_symbols_map

    method private is_directive_statement (stmt : (Loc.t, Loc.t) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (_loc, Expression { Expression.directive = Some _; _ })
      | (_loc, ImportDeclaration { ImportDeclaration.import_kind = ImportDeclaration.ImportType; _ })
        ->
        true
      | _ -> false

    method private add_statement_after_directive_and_type_imports
        (block_stmts : (Loc.t, Loc.t) Ast.Statement.t list)
        (insert_stmts : (Loc.t, Loc.t) Ast.Statement.t list) =
      match block_stmts with
      | [] -> insert_stmts
      | stmt :: block when this#is_directive_statement stmt ->
        (* TODO make tail-recursive *)
        stmt :: this#add_statement_after_directive_and_type_imports block insert_stmts
      | _ -> insert_stmts @ block_stmts

    method private make_annotation loc ty =
      match ty with
      | Ok (Ty.Type ty) ->
        begin
          match this#replace_type_node_with_ty loc ty with
          | Ok t -> Ast.Type.Available (loc, t)
          | Error _ -> Ast.Type.Missing loc
        end
      | _ -> Ast.Type.Missing loc

    method private post_run () = ()

    method! binding_pattern ?(kind = Ast.Statement.VariableDeclaration.Var) ((pat_loc, patt) as expr)
        =
      let open Ast.Pattern in
      let open Ast.Pattern.Identifier in
      match patt with
      | Identifier ({ Identifier.name = (loc, _); annot = Ast.Type.Missing annot_loc; _ } as id) ->
        let aloc = ALoc.of_loc loc in
        if ALocSet.mem aloc escape_locs then
          let annot =
            this#make_annotation annot_loc (Codemod_context.Typed.ty_at_loc norm_opts ask loc)
          in
          super#binding_pattern ~kind (pat_loc, Identifier { id with annot })
        else
          super#binding_pattern ~kind expr
      | _ -> super#binding_pattern ~kind expr

    method! type_annotation_hint annot =
      match annot with
      | Flow_ast.Type.Available _ -> annot
      | Flow_ast.Type.Missing loc ->
        let aloc = ALoc.of_loc loc in
        if ALocSet.mem aloc escape_locs then
          this#make_annotation loc (Codemod_context.Typed.ty_at_loc norm_opts ask loc)
        else
          annot

    method! program prog =
      remote_symbols_map <- SymbolMap.empty;
      let (loc, { Ast.Program.statements = stmts; comments; all_comments }) = super#program prog in
      let { Codemod_context.Typed.file; _ } = ask in
      let import_stmts = gen_import_statements file remote_symbols_map in
      let stmts = this#add_statement_after_directive_and_type_imports stmts import_stmts in
      (loc, { Ast.Program.statements = stmts; comments; all_comments })
  end

let visit ~default_any ~preserve_literals ~max_type_size =
  Codemod_utils.make_visitor
    (Codemod_utils.Mapper (mapper ~default_any ~preserve_literals ~max_type_size))
