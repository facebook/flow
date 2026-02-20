(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(** Token counting using character-based estimate.
    Following Devmate VS Code convention of ~3.5 characters per token. *)
let count_tokens (s : string) : int =
  int_of_float (Float.ceil (float_of_int (String.length s) /. 3.5))

(** Extract the name from an identifier in typed AST *)
let name_of_typed_identifier ((_, _), { Ast.Identifier.name; _ }) = name

(** Extract the name from a pattern in typed AST (for variable declarations) *)
let name_of_typed_pattern pattern =
  match pattern with
  | (_, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
    Some (name_of_typed_identifier name)
  | _ -> None

(** Normalize a type to a string representation with refs *)
let normalize_type ~cx ~file_sig ~typed_ast ~reader (t : Type.t) :
    string * (string * Loc.t) list option =
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      expand_enum_members = false;
      evaluate_type_destructors = Ty_normalizer_env.EvaluateNone;
      optimize_types = true;
      omit_targ_defaults_option = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 10;
      toplevel_is_type_identifier_reference = false;
    }
  in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast) in
  match Ty_normalizer_flow.from_type genv t with
  | Ok ty ->
    let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
    let refs = Ty.symbols_of_elt ~loc_of_aloc ty in
    let result = { Ty.unevaluated = ty; evaluated = None; refs = Some refs } in
    let (type_str, refs) =
      Ty_printer.string_of_type_at_pos_result ~exact_by_default:true ~ts_syntax:false result
    in
    (type_str, refs)
  | Error _ -> ("<unknown>", None)

(** Format refs as a "where" clause like type_of_name.ml *)
let format_refs ~(strip_root : File_path.t option) (refs : (string * Loc.t) list option) : string =
  match refs with
  | None -> ""
  | Some refs_list ->
    if refs_list = [] then
      ""
    else
      let ref_strs =
        List.filter_map
          (fun (name, loc) ->
            match loc.Loc.source with
            | Some _ ->
              let loc_str = Reason.string_of_loc ~strip_root loc in
              Some (Printf.sprintf "'%s' is defined at %s" name loc_str)
            | None -> None)
          refs_list
      in
      if ref_strs = [] then
        ""
      else
        Printf.sprintf "\nwhere\n%s" (String.concat "\n" ref_strs)

(** Typed AST visitor to extract export declarations and type signatures *)
class context_extractor ~strip_root ~cx ~file_sig ~typed_ast ~reader =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable exports : string list = []

    method get_exports = Base.List.rev exports

    method private add_export (s : string) = exports <- s :: exports

    method on_loc_annot x = x

    method on_type_annot x = x

    (** Helper to extract variable names and kinds from a variable declaration *)
    method private extract_variables kind decls =
      let kind_str =
        match kind with
        | Ast.Variable.Var -> "var"
        | Ast.Variable.Let -> "let"
        | Ast.Variable.Const -> "const"
      in
      List.iter
        (fun decl ->
          let open Ast.Statement.VariableDeclaration.Declarator in
          let (_, { id; _ }) = decl in
          match name_of_typed_pattern id with
          | Some name -> this#add_export (Printf.sprintf "export %s %s" kind_str name)
          | None -> ())
        decls

    method! export_named_declaration _loc decl =
      let open Ast.Statement.ExportNamedDeclaration in
      (match decl.declaration with
      | Some (_, Ast.Statement.VariableDeclaration var_decl) ->
        let open Ast.Statement.VariableDeclaration in
        this#extract_variables var_decl.kind var_decl.declarations
      | Some (_, Ast.Statement.FunctionDeclaration func) ->
        let open Ast.Function in
        (match func.id with
        | Some ((_, func_type), { Ast.Identifier.name; _ }) ->
          let (type_str, refs) = normalize_type ~cx ~file_sig ~typed_ast ~reader func_type in
          let refs_str = format_refs ~strip_root refs in
          this#add_export (Printf.sprintf "export function %s: %s%s" name type_str refs_str)
        | None -> ())
      | Some (_, Ast.Statement.ClassDeclaration cls) ->
        let open Ast.Class in
        (match cls.id with
        | Some id ->
          let name = name_of_typed_identifier id in
          this#add_export (Printf.sprintf "export class %s" name)
        | None -> ())
      | Some (_, Ast.Statement.ComponentDeclaration component) ->
        let open Ast.Statement.ComponentDeclaration in
        let ((_, component_type), { Ast.Identifier.name; _ }) = component.id in
        let (type_str, refs) = normalize_type ~cx ~file_sig ~typed_ast ~reader component_type in
        let refs_str = format_refs ~strip_root refs in
        this#add_export (Printf.sprintf "export component %s: %s%s" name type_str refs_str)
      | Some (_, Ast.Statement.TypeAlias alias) ->
        let open Ast.Statement.TypeAlias in
        let name = name_of_typed_identifier alias.id in
        this#add_export (Printf.sprintf "export type %s = ..." name)
      | Some (_, Ast.Statement.InterfaceDeclaration iface) ->
        let open Ast.Statement.Interface in
        let name = name_of_typed_identifier iface.id in
        this#add_export (Printf.sprintf "export interface %s" name)
      | Some (_, Ast.Statement.EnumDeclaration enum) ->
        let open Ast.Statement.EnumDeclaration in
        let name = name_of_typed_identifier enum.id in
        this#add_export (Printf.sprintf "export enum %s" name)
      | _ -> ());
      super#export_named_declaration _loc decl

    method! export_default_declaration _loc decl =
      let open Ast.Statement.ExportDefaultDeclaration in
      (match decl.declaration with
      | Declaration (_, Ast.Statement.ComponentDeclaration component) ->
        let open Ast.Statement.ComponentDeclaration in
        let ((_, component_type), { Ast.Identifier.name; _ }) = component.id in
        let (type_str, refs) = normalize_type ~cx ~file_sig ~typed_ast ~reader component_type in
        let refs_str = format_refs ~strip_root refs in
        this#add_export (Printf.sprintf "export default component %s: %s%s" name type_str refs_str)
      | Declaration (_, Ast.Statement.FunctionDeclaration func) ->
        let open Ast.Function in
        (match func.id with
        | Some ((_, func_type), { Ast.Identifier.name; _ }) ->
          let (type_str, refs) = normalize_type ~cx ~file_sig ~typed_ast ~reader func_type in
          let refs_str = format_refs ~strip_root refs in
          this#add_export (Printf.sprintf "export default function %s: %s%s" name type_str refs_str)
        | None -> this#add_export "export default function(...)")
      | Declaration (_, Ast.Statement.ClassDeclaration cls) ->
        let open Ast.Class in
        (match cls.id with
        | Some id ->
          let name = name_of_typed_identifier id in
          this#add_export (Printf.sprintf "export default class %s" name)
        | None -> this#add_export "export default class")
      | _ -> this#add_export "export default ...");
      super#export_default_declaration _loc decl
  end

(** Extract imports from AST using File_sig *)
let extract_imports ~(file_key : File_key.t) ~(ast : (Loc.t, Loc.t) Ast.Program.t) : string list =
  let file_sig = File_sig.program ~file_key ~ast ~opts:File_sig.default_opts in
  let requires = File_sig.requires file_sig in
  List.filter_map
    (fun require ->
      match require with
      | File_sig.Require { source = (_, module_name); _ } ->
        Some (Printf.sprintf "require('%s')" module_name)
      | File_sig.Import { source = (_, module_name); _ }
      | File_sig.Import0 { source = (_, module_name) } ->
        Some (Printf.sprintf "import ... from '%s'" module_name)
      | File_sig.ImportDynamic { source = (_, module_name); _ } ->
        Some (Printf.sprintf "import('%s')" module_name)
      | File_sig.ExportFrom { source = (_, module_name) } ->
        Some (Printf.sprintf "export ... from '%s'" module_name)
      | File_sig.ImportSyntheticUserland _ -> None
      | File_sig.ImportSyntheticHaste _ -> None)
    requires

(** Extract declarations from typed AST *)
let extract_declarations
    ~(strip_root : File_path.t option)
    ~(cx : Context.t)
    ~(file_sig : File_sig.t)
    ~(typed_ast : (ALoc.t, ALoc.t * Type.t) Ast.Program.t)
    ~(reader : Parsing_heaps.Reader.reader) : string list =
  let extractor = new context_extractor ~strip_root ~cx ~file_sig ~typed_ast ~reader in
  ignore (extractor#program typed_ast);
  extractor#get_exports

(** Format context for a single file *)
let format_file_context
    ~(strip_root : File_path.t option)
    ~(file_key : File_key.t)
    ~(imports : string list)
    ~(declarations : string list) : string =
  let file_path = Reason.string_of_source ~strip_root file_key in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "=== File: %s ===\n\n" file_path);
  if imports <> [] then begin
    Buffer.add_string buf "Imports:\n";
    List.iter (fun imp -> Buffer.add_string buf (Printf.sprintf "  %s\n" imp)) imports;
    Buffer.add_string buf "\n"
  end;
  if declarations <> [] then begin
    Buffer.add_string buf "Declarations:\n";
    List.iter (fun decl -> Buffer.add_string buf (Printf.sprintf "  %s\n" decl)) declarations
  end;
  Buffer.contents buf

(** Generate context for a file given its AST and typing artifacts.
    This version receives the typing context (cx) and typed_ast for
    extracting actual type information. *)
let generate_file_context
    ~(strip_root : File_path.t option)
    ~(file_key : File_key.t)
    ~(ast : (Loc.t, Loc.t) Ast.Program.t)
    ~(cx : Context.t)
    ~(typed_ast : (ALoc.t, ALoc.t * Type.t) Ast.Program.t)
    ~(reader : Parsing_heaps.Reader.reader) : string =
  let imports = extract_imports ~file_key ~ast in
  let file_sig = File_sig.program ~file_key ~ast ~opts:File_sig.default_opts in
  let declarations = extract_declarations ~strip_root ~cx ~file_sig ~typed_ast ~reader in
  format_file_context ~strip_root ~file_key ~imports ~declarations

type file_context = {
  path: string;
  context: string;
  tokens: int;
}

type typed_file_info = {
  file_key: File_key.t;
  ast: (Loc.t, Loc.t) Ast.Program.t;
  cx: Context.t;
  typed_ast: (ALoc.t, ALoc.t * Type.t) Ast.Program.t;
  reader: Parsing_heaps.Reader.reader;
}

let legacy_syntax_header =
  String.concat
    "\n"
    [
      "IMPORTANT:";
      "The type `$ReadOnly<T>` is deprecated, use `Readonly<T>` instead.";
      "The type `$ReadOnlyArray<T>` is deprecated, use `ReadonlyArray<T>` instead.";
      "The type `mixed` is deprecated, use `unknown` instead.";
    ]

(** Generate context for multiple files with token budget.
    This version receives typed file info including cx and typed_ast.
    When include_imports is false, file_contexts will be empty. *)
let generate_context
    ~(strip_root : File_path.t option)
    ~(files : typed_file_info list)
    ~(token_budget : int)
    ~(include_imports : bool) : Lsp.LLMContext.result =
  let file_contexts =
    if include_imports then
      List.map
        (fun { file_key; ast; cx; typed_ast; reader } ->
          let context = generate_file_context ~strip_root ~file_key ~ast ~cx ~typed_ast ~reader in
          let tokens = count_tokens context in
          let path = Reason.string_of_source ~strip_root file_key in
          { path; context; tokens })
        files
    else
      []
  in
  let rec accumulate ~remaining_budget ~acc_context ~acc_files ~truncated = function
    | [] -> (acc_context, acc_files, truncated)
    | fc :: rest ->
      if fc.tokens <= remaining_budget then
        accumulate
          ~remaining_budget:(remaining_budget - fc.tokens)
          ~acc_context:(acc_context ^ fc.context ^ "\n")
          ~acc_files:(fc.path :: acc_files)
          ~truncated
          rest
      else
        (acc_context, acc_files, true)
  in
  let header_tokens = count_tokens legacy_syntax_header in
  let (context, files_processed, truncated) =
    accumulate
      ~remaining_budget:(token_budget - header_tokens)
      ~acc_context:""
      ~acc_files:[]
      ~truncated:false
      file_contexts
  in
  let full_context = legacy_syntax_header ^ "\n" ^ context in
  let tokens_used = count_tokens full_context in
  {
    Lsp.LLMContext.llmContext = full_context;
    filesProcessed = Base.List.rev files_processed;
    tokensUsed = tokens_used;
    truncated;
  }
