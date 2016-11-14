(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js
open Sys_utils
module Ast = Spider_monkey_ast

type result =
  | Parse_ok of Spider_monkey_ast.program
  | Parse_err of Errors.ErrorSet.t
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

(* results of parse job, returned by parse and reparse *)
type results = {
  parse_ok: FilenameSet.t;                   (* successfully parsed files *)
  parse_skips: (filename * Docblock.t) list; (* list of skipped files *)
  parse_fails: (filename * Docblock.t) list; (* list of failed files *)
  parse_errors: Errors.ErrorSet.t list;      (* parallel list of error sets *)
  parse_resource_files: FilenameSet.t;       (* resource files *)
}

let empty_result = {
  parse_ok = FilenameSet.empty;
  parse_skips = [];
  parse_fails = [];
  parse_errors = [];
  parse_resource_files = FilenameSet.empty;
}

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ParserHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
    type t = (Spider_monkey_ast.program * Docblock.t)
    let prefix = Prefix.make()
    let description = "Parser"
  end)

let (parser_hook: (filename -> Ast.program option -> unit) list ref) = ref []
let register_hook f = parser_hook := f :: !parser_hook

let execute_hook file ast =
  try
    List.iter (fun callback -> callback file ast) !parser_hook
  with e ->
    Printf.printf
      "Hook failed: %s
      (you can restart the server with OCAMLRUNPARAM=b to see a stack trace)\n"
      (Printexc.to_string e);
    Printexc.print_backtrace stdout

let delete_file fn =
  execute_hook fn None

(* TODO: add TypesForbidden (disables types even on files with @flow) and
   TypesAllowedByDefault (enables types even on files without @flow, but allows
   something like @noflow to disable them) *)
type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

let parse_source_file ~fail ~types ~use_strict content file =
  let parse_options = Some Parser_env.({
    (**
     * Always parse ES proposal syntax. The user-facing config option to
     * ignore/warn/enable them is handled during inference so that a clean error
     * can be surfaced (rather than a more cryptic parse error).
     *)
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    types = types;
    use_strict;
  }) in
  let ast, parse_errors =
    Parser_flow.program_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);
  ast

let parse_json_file ~fail content file =
  let parse_options = Some Parser_env.({
    esproposal_class_instance_fields = false;
    esproposal_class_static_fields = false;
    esproposal_decorators = false;
    esproposal_export_star_as = false;
    types = true;
    use_strict = false;
  }) in

  (* parse the file as JSON, then munge the AST to convert from an object
     into a `module.exports = {...}` statement *)
  let expr, parse_errors =
    Parser_flow.json_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);

  let open Parser_flow.Ast in
  let loc_none = Loc.none in
  let module_exports = loc_none, Expression.(Member { Member.
    _object = loc_none, Identifier (loc_none, "module");
    property = Member.PropertyIdentifier (loc_none, "exports");
    computed = false;
  }) in
  let loc = fst expr in
  let statement =
    loc, Statement.Expression { Statement.Expression.
      expression = loc, Expression.Assignment { Expression.Assignment.
        operator = Expression.Assignment.Assign;
        left = loc_none, Pattern.Expression module_exports;
        right = expr;
      }
    }
  in
  let comments = ([]: Comment.t list) in
  (loc, [statement], comments)

let string_of_docblock_error = function
  | Docblock.MultipleFlowAttributes ->
    "Unexpected @flow declaration. Only one per file is allowed."
  | Docblock.MultipleProvidesModuleAttributes ->
    "Unexpected @providesModule declaration. Only one per file is allowed."
  | Docblock.MultipleJSXAttributes ->
    "Unexpected @jsx declaration. Only one per file is allowed."
  | Docblock.InvalidJSXAttribute first_error ->
    "Invalid @jsx declaration. Should have form `@jsx LeftHandSideExpression` "^
    "with no spaces."^
    (match first_error with
    | None -> ""
    | Some first_error -> spf " Parse error: %s" first_error)

let get_docblock
  ~max_tokens file content
: Errors.ErrorSet.t option * Docblock.t =
  match file with
  | Loc.ResourceFile _
  | Loc.JsonFile _ -> None, Docblock.default_info
  | _ ->
    let errors, docblock = Docblock.extract ~max_tokens file content in
    if errors = [] then None, docblock
    else
      let errs = List.fold_left (fun acc (loc, err) ->
        let err = Errors.mk_error
          ~kind:Errors.ParseError
          [loc, [string_of_docblock_error err]]
        in
        Errors.ErrorSet.add err acc
      ) Errors.ErrorSet.empty errors in
      Some errs, docblock

let do_parse ?(fail=true) ~types_mode ~use_strict ~info content file =
  try (
    match file with
    | Loc.JsonFile _ ->
      Parse_ok (parse_json_file ~fail content file)
    | Loc.ResourceFile _ ->
      Parse_skip Skip_resource_file
    | _ ->
      (* Allow types based on `types_mode`, using the @flow annotation in the
       file header if possible. *)
      let types = match types_mode with
      | TypesAllowed -> true
      | TypesForbiddenByDefault ->
          Docblock.isDeclarationFile info ||
            begin match Docblock.flow info with
            | None
            | Some Docblock.OptOut -> false
            | Some Docblock.OptIn
            | Some Docblock.OptInWeak -> true
            end
      in
      (* don't bother to parse if types are disabled *)
      if types
      then Parse_ok (parse_source_file ~fail ~types ~use_strict content file)
      else Parse_skip Skip_non_flow_file
  )
  with
  | Parse_error.Error (first_parse_error::_) ->
    let err = Errors.parse_error_to_flow_error first_parse_error in
    Parse_err (Errors.ErrorSet.singleton err)
  | e ->
    let s = Printexc.to_string e in
    let msg = spf "unexpected parsing exception: %s" s in
    let loc = Loc.({ none with source = Some file }) in
    let err = Errors.(simple_error ~kind:ParseError loc msg) in
    Parse_err (Errors.ErrorSet.singleton err)

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer
  ~types_mode ~use_strict ~max_header_tokens
  parse_results
  file
: results =
  (* It turns out that sometimes files appear and disappear very quickly. Just
   * because someone told us that this file exists and needs to be parsed, it
   * doesn't mean it actually still exists. If anything goes wrong reading this
   * file, let's skip it. We don't need to notify our caller, since they'll
   * probably get the delete event anyway *)
  let content =
    let filename_string = string_of_filename file in
    try Some (cat filename_string)
    with e ->
      prerr_endlinef
        "Parsing service failed to cat %s, so skipping it. Exception: %s"
        filename_string
        (Printexc.to_string e);
      None in
  match content with
  | Some content ->
      begin match get_docblock ~max_tokens:max_header_tokens file content with
      | None, info ->
        begin match (do_parse ~types_mode ~use_strict ~info content file) with
        | Parse_ok ast ->
            (* Consider the file unchanged if its reparsing info is the same as
             * its old parsing info. A complication is that we don't want to
             * drop a .flow file, even if it is unchanged, since it might have
             * been added to the modified set simply because a corresponding
             * implementation file was also added. *)
            if not (Loc.check_suffix file Files.flow_ext)
              && ParserHeap.get_old file = Some (ast, info)
            then parse_results
            else begin
              ParserHeap.add file (ast, info);
              execute_hook file (Some ast);
              let parse_ok = FilenameSet.add file parse_results.parse_ok in
              { parse_results with parse_ok; }
            end
        | Parse_err converted ->
            execute_hook file None;
            let parse_fails = (file, info) :: parse_results.parse_fails in
            let parse_errors = converted :: parse_results.parse_errors in
            { parse_results with parse_fails; parse_errors; }
        | Parse_skip Skip_non_flow_file ->
            execute_hook file None;
            let parse_skips = (file, info) :: parse_results.parse_skips in
            { parse_results with parse_skips; }
        | Parse_skip Skip_resource_file ->
            execute_hook file None;
            let parse_resource_files =
              FilenameSet.add file parse_results.parse_resource_files in
            { parse_results with parse_resource_files; }
        end
      | Some docblock_errors, info ->
        execute_hook file None;
        let parse_fails = (file, info) :: parse_results.parse_fails in
        let parse_errors = docblock_errors :: parse_results.parse_errors in
        { parse_results with parse_fails; parse_errors; }
      end
  | None ->
      execute_hook file None;
      let info = Docblock.default_info in
      let parse_skips = (file, info) :: parse_results.parse_skips in
      { parse_results with parse_skips; }

(* merge is just memberwise union/concat of results *)
let merge r1 r2 =
  {
    parse_ok = FilenameSet.union r1.parse_ok r2.parse_ok;
    parse_skips = r1.parse_skips @ r2.parse_skips;
    parse_fails = r1.parse_fails @ r2.parse_fails;
    parse_errors = r1.parse_errors @ r2.parse_errors;
    parse_resource_files =
      FilenameSet.union r1.parse_resource_files r2.parse_resource_files;
  }

let opt_or_alternate opt alternate =
  match opt with
    | Some x -> x
    | None -> alternate

(* types_mode and use_strict aren't special, they just happen to be the ones that needed to be
overridden *)
let get_defaults ~types_mode ~use_strict options =
  let types_mode = opt_or_alternate
    types_mode
    (* force types when --all is set, but otherwise forbid them unless the file
       has @flow in it. *)
    (if Options.all options then TypesAllowed else TypesForbiddenByDefault)
  in
  let use_strict = opt_or_alternate
    use_strict
    (Options.modules_are_use_strict options)
  in
  let profile = Options.should_profile options in
  let max_header_tokens = Options.max_header_tokens options in
  types_mode, use_strict, profile, max_header_tokens

(***************************** public ********************************)

let next_of_filename_set workers filenames =
  MultiWorker.next workers (FilenameSet.elements filenames)

let parse
  ~types_mode ~use_strict ~profile ~max_header_tokens
  workers next
: results =
  let t = Unix.gettimeofday () in
  let results = MultiWorker.call
    workers
    ~job: (List.fold_left (reducer ~types_mode ~use_strict ~max_header_tokens))
    ~neutral: empty_result
    ~merge: merge
    ~next: next in

  if profile then
    let t2 = Unix.gettimeofday () in
    let ok_count = FilenameSet.cardinal results.parse_ok in
    let skip_count = List.length results.parse_skips in
    let fail_count = List.length results.parse_fails in
    let resource_file_count =
      FilenameSet.cardinal results.parse_resource_files in
    prerr_endlinef "parsed %d files (%d ok, %d skipped, %d failed, %d resource files) in %f"
      (ok_count + skip_count + fail_count)
      ok_count skip_count fail_count resource_file_count
      (t2 -. t)
  else ();

  results

let reparse ~types_mode ~use_strict ~profile ~max_header_tokens ~options workers files =
  (* save old parsing info for files *)
  ParserHeap.oldify_batch files;
  let next = next_of_filename_set workers files in
  let results =
    parse ~types_mode ~use_strict ~profile ~max_header_tokens workers next in
  let modified =
    FilenameSet.union results.parse_ok results.parse_resource_files in
  let modified = List.fold_left (fun acc (fail, _) ->
    FilenameSet.add fail acc
  ) modified results.parse_fails in
  let modified = List.fold_left (fun acc (skip, _) ->
    FilenameSet.add skip acc
  ) modified results.parse_skips in
  (* discard old parsing info for modified files *)
  ParserHeap.remove_old_batch modified;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  ParserHeap.revive_batch unchanged;
  SharedMem_js.collect options `gentle;
  modified, results

let parse_with_defaults ?types_mode ?use_strict options workers next =
  let types_mode, use_strict, profile, max_header_tokens =
    get_defaults ~types_mode ~use_strict options
  in
  parse ~types_mode ~use_strict ~profile ~max_header_tokens workers next

let reparse_with_defaults options workers files =
  let types_mode, use_strict, profile, max_header_tokens =
    get_defaults ~types_mode:None ~use_strict:None options
  in
  reparse ~types_mode ~use_strict ~profile ~max_header_tokens ~options workers files

let has_ast file =
  ParserHeap.mem file

let get_ast_unsafe file =
  let ast, _ = ParserHeap.find_unsafe file in
  ast

let get_ast file =
  if has_ast file then
    Some (get_ast_unsafe file)
  else
    None

let get_ast_and_info_unsafe file =
  ParserHeap.find_unsafe file

let remove_asts files =
  ParserHeap.remove_batch files;
  FilenameSet.iter delete_file files
