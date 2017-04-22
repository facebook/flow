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

type result =
  | Parse_ok of Ast.program
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_failure =
  | Docblock_errors of Docblock.error list
  | Parse_error of (Loc.t * Parse_error.t)

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: FilenameSet.t;

  (* list of skipped files *)
  parse_skips: (filename * Docblock.t) list;

  (* list of failed files *)
  parse_fails: (filename * Docblock.t * parse_failure) list;
}

let empty_result = {
  parse_ok = FilenameSet.empty;
  parse_skips = [];
  parse_fails = [];
}

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ASTHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
    type t = Ast.program
    let prefix = Prefix.make()
    let description = "AST"
end)

module DocblockHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
    type t = Docblock.t
    let prefix = Prefix.make()
    let description = "Docblock"
end)

module RequiresHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
    type t = Loc.t SMap.t
    let prefix = Prefix.make()
    let description = "Requires"
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

  let open Ast in
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
      };
      directive = None;
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

let error_of_docblock_error (loc, err) =
  Errors.mk_error ~kind:Errors.ParseError [loc, [string_of_docblock_error err]]

let set_of_docblock_errors errors =
  List.fold_left (fun acc err ->
    Errors.ErrorSet.add (error_of_docblock_error err) acc
  ) Errors.ErrorSet.empty errors

let error_of_parse_error (loc, err) =
  Errors.mk_error ~kind:Errors.ParseError [loc, [Parse_error.PP.error err]]

let set_of_parse_error error =
  Errors.ErrorSet.singleton (error_of_parse_error error)

let get_docblock
  ~max_tokens file content
: Docblock.error list * Docblock.t =
  match file with
  | Loc.ResourceFile _
  | Loc.JsonFile _ -> [], Docblock.default_info
  | _ -> Docblock.extract ~max_tokens file content

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
    Parse_fail (Parse_error first_parse_error)
  | e ->
    let s = Printexc.to_string e in
    let loc = Loc.({ none with source = Some file }) in
    let err = loc, Parse_error.Assertion s in
    Parse_fail (Parse_error err)

let calc_requires ast is_react =
  let mapper = new Require.mapper is_react in
  let _ = mapper#program ast in
  mapper#requires

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
      | [], info ->
        begin match (do_parse ~types_mode ~use_strict ~info content file) with
        | Parse_ok ast ->
            (* Consider the file unchanged if its reparsing info is the same as
             * its old parsing info. A complication is that we don't want to
             * drop a .flow file, even if it is unchanged, since it might have
             * been added to the modified set simply because a corresponding
             * implementation file was also added. *)
            if not (Loc.check_suffix file Files.flow_ext)
              && ASTHeap.get_old file = Some ast
            then parse_results
            else begin
              ASTHeap.add file ast;
              DocblockHeap.add file info;
              let require_loc = calc_requires ast (info.Docblock.jsx = None) in
              RequiresHeap.add file require_loc;
              execute_hook file (Some ast);
              let parse_ok = FilenameSet.add file parse_results.parse_ok in
              { parse_results with parse_ok; }
            end
        | Parse_fail converted ->
            execute_hook file None;
            let fail = (file, info, converted) in
            let parse_fails = fail :: parse_results.parse_fails in
            { parse_results with parse_fails; }
        | Parse_skip Skip_non_flow_file
        | Parse_skip Skip_resource_file ->
            execute_hook file None;
            let parse_skips = (file, info) :: parse_results.parse_skips in
            { parse_results with parse_skips; }
        end
      | docblock_errors, info ->
        execute_hook file None;
        let fail = (file, info, Docblock_errors docblock_errors) in
        let parse_fails = fail :: parse_results.parse_fails in
        { parse_results with parse_fails; }
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
    prerr_endlinef "parsed %d files (%d ok, %d skipped, %d failed) in %f"
      (ok_count + skip_count + fail_count)
      ok_count skip_count fail_count
      (t2 -. t)
  else ();

  results

let reparse ~types_mode ~use_strict ~profile ~max_header_tokens ~options workers files =
  (* save old parsing info for files *)
  ASTHeap.oldify_batch files;
  DocblockHeap.oldify_batch files;
  RequiresHeap.oldify_batch files;
  let next = next_of_filename_set workers files in
  let results =
    parse ~types_mode ~use_strict ~profile ~max_header_tokens workers next in
  let modified = results.parse_ok in
  let modified = List.fold_left (fun acc (fail, _, _) ->
    FilenameSet.add fail acc
  ) modified results.parse_fails in
  let modified = List.fold_left (fun acc (skip, _) ->
    FilenameSet.add skip acc
  ) modified results.parse_skips in
  (* discard old parsing info for modified files *)
  ASTHeap.remove_old_batch modified;
  DocblockHeap.remove_old_batch modified;
  RequiresHeap.remove_old_batch modified;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  ASTHeap.revive_batch unchanged;
  DocblockHeap.revive_batch unchanged;
  RequiresHeap.revive_batch unchanged;
  SharedMem_js.collect options `gentle;
  modified, results

let parse_with_defaults ?types_mode ?use_strict options workers next =
  let types_mode, use_strict, profile, max_header_tokens =
    get_defaults ~types_mode ~use_strict options
  in
  parse ~types_mode ~use_strict ~profile ~max_header_tokens workers next

let reparse_with_defaults ?types_mode ?use_strict options workers files =
  let types_mode, use_strict, profile, max_header_tokens =
    get_defaults ~types_mode ~use_strict options
  in
  reparse ~types_mode ~use_strict ~profile ~max_header_tokens ~options workers files

let has_ast = ASTHeap.mem

let get_ast = ASTHeap.get

let get_ast_unsafe = ASTHeap.find_unsafe

let get_docblock_unsafe = DocblockHeap.find_unsafe

let get_requires_unsafe = RequiresHeap.find_unsafe

let remove_batch files =
  ASTHeap.remove_batch files;
  DocblockHeap.remove_batch files;
  RequiresHeap.remove_batch files;
  FilenameSet.iter delete_file files
