(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Utils_js
open Sys_utils
module Reason = Reason_js
module Ast = Spider_monkey_ast

type result =
  | Parse_ok of Spider_monkey_ast.program
  | Parse_err of Errors_js.ErrorSet.t
  | Parse_skip

(* results of parse job, returned by parse and reparse *)
type results =
  FilenameSet.t *                (* successfully parsed files *)
  (filename * Docblock.t) list * (* list of skipped files *)
  (filename * Docblock.t) list * (* list of failed files *)
  Errors_js.ErrorSet.t list      (* parallel list of error sets *)

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ParserHeap = SharedMem.WithCache (Loc.FilenameKey) (struct
    type t = (Spider_monkey_ast.program * Docblock.t)
    let prefix = Prefix.make()
  end)

let (parser_hook: (filename -> Ast.program -> unit) list ref) = ref []
let call_on_success f = parser_hook := f :: !parser_hook

let execute_hook file ast =
  let ast = match ast with
  | None ->
     let empty_ast, _ = Parser_flow.parse_program true (Some file) "" in
     empty_ast
  | Some ast -> ast
  in
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

let parse_source_file ~fail ~types content file =
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
    types = types
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
  }) in

  (* parse the file as JSON, then munge the AST to convert from an object
     into a `module.exports = {...}` statement *)
  let ast, parse_errors =
    Parser_flow.json_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);

  let open Parser_flow.Ast in
  let loc_none = Loc.none in
  let module_exports = loc_none, Expression.(Member { Member.
    _object = loc_none, Identifier (loc_none, { Identifier.
      name = "module";
      typeAnnotation = None;
      optional = false;
    });
    property = Member.PropertyIdentifier (loc_none, { Identifier.
      name = "exports";
      typeAnnotation = None;
      optional = false;
    });
    computed = false;
  }) in
  let expr =
    match ast with
    | Parser_flow.JSONArray (loc, arr) -> (loc, Expression.Array arr)
    | Parser_flow.JSONObject (loc, obj) -> (loc, Expression.Object obj)
  in
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

let get_docblock file content =
  match file with
  | Loc.JsonFile _ -> Docblock.default_info
  | _ ->
    let filename = string_of_filename file in
    Docblock.extract filename content

let do_parse ?(fail=true) ~types_mode ~info content file =
  try (
    match file with
    | Loc.JsonFile _ ->
      Parse_ok (parse_json_file ~fail content file)
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
      then Parse_ok (parse_source_file ~fail ~types content file)
      else Parse_skip
  )
  with
  | Parse_error.Error parse_errors ->
    let converted = List.fold_left (fun acc err ->
      Errors_js.(ErrorSet.add (parse_error_to_flow_error err) acc)
    ) Errors_js.ErrorSet.empty parse_errors in
    Parse_err converted
  | e ->
    let s = Printexc.to_string e in
    let msg = spf "unexpected parsing exception: %s" s in
    let loc = Loc.({ none with source = Some file }) in
    let err = Errors_js.({
      kind = ParseError;
      messages = [BlameM (loc, msg)];
      op = None;
      trace = []
    }) in
    Parse_err (Errors_js.ErrorSet.singleton err)

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer ~types_mode (ok, skips, fails, errors) file =
  let content = cat (string_of_filename file) in
  let info = get_docblock file content in
  match (do_parse ~types_mode ~info content file) with
  | Parse_ok ast ->
      (* Consider the file unchanged if its reparsing info is the same as its
         old parsing info. A complication is that we don't want to drop a .flow
         file, even if it is unchanged, since it might have been added to the
         modified set simply because a corresponding implementation file was
         also added. *)
      if not (Loc.check_suffix file FlowConfig.flow_ext)
        && ParserHeap.get_old file = Some (ast, info)
      then (ok, skips, fails, errors)
      else begin
        ParserHeap.add file (ast, info);
        execute_hook file (Some ast);
        (FilenameSet.add file ok, skips, fails, errors)
      end
  | Parse_err converted ->
      execute_hook file None;
      (ok, skips, (file, info) :: fails, converted :: errors)
  | Parse_skip ->
      execute_hook file None;
      (ok, (file, info) :: skips, fails, errors)

(* merge is just memberwise union/concat of results *)
let merge (ok1, skip1, fail1, errors1) (ok2, skip2, fail2, errors2) =
  (FilenameSet.union ok1 ok2, skip1 @ skip2, fail1 @ fail2, errors1 @ errors2)

(***************************** public ********************************)

let parse ~types_mode ~profile workers next =
  let t = Unix.gettimeofday () in
  let ok, skip, fail, errors = MultiWorker.call
    workers
    ~job: (List.fold_left (reducer ~types_mode ))
    ~neutral: (FilenameSet.empty, [], [], [])
    ~merge: merge
    ~next: next in

  if profile then
    let t2 = Unix.gettimeofday () in
    let ok_count = FilenameSet.cardinal ok in
    let skip_count = List.length skip in
    let fail_count = List.length fail in
    prerr_endlinef "parsed %d files (%d ok, %d skipped, %d failed) in %f"
      (ok_count + skip_count + fail_count)
      ok_count skip_count fail_count
      (t2 -. t)
  else ();

  (ok, skip, fail, errors)

let reparse ~types_mode ~profile workers files =
  (* save old parsing info for files *)
  ParserHeap.oldify_batch files;
  let next = Bucket.make (FilenameSet.elements files) in
  let ok, skips, fails, errors = parse ~types_mode ~profile workers next in
  let modified = List.fold_left (fun acc (fail, _) ->
    FilenameSet.add fail acc
  ) ok fails in
  let modified = List.fold_left (fun acc (skip, _) ->
    FilenameSet.add skip acc
  ) modified skips in
  (* discard old parsing info for modified files *)
  ParserHeap.remove_old_batch modified;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  ParserHeap.revive_batch unchanged;
  SharedMem.collect `gentle;
  modified, (ok, skips, fails, errors)

let has_ast file =
  ParserHeap.mem file

let get_ast_unsafe file =
  let ast, _ = ParserHeap.find_unsafe file in
  ast

let get_ast_and_info_unsafe file =
  ParserHeap.find_unsafe file

let remove_asts files =
  ParserHeap.remove_batch files;
  FilenameSet.iter delete_file files
