(**
 * Copyright (c) 2014, Facebook, Inc.
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

(* results of parse job, returned by parse and reparse *)
type results =
  FilenameSet.t *           (* successfully parsed files *)
  filename list *           (* list of failed files *)
  Errors_js.ErrorSet.t list (* parallel list of error sets *)

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

let parse_source_file ~fail ~types_mode content file =
  let filename = string_of_filename file in
  let info = Docblock.extract filename content in
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

    (* Allow types based on `types_mode`, using the @flow annotation in the
       file header if possible. *)
    types =
      match types_mode with
      | TypesAllowed -> true
      | TypesForbiddenByDefault ->
          Docblock.isDeclarationFile info ||
            begin match Docblock.flow info with
            | None -> false
            | Some Docblock.OptIn
            | Some Docblock.OptInWeak -> true
            (* parse types even in @noflow mode; they just don't get checked *)
            | Some Docblock.OptOut -> true
            end;
  }) in
  let ast, parse_errors =
    Parser_flow.program_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);
  ast, info

let parse_json_file ~fail content file =
  let parse_options = Some Parser_env.({
    esproposal_class_instance_fields = false;
    esproposal_class_static_fields = false;
    esproposal_decorators = false;
    esproposal_export_star_as = false;
    types = true;
  }) in
  let info = Docblock.default_info in

  (* parse the file as JSON, then munge the AST to convert from an object
     into a `module.exports = {...}` statement *)
  let ast, parse_errors =
    Parser_flow.json_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);

  let open Parser_flow.Ast in
  let loc_none = Loc.none in
  let loc = fst ast in
  let expr = loc, (Expression.Object (snd ast)) in
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
  (loc, [statement], comments), info

let do_parse ?(fail=true) ~types_mode content file =
  try (
    match file with
    | Loc.JsonFile _ ->
      OK (parse_json_file ~fail content file)
    | _ ->
      OK (parse_source_file ~fail ~types_mode content file)
  )
  with
  | Parse_error.Error parse_errors ->
    let converted = List.fold_left (fun acc err ->
      Errors_js.(ErrorSet.add (parse_error_to_flow_error err) acc)
    ) Errors_js.ErrorSet.empty parse_errors in
    Err converted
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
    Err (Errors_js.ErrorSet.singleton err)

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer ~types_mode (ok, fails, errors) file =
  let content = cat (string_of_filename file) in
  match (do_parse ~types_mode content file) with
  | OK (ast, info) ->
      (* Consider the file unchanged if its reparsing info is the same as its
         old parsing info. A complication is that we don't want to drop a .flow
         file, even if it is unchanged, since it might have been added to the
         modified set simply because a corresponding implementation file was
         also added. *)
      if not (Loc.check_suffix file FlowConfig.flow_ext)
        && ParserHeap.get_old file = Some (ast, info)
      then (ok, fails, errors)
      else begin
        ParserHeap.add file (ast, info);
        execute_hook file (Some ast);
        (FilenameSet.add file ok, fails, errors)
      end
  | Err converted ->
      execute_hook file None;
      (ok, file :: fails, converted :: errors)

(* merge is just memberwise union/concat of results *)
let merge (ok1, fail1, errors1) (ok2, fail2, errors2) =
  (FilenameSet.union ok1 ok2, fail1 @ fail2, errors1 @ errors2)

(***************************** public ********************************)

let parse ~types_mode ~profile workers next =
  let t = Unix.gettimeofday () in
  let ok, fail, errors = MultiWorker.call
    workers
    ~job: (List.fold_left (reducer ~types_mode ))
    ~neutral: (FilenameSet.empty, [], [])
    ~merge: merge
    ~next: next in

  if profile then
    let t2 = Unix.gettimeofday () in
    prerr_endlinef "parsed %d + %d files in %f"
      (FilenameSet.cardinal ok) (List.length fail) (t2 -. t)
  else ();

  (ok, fail, errors)

let reparse ~types_mode ~profile workers files =
  (* save old parsing info for files *)
  ParserHeap.oldify_batch files;
  let next = Bucket.make (FilenameSet.elements files) in
  let ok, fails, errors = parse ~types_mode ~profile workers next in
  let modified =
    List.fold_left (fun acc fail -> FilenameSet.add fail acc) ok fails in
  (* discard old parsing info for modified files *)
  ParserHeap.remove_old_batch modified;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  ParserHeap.revive_batch unchanged;
  SharedMem.collect `gentle;
  modified, (ok, fails, errors)

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
