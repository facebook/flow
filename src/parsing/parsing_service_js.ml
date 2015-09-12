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
  SSet.t *                  (* successfully parsed files *)
  string list *             (* list of failed files *)
  Errors_js.ErrorSet.t list (* parallel list of error sets *)

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ParserHeap = SharedMem.WithCache (String) (struct
    type t = Spider_monkey_ast.program
    let prefix = Prefix.make()
  end)

(* . matches any character except the newline, so this is a little more
 * complicated than one might think *)
let flow_check_regexp = (Str.regexp "\\(.\\|\n\\)*@flow")

let is_flow content =
  Str.string_match flow_check_regexp content 0

let match_flow content file =
  (Files_js.is_lib_file file) || is_flow content

let in_flow content file =
  Modes_js.(modes.all) || match_flow content file

let (parser_hook: (string -> Ast.program -> unit) list ref) = ref []
let call_on_success f = parser_hook := f :: !parser_hook

let parse_options = Some {
  (**
   * We always parse decorators. The user-facing config option to ignore/warn
   * on them happens during inference time so a clean error can be surfaced.
   *)
  Parser_env.experimental_decorators = true;
}

let execute_hook file ast =
  let ast = match ast with
  | None ->
      let empty_ast, _ = Parser_flow.parse_program true ~parse_options (Some file) "" in
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

let do_parse ?(keep_errors=false) content file =
  try (
    let ast, parse_errors = Parser_flow.program_file ~parse_options content file in
    assert (parse_errors = []);
    OK ast
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
    let err = Errors_js.(ERROR, [BlameM (loc, msg)], []) in
    Err (Errors_js.ErrorSet.singleton err)

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer init_modes (ok, fails, errors) file =
  init_modes ();
  let content = cat file in
  match (do_parse content file) with
  | OK ast ->
      ParserHeap.add file ast;
      execute_hook file (Some ast);
      (SSet.add file ok, fails, errors)
  | Err converted ->
      execute_hook file None;
      (ok, file :: fails, converted :: errors)

(* merge is just memberwise union/concat of results *)
let merge (ok1, fail1, errors1) (ok2, fail2, errors2) =
  (SSet.union ok1 ok2, fail1 @ fail2, errors1 @ errors2)

(***************************** public ********************************)

let parse workers next init_modes =
  let t = Unix.gettimeofday () in
  let ok, fail, errors = MultiWorker.call
    workers
    ~job: (List.fold_left (reducer init_modes))
    ~neutral: (SSet.empty, [], [])
    ~merge: merge
    ~next: next in

  if Modes_js.(modes.profile) && not Modes_js.(modes.quiet) then
    let t2 = Unix.gettimeofday () in
    prerr_endlinef "parsed %d + %d files in %f"
      (SSet.cardinal ok) (List.length fail) (t2 -. t)
  else ();

  (ok, fail, errors)

let reparse workers files init_modes =
  ParserHeap.remove_batch files;
  SharedMem.collect `gentle;
  let next = Bucket.make (SSet.elements files) in
  parse workers next init_modes

let get_ast_unsafe file =
  ParserHeap.find_unsafe file

let remove_asts files =
  ParserHeap.remove_batch files;
  SSet.iter delete_file files
