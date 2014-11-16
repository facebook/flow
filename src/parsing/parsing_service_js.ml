(**
 *  Copyright 2012-2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

open Utils
module Errors = Errors_js
module Reason = Reason_js
module Ast = Spider_monkey_ast

(* results of parse job, returned by parse and reparse *)
type results =
  SSet.t *                  (* successfully parsed files *)
  string list *             (* list of failed files *)
  Errors.ErrorSet.t list    (* parallel list of error sets *)

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ParserHeap = SharedMem.WithCache (String) (struct
    type t = Spider_monkey_ast.program
    let prefix = Prefix.make()
  end)

let is_flow contents =
  contents
  |> split_lines
  |> List.exists (fun line -> Str.string_match (Str.regexp ".*@flow") line 0)

let match_flow file =
  (Files_js.is_lib_file file) || is_flow (cat file)

let in_flow file =
  Modes_js.(modes.all) || match_flow file

let do_parse content file =
  try (
    let ast, parse_errors = Parser_flow.program_file content file in
    assert (parse_errors = []);
    Some ast, None
  )
  with
  | Parse_error.Error parse_errors ->
    if in_flow file then
      let converted = List.fold_left (fun acc err ->
        Errors.(ErrorSet.add (parse_error_to_flow_error err) acc)
      ) Errors.ErrorSet.empty parse_errors in
      None, Some converted
    else None, None
  | e ->
    if in_flow file then
      let s = Printexc.to_string e in
      let msg = spf "unexpected parsing exception: %s" s in
      let reason = Reason.new_reason "" (Pos.make_from
        (Relative_path.create Relative_path.Dummy file)) in
      let err = Errors.ERROR, [reason, msg] in
      None, Some (Errors.ErrorSet.singleton err)
    else None, None

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer init_modes (ok, fails, errors) file =
  init_modes ();
  let content = cat file in
  match (do_parse content file) with
  | Some ast, None ->
      ParserHeap.add file ast;
      (SSet.add file ok, fails, errors)
  | None, Some converted ->
      (ok, file :: fails, converted :: errors)
  | None, None ->
      (ok, fails, errors)
  | _ -> assert false

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
    prerr_endline (spf "parsed %d + %d files in %f"
      (SSet.cardinal ok) (List.length fail) (t2 -. t))
  else ();

  (ok, fail, errors)

let reparse workers files init_modes =
  ParserHeap.remove_batch files;
  SharedMem.collect();
  let next = Bucket.make (SSet.elements files) in
  parse workers next init_modes

let get_ast_unsafe file =
  ParserHeap.find_unsafe file

let remove_asts files =
  ParserHeap.remove_batch files
