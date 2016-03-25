(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let neutral = (
  Relative_path.Map.empty, [],
  Relative_path.Set.empty
  )

let empty_file_info : FileInfo.t = {
  file_mode = None;
  FileInfo.funs = [];
  classes = [];
  typedefs = [];
  consts = [];
  comments = [];
  consider_names_just_for_autoload = false;
}

let legacy_php_file_info = ref (fun fn ->
  empty_file_info
)

(* Parsing a file without failing
 * acc is a file_info
 * errorl is a list of errors
 * error_files is Relative_path.Set.t of files that we failed to parse
 *)
let really_parse (acc, errorl, error_files) fn =
  let errorl', {Parser_hack.file_mode; comments; ast} =
    Errors.do_ begin fun () ->
      Parser_hack.from_file fn
    end
  in
  Parsing_hooks.dispatch_file_parsed_hook fn ast;
  if file_mode <> None then begin
    let funs, classes, typedefs, consts = Ast_utils.get_defs ast in
    Parser_heap.ParserHeap.write_through fn ast;
    let defs =
      {FileInfo.funs; classes; typedefs; consts; comments; file_mode;
       consider_names_just_for_autoload = false}
    in
    let acc = Relative_path.Map.add acc ~key:fn ~data:defs in
    let errorl = List.rev_append errorl' errorl in
    let error_files =
      if errorl' = []
      then error_files
      else Relative_path.Set.add error_files fn
    in
    acc, errorl, error_files
  end
  else begin
    let info = try !legacy_php_file_info fn with _ -> empty_file_info in
    (* we also now keep in the file_info regular php files
     * as we need at least their names in hack build
     *)
    let acc = Relative_path.Map.add acc ~key:fn ~data:info in
    acc, errorl, error_files
  end

let parse (acc, errorl, error_files) fn =
  (* Ugly hack... hack build requires that we keep JS files in our
   * files_info map, but we don't want to actually read them from disk
   * because we don't do anything with them. See also
   * ServerMain.Program.make_next_files *)
  if FindUtils.is_php (Relative_path.suffix fn) then
    really_parse (acc, errorl, error_files) fn
  else
    let info = empty_file_info in
    let acc = Relative_path.Map.add acc ~key:fn ~data:info in
    acc, errorl, error_files

(* Merging the results when the operation is done in parallel *)
let merge_parse
    (acc1, status1, files1)
    (acc2, status2, files2) =
  Relative_path.Map.union acc1 acc2,
  List.rev_append status1 status2,
  Relative_path.Set.union files1 files2

let parse_files acc fnl =
  let parse =
    if !Utils.profile
    then (fun acc fn ->
      let t = Unix.gettimeofday () in
      let result = parse acc fn in
      let t' = Unix.gettimeofday () in
      let msg =
        Printf.sprintf "%f %s [parsing]" (t' -. t) (Relative_path.suffix fn) in
      !Utils.log msg;
      result)
    else parse in
  List.fold_left fnl ~init:acc ~f:parse

let parse_parallel workers get_next =
  MultiWorker.call
      workers
      ~job:parse_files
      ~neutral:neutral
      ~merge:merge_parse
      ~next:get_next

(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let go workers ~get_next =
  let fast, errorl, failed_parsing =
    parse_parallel workers get_next in
  Parsing_hooks.dispatch_parse_task_completed_hook
    (Relative_path.Map.keys fast);
  fast, errorl, failed_parsing
