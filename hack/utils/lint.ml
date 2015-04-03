(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(* These severity levels are based on those provided by Arcanist. "Advice"
 * means notify the user of the lint without requiring confirmation if the lint
 * is benign; "Warning" will raise a confirmation prompt if the lint applies to
 * a line that was changed in the given diff; and "Error" will always raise a
 * confirmation prompt, regardless of where the lint occurs in the file. *)
type severity =
  | Error
  | Warning
  | Advice

let string_of_severity = function
  | Error -> "error"
  | Warning -> "warning"
  | Advice -> "advice"

type 'a t = {
  code : int;
  severity : severity;
  pos : 'a Pos.pos;
  message : string;
}

let (lint_list: Relative_path.t t list option ref) = ref None

let get_code {code; _} = code
let get_pos {pos; _} = pos

let add code severity pos message =
  if !Errors.is_hh_fixme pos code then () else begin
    let lint = { code; severity; pos; message } in
    match !lint_list with
    | Some lst -> lint_list := Some (lint :: lst)
    (* by default, we ignore lint errors *)
    | None -> ()
  end

let to_absolute {code; severity; pos; message} =
  {code; severity; pos = Pos.to_absolute pos; message}

let to_string lint =
  let code = Errors.error_code_to_string lint.code in
  Printf.sprintf "%s\n%s (%s)" (Pos.string lint.pos) lint.message code

let to_json {pos; code; severity; message} =
  let open Hh_json in
  let line, scol, ecol = Pos.info_pos pos in
  JAssoc [ "descr", JString message;
           "severity", JString (string_of_severity severity);
           "path",  JString pos.Pos.pos_file;
           "line",  JInt line;
           "start", JInt scol;
           "end",   JInt ecol;
           "code",  JInt code
         ]

module Codes = struct
  let lowercase_constant                    = 5001 (* DONT MODIFY!!!! *)
  let use_collection_literal                = 5002 (* DONT MODIFY!!!! *)
  let static_string                         = 5003 (* DONT MODIFY!!!! *)

  (* Values 5501 - 5999 are reserved for FB-internal use *)

  (* EXTEND HERE WITH NEW VALUES IF NEEDED *)
end

let lowercase_constant pos cst =
  let lower = String.lowercase cst in
  add Codes.lowercase_constant Warning pos
    (spf "Please use '%s' instead of '%s'" lower cst)

let use_collection_literal pos coll =
  let coll = strip_ns coll in
  add Codes.use_collection_literal Warning pos
    (spf "Use `%s {...}` instead of `new %s(...)`" coll coll)

let static_string ?(no_consts=false) pos =
  add Codes.static_string Warning pos begin
    if no_consts
    then
      "This should be a string literal so that lint can analyze it."
    else
      "This should be a string literal or string constant so that lint can "^
      "analyze it."
  end

let do_ f =
  let list_copy = !lint_list in
  lint_list := Some [];
  let result = f () in
  let out = match !lint_list with
    | Some lst -> lst
    | None -> assert false in
  lint_list := list_copy;
  List.rev out, result
