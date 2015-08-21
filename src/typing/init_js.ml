(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module sets up the definitions for JavaScript globals. Eventually, this
   module should become redundant: we should be able to automatically interpret
   TypeScript type definition files for these and many other primitives. That
   said, in some cases handcoding may turn out to be necessary because the type
   system is not powerful enough to encode the invariants of a library
   function. In any case, this part of the design must be revisited in the
   future. *)

open Utils
open Utils_js
open Sys_utils

(* helper - slap a big banner onto an error.
   yucky but library errors tend to screw lots of other stuff up,
   so we want them to stand out. They're also forced to the
   top of get_errors () *)
let add_banner banner err = Errors_js.(
  let level, messages, traces = err in
  let banner = match List.hd messages with
    | BlameM (loc, desc) -> BlameM (loc, banner)
    | CommentM desc -> CommentM banner in
  let messages = banner :: messages in
  level, messages, traces
)

(* parse all library files, as supplied by Files_js.get_lib_files.
   use passed function to report errors.
   returns list of (filename, Some ast | None) depending on success.
 *)
let parse_lib save_parse_errors =
  Files_js.get_lib_files ()
  |> SSet.elements
  |> List.map (fun lib_file ->
    try Parsing_service_js.(
      let lib_content = cat lib_file in
      match do_parse lib_content lib_file with
      | OK ast ->
        lib_file, Some ast
      | Err errors ->
        (* patch a big banner onto each library parse error *)
        let errors = Errors_js.(ErrorSet.fold (fun err acc ->
          ErrorSet.add (add_banner "Library parse error:" err) acc
        ) errors ErrorSet.empty) in
        save_parse_errors lib_file errors;
        lib_file, None
    )
    with _ ->
      failwith (spf "Can't read library definitions file %s, exiting." lib_file)
  )

(* parse all lib files, and do local inference on those successfully parsed.
   returns list of (filename, success) pairs
 *)
let init_lib save_parse_errors save_infer_errors save_suppressions =
  let save_bannerized_infer_errors file errors =
    let errors = Errors_js.(ErrorSet.fold (fun err acc ->
      ErrorSet.add (add_banner "Library type error:" err) acc
    ) errors ErrorSet.empty) in
    save_infer_errors file errors
  in
  (parse_lib save_parse_errors) |> List.fold_left (
    fun acc (file, ast) ->
      match ast with
      | Some (_, statements, comments) ->
        Type_inference_js.init_lib_file
          file
          statements
          comments
          (save_bannerized_infer_errors file)
          (save_suppressions file);
        (file, true) :: acc
      | None ->
        (file, false) :: acc
    ) []

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
 *)
let init save_parse_errors save_infer_errors save_suppressions =
  let res = init_lib save_parse_errors save_infer_errors save_suppressions in
  Flow_js.Cache.clear();
  let cx = Flow_js.master_cx () in
  let reason = Reason_js.builtin_reason "module" in
  let builtin_module = Type_inference_js.mk_object cx reason in
  Flow_js.(flow cx (builtin_module, builtins ()));
  res
