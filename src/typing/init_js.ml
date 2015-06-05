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
open Sys_utils

module Ast = Spider_monkey_ast
module Errors = Errors_js
module Files = Files_js
module Flow = Flow_js
module Reason = Reason_js
module TI = Type_inference_js

let parse_lib () =
  Files_js.get_lib_files ()
  |> SSet.elements
  |> List.map (fun lib_file ->
    try (
      let lib_content = cat lib_file in
      match (Parsing_service_js.do_parse lib_content lib_file) with
      | Some ast, _ -> lib_file, ast
      | _, Some err -> Errors_js.print_error_summary true (Errors_js.to_list err); assert false
      | _ -> assert false
    )
    with _ ->
      failwith (spf "Can't read library definitions file %s, exiting." lib_file)
  )

let init_lib save_errors save_suppressions =
  parse_lib () |> List.iter (fun (file, ast) ->
    let _, statements, comments = ast in
    Type_inference_js.init
      file
      statements
      comments
      (save_errors file)
      (save_suppressions file)
  )

let init save_errors save_suppressions =
  init_lib save_errors save_suppressions;
  Flow.Cache.clear();
  let cx = Flow.master_cx in
  let reason = Reason.builtin_reason "module" in
  let builtin_module = TI.mk_object cx reason in
  Flow.flow cx (builtin_module, Flow.builtins)
