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

module Files = Files_js
module Parsing = Parsing_service_js
module Infer = Type_inference_js

let parse_lib_file save_parse_errors file =
  (* types are always allowed in lib files *)
  let types_mode = Parsing.TypesAllowed in
  try Parsing.(
    let lib_content = cat file in
    let lib_file = Loc.LibFile file in
    lib_file, match do_parse ~types_mode lib_content lib_file with
    | OK (ast, _) ->
      Some ast
    | Err errors ->
      save_parse_errors lib_file errors;
      None
  )
  with _ -> failwith (
    spf "Can't read library definitions file %s, exiting." file
  )

(* process all lib files: parse, infer, and add the symbols they define
   to the builtins object.

   Note: we support overrides of definitions found earlier in the list of
   files by those of the same name found in later ones, so caller must
   preserve lib path declaration order in the (flattened) list of files
   passed.

   returns list of (filename, success) pairs
 *)
let load_lib_files files ~verbose
  save_parse_errors save_infer_errors save_suppressions =

  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

    fun (exclude_syms, result) file ->

      match parse_lib_file save_parse_errors file with
      | lib_file, Some (_, statements, comments) ->

        let cx, syms = Infer.infer_lib_file
          ~verbose ~exclude_syms
          lib_file statements comments
        in

        Merge_js.merge_lib_file cx save_infer_errors save_suppressions;

        (if verbose != None then
          prerr_endlinef "load_lib %s: added symbols { %s }"
            (Loc.string_of_filename lib_file)
            (String.concat ", " syms));

        (* symbols loaded from this file are suppressed
           if found in later ones *)
        let exclude_syms = SSet.union exclude_syms (set_of_list syms) in
        let result = (lib_file, true) :: result in
        exclude_syms, result

      | lib_file, None ->
        exclude_syms, ((lib_file, false) :: result)

    ) (SSet.empty, [])

  in result

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
 *)
let init ~verbose save_parse_errors save_infer_errors save_suppressions =

  let lib_files = Files.get_lib_files () in
  let result = load_lib_files lib_files ~verbose
    save_parse_errors save_infer_errors save_suppressions in

  Flow_js.Cache.clear();
  let master_cx = Flow_js.master_cx () in
  let reason = Reason_js.builtin_reason "module" in
  let builtin_module = Infer.mk_object master_cx reason in
  Flow_js.flow_t master_cx (builtin_module, Flow_js.builtins master_cx);
  Flow_js.ContextOptimizer.sig_context [master_cx];

  result
