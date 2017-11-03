(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module sets up the definitions for JavaScript globals. Eventually, this
   module should become redundant: we should be able to automatically interpret
   TypeScript type definition files for these and many other primitives. That
   said, in some cases handcoding may turn out to be necessary because the type
   system is not powerful enough to encode the invariants of a library
   function. In any case, this part of the design must be revisited in the
   future. *)

open Utils_js

module Files = Files
module Flow = Flow_js
module Parsing = Parsing_service_js
module Infer = Type_inference_js

let parse_lib_file options file =
  (* types are always allowed in lib files *)
  let types_mode = Parsing.TypesAllowed in
  (* lib files are always "use strict" *)
  let use_strict = true in
  try
    let lib_file = File_key.LibFile file in
    let filename_set = FilenameSet.singleton lib_file in
    let next = Parsing.next_of_filename_set (* workers *) None filename_set in
    let results =
      Parsing.parse_with_defaults
        ~types_mode
        ~use_strict
        options
        (* workers *) None
        next
    in
    if not (FilenameSet.is_empty results.Parsing.parse_ok) then
      Parsing.Parse_ok (Parsing.get_ast_unsafe lib_file)
    else if List.length results.Parsing.parse_fails > 0 then
      let _, _, parse_fails = List.hd results.Parsing.parse_fails in
      Parsing.Parse_fail parse_fails
    else if List.length results.Parsing.parse_skips > 0 then
      Parsing.Parse_skip Parsing.Skip_non_flow_file
    else
      failwith "Internal error: no parse results found"
  with _ -> failwith (
    spf "Can't read library definitions file %s, exiting." file
  )

(* process all lib files: parse, infer, and add the symbols they define
   to the builtins object.

   Note: we support overrides of definitions found earlier in the list of
   files by those of the same name found in later ones, so caller must
   preserve lib path declaration order in the (flattened) list of files
   passed.

   returns list of (filename, success, errors, suppressions) tuples
 *)
let load_lib_files ~master_cx ~options files =

  let verbose = Options.verbose options in

  (* iterate in reverse override order *)
  let _, result = List.rev files |> List.fold_left (

    fun (exclude_syms, results) file ->

      let lib_file = File_key.LibFile file in
      let lint_severities = options.Options.opt_lint_severities in
      match parse_lib_file options file with
      | Parsing.Parse_ok ast ->

        let metadata =
          let open Context in
          let metadata = metadata_of_options options in
          let local_metadata = { metadata.local_metadata with checked = false; weak = false; } in
          { metadata with local_metadata }
        in

        let cx, syms = Infer.infer_lib_file
          ~metadata ~exclude_syms ~lint_severities
          lib_file ast
        in

        let errs, suppressions, severity_cover = Merge_js.merge_lib_file cx master_cx in

        (if verbose != None then
          prerr_endlinef "load_lib %s: added symbols { %s }"
            file (String.concat ", " syms));

        (* symbols loaded from this file are suppressed
           if found in later ones *)
        let exclude_syms = SSet.union exclude_syms (SSet.of_list syms) in
        let result = (lib_file, true, errs, suppressions, severity_cover) in
        exclude_syms, (result :: results)

      | Parsing.Parse_fail fail ->
        let errors = match fail with
        | Parsing.Parse_error error ->
          Inference_utils.set_of_parse_error ~source_file:lib_file error
        | Parsing.Docblock_errors errs ->
          Inference_utils.set_of_docblock_errors ~source_file:lib_file errs
        in
        let result = lib_file, false, errors, Error_suppressions.empty,
          ExactCover.file_cover lib_file lint_severities in
        exclude_syms, (result :: results)

      | Parsing.Parse_skip
          (Parsing.Skip_non_flow_file | Parsing.Skip_resource_file) ->
        (* should never happen *)
        let errs = Errors.ErrorSet.empty in
        let suppressions = Error_suppressions.empty in
        let severity_cover = ExactCover.file_cover lib_file lint_severities in
        let result = lib_file, false, errs, suppressions, severity_cover in
        exclude_syms, (result :: results)

    ) (SSet.empty, [])

  in result

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
 *)
let init ~options lib_files =

  let master_cx =
    let metadata =
      let open Context in
      let metadata = metadata_of_options options in
      let local_metadata = { metadata.local_metadata with checked = false; weak = false; } in
      { metadata with local_metadata }
    in
    Flow.fresh_context metadata File_key.Builtins Files.lib_module_ref
  in

  let result = load_lib_files ~master_cx ~options lib_files in

  Flow.Cache.clear();
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Obj_type.mk master_cx reason in
  Flow.flow_t master_cx (builtin_module, Flow.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context master_cx [Files.lib_module_ref] |> ignore;

  (* store master signature context to heap *)
  Context_cache.add_sig ~audit:Expensive.ok master_cx;

  result
