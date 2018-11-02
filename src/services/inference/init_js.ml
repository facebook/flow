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
  try%lwt
    let lib_file = File_key.LibFile file in
    let filename_set = FilenameSet.singleton lib_file in
    let next = Parsing.next_of_filename_set (* workers *) None filename_set in
    let%lwt results = Parsing.parse_with_defaults
      ~types_mode
      ~use_strict
      options
      (* workers *) None
      next
    in
    Lwt.return (
      if not (FilenameMap.is_empty results.Parsing.parse_ok) then
        let ast = Parsing_heaps.get_ast_unsafe lib_file in
        let file_sig = Parsing_heaps.get_file_sig_unsafe lib_file in
        Parsing.Parse_ok (ast, file_sig)
      else if List.length results.Parsing.parse_fails > 0 then
        let _, _, parse_fails = List.hd results.Parsing.parse_fails in
        Parsing.Parse_fail parse_fails
      else if List.length results.Parsing.parse_skips > 0 then
        Parsing.Parse_skip Parsing.Skip_non_flow_file
      else
        failwith "Internal error: no parse results found"
    )
  with _ ->
    failwith (spf "Can't read library definitions file %s, exiting." file)

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
  let%lwt (_, result) =
    List.rev files
    |> Lwt_list.fold_left_s (
      fun (exclude_syms, results) file ->

        let lib_file = File_key.LibFile file in
        let lint_severities = options.Options.opt_lint_severities in
        let file_options = Options.file_options options in
        let%lwt result = parse_lib_file options file in
        Lwt.return (match result with
        | Parsing.Parse_ok (ast, file_sig) ->

          let metadata =
            let open Context in
            let metadata = metadata_of_options options in
            { metadata with checked = false; weak = false }
          in

          let sig_cx = Context.make_sig () in
          let cx = Context.make sig_cx metadata lib_file Files.lib_module_ref in
          Flow.mk_builtins cx;

          let syms = Infer.infer_lib_file cx ast
            ~exclude_syms ~lint_severities ~file_options:(Some file_options) ~file_sig:(File_sig.abstractify_locs file_sig)
          in

          Context.merge_into (Context.sig_cx master_cx) sig_cx;

          let () =
            let from_t = Context.find_module master_cx Files.lib_module_ref in
            let to_t = Context.find_module cx Files.lib_module_ref in
            Flow.flow_t master_cx (from_t, to_t)
          in

          let errors = Context.errors cx in
          let errors =
            if options.Options.opt_enforce_well_formed_exports then
              Inference_utils.set_of_file_sig_tolerable_errors
                ~source_file:lib_file
                file_sig.File_sig.With_Loc.tolerable_errors
              |> Errors.ErrorSet.union errors
            else
              errors
          in
          let suppressions = Context.error_suppressions cx in
          let severity_cover = Context.severity_cover cx in

          Context.remove_all_errors cx;
          Context.remove_all_error_suppressions cx;
          Context.remove_all_lint_severities cx;

          (if verbose != None then
            prerr_endlinef "load_lib %s: added symbols { %s }"
              file (String.concat ", " syms));

          (* symbols loaded from this file are suppressed
             if found in later ones *)
          let exclude_syms = SSet.union exclude_syms (SSet.of_list syms) in
          let result = (lib_file, true, errors, suppressions, severity_cover) in
          exclude_syms, (result :: results)

        | Parsing.Parse_fail fail ->
          let errors = match fail with
          | Parsing.Parse_error error ->
            Inference_utils.set_of_parse_error ~source_file:lib_file error
          | Parsing.Docblock_errors errs ->
            Inference_utils.set_of_docblock_errors ~source_file:lib_file errs
          | Parsing.File_sig_error error ->
            Inference_utils.set_of_file_sig_error ~source_file:lib_file error
          in
          let severity_cover =
            Utils_js.FilenameMap.singleton
              lib_file
              (ExactCover.file_cover lib_file lint_severities)
          in
          let result = lib_file, false, errors, Error_suppressions.empty, severity_cover in
          exclude_syms, (result :: results)

        | Parsing.Parse_skip
            (Parsing.Skip_non_flow_file | Parsing.Skip_resource_file) ->
          (* should never happen *)
          let errs = Errors.ErrorSet.empty in
          let suppressions = Error_suppressions.empty in
          let severity_cover =
            Utils_js.FilenameMap.singleton
              lib_file
              (ExactCover.file_cover lib_file lint_severities)
          in
          let result = lib_file, false, errs, suppressions, severity_cover in
          exclude_syms, (result :: results)
        )
      ) (SSet.empty, [])
  in
  Lwt.return result

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
 *)
let init ~options lib_files =
  let master_cx =
    let sig_cx = Context.make_sig () in
    let metadata =
      let open Context in
      let metadata = metadata_of_options options in
      { metadata with checked = false; weak = false }
    in
    Context.make sig_cx metadata File_key.Builtins Files.lib_module_ref
  in

  Flow_js.mk_builtins master_cx;

  let%lwt result = load_lib_files ~master_cx ~options lib_files in

  Flow.Cache.clear();
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Obj_type.mk master_cx reason in
  Flow.flow_t master_cx (builtin_module, Flow.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context master_cx [Files.lib_module_ref] |> ignore;

  Context.clear_intermediates master_cx;

  (* store master signature context to heap *)
  Context_heaps.Init_master_context_mutator.add_master_sig ~audit:Expensive.ok master_cx;

  Lwt.return result
