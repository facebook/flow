(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

let is_ok { Parsing.parse_ok; _ } = not (FilenameMap.is_empty parse_ok)

let is_fail { Parsing.parse_fails; _ } = not (Base.List.is_empty parse_fails)

let is_skip { Parsing.parse_skips; parse_not_found_skips; _ } =
  (not (Base.List.is_empty parse_skips)) || not (FilenameSet.is_empty parse_not_found_skips)

type lib_result =
  | Lib_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
    }
  | Lib_fail of Parsing.parse_failure
  | Lib_skip

let parse_lib_file ~reader options file =
  (* types are always allowed in lib files *)
  let types_mode = Parsing.TypesAllowed in
  (* lib files are always "use strict" *)
  let use_strict = true in
  try%lwt
    let lib_file = File_key.LibFile file in
    let filename_set = FilenameSet.singleton lib_file in
    let next = Parsing.next_of_filename_set (* workers *) None filename_set in
    let%lwt results =
      Parsing.parse_with_defaults ~types_mode ~use_strict ~reader options (* workers *) None next
    in
    Lwt.return
      ( if is_ok results then
        let tolerable_errors = FilenameMap.find lib_file results.Parsing.parse_ok in
        let ast = Parsing_heaps.Mutator_reader.get_ast_unsafe reader lib_file in
        let file_sig = Parsing_heaps.Mutator_reader.get_file_sig_unsafe reader lib_file in
        Lib_ok { ast; file_sig; tolerable_errors }
      else if is_fail results then
        let (_, _, parse_fail) = List.hd results.Parsing.parse_fails in
        Lib_fail parse_fail
      else if is_skip results then
        Lib_skip
      else
        failwith "Internal error: no parse results found" )
  with _ -> failwith (spf "Can't read library definitions file %s, exiting." file)

let infer_lib_file ~ccx ~options ~exclude_syms lib_file ast file_sig =
  let verbose = Options.verbose options in
  let lint_severities = Options.lint_severities options in
  let metadata =
    Context.(
      let metadata = metadata_of_options options in
      { metadata with checked = false; weak = false })
  in
  (* Lib files use only concrete locations, so this is not used. *)
  let aloc_table = lazy (ALoc.make_table lib_file) in
  let cx = Context.make ccx metadata lib_file aloc_table Files.lib_module_ref Context.InitLib in
  let syms = Infer.infer_lib_file cx ast ~exclude_syms ~lint_severities ~file_sig in

  if verbose != None then
    prerr_endlinef
      "load_lib %s: added symbols { %s }"
      (File_key.to_string lib_file)
      (String.concat ", " syms);

  (* symbols loaded from this file are suppressed if found in later ones *)
  SSet.union exclude_syms (SSet.of_list syms)

(* process all lib files: parse, infer, and add the symbols they define
   to the builtins object.

   Note: we support overrides of definitions found earlier in the list of
   files by those of the same name found in later ones, so caller must
   preserve lib path declaration order in the (flattened) list of files
   passed.

   returns (success, parse and signature errors, exports)
*)
let load_lib_files ~ccx ~options ~reader files =
  (* iterate in reverse override order *)
  let%lwt (_, ok, errors, ordered_asts) =
    List.rev files
    |> Lwt_list.fold_left_s
         (fun (exclude_syms, ok_acc, errors_acc, asts_acc) file ->
           let lib_file = File_key.LibFile file in
           match%lwt parse_lib_file ~reader options file with
           | Lib_ok { ast; file_sig; tolerable_errors } ->
             let file_sig = File_sig.abstractify_locs file_sig in
             let tolerable_errors = File_sig.abstractify_tolerable_errors tolerable_errors in
             let exclude_syms = infer_lib_file ~ccx ~options ~exclude_syms lib_file ast file_sig in
             let errors =
               tolerable_errors
               |> Inference_utils.set_of_file_sig_tolerable_errors ~source_file:lib_file
             in
             let errors_acc = Flow_error.ErrorSet.union errors errors_acc in
             let asts_acc = ast :: asts_acc in
             Lwt.return (exclude_syms, ok_acc, errors_acc, asts_acc)
           | Lib_fail fail ->
             let errors =
               match fail with
               | Parsing.Parse_error error ->
                 Inference_utils.set_of_parse_error ~source_file:lib_file error
               | Parsing.Docblock_errors errs ->
                 Inference_utils.set_of_docblock_errors ~source_file:lib_file errs
               | Parsing.File_sig_error error ->
                 Inference_utils.set_of_file_sig_error ~source_file:lib_file error
             in
             let errors_acc = Flow_error.ErrorSet.union errors errors_acc in
             Lwt.return (exclude_syms, false, errors_acc, asts_acc)
           | Lib_skip -> Lwt.return (exclude_syms, false, errors_acc, asts_acc))
         (SSet.empty, true, Flow_error.ErrorSet.empty, [])
  in
  let builtin_exports =
    if ok then
      let sig_opts =
        {
          Type_sig_parse.type_asserts = Options.type_asserts options;
          suppress_types = Options.suppress_types options;
          munge = (* libs shouldn't have private fields *) false;
          ignore_static_propTypes = true;
          facebook_keyMirror = (* irrelevant for libs *) false;
          facebook_fbt = Options.facebook_fbt options;
          max_literal_len = Options.max_literal_length options;
          exact_by_default = Options.exact_by_default options;
          module_ref_prefix = Options.haste_module_ref_prefix options;
          enable_enums = Options.enums options;
          enable_this_annot = Options.this_annot options;
        }
      in
      let (_builtin_errors, _builtin_locs, builtins) =
        Type_sig_utils.parse_and_pack_builtins sig_opts ordered_asts
      in
      let builtins =
        (* hide #flow-internal-react-server-module module *)
        let { Packed_type_sig.Builtins.modules; _ } = builtins in
        let modules = SMap.remove Type.react_server_module_ref modules in
        { builtins with Packed_type_sig.Builtins.modules }
      in
      Exports.of_builtins builtins
    else
      Exports.empty
  in
  Lwt.return (ok, errors, builtin_exports)

type init_result = {
  ok: bool;
  errors: Flow_error.ErrorSet.t FilenameMap.t;
  warnings: Flow_error.ErrorSet.t FilenameMap.t;
  suppressions: Error_suppressions.t;
  exports: Exports.t;
}

let error_set_to_filemap err_set =
  Flow_error.ErrorSet.fold
    (fun error map ->
      let file = Flow_error.source_file error in
      FilenameMap.update
        file
        (function
          | None -> Some (Flow_error.ErrorSet.singleton error)
          | Some set -> Some (Flow_error.ErrorSet.add error set))
        map)
    err_set
    FilenameMap.empty

(* initialize builtins:
   parse and do local inference on library files, and set up master context.
   returns list of (lib file, success) pairs.
*)
let init ~options ~reader lib_files =
  let ccx = Context.make_ccx () in
  let master_cx =
    let metadata =
      Context.(
        let metadata = metadata_of_options options in
        { metadata with checked = false; weak = false })
    in
    (* Lib files use only concrete locations, so this is not used. *)
    let aloc_table = lazy (ALoc.make_table File_key.Builtins) in
    Context.make ccx metadata File_key.Builtins aloc_table Files.lib_module_ref Context.InitLib
  in
  Flow_js_utils.mk_builtins master_cx;

  let%lwt (ok, parse_and_sig_errors, exports) = load_lib_files ~ccx ~options ~reader lib_files in
  let reason = Reason.builtin_reason (Reason.RCustom "module") in
  let builtin_module = Obj_type.mk_unsealed master_cx reason in
  Flow.flow_t master_cx (builtin_module, Flow_js_utils.builtins master_cx);
  Merge_js.ContextOptimizer.sig_context master_cx [Files.lib_module_ref] |> ignore;

  let (errors, warnings, suppressions) =
    let suppressions = Context.error_suppressions master_cx in
    let severity_cover = Context.severity_cover master_cx in
    let include_suppressions = Context.include_suppressions master_cx in
    let (errors, warnings, suppressions) =
      Error_suppressions.filter_lints
        ~include_suppressions
        suppressions
        (Context.errors master_cx)
        (Context.aloc_tables master_cx)
        severity_cover
    in
    ( error_set_to_filemap (Flow_error.ErrorSet.union parse_and_sig_errors errors),
      error_set_to_filemap warnings,
      suppressions )
  in
  (* store master signature context to heap *)
  Context_heaps.Init_master_context_mutator.add_master_sig ~audit:Expensive.ok master_cx;

  Lwt.return { ok; errors; warnings; suppressions; exports }
