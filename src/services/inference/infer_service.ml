(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

let apply_docblock_overrides metadata docblock_info =
  let open Context in

  let metadata = { metadata with jsx = Docblock.jsx docblock_info } in

  let metadata = match Docblock.flow docblock_info with
  | None -> metadata
  | Some Docblock.OptIn -> { metadata with checked = true; }
  | Some Docblock.OptInWeak -> { metadata with checked = true; weak = true }

  (* --all (which sets metadata.checked = true) overrides @noflow, so there are
     currently no scenarios where we'd change checked = true to false. in the
     future, there may be a case where checked defaults to true (but is not
     forced to be true ala --all), but for now we do *not* want to force
     checked = false here. *)
  | Some Docblock.OptOut -> metadata
  in

  match Docblock.preventMunge docblock_info with
  | Some value -> { metadata with munge_underscores = not value; }
  | None -> metadata

(* Given a filename, retrieve the parsed AST, derive a module name,
   and invoke the local (infer) pass. This will build and return a
   fresh context object for the module. *)
let infer_module ~options ~metadata filename =
  let ast, info = Parsing_service_js.get_ast_and_info_unsafe filename in
  let module_name = Module_js.exported_module ~options filename info in
  let metadata = apply_docblock_overrides metadata info in
  Type_inference_js.infer_ast ~metadata ~filename ~module_name ast

(* local inference job:
   takes list of filenames, accumulates into parallel lists of
   filenames, error sets *)
let infer_job ~options acc files =
  let metadata = Context.metadata_of_options options in
  List.fold_left (fun acc file ->
    let file_str = string_of_filename file in
    try Profile_utils.checktime ~options 1.0
      (fun t -> spf "perf: inferred %s in %f" file_str t)
      (fun () ->
        (* prerr_endlinef "[%d] INFER: %s" (Unix.getpid()) file_str; *)

        (* infer produces a context for this module *)
        let cx = infer_module ~options ~metadata file in
        (* register module info *)
        Module_js.add_module_info ~audit:Expensive.ok ~options cx;
        (* note: save and clear errors and error suppressions before storing
         * cx to shared heap *)
        let errs = Context.errors cx in
        let suppressions = Context.error_suppressions cx in
        Context.remove_all_errors cx;
        Context.remove_all_error_suppressions cx;

        Context_cache.add ~audit:Expensive.ok cx;

        (* add filename, errorset, suppressions *)
        (Context.file cx, errs, suppressions) :: acc
      )
    with
    (* Unrecoverable exceptions *)
    | SharedMem_js.Out_of_shared_memory
    | SharedMem_js.Heap_full
    | SharedMem_js.Hash_table_full
    | SharedMem_js.Dep_table_full as exc -> raise exc
    (* A catch all suppression is probably a bad idea... *)
    | exc ->
      let msg = "infer_job exception: "^(fmt_exc exc) in
      let errorset = Errors.ErrorSet.singleton
        (Errors.internal_error file msg) in
      prerr_endlinef "(%d) infer_job THROWS: %s"
        (Unix.getpid()) (fmt_file_exc (string_of_filename file) exc);
      (file, errorset, Errors.ErrorSuppressions.empty) :: acc
  ) acc files

(* local type inference pass.
   Returns a set of successfully inferred files.
   Creates contexts for inferred files, with errors in cx.errors *)
let infer ~options ~workers files =
  let files = FilenameSet.elements files in
  Profile_utils.logtime ~options
    (fun t -> spf "inferred %d files in %f" (List.length files) t)
    (fun () ->
      MultiWorker.call
        workers
        ~job: (infer_job ~options)
        ~neutral: []
        ~merge: List.rev_append
        ~next: (MultiWorker.next workers files)
    )
