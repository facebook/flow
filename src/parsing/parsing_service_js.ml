(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Sys_utils
open Docblock_parser

type result =
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      requires: string array;
      file_sig: File_sig.t;
      locs: Parsing_heaps.locs_tbl;
      type_sig: Parsing_heaps.type_sig;
      tolerable_errors: File_sig.tolerable_error list;
      exports: Exports.t;
      imports: Imports.t;
    }
  | Parse_recovered of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      requires: string array;
      file_sig: File_sig.t;
      tolerable_errors: File_sig.tolerable_error list;
      parse_errors: parse_error Nel.t;
    }
  | Parse_exn of Exception.t
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file
  | Skip_package_json of (Package_json.t, parse_error) Result.t

and parse_error = Loc.t * Parse_error.t

and parse_failure =
  | Uncaught_exception of Exception.t
  | Docblock_errors of docblock_error list
  | Parse_error of parse_error

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parsed: FilenameSet.t;
  (* list of skipped files *)
  unparsed: FilenameSet.t;
  (* list of files skipped due to an out of date hash *)
  changed: FilenameSet.t;
  (* list of failed files *)
  failed: File_key.t list * parse_failure list;
  (* set of unchanged files *)
  unchanged: FilenameSet.t;
  (* set of files that were not found on disk *)
  not_found: FilenameSet.t;
  (* package.json files parsed *)
  package_json: File_key.t list * parse_error option list;
  (* set of modules that need to be committed *)
  dirty_modules: Modulename.Set.t;
}

let empty_result =
  {
    parsed = FilenameSet.empty;
    unparsed = FilenameSet.empty;
    changed = FilenameSet.empty;
    failed = ([], []);
    unchanged = FilenameSet.empty;
    not_found = FilenameSet.empty;
    package_json = ([], []);
    dirty_modules = Modulename.Set.empty;
  }

(**************************** internal *********************************)
let parse_source_file ~options content file =
  let use_strict =
    if File_key.is_lib_file file then
      (* lib files are always "use strict" *)
      true
    else
      Options.modules_are_use_strict options
  in
  let parse_options =
    Some
      {
        Parser_env.components = true;
        (*
         * Always parse ES proposal syntax. The user-facing config option to
         * ignore/warn/enable them is handled during inference so that a clean error
         * can be surfaced (rather than a more cryptic parse error).
         *)
        enums = true;
        esproposal_decorators = true;
        types = true;
        use_strict;
        module_ref_prefix = Options.haste_module_ref_prefix options;
        module_ref_prefix_LEGACY_INTEROP = Options.haste_module_ref_prefix_LEGACY_INTEROP options;
      }
  in
  Parser_flow.program_file ~fail:false ~parse_options content (Some file)

let parse_package_json_file ~options content file =
  let node_main_fields = Options.node_main_fields options in
  let parse_options =
    Some
      {
        Parser_env.components = false;
        enums = false;
        esproposal_decorators = false;
        types = true;
        use_strict = false;
        module_ref_prefix = None;
        module_ref_prefix_LEGACY_INTEROP = None;
      }
  in
  match Parser_flow.package_json_file ~parse_options content (Some file) with
  | exception Parse_error.Error (err, _) -> Error err
  | ((_loc, obj), _parse_errors) -> Ok (Package_json.parse ~node_main_fields obj)

(* Allow types based on `types_mode`, using the @flow annotation in the
   file header if possible. Note, this should be consistent with
   Infer_service.apply_docblock_overrides w.r.t. the metadata.checked flag. *)
let types_checked options file docblock =
  if File_key.is_lib_file file then
    (* types are always allowed in lib files *)
    true
  else
    let open Docblock in
    match flow docblock with
    | None -> Options.all options
    | Some OptOut -> false
    | Some (OptIn | OptInStrict | OptInStrictLocal) -> true

let parse_file_sig options file docblock ast =
  let enable_enums = Options.enums options in

  let haste_module_ref_prefix = Options.haste_module_ref_prefix options in
  let haste_module_ref_prefix_LEGACY_INTEROP =
    Options.haste_module_ref_prefix_LEGACY_INTEROP options
  in
  let enable_relay_integration = Options.enable_relay_integration options in
  let relay_integration_excludes = Options.relay_integration_excludes options in
  let relay_integration_module_prefix = Options.relay_integration_module_prefix options in
  let relay_integration_module_prefix_includes =
    Options.relay_integration_module_prefix_includes options
  in
  let enable_relay_integration =
    enable_relay_integration && Relay_options.enabled_for_file relay_integration_excludes file
  in
  let file_options = Options.file_options options in
  let relay_integration_module_prefix =
    Relay_options.module_prefix_for_file
      relay_integration_module_prefix_includes
      file
      relay_integration_module_prefix
  in
  let file_sig_opts =
    {
      File_sig.enable_enums;
      enable_relay_integration;
      explicit_available_platforms = Docblock.supportsPlatform docblock;
      file_options;
      haste_module_ref_prefix;
      haste_module_ref_prefix_LEGACY_INTEROP;
      relay_integration_module_prefix;
    }
  in
  File_sig.program ~file_key:file ~ast ~opts:file_sig_opts

let parse_type_sig options docblock locs_to_dirtify file ast =
  let sig_opts = Type_sig_options.of_options options docblock locs_to_dirtify file in
  let strict = Docblock.is_strict docblock in
  let platform_availability_set =
    Platform_set.available_platforms
      ~file_options:(Options.file_options options)
      ~filename:(File_key.to_string file)
      ~explicit_available_platforms:(Docblock.supportsPlatform docblock)
  in
  Type_sig_utils.parse_and_pack_module ~strict ~platform_availability_set sig_opts (Some file) ast

let do_parse ~options ~docblock ?(locs_to_dirtify = []) content file =
  try
    match file with
    | File_key.JsonFile str ->
      if Filename.basename str = "package.json" then
        let result = parse_package_json_file ~options content file in
        Parse_skip (Skip_package_json result)
      else
        Parse_skip Skip_resource_file
    | File_key.ResourceFile _ -> Parse_skip Skip_resource_file
    | File_key.LibFile _
    | File_key.SourceFile _ ->
      (* either all=true or @flow pragma exists *)
      if not (types_checked options file docblock) then
        Parse_skip Skip_non_flow_file
      else
        let (ast, parse_errors) = parse_source_file ~options content file in
        let file_sig = parse_file_sig options file docblock ast in
        let requires = File_sig.require_set file_sig |> SSet.elements |> Array.of_list in
        (*If you want efficiency, can compute globals along with file_sig in the above function since scope is computed when computing file_sig*)
        let (_, (_, _, globals)) =
          let enable_enums = Options.enums options in
          Ssa_builder.program_with_scope ~enable_enums ast
        in
        if not (Base.List.is_empty parse_errors) then
          Parse_recovered
            {
              ast;
              requires;
              file_sig;
              tolerable_errors = [];
              parse_errors = Nel.of_list_exn parse_errors;
            }
        else
          let (sig_errors, locs, type_sig) =
            parse_type_sig options docblock locs_to_dirtify file ast
          in
          let exports = Exports.of_module type_sig in
          let imports = Imports.of_file_sig file_sig in
          let imports = Imports.add_globals globals imports in
          let tolerable_errors =
            List.fold_left
              (fun acc err ->
                match err with
                | Type_sig.SigError err ->
                  let err = Signature_error.map (Type_sig_collections.Locs.get locs) err in
                  File_sig.SignatureVerificationError err :: acc
                | Type_sig.CheckError -> acc)
              []
              sig_errors
          in
          Parse_ok { ast; requires; file_sig; locs; type_sig; tolerable_errors; exports; imports }
  with
  | e ->
    let e = Exception.wrap e in
    ( if FlowEventLogger.should_log () then
      let e_str =
        Printf.sprintf
          "%s\nBacktrace: %s"
          (Exception.get_ctor_string e)
          (Exception.get_full_backtrace_string max_int e)
      in
      FlowEventLogger.parsing_exception e_str
    );
    Parse_exn e

let hash_content content =
  let state = Xx.init 0L in
  Xx.update state content;
  Xx.digest state

let content_hash_matches_file_hash ~reader file content_hash =
  match Parsing_heaps.Mutator_reader.get_file_hash ~reader file with
  | None -> false
  | Some hash -> hash = content_hash

let content_hash_matches_old_file_hash ~reader file content_hash =
  match Parsing_heaps.Mutator_reader.get_old_file_hash ~reader file with
  | None -> false
  | Some hash -> hash = content_hash

let does_content_match_file_hash ~reader file content =
  let content_hash = hash_content content in
  match Parsing_heaps.Reader_dispatcher.get_file_hash ~reader file with
  | None -> false
  | Some hash -> hash = content_hash

let fold_failed acc worker_mutator file_key file_opt hash module_name error =
  let dirty_modules =
    worker_mutator.Parsing_heaps.add_unparsed file_key file_opt hash module_name
  in
  let failed = (file_key :: fst acc.failed, error :: snd acc.failed) in
  let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
  { acc with failed; dirty_modules }

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer
    ~worker_mutator
    ~reader
    ~options
    ~skip_changed
    ~skip_unchanged
    ~locs_to_dirtify
    ~noflow
    ~exported_module
    acc
    file_key : results =
  let file_opt = Parsing_heaps.get_file_addr file_key in
  match Option.bind file_opt (Parsing_heaps.Mutator_reader.get_parse ~reader) with
  | Some _ when SharedMem.is_init_transaction () ->
    (* If we can find an existing entry during the initial transaction, we must
     * have been asked to parse the same file twice. This can happen if we init
     * from scratch and walking the file system finds the same file twice.
     *
     * Since we must have already parsed the file during this transaction, we
     * can skip this entirely. *)
    acc
  | _ ->
    let filename_string = File_key.to_string file_key in
    (match cat filename_string with
    | exception _ ->
      (* The file watcher does not distinguish between modified or deleted files,
       * we distinguish by parsing. Either the wather notified us because the file
       * was deleted, or because the file was modified and then the file was
       * deleted before we got to this point.
       *
       * In either case, we update the file entity so that the latest data is
       * empty, indicating no file. We also record these files so their shared
       * hash table keys can be removed when the transaction commits.
       *
       * When `skip_changed` is true, we are ensuring that some files are parsed. We
       * only want to return the set of files which have unexpectedly changed, but we
       * do not want to actually modify the heap to reflect those changes. *)
      let dirty_modules =
        if skip_changed then
          Modulename.Set.empty
        else
          let module_name = exported_module file_key ~package_info:None in
          worker_mutator.Parsing_heaps.clear_not_found file_key module_name
      in
      let not_found = FilenameSet.add file_key acc.not_found in
      let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
      { acc with not_found; dirty_modules }
    | content ->
      let hash = hash_content content in
      (* If skip_changed is true, then we're currently ensuring some files are parsed. That
       * means we don't currently have the file's AST but we might have the file's hash in the
       * non-oldified heap. What we want to avoid is parsing files which differ from the hash *)
      if skip_changed && not (content_hash_matches_file_hash ~reader file_key hash) then
        { acc with changed = FilenameSet.add file_key acc.changed }
      else if skip_unchanged && content_hash_matches_old_file_hash ~reader file_key hash then
        { acc with unchanged = FilenameSet.add file_key acc.unchanged }
      else (
        match
          parse_docblock
            ~max_tokens:(Options.max_header_tokens options)
            ~file_options:(Options.file_options options)
            file_key
            content
        with
        | ([], docblock) ->
          let docblock =
            if noflow file_key then
              { docblock with Docblock.flow = Some Docblock.OptOut }
            else
              docblock
          in
          begin
            match do_parse ~options ~docblock ~locs_to_dirtify content file_key with
            | Parse_ok
                { ast; requires; file_sig; exports; imports; locs; type_sig; tolerable_errors } ->
              let file_sig = (file_sig, tolerable_errors) in
              let module_name = exported_module file_key ~package_info:None in
              let dirty_modules =
                worker_mutator.Parsing_heaps.add_parsed
                  file_key
                  file_opt
                  ~exports
                  ~imports
                  hash
                  module_name
                  docblock
                  ast
                  requires
                  file_sig
                  locs
                  type_sig
              in
              let parsed = FilenameSet.add file_key acc.parsed in
              let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
              { acc with parsed; dirty_modules }
            | Parse_recovered { parse_errors = (error, _); _ } ->
              let module_name = exported_module file_key ~package_info:None in
              let failure = Parse_error error in
              fold_failed acc worker_mutator file_key file_opt hash module_name failure
            | Parse_exn exn ->
              let module_name = exported_module file_key ~package_info:None in
              let failure = Uncaught_exception exn in
              fold_failed acc worker_mutator file_key file_opt hash module_name failure
            | Parse_skip (Skip_package_json result) ->
              let (error, module_name, package_info) =
                match result with
                | Ok pkg ->
                  let module_name = exported_module file_key ~package_info:(Some pkg) in
                  (None, module_name, Ok pkg)
                | Error err -> (Some err, None, Error ())
              in
              let dirty_modules =
                worker_mutator.Parsing_heaps.add_package
                  file_key
                  file_opt
                  hash
                  module_name
                  package_info
              in
              let package_json =
                (file_key :: fst acc.package_json, error :: snd acc.package_json)
              in
              let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
              { acc with package_json; dirty_modules }
            | Parse_skip Skip_non_flow_file
            | Parse_skip Skip_resource_file ->
              let module_name = exported_module file_key ~package_info:None in
              let dirty_modules =
                worker_mutator.Parsing_heaps.add_unparsed file_key file_opt hash module_name
              in
              let unparsed = FilenameSet.add file_key acc.unparsed in
              let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
              { acc with unparsed; dirty_modules }
          end
        | (docblock_errors, _docblock) ->
          let module_name = exported_module file_key ~package_info:None in
          let dirty_modules =
            worker_mutator.Parsing_heaps.add_unparsed file_key file_opt hash module_name
          in
          let error = Docblock_errors docblock_errors in
          let failed = (file_key :: fst acc.failed, error :: snd acc.failed) in
          let dirty_modules = Modulename.Set.union dirty_modules acc.dirty_modules in
          { acc with failed; dirty_modules }
      ))

(* merge is just memberwise union/concat of results *)
let merge a b =
  {
    parsed = FilenameSet.union a.parsed b.parsed;
    unparsed = FilenameSet.union a.unparsed b.unparsed;
    changed = FilenameSet.union a.changed b.changed;
    failed =
      (let (a1, a2) = a.failed in
       let (b1, b2) = b.failed in
       (List.rev_append a1 b1, List.rev_append a2 b2)
      );
    unchanged = FilenameSet.union a.unchanged b.unchanged;
    not_found = FilenameSet.union a.not_found b.not_found;
    package_json =
      (let (a1, a2) = a.package_json in
       let (b1, b2) = b.package_json in
       (List.rev_append a1 b1, List.rev_append a2 b2)
      );
    dirty_modules = Modulename.Set.union a.dirty_modules b.dirty_modules;
  }

(***************************** public ********************************)

let progress_fn ~total ~start ~length:_ =
  let finished = start in
  MonitorRPC.status_update ~event:ServerStatus.(Parsing_progress { total = Some total; finished })

let next_of_filename_set ?(with_progress = false) workers filenames =
  if with_progress then
    MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements filenames)
  else
    MultiWorkerLwt.next workers (FilenameSet.elements filenames)

let parse
    ~worker_mutator ~reader ~options ~skip_changed ~skip_unchanged ~locs_to_dirtify workers next :
    results Lwt.t =
  let t = Unix.gettimeofday () in
  let noflow fn = Files.is_untyped (Options.file_options options) (File_key.to_string fn) in
  let exported_module = Module_js.exported_module ~options in
  let job =
    reducer
      ~worker_mutator
      ~reader
      ~options
      ~skip_changed
      ~skip_unchanged
      ~locs_to_dirtify
      ~noflow
      ~exported_module
  in
  let%lwt results =
    MultiWorkerLwt.fold
      workers
      ~blocking:(Options.blocking_worker_communication options)
      ~job
      ~neutral:empty_result
      ~merge
      ~next
  in
  if Options.should_profile options then
    let t2 = Unix.gettimeofday () in
    let num_parsed = FilenameSet.cardinal results.parsed in
    let num_unparsed = FilenameSet.cardinal results.unparsed in
    let num_changed = FilenameSet.cardinal results.changed in
    let num_failed = List.length (fst results.failed) in
    let num_unchanged = FilenameSet.cardinal results.unchanged in
    let num_not_found = FilenameSet.cardinal results.not_found in
    let total =
      num_parsed + num_unparsed + num_changed + num_failed + num_unchanged + num_not_found
    in
    Hh_logger.info
      "parsed %d files (%d ok, %d skipped, %d not found, %d bad hashes, %d failed, %d unchanged) in %f"
      total
      num_parsed
      num_unparsed
      num_not_found
      num_changed
      num_failed
      num_unchanged
      (t2 -. t)
  else
    ();
  Lwt.return results

let reparse ~transaction ~reader ~options ~locs_to_dirtify ~with_progress ~workers ~modified:files =
  let (master_mutator, worker_mutator) = Parsing_heaps.Reparse_mutator.create transaction files in
  let next = next_of_filename_set ~with_progress workers files in
  let%lwt results =
    parse
      ~worker_mutator
      ~reader
      ~options
      ~skip_changed:false
      ~skip_unchanged:(locs_to_dirtify = [])
      ~locs_to_dirtify
      workers
      next
  in
  Parsing_heaps.Reparse_mutator.record_unchanged master_mutator results.unchanged;
  Parsing_heaps.Reparse_mutator.record_not_found master_mutator results.not_found;
  Lwt.return results

let parse_with_defaults ?(locs_to_dirtify = []) ~reader options workers next =
  (* This isn't a recheck, so there shouldn't be any unchanged *)
  let worker_mutator = Parsing_heaps.Parse_mutator.create () in
  parse
    ~worker_mutator
    ~reader
    ~options
    ~skip_changed:false
    ~skip_unchanged:false
    ~locs_to_dirtify
    workers
    next

let reparse_with_defaults
    ~transaction ~reader ?(locs_to_dirtify = []) ~with_progress ~workers ~modified options =
  reparse ~transaction ~reader ~options ~locs_to_dirtify ~with_progress ~workers ~modified

(* ensure_parsed takes a set of files, finds the files which haven't been parsed, and parses them.
 * Any not-yet-parsed files who's on-disk contents don't match their already-known hash are skipped
 * and returned to the caller. *)
let ensure_parsed ~reader options workers files =
  (* We're not replacing any info, so there's nothing to roll back. That means we can just use the
   * simple Parse_mutator rather than the rollback-able Reparse_mutator *)
  let worker_mutator = Parsing_heaps.Parse_mutator.create () in
  let progress_fn ~total ~start ~length:_ =
    MonitorRPC.status_update
      ~event:ServerStatus.(Parsing_progress { total = Some total; finished = start })
  in
  let job acc fn =
    if Parsing_heaps.Mutator_reader.has_ast ~reader fn then
      acc
    else
      FilenameSet.add fn acc
  in
  (* NOTE: This call can be slow with large fanouts. It might be worth including this
   * in its own server status. *)
  let%lwt files_missing_asts =
    MultiWorkerLwt.fold
      workers
      ~blocking:(Options.blocking_worker_communication options)
      ~job
      ~merge:FilenameSet.union
      ~neutral:FilenameSet.empty
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements files))
  in
  let next = MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements files_missing_asts) in
  let%lwt {
        parsed = _;
        unparsed = _;
        changed;
        failed = _;
        unchanged = _;
        not_found;
        package_json = _;
        dirty_modules = _;
      } =
    parse
      ~worker_mutator
      ~reader
      ~options
      ~skip_changed:true
      ~skip_unchanged:false
      ~locs_to_dirtify:[]
      workers
      next
  in
  Lwt.return (FilenameSet.union changed not_found)
