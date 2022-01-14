(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing
module Ast = Flow_ast
open Utils_js
open Sys_utils

type result =
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      locs: Parsing_heaps.locs_tbl;
      type_sig: Parsing_heaps.type_sig;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      parse_errors: parse_error list;
      exports: Exports.t;
    }
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file
  | Skip_package_json of (Package_json.t, parse_error) Result.t

and parse_error = Loc.t * Parse_error.t

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of parse_error
  | File_sig_error of File_sig.With_Loc.error

and docblock_error = Loc.t * docblock_error_kind

and docblock_error_kind =
  | MultipleFlowAttributes
  | InvalidFlowMode of string
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

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
  }

(**************************** internal *********************************)
(* TODO: add TypesForbidden (disables types even on files with @flow) and
   TypesAllowedByDefault (enables types even on files without @flow, but allows
   something like @noflow to disable them) *)
type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

type parse_options = {
  parse_fail: bool;
  parse_types_mode: types_mode;
  parse_use_strict: bool;
  parse_prevent_munge: bool;
  parse_module_ref_prefix: string option;
  parse_facebook_fbt: string option;
  parse_type_asserts: bool;
  parse_suppress_types: SSet.t;
  parse_max_literal_len: int;
  parse_exact_by_default: bool;
  parse_enable_enums: bool;
  parse_enable_relay_integration: bool;
  parse_relay_integration_excludes: Str.regexp list;
  parse_relay_integration_module_prefix: string option;
  parse_relay_integration_module_prefix_includes: Str.regexp list;
  parse_node_main_fields: string list;
}

let parse_source_file ~fail ~types ~use_strict content file =
  let parse_options =
    Some
      Parser_env.
        {
          (*
           * Always parse ES proposal syntax. The user-facing config option to
           * ignore/warn/enable them is handled during inference so that a clean error
           * can be surfaced (rather than a more cryptic parse error).
           *)
          enums = true;
          esproposal_decorators = true;
          esproposal_export_star_as = true;
          esproposal_optional_chaining = true;
          esproposal_nullish_coalescing = true;
          types;
          use_strict;
        }
      
  in

  let (ast, parse_errors) = Parser_flow.program_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);
  (ast, parse_errors)

let parse_package_json_file ~node_main_fields content file =
  let parse_options =
    Some
      Parser_env.
        {
          enums = false;
          esproposal_decorators = false;
          esproposal_export_star_as = false;
          esproposal_optional_chaining = false;
          esproposal_nullish_coalescing = false;
          types = true;
          use_strict = false;
        }
      
  in

  match Parser_flow.package_json_file ~parse_options content (Some file) with
  | exception Parse_error.Error (err, _) -> Error err
  | ((_loc, obj), _parse_errors) -> Ok (Package_json.parse ~node_main_fields obj)

(* Avoid lexing unbounded in perverse cases *)
let docblock_max_tokens = 10

let attributes_rx = Str.regexp "[ \t\r\n\\*/]+"

let lines_rx = Str.regexp "\\(\r\n\\|\n\\|\r\\)"

let pragma_rx = Str.regexp "^@"

let extract_docblock =
  Docblock.(
    (* walks a list of words, returns a list of errors and the extracted info.
       if @flow or @providesModule is found more than once, the first one is used
       and an error is returned. *)
    let rec parse_attributes (errors, info) = function
      | (loc, "@flow") :: (_, "strict") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptInStrict })
        in
        parse_attributes acc xs
      | (loc, "@flow") :: (_, "strict-local") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptInStrictLocal })
        in
        parse_attributes acc xs
      | (flow_loc, "@flow") :: (next_loc, next) :: xs
        when Loc.lines_intersect flow_loc next_loc && (not @@ Str.string_match pragma_rx next 0) ->
        let (errors, info) =
          if info.flow <> None then
            ((flow_loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptIn })
        in
        let errors = (next_loc, InvalidFlowMode next) :: errors in
        parse_attributes (errors, info) xs
      | (loc, "@flow") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptIn })
        in
        parse_attributes acc xs
      | (loc, "@noflow") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptOut })
        in
        parse_attributes acc xs
      | (loc, "@providesModule") :: (_, m) :: xs ->
        let acc =
          if info.providesModule <> None then
            ((loc, MultipleProvidesModuleAttributes) :: errors, info)
          else
            (errors, { info with providesModule = Some m })
        in
        parse_attributes acc xs
      | (_, "@preventMunge") :: xs ->
        (* dupes are ok since they can only be truthy *)
        parse_attributes (errors, { info with preventMunge = true }) xs
      | [(jsx_loc, "@jsx")] -> ((jsx_loc, InvalidJSXAttribute None) :: errors, info)
      | (jsx_loc, "@jsx") :: (expr_loc, expr) :: xs ->
        let acc =
          if info.jsx <> None then
            ((jsx_loc, MultipleJSXAttributes) :: errors, info)
          else
            (* The point of the padding is to make the parsed code line up
             * with the comment in the original source *)
            let padding =
              String.make Loc.(expr_loc.start.line - 1) '\n'
              ^ String.make Loc.(expr_loc.start.column) ' '
            in
            try
              let (jsx_expr, _) =
                Parser_flow.jsx_pragma_expression (padding ^ expr) expr_loc.Loc.source
              in
              (errors, { info with jsx = Some (expr, jsx_expr) })
            with
            | Parse_error.Error ((_, e), _) ->
              let e = Some (Parse_error.PP.error e) in
              ((expr_loc, InvalidJSXAttribute e) :: errors, info)
        in
        parse_attributes acc xs
      | (_, "@typeAssert") :: xs -> parse_attributes (errors, { info with typeAssert = true }) xs
      | _ :: xs -> parse_attributes (errors, info) xs
      | [] -> (errors, info)
    in
    let calc_end start s =
      Str.full_split lines_rx s
      |> List.fold_left
           Loc.(
             fun _end elem ->
               match elem with
               | Str.Delim delim ->
                 let line_incr =
                   if delim = "\r" then
                     0
                   else
                     1
                 in
                 let column = 0 in
                 let line = _end.line + line_incr in
                 { column; line }
               | Str.Text text ->
                 let length = String.length text in
                 let column = _end.column + length in
                 { _end with column }
           )
           start
    in
    let split loc s =
      (* Need to add 2 characters for the start of the comment *)
      let start = Loc.{ loc.start with column = loc.start.column + 2 } in
      Str.full_split attributes_rx s
      |> List.fold_left
           (fun (start, attributes) elem ->
             match elem with
             | Str.Delim s -> (calc_end start s, attributes)
             | Str.Text s ->
               let _end = calc_end start s in
               (_end, Loc.({ loc with start; _end }, s) :: attributes))
           (start, [])
      |> snd
      |> List.rev
    in
    let string_of_comment = function
      | (loc, { Ast.Comment.text; _ }) -> (loc, text)
    in
    let map_n =
      let rec helper f remaining acc = function
        | [] -> List.rev acc
        | hd :: rest ->
          if remaining <= 0 then
            List.rev acc
          else
            helper f (remaining - 1) (f hd :: acc) rest
      in
      (fun f n lst -> helper f n [] lst)
    in
    fun ~max_tokens filename content ->
      (* Consume tokens in the file until we get a comment. This is a hack to
       * support Nuclide, which needs 'use babel' as the first token due to
       * contstraints with Atom (see https://github.com/atom/atom/issues/8416 for
       * more context). At some point this should change back to consuming only
       * the first token. *)
      let lb =
        try Sedlexing.Utf8.from_string content with
        | Sedlexing.MalFormed ->
          Hh_logger.warn "File %s is malformed" (File_key.to_string filename);
          Sedlexing.Utf8.from_string ""
      in
      let env = Lex_env.new_lex_env (Some filename) lb ~enable_types_in_comments:false in
      let rec get_first_comment_contents ?(i = 0) env =
        if i < max_tokens then
          let (env, lexer_result) = Flow_lexer.token env in
          match Lex_result.comments lexer_result with
          | [] ->
            Token.(
              (*
               * Stop looking for docblocks if we see any tokens other than a
               * string or a semicolon (`"use babel";` or `"use strict";`).
               *)
              (match Lex_result.token lexer_result with
              | T_STRING _
              | T_SEMICOLON ->
                get_first_comment_contents ~i:(i + 1) env
              | _ -> None)
            )
          | comments -> Some (map_n string_of_comment (max_tokens - i) comments)
        else
          None
      in
      match get_first_comment_contents env with
      | Some comments ->
        List.fold_left
          (fun acc (loc, s) -> parse_attributes acc (split loc s))
          ([], default_info)
          comments
      | None -> ([], default_info)
  )

let parse_docblock ~max_tokens file content : docblock_error list * Docblock.t =
  match file with
  | File_key.ResourceFile _
  | File_key.JsonFile _ ->
    ([], Docblock.default_info)
  | _ -> extract_docblock ~max_tokens file content

(* Allow types based on `types_mode`, using the @flow annotation in the
   file header if possible. Note, this should be consistent with
   Infer_service.apply_docblock_overrides w.r.t. the metadata.checked flag. *)
let types_checked types_mode info =
  match types_mode with
  | TypesAllowed -> true
  | TypesForbiddenByDefault ->
    (match Docblock.flow info with
    | None
    | Some Docblock.OptOut ->
      false
    | Some Docblock.OptIn
    | Some Docblock.OptInStrict
    | Some Docblock.OptInStrictLocal ->
      true)

let do_parse ~parse_options ~info content file =
  let {
    parse_fail = fail;
    parse_types_mode = types_mode;
    parse_use_strict = use_strict;
    parse_prevent_munge = prevent_munge;
    parse_module_ref_prefix = module_ref_prefix;
    parse_facebook_fbt = facebook_fbt;
    parse_type_asserts = type_asserts;
    parse_suppress_types = suppress_types;
    parse_max_literal_len = max_literal_len;
    parse_exact_by_default = exact_by_default;
    parse_enable_enums = enable_enums;
    parse_enable_relay_integration = enable_relay_integration;
    parse_relay_integration_excludes = relay_integration_excludes;
    parse_relay_integration_module_prefix = relay_integration_module_prefix;
    parse_relay_integration_module_prefix_includes = relay_integration_module_prefix_includes;
    parse_node_main_fields = node_main_fields;
  } =
    parse_options
  in
  try
    match file with
    | File_key.JsonFile str ->
      if Filename.basename str = "package.json" then
        let result = parse_package_json_file ~node_main_fields content file in
        Parse_skip (Skip_package_json result)
      else
        Parse_skip Skip_resource_file
    | File_key.ResourceFile _ -> Parse_skip Skip_resource_file
    | _ ->
      (* either all=true or @flow pragma exists *)
      let types_checked = types_checked types_mode info in
      if not types_checked then
        Parse_skip Skip_non_flow_file
      else
        (* NOTE: if ~fail:true, we'll never get parse errors here *)
        let (ast, parse_errors) = parse_source_file ~fail ~types:true ~use_strict content file in
        let prevent_munge = Docblock.preventMunge info || prevent_munge in
        (* NOTE: This is a temporary hack that makes the signature verifier ignore any static
           property named `propTypes` in any class. It should be killed with fire or replaced with
           something that only works for React classes, in which case we must make a corresponding
           change in the type system that enforces that any such property is private. *)
        let ignore_static_propTypes = true in
        (* NOTE: This is a Facebook-specific hack that makes the signature verifier and generator
           recognize and process a custom `keyMirror` function that makes an enum out of the keys
           of an object. *)
        let facebook_keyMirror = true in
        let enable_relay_integration =
          enable_relay_integration && Relay_options.enabled_for_file relay_integration_excludes file
        in
        let relay_integration_module_prefix =
          Relay_options.module_prefix_for_file
            relay_integration_module_prefix_includes
            file
            relay_integration_module_prefix
        in
        let file_sig_opts =
          {
            File_sig.With_Loc.module_ref_prefix;
            enable_enums;
            enable_relay_integration;
            relay_integration_module_prefix;
          }
        in
        (match File_sig.With_Loc.program ~ast ~opts:file_sig_opts with
        | Error e -> Parse_fail (File_sig_error e)
        | Ok (file_sig, tolerable_errors) ->
          let sig_opts =
            {
              Type_sig_parse.type_asserts;
              suppress_types;
              munge = not prevent_munge;
              ignore_static_propTypes;
              facebook_keyMirror;
              facebook_fbt;
              max_literal_len;
              exact_by_default;
              module_ref_prefix;
              enable_enums;
              enable_relay_integration;
              relay_integration_module_prefix;
            }
          in
          let (sig_errors, locs, type_sig) =
            let strict = Docblock.is_strict info in
            Type_sig_utils.parse_and_pack_module ~strict sig_opts (Some file) ast
          in
          let exports = Exports.of_module type_sig in
          let tolerable_errors =
            List.fold_left
              (fun acc (_, err) ->
                match err with
                | Type_sig.SigError err ->
                  let err = Signature_error.map (Type_sig_collections.Locs.get locs) err in
                  File_sig.With_Loc.SignatureVerificationError err :: acc
                | Type_sig.CheckError -> acc)
              tolerable_errors
              sig_errors
          in
          Parse_ok { ast; file_sig; locs; type_sig; tolerable_errors; parse_errors; exports })
  with
  | Parse_error.Error (e, _) -> Parse_fail (Parse_error e)
  | e ->
    let e = Exception.wrap e in
    let s = Exception.get_ctor_string e in
    let loc = Loc.{ none with source = Some file } in
    let err = (loc, Parse_error.Assertion s) in
    Parse_fail (Parse_error err)

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
  match Parsing_heaps.Reader.get_file_hash ~reader file with
  | None -> false
  | Some hash -> hash = content_hash

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer
    ~worker_mutator
    ~reader
    ~parse_options
    ~skip_changed
    ~skip_unchanged
    ~max_header_tokens
    ~noflow
    exported_module
    acc
    file : results =
  let filename_string = File_key.to_string file in
  match cat filename_string with
  | exception _ ->
    (* It turns out that sometimes files appear and disappear very quickly. Just
     * because someone told us that this file exists and needs to be parsed, it
     * doesn't mean it actually still exists. If anything goes wrong reading this
     * file, let's skip it. We don't need to notify our caller, since they'll
     * probably get the delete event anyway *)
    { acc with not_found = FilenameSet.add file acc.not_found }
  | content ->
    let hash = hash_content content in
    (* If skip_changed is true, then we're currently ensuring some files are parsed. That
     * means we don't currently have the file's AST but we might have the file's hash in the
     * non-oldified heap. What we want to avoid is parsing files which differ from the hash *)
    if skip_changed && not (content_hash_matches_file_hash ~reader file hash) then
      { acc with changed = FilenameSet.add file acc.changed }
    else if skip_unchanged && content_hash_matches_old_file_hash ~reader file hash then
      { acc with unchanged = FilenameSet.add file acc.unchanged }
    else (
      match parse_docblock ~max_tokens:max_header_tokens file content with
      | ([], info) ->
        let info =
          if noflow file then
            { info with Docblock.flow = Some Docblock.OptOut }
          else
            info
        in
        let module_name = exported_module file info in
        begin
          match do_parse ~parse_options ~info content file with
          | Parse_ok { ast; file_sig; exports; locs; type_sig; tolerable_errors; parse_errors = _ }
            ->
            (* if parse_options.fail == true, then parse errors will hit Parse_fail below. otherwise,
               ignore any parse errors we get here. *)
            let file_sig = (file_sig, tolerable_errors) in
            worker_mutator.Parsing_heaps.add_parsed
              file
              ~exports
              hash
              module_name
              info
              ast
              file_sig
              locs
              type_sig;
            { acc with parsed = FilenameSet.add file acc.parsed }
          | Parse_fail error ->
            worker_mutator.Parsing_heaps.add_unparsed file hash module_name;
            let failed = (file :: fst acc.failed, error :: snd acc.failed) in
            { acc with failed }
          | Parse_skip (Skip_package_json result) ->
            let error =
              let file_str = File_key.to_string file in
              match result with
              | Ok pkg ->
                Package_heaps.Package_heap_mutator.add_package_json file_str pkg;
                None
              | Error err ->
                Package_heaps.Package_heap_mutator.add_error file_str;
                Some err
            in
            worker_mutator.Parsing_heaps.add_unparsed file hash module_name;
            let unparsed = FilenameSet.add file acc.unparsed in
            let package_json = (file :: fst acc.package_json, error :: snd acc.package_json) in
            { acc with unparsed; package_json }
          | Parse_skip Skip_non_flow_file
          | Parse_skip Skip_resource_file ->
            worker_mutator.Parsing_heaps.add_unparsed file hash module_name;
            { acc with unparsed = FilenameSet.add file acc.unparsed }
        end
      | (docblock_errors, info) ->
        let module_name = exported_module file info in
        worker_mutator.Parsing_heaps.add_unparsed file hash module_name;
        let error = Docblock_errors docblock_errors in
        let failed = (file :: fst acc.failed, error :: snd acc.failed) in
        { acc with failed }
    )

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
  }

let opt_or_alternate opt alternate =
  match opt with
  | Some x -> x
  | None -> alternate

(* types_mode and use_strict aren't special, they just happen to be the ones that needed to be
   overridden *)
let get_defaults ~types_mode ~use_strict options =
  let types_mode =
    opt_or_alternate
      types_mode
      (* force types when --all is set, but otherwise forbid them unless the file
         has @flow in it. *)
      ( if Options.all options then
        TypesAllowed
      else
        TypesForbiddenByDefault
      )
  in
  let use_strict = opt_or_alternate use_strict (Options.modules_are_use_strict options) in
  let profile = Options.should_profile options in
  let max_header_tokens = Options.max_header_tokens options in
  let noflow fn = Files.is_untyped (Options.file_options options) (File_key.to_string fn) in
  (types_mode, use_strict, profile, max_header_tokens, noflow)

(***************************** public ********************************)

let progress_fn ~total ~start ~length:_ =
  let finished = start in
  MonitorRPC.status_update ServerStatus.(Parsing_progress { total = Some total; finished })

let next_of_filename_set ?(with_progress = false) workers filenames =
  if with_progress then
    MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements filenames)
  else
    MultiWorkerLwt.next workers (FilenameSet.elements filenames)

let parse
    ~worker_mutator
    ~reader
    ~parse_options
    ~skip_changed
    ~skip_unchanged
    ~profile
    ~max_header_tokens
    ~noflow
    exported_module
    workers
    next : results Lwt.t =
  let t = Unix.gettimeofday () in
  let reducer =
    reducer
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_changed
      ~skip_unchanged
      ~max_header_tokens
      ~noflow
      exported_module
  in
  let%lwt results =
    MultiWorkerLwt.call workers ~job:(List.fold_left reducer) ~neutral:empty_result ~merge ~next
  in
  if profile then
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

let reparse
    ~transaction
    ~reader
    ~parse_options
    ~profile
    ~max_header_tokens
    ~noflow
    exported_module
    ~with_progress
    ~workers
    ~modified:files =
  (* save old parsing info for files *)
  let (master_mutator, worker_mutator) = Parsing_heaps.Reparse_mutator.create transaction files in
  let next = next_of_filename_set ?with_progress workers files in
  let%lwt results =
    parse
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_changed:false
      ~skip_unchanged:true
      ~profile
      ~max_header_tokens
      ~noflow
      exported_module
      workers
      next
  in
  (* restore old parsing info for unchanged files *)
  Parsing_heaps.Reparse_mutator.revive_files master_mutator results.unchanged;
  Lwt.return results

let make_parse_options_internal
    ?(fail = true) ?(types_mode = TypesAllowed) ?use_strict ~docblock options =
  let use_strict =
    match use_strict with
    | Some use_strict -> use_strict
    | None -> Options.modules_are_use_strict options
  in
  let module_ref_prefix = Options.haste_module_ref_prefix options in
  let facebook_fbt = Options.facebook_fbt options in
  let prevent_munge =
    let default = not (Options.should_munge_underscores options) in
    match docblock with
    | Some docblock -> Docblock.preventMunge docblock || default
    | None -> default
  in
  {
    parse_fail = fail;
    parse_types_mode = types_mode;
    parse_use_strict = use_strict;
    parse_prevent_munge = prevent_munge;
    parse_module_ref_prefix = module_ref_prefix;
    parse_facebook_fbt = facebook_fbt;
    parse_type_asserts = Options.type_asserts options;
    parse_suppress_types = Options.suppress_types options;
    parse_max_literal_len = Options.max_literal_length options;
    parse_exact_by_default = Options.exact_by_default options;
    parse_enable_enums = Options.enums options;
    parse_enable_relay_integration = Options.enable_relay_integration options;
    parse_relay_integration_excludes = Options.relay_integration_excludes options;
    parse_relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    parse_relay_integration_module_prefix_includes =
      Options.relay_integration_module_prefix_includes options;
    parse_node_main_fields = Options.node_main_fields options;
  }

let make_parse_options ?fail ?types_mode ?use_strict docblock options =
  make_parse_options_internal ?fail ?types_mode ?use_strict ~docblock:(Some docblock) options

let parse_with_defaults ?types_mode ?use_strict ~reader options workers next =
  let (types_mode, use_strict, profile, max_header_tokens, noflow) =
    get_defaults ~types_mode ~use_strict options
  in
  let parse_options =
    make_parse_options_internal ~fail:true ~use_strict ~types_mode ~docblock:None options
  in
  let exported_module = Module_js.exported_module ~options in
  (* This isn't a recheck, so there shouldn't be any unchanged *)
  let worker_mutator = Parsing_heaps.Parse_mutator.create () in
  parse
    ~worker_mutator
    ~reader
    ~parse_options
    ~skip_changed:false
    ~skip_unchanged:false
    ~profile
    ~max_header_tokens
    ~noflow
    exported_module
    workers
    next

let reparse_with_defaults
    ~transaction ~reader ?types_mode ?use_strict ?with_progress ~workers ~modified options =
  let (types_mode, use_strict, profile, max_header_tokens, noflow) =
    get_defaults ~types_mode ~use_strict options
  in
  let parse_options = make_parse_options_internal ~types_mode ~use_strict ~docblock:None options in
  let exported_module = Module_js.exported_module ~options in
  reparse
    ~transaction
    ~reader
    ~parse_options
    ~profile
    ~max_header_tokens
    ~noflow
    exported_module
    ~with_progress
    ~workers
    ~modified

(* ensure_parsed takes a set of files, finds the files which haven't been parsed, and parses them.
 * Any not-yet-parsed files who's on-disk contents don't match their already-known hash are skipped
 * and returned to the caller. *)
let ensure_parsed ~reader options workers files =
  let (types_mode, use_strict, profile, max_header_tokens, noflow) =
    get_defaults ~types_mode:None ~use_strict:None options
  in
  (* We're not replacing any info, so there's nothing to roll back. That means we can just use the
   * simple Parse_mutator rather than the rollback-able Reparse_mutator *)
  let worker_mutator = Parsing_heaps.Parse_mutator.create () in
  let progress_fn ~total ~start ~length:_ =
    MonitorRPC.status_update
      ServerStatus.(Parsing_progress { total = Some total; finished = start })
  in
  let%lwt files_missing_asts =
    MultiWorkerLwt.call
      workers
      ~job:
        (List.fold_left (fun acc fn ->
             if Parsing_heaps.Mutator_reader.has_ast ~reader fn then
               acc
             else
               FilenameSet.add fn acc
         )
        )
      ~merge:FilenameSet.union
      ~neutral:FilenameSet.empty
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements files))
  in
  let next = MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements files_missing_asts) in
  let parse_options = make_parse_options_internal ~types_mode ~use_strict ~docblock:None options in
  let exported_module = Module_js.exported_module ~options in
  let%lwt {
        parsed = _;
        unparsed = _;
        changed;
        failed = _;
        unchanged = _;
        not_found;
        package_json = _;
      } =
    parse
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_changed:true
      ~skip_unchanged:false
      ~profile
      ~max_header_tokens
      ~noflow
      exported_module
      workers
      next
  in
  Lwt.return (FilenameSet.union changed not_found)
