(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Utils_js
open Sys_utils

type result =
  | Parse_ok of {
      ast: (Loc.t, Loc.t) Flow_ast.Program.t;
      file_sig: File_sig.With_Loc.t;
      sig_extra: Parsing_heaps.sig_extra;
      tolerable_errors: File_sig.With_Loc.tolerable_error list;
      parse_errors: parse_error list;
    }
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_error = Loc.t * Parse_error.t

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of parse_error
  | File_sig_error of File_sig.With_Loc.error

and docblock_error = Loc.t * docblock_error_kind

and docblock_error_kind =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: File_sig.With_Loc.tolerable_error list FilenameMap.t;
  (* list of skipped files *)
  parse_skips: (File_key.t * Docblock.t) list;
  (* list of files skipped due to an out of date hash *)
  parse_hash_mismatch_skips: FilenameSet.t;
  (* list of failed files *)
  parse_fails: (File_key.t * Docblock.t * parse_failure) list;
  (* set of unchanged files *)
  parse_unchanged: FilenameSet.t;
}

let empty_result =
  {
    parse_ok = FilenameMap.empty;
    parse_skips = [];
    parse_hash_mismatch_skips = FilenameSet.empty;
    parse_fails = [];
    parse_unchanged = FilenameSet.empty;
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
  parse_arch: Options.arch;
  parse_abstract_locations: bool;
  parse_type_asserts: bool;
  parse_suppress_types: SSet.t;
  parse_max_literal_len: int;
  parse_exact_by_default: bool;
  parse_enable_enums: bool;
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
          esproposal_class_instance_fields = true;
          esproposal_class_static_fields = true;
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

let parse_json_file ~fail content file =
  let parse_options =
    Some
      Parser_env.
        {
          enums = false;
          esproposal_class_instance_fields = false;
          esproposal_class_static_fields = false;
          esproposal_decorators = false;
          esproposal_export_star_as = false;
          esproposal_optional_chaining = false;
          esproposal_nullish_coalescing = false;
          types = true;
          use_strict = false;
        }
  in
  (* parse the file as JSON, then munge the AST to convert from an object
     into a `module.exports = {...}` statement *)
  let (expr, parse_errors) = Parser_flow.json_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);
  let open Ast in
  let loc_none = Loc.none in
  let module_exports =
    ( loc_none,
      let open Expression in
      Member
        {
          Member._object =
            (loc_none, Identifier (Flow_ast_utils.ident_of_source (loc_none, "module")));
          property =
            Member.PropertyIdentifier (Flow_ast_utils.ident_of_source (loc_none, "exports"));
          comments = None;
        } )
  in
  let loc = fst expr in
  let statement =
    ( loc,
      Statement.Expression
        {
          Statement.Expression.expression =
            ( loc,
              Expression.Assignment
                {
                  Expression.Assignment.operator = None;
                  left = (loc_none, Pattern.Expression module_exports);
                  right = expr;
                  comments = None;
                } );
          directive = None;
          comments = None;
        } )
  in
  let all_comments = ([] : Loc.t Comment.t list) in
  ((loc, { Program.statements = [statement]; comments = None; all_comments }), parse_errors)

(* Avoid lexing unbounded in perverse cases *)
let docblock_max_tokens = 10

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
      | (loc, "@flow") :: (_, "weak") :: xs ->
        let acc =
          if info.flow <> None then
            ((loc, MultipleFlowAttributes) :: errors, info)
          else
            (errors, { info with flow = Some OptInWeak })
        in
        parse_attributes acc xs
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
            | Parse_error.Error [] -> ((expr_loc, InvalidJSXAttribute None) :: errors, info)
            | Parse_error.Error ((_, e) :: _) ->
              let first_error = Some (Parse_error.PP.error e) in
              ((expr_loc, InvalidJSXAttribute first_error) :: errors, info)
        in
        parse_attributes acc xs
      | (_, "@typeAssert") :: xs -> parse_attributes (errors, { info with typeAssert = true }) xs
      | _ :: xs -> parse_attributes (errors, info) xs
      | [] -> (errors, info)
    in
    let attributes_rx = Str.regexp "[ \t\r\n\\*/]+" in
    let lines_rx = Str.regexp "\\(\r\n\\|\n\\|\r\\)" in
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
                 { _end with column })
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
        try Sedlexing.Utf8.from_string content
        with Sedlexing.MalFormed ->
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
              | _ -> None))
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
      | None -> ([], default_info))

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
    | Some Docblock.OptInStrictLocal
    | Some Docblock.OptInWeak ->
      true)

let do_parse ~parse_options ~info content file =
  let {
    parse_fail = fail;
    parse_types_mode = types_mode;
    parse_use_strict = use_strict;
    parse_prevent_munge = prevent_munge;
    parse_module_ref_prefix = module_ref_prefix;
    parse_facebook_fbt = facebook_fbt;
    parse_arch = arch;
    parse_abstract_locations = abstract_locations;
    parse_type_asserts = type_asserts;
    parse_suppress_types = suppress_types;
    parse_max_literal_len = max_literal_len;
    parse_exact_by_default = exact_by_default;
    parse_enable_enums = enable_enums;
  } =
    parse_options
  in
  try
    match file with
    | File_key.JsonFile _ ->
      let (ast, parse_errors) = parse_json_file ~fail content file in
      (* NOTE: if ~fail:true, we'll never get parse errors here *)
      Parse_ok
        {
          ast;
          file_sig = File_sig.With_Loc.init;
          sig_extra = Parsing_heaps.Classic;
          tolerable_errors = [];
          parse_errors;
        }
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
        let exports_info = File_sig.With_Loc.program_with_exports_info ~ast ~module_ref_prefix in
        (match exports_info with
        | Error e -> Parse_fail (File_sig_error e)
        | Ok (exports_info, tolerable_errors) ->
          let (env, errors, sig_extra) =
            match arch with
            | Options.Classic ->
              let signature = Signature_builder.program ast ~exports_info in
              let (errors, _, _) =
                Signature_builder.Signature.verify
                  ~prevent_munge
                  ~facebook_fbt
                  ~ignore_static_propTypes
                  ~facebook_keyMirror
                  signature
              in
              (None, errors, Parsing_heaps.Classic)
            | Options.TypesFirst { new_signatures = false } ->
              let signature = Signature_builder.program ast ~exports_info in
              let (errors, env, sig_ast) =
                Signature_builder.Signature.verify_and_generate
                  ~prevent_munge
                  ~facebook_fbt
                  ~ignore_static_propTypes
                  ~facebook_keyMirror
                  signature
                  ast
              in
              let env =
                Some
                  (SMap.map
                     (fun lmap ->
                       Loc_collections.LocMap.fold
                         (fun loc _ acc -> Loc_collections.LocSet.add loc acc)
                         lmap
                         Loc_collections.LocSet.empty)
                     env)
              in
              let sig_ast = Ast_loc_utils.loc_to_aloc_mapper#program sig_ast in
              let (aloc_table, sig_ast) =
                if abstract_locations then
                  let (aloc_table, sig_ast) = Ast_loc_utils.keyify_alocs file sig_ast in
                  (Some aloc_table, sig_ast)
                else
                  (None, sig_ast)
              in
              let (sig_file_sig, _) =
                match File_sig.With_ALoc.program ~ast:sig_ast ~module_ref_prefix with
                | Ok fs -> fs
                | Error _ -> assert false
              in
              (env, errors, Parsing_heaps.TypesFirst { sig_ast; sig_file_sig; aloc_table })
            | Options.TypesFirst { new_signatures = true } ->
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
                }
              in
              let (errors, locs, type_sig) =
                let strict = Docblock.is_strict info in
                Type_sig_utils.parse_and_pack_module ~strict sig_opts (Some file) ast
              in
              (* TODO: extract env from type_sig *)
              let env = None in
              (* TODO: make type sig errors match signature builder errors *)
              let errors =
                List.fold_left
                  (fun acc (loc, err) ->
                    let loc = Type_sig_collections.Locs.get locs loc in
                    let err = Signature_error.TODO (Type_sig.show_errno err, loc) in
                    Signature_builder_deps.PrintableErrorSet.add err acc)
                  Signature_builder_deps.PrintableErrorSet.empty
                  errors
              in
              let aloc_table =
                Type_sig_collections.Locs.to_array locs
                |> ALoc.ALocRepresentationDoNotUse.make_table file
              in
              (env, errors, Parsing_heaps.TypeSig (type_sig, aloc_table))
          in
          let tolerable_errors =
            Signature_builder_deps.PrintableErrorSet.fold
              (fun error acc -> File_sig.With_Loc.SignatureVerificationError error :: acc)
              errors
              tolerable_errors
          in
          let file_sig = File_sig.With_Loc.verified env exports_info in
          Parse_ok { ast; file_sig; sig_extra; tolerable_errors; parse_errors })
  with
  | Parse_error.Error (first_parse_error :: _) -> Parse_fail (Parse_error first_parse_error)
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
    ~skip_hash_mismatch
    ~max_header_tokens
    ~noflow
    ~parse_unchanged
    parse_results
    file : results =
  (* It turns out that sometimes files appear and disappear very quickly. Just
   * because someone told us that this file exists and needs to be parsed, it
   * doesn't mean it actually still exists. If anything goes wrong reading this
   * file, let's skip it. We don't need to notify our caller, since they'll
   * probably get the delete event anyway *)
  let content =
    let filename_string = File_key.to_string file in
    try Some (cat filename_string)
    with e ->
      let e = Exception.wrap e in
      prerr_endlinef
        "Parsing service failed to cat %s, so skipping it. Exception: %s"
        filename_string
        (Exception.to_string e);
      None
  in
  match content with
  | Some content ->
    let new_hash = hash_content content in
    (* If skip_hash_mismatch is true, then we're currently ensuring some files are parsed. That
     * means we don't currently have the file's AST but we might have the file's hash in the
     * non-oldified heap. What we want to avoid is parsing files which differ from the hash *)
    if
      skip_hash_mismatch && Some new_hash <> Parsing_heaps.Mutator_reader.get_file_hash ~reader file
    then
      let parse_hash_mismatch_skips =
        FilenameSet.add file parse_results.parse_hash_mismatch_skips
      in
      { parse_results with parse_hash_mismatch_skips }
    else
      let unchanged =
        match Parsing_heaps.Mutator_reader.get_old_file_hash ~reader file with
        | Some old_hash when old_hash = new_hash ->
          (* If this optimization is turned off then still parse the file, even though it's
           * unchanged *)
          not parse_unchanged
        | _ ->
          (* The file has changed. Let's record the new hash *)
          worker_mutator.Parsing_heaps.add_hash file new_hash;
          false
      in
      if unchanged then
        let parse_unchanged = FilenameSet.add file parse_results.parse_unchanged in
        { parse_results with parse_unchanged }
      else (
        match parse_docblock ~max_tokens:max_header_tokens file content with
        | ([], info) ->
          let info =
            if noflow file then
              { info with Docblock.flow = Some Docblock.OptOut }
            else
              info
          in
          begin
            match do_parse ~parse_options ~info content file with
            | Parse_ok { ast; file_sig; sig_extra; tolerable_errors; parse_errors = _ } ->
              (* if parse_options.fail == true, then parse errors will hit Parse_fail below. otherwise,
                 ignore any parse errors we get here. *)
              worker_mutator.Parsing_heaps.add_file file info (ast, file_sig) sig_extra;
              let parse_ok = FilenameMap.add file tolerable_errors parse_results.parse_ok in
              { parse_results with parse_ok }
            | Parse_fail converted ->
              let fail = (file, info, converted) in
              let parse_fails = fail :: parse_results.parse_fails in
              { parse_results with parse_fails }
            | Parse_skip Skip_non_flow_file
            | Parse_skip Skip_resource_file ->
              let parse_skips = (file, info) :: parse_results.parse_skips in
              { parse_results with parse_skips }
          end
        | (docblock_errors, info) ->
          let fail = (file, info, Docblock_errors docblock_errors) in
          let parse_fails = fail :: parse_results.parse_fails in
          { parse_results with parse_fails }
      )
  | None ->
    let info = Docblock.default_info in
    let parse_skips = (file, info) :: parse_results.parse_skips in
    { parse_results with parse_skips }

(* merge is just memberwise union/concat of results *)
let merge r1 r2 =
  {
    parse_ok = FilenameMap.union r1.parse_ok r2.parse_ok;
    parse_skips = r1.parse_skips @ r2.parse_skips;
    parse_hash_mismatch_skips =
      FilenameSet.union r1.parse_hash_mismatch_skips r2.parse_hash_mismatch_skips;
    parse_fails = r1.parse_fails @ r2.parse_fails;
    parse_unchanged = FilenameSet.union r1.parse_unchanged r2.parse_unchanged;
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
        TypesForbiddenByDefault )
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
    ~skip_hash_mismatch
    ~profile
    ~max_header_tokens
    ~noflow
    ~parse_unchanged
    workers
    next : results Lwt.t =
  let t = Unix.gettimeofday () in
  let reducer =
    reducer
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_hash_mismatch
      ~max_header_tokens
      ~noflow
      ~parse_unchanged
  in
  let%lwt results =
    MultiWorkerLwt.call workers ~job:(List.fold_left reducer) ~neutral:empty_result ~merge ~next
  in
  if profile then
    let t2 = Unix.gettimeofday () in
    let ok_count = FilenameMap.cardinal results.parse_ok in
    let skip_count = List.length results.parse_skips in
    let mismatch_count = FilenameSet.cardinal results.parse_hash_mismatch_skips in
    let fail_count = List.length results.parse_fails in
    let unchanged_count = FilenameSet.cardinal results.parse_unchanged in
    Hh_logger.info
      "parsed %d files (%d ok, %d skipped, %d bad hashes, %d failed, %d unchanged) in %f"
      (ok_count + skip_count + mismatch_count + fail_count)
      ok_count
      skip_count
      mismatch_count
      fail_count
      unchanged_count
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
    ~parse_unchanged
    ~with_progress
    ~workers
    ~modified:files
    ~deleted =
  (* save old parsing info for files *)
  let all_files = FilenameSet.union files deleted in
  let (master_mutator, worker_mutator) =
    Parsing_heaps.Reparse_mutator.create transaction all_files
  in
  let next = next_of_filename_set ?with_progress workers files in
  let%lwt results =
    parse
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_hash_mismatch:false
      ~profile
      ~max_header_tokens
      ~noflow
      ~parse_unchanged
      workers
      next
  in
  let modified = results.parse_ok |> FilenameMap.keys |> FilenameSet.of_list in
  let modified =
    List.fold_left (fun acc (fail, _, _) -> FilenameSet.add fail acc) modified results.parse_fails
  in
  let modified =
    List.fold_left (fun acc (skip, _) -> FilenameSet.add skip acc) modified results.parse_skips
  in
  let modified = FilenameSet.union modified results.parse_hash_mismatch_skips in
  SharedMem_js.collect `gentle;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  Parsing_heaps.Reparse_mutator.revive_files master_mutator unchanged;
  Lwt.return (modified, results)

let make_parse_options_internal
    ?(fail = true) ?(types_mode = TypesAllowed) ?use_strict ~docblock options =
  let use_strict =
    match use_strict with
    | Some use_strict -> use_strict
    | None -> Options.modules_are_use_strict options
  in
  let module_ref_prefix = Options.haste_module_ref_prefix options in
  let facebook_fbt = Options.facebook_fbt options in
  let arch = Options.arch options in
  let abstract_locations = Options.abstract_locations options in
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
    parse_arch = arch;
    parse_abstract_locations = abstract_locations;
    parse_type_asserts = Options.type_asserts options;
    parse_suppress_types = Options.suppress_types options;
    parse_max_literal_len = Options.max_literal_length options;
    parse_exact_by_default = Options.exact_by_default options;
    parse_enable_enums = Options.enums options;
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
  let parse_unchanged = true in
  (* This isn't a recheck, so there shouldn't be any unchanged *)
  let worker_mutator = Parsing_heaps.Parse_mutator.create () in
  parse
    ~worker_mutator
    ~reader
    ~parse_options
    ~skip_hash_mismatch:false
    ~profile
    ~max_header_tokens
    ~noflow
    ~parse_unchanged
    workers
    next

let reparse_with_defaults
    ~transaction ~reader ?types_mode ?use_strict ?with_progress ~workers ~modified ~deleted options
    =
  let (types_mode, use_strict, profile, max_header_tokens, noflow) =
    get_defaults ~types_mode ~use_strict options
  in
  let parse_unchanged = false in
  (* We're rechecking, so let's skip files which haven't changed *)
  let parse_options = make_parse_options_internal ~types_mode ~use_strict ~docblock:None options in
  reparse
    ~transaction
    ~reader
    ~parse_options
    ~profile
    ~max_header_tokens
    ~noflow
    ~parse_unchanged
    ~with_progress
    ~workers
    ~modified
    ~deleted

(* ensure_parsed takes a set of files, finds the files which haven't been parsed, and parses them.
 * Any not-yet-parsed files who's on-disk contents don't match their already-known hash are skipped
 * and returned to the caller. *)
let ensure_parsed ~reader options workers files =
  let (types_mode, use_strict, profile, max_header_tokens, noflow) =
    get_defaults ~types_mode:None ~use_strict:None options
  in
  (* We want to parse unchanged files, since this is our first time parsing them *)
  let parse_unchanged = true in
  (* We're not replacing any info, so there's nothing to roll back. That means we can just use the
   * simle Parse_mutator rather than the rollback-able Reparse_mutator *)
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
               FilenameSet.add fn acc))
      ~merge:FilenameSet.union
      ~neutral:FilenameSet.empty
      ~next:(MultiWorkerLwt.next workers (FilenameSet.elements files))
  in
  let next = MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements files_missing_asts) in
  let parse_options = make_parse_options_internal ~types_mode ~use_strict ~docblock:None options in
  let%lwt results =
    parse
      ~worker_mutator
      ~reader
      ~parse_options
      ~skip_hash_mismatch:true
      ~profile
      ~max_header_tokens
      ~noflow
      ~parse_unchanged
      workers
      next
  in
  Lwt.return results.parse_hash_mismatch_skips
