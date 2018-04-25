(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Sys_utils

exception Ast_not_found of string
exception Docblock_not_found of string
exception Requires_not_found of string

type result =
  | Parse_ok of Loc.t Ast.program * File_sig.t
  | Parse_fail of parse_failure
  | Parse_skip of parse_skip_reason

and parse_skip_reason =
  | Skip_resource_file
  | Skip_non_flow_file

and parse_failure =
  | Docblock_errors of docblock_error list
  | Parse_error of (Loc.t * Parse_error.t)
  | File_sig_error of File_sig.error

and docblock_error = Loc.t * docblock_error_kind
and docblock_error_kind =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

(* results of parse job, returned by parse and reparse *)
type results = {
  (* successfully parsed files *)
  parse_ok: (File_sig.tolerable_error list) FilenameMap.t;

  (* list of skipped files *)
  parse_skips: (File_key.t * Docblock.t) list;

  (* list of failed files *)
  parse_fails: (File_key.t * Docblock.t * parse_failure) list;
}

let empty_result = {
  parse_ok = FilenameMap.empty;
  parse_skips = [];
  parse_fails = [];
}

(**************************** internal *********************************)

(* shared heap for parsed ASTs by filename *)
module ASTHeap = SharedMem_js.WithCache (File_key) (struct
    type t = Loc.t Ast.program
    let prefix = Prefix.make()
    let description = "AST"
    let use_sqlite_fallback () = false
end)

module DocblockHeap = SharedMem_js.WithCache (File_key) (struct
    type t = Docblock.t
    let prefix = Prefix.make()
    let description = "Docblock"
    let use_sqlite_fallback () = false
end)

module FileSigHeap = SharedMem_js.WithCache (File_key) (struct
    type t = File_sig.t
    let prefix = Prefix.make()
    let description = "Requires"
    let use_sqlite_fallback () = false
end)

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add file ast info file_sig =
    ASTHeap.add file ast;
    DocblockHeap.add file info;
    FileSigHeap.add file file_sig

  let oldify_batch files =
    ASTHeap.oldify_batch files;
    DocblockHeap.oldify_batch files;
    FileSigHeap.oldify_batch files

  let remove_batch files =
    ASTHeap.remove_batch files;
    DocblockHeap.remove_batch files;
    FileSigHeap.remove_batch files

  let remove_old_batch files =
    ASTHeap.remove_old_batch files;
    DocblockHeap.remove_old_batch files;
    FileSigHeap.remove_old_batch files

  let revive_batch files =
    ASTHeap.revive_batch files;
    DocblockHeap.revive_batch files;
    FileSigHeap.revive_batch files
end

(* TODO: add TypesForbidden (disables types even on files with @flow) and
   TypesAllowedByDefault (enables types even on files without @flow, but allows
   something like @noflow to disable them) *)
type types_mode =
  | TypesAllowed
  | TypesForbiddenByDefault

let parse_source_file ~fail ~types ~use_strict content file =
  let parse_options = Some Parser_env.({
    (**
     * Always parse ES proposal syntax. The user-facing config option to
     * ignore/warn/enable them is handled during inference so that a clean error
     * can be surfaced (rather than a more cryptic parse error).
     *)
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    esproposal_optional_chaining = true;
    types = types;
    use_strict;
  }) in
  let ast, parse_errors =
    Parser_flow.program_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);
  ast

let parse_json_file ~fail content file =
  let parse_options = Some Parser_env.({
    esproposal_class_instance_fields = false;
    esproposal_class_static_fields = false;
    esproposal_decorators = false;
    esproposal_export_star_as = false;
    esproposal_optional_chaining = false;
    types = true;
    use_strict = false;
  }) in

  (* parse the file as JSON, then munge the AST to convert from an object
     into a `module.exports = {...}` statement *)
  let expr, parse_errors =
    Parser_flow.json_file ~fail ~parse_options content (Some file) in
  if fail then assert (parse_errors = []);

  let open Ast in
  let loc_none = Loc.none in
  let module_exports = loc_none, Expression.(Member { Member.
    _object = loc_none, Identifier (loc_none, "module");
    property = Member.PropertyIdentifier (loc_none, "exports");
    computed = false;
    optional = false;
  }) in
  let loc = fst expr in
  let statement =
    loc, Statement.Expression { Statement.Expression.
      expression = loc, Expression.Assignment { Expression.Assignment.
        operator = Expression.Assignment.Assign;
        left = loc_none, Pattern.Expression module_exports;
        right = expr;
      };
      directive = None;
    }
  in
  let comments = ([]: Loc.t Comment.t list) in
  (loc, [statement], comments)

(* Avoid lexing unbounded in perverse cases *)
let docblock_max_tokens = 10

let extract_docblock =
  let open Docblock in
  (* walks a list of words, returns a list of errors and the extracted info.
     if @flow or @providesModule is found more than once, the first one is used
     and an error is returned. *)
  let rec parse_attributes (errors, info) = function
    | (loc, "@flow") :: (_, "strict") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptInStrict } in
        parse_attributes acc xs
    | (loc, "@flow") :: (_, "strict-local") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptInStrictLocal } in
        parse_attributes acc xs
    | (loc, "@flow") :: (_, "weak") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptInWeak } in
        parse_attributes acc xs
    | (loc, "@flow") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptIn } in
        parse_attributes acc xs
    | (loc, "@noflow") :: xs ->
        let acc =
          if info.flow <> None then (loc, MultipleFlowAttributes)::errors, info
          else errors, { info with flow = Some OptOut } in
        parse_attributes acc xs
    | (loc, "@providesModule") :: (_, m) :: xs ->
        let acc =
          if info.providesModule <> None then
            (loc, MultipleProvidesModuleAttributes)::errors, info
          else
            errors, { info with providesModule = Some m }
        in
        parse_attributes acc xs
    | (_, "@preventMunge") :: xs ->
        (* dupes are ok since they can only be truthy *)
        let preventMunge = Some true in
        parse_attributes (errors, { info with preventMunge }) xs
    | (csx_loc, "@csx") :: xs ->
        let acc =
          if info.jsx <> None then
            (csx_loc, MultipleJSXAttributes)::errors, info
          else
            errors, { info with jsx = Some Csx_pragma }
        in
        parse_attributes acc xs
    | [jsx_loc, "@jsx"] -> (jsx_loc, InvalidJSXAttribute None)::errors, info
    | (jsx_loc, "@jsx") :: (expr_loc, expr) :: xs ->
        let acc =
          if info.jsx <> None
          then (jsx_loc, MultipleJSXAttributes)::errors, info
          else begin
            (* The point of the padding is to make the parsed code line up
             * with the comment in the original source *)
            let padding = (String.make Loc.(expr_loc.start.line - 1) '\n') ^
              (String.make Loc.(expr_loc.start.column) ' ') in
            try
              let (jsx_expr, _) = Parser_flow.jsx_pragma_expression
                (padding ^ expr)
                expr_loc.Loc.source in
              errors, { info with jsx = Some (Jsx_pragma (expr, jsx_expr)) }
            with
            | Parse_error.Error [] ->
                (expr_loc, InvalidJSXAttribute None)::errors, info
            | Parse_error.Error ((_, e)::_) ->
                let first_error = Some (Parse_error.PP.error e) in
                (expr_loc, InvalidJSXAttribute first_error)::errors, info
          end in
        parse_attributes acc xs

    | _ :: xs ->
        parse_attributes (errors, info) xs
    | [] -> (errors, info)
  in

  let attributes_rx = Str.regexp "[ \t\r\n\\*/]+" in
  let lines_rx = Str.regexp "\\(\r\n\\|\n\\|\r\\)" in
  let calc_end start s =
    Str.full_split lines_rx s
    |> List.fold_left Loc.(fun _end elem ->
      match elem with
      | Str.Delim delim ->
          let line_incr = if delim = "\r" then 0 else 1 in
          let column = 0 in
          let line = _end.line + line_incr in
          let offset = _end.offset + (String.length delim) in
          { column; line; offset; }
      | Str.Text text ->
          let length = String.length text in
          let column = _end.column + length in
          let offset = _end.offset + length in
          { _end with column; offset; }
    ) start in
  let split loc s =
    (* Need to add 2 characters for the start of the comment *)
    let start = Loc.({ loc.start with
      column = loc.start.column + 2;
      offset = loc.start.offset + 2;
    }) in
    Str.full_split attributes_rx s
    |> List.fold_left (fun (start, attributes) elem ->
      match elem with
      | Str.Delim s ->
          (calc_end start s, attributes)
      | Str.Text s ->
          let _end = calc_end start s in
          (_end, Loc.({loc with start; _end; }, s)::attributes)
    ) (start, [])
    |> snd
    |> List.rev

  in

  let string_of_comment = function
  | (loc, Ast.Comment.Block s)
  | (loc, Ast.Comment.Line s)
    -> loc, s
  in

  let map_n =
    let rec helper f remaining acc = function
      | [] -> List.rev acc
      | hd::rest ->
        if remaining <= 0 then List.rev acc
        else helper f (remaining - 1) ((f hd)::acc) rest
    in
    fun f n lst -> helper f n [] lst
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
        Sedlexing.Utf8.from_string "" in
    let env =
      Lex_env.new_lex_env (Some filename) lb ~enable_types_in_comments:false in
    let rec get_first_comment_contents ?(i=0) env =
      if i < max_tokens then
        let env, lexer_result = Lexer.token env in
        match Lex_result.comments lexer_result with
        | [] -> Token.(
            (**
             * Stop looking for docblocks if we see any tokens other than a
             * string or a semicolon (`"use babel";` or `"use strict";`).
             *)
            match Lex_result.token lexer_result with
            | T_STRING _
            | T_SEMICOLON
              -> get_first_comment_contents ~i:(i + 1) env
            | _ -> None
          )
        | comments ->
          Some (map_n string_of_comment (max_tokens - i) comments)
      else None in
    let info =
      let filename_str = File_key.to_string filename in
      if Filename.check_suffix filename_str Files.flow_ext
      then { default_info with isDeclarationFile = true; }
      else default_info in
    match get_first_comment_contents env with
    | Some comments ->
        List.fold_left (fun acc (loc, s) ->
          parse_attributes acc (split loc s)
        ) ([], info) comments
    | None -> [], info

let parse_docblock
  ~max_tokens file content
: docblock_error list * Docblock.t =
  match file with
  | File_key.ResourceFile _
  | File_key.JsonFile _ -> [], Docblock.default_info
  | _ -> extract_docblock ~max_tokens file content

(* Allow types based on `types_mode`, using the @flow annotation in the
   file header if possible. Note, this should be consistent with
   Infer_service.apply_docblock_overrides w.r.t. the metadata.checked flag. *)
let types_checked types_mode info =
  match types_mode with
  | TypesAllowed -> true
  | TypesForbiddenByDefault ->
    match Docblock.flow info with
    | None
    | Some Docblock.OptOut -> false
    | Some Docblock.OptIn
    | Some Docblock.OptInStrict
    | Some Docblock.OptInStrictLocal
    | Some Docblock.OptInWeak -> true

let do_parse ?(fail=true) ~types_mode ~use_strict ~info content file =
  try (
    match file with
    | File_key.JsonFile _ ->
      let ast = parse_json_file ~fail content file in
      Parse_ok (ast, File_sig.empty_file_sig)
    | File_key.ResourceFile _ ->
      Parse_skip Skip_resource_file
    | _ ->
      (* either all=true or @flow pragma exists *)
      let types_checked = types_checked types_mode info in
      (* always parse types for .flow files -- NB: will _not_ be inferred *)
      let types = types_checked || Docblock.isDeclarationFile info in
      if not types
      then Parse_skip Skip_non_flow_file
      else
        let ast = parse_source_file ~fail ~types ~use_strict content file in
        (* Only calculate file sigs for files which will actually be inferred.
         * The only files which are parsed but not inferred are .flow files with
         * no @flow pragma. *)
        if types_checked then
          match File_sig.program ~ast with
          | Ok file_sig -> Parse_ok (ast, file_sig)
          | Error e -> Parse_fail (File_sig_error e)
        else
          Parse_ok (ast, File_sig.empty_file_sig))
  with
  | Parse_error.Error (first_parse_error::_) ->
    Parse_fail (Parse_error first_parse_error)
  | e ->
    let s = Printexc.to_string e in
    let loc = Loc.({ none with source = Some file }) in
    let err = loc, Parse_error.Assertion s in
    Parse_fail (Parse_error err)

(* parse file, store AST to shared heap on success.
 * Add success/error info to passed accumulator. *)
let reducer
  ~types_mode ~use_strict ~max_header_tokens ~lazy_mode ~noflow
  parse_results
  file
: results =
  (* It turns out that sometimes files appear and disappear very quickly. Just
   * because someone told us that this file exists and needs to be parsed, it
   * doesn't mean it actually still exists. If anything goes wrong reading this
   * file, let's skip it. We don't need to notify our caller, since they'll
   * probably get the delete event anyway *)
  let content =
    let filename_string = File_key.to_string file in
    try Some (cat filename_string)
    with e ->
      prerr_endlinef
        "Parsing service failed to cat %s, so skipping it. Exception: %s"
        filename_string
        (Printexc.to_string e);
      None in
  match content with
  | Some content ->
      begin match parse_docblock ~max_tokens:max_header_tokens file content with
      | [], info ->
        let info =
          if noflow file then { info with Docblock.flow = Some Docblock.OptOut }
          else info
        in
        begin match (do_parse ~types_mode ~use_strict ~info content file) with
        | Parse_ok (ast, file_sig) ->
            (* Consider the file unchanged if its reparsing info is the same as
             * its old parsing info. A complication is that we don't want to
             * drop a .flow file, even if it is unchanged, since it might have
             * been added to the modified set simply because a corresponding
             * implementation file was also added. *)
            if not (File_key.check_suffix file Files.flow_ext)
              (* In --lazy mode, a file is parsed initially but not
                 checked, and reparsing it later triggers a check even if the
                 file hasn't changed.

                 TODO: this optimization is never hit in this mode, but we can
                 use it if `file` is in the current set of checked files. *)
              && not lazy_mode
              && (ASTHeap.get_old file = Some ast)
            then parse_results
            else begin
              ParsingHeaps.add file ast info file_sig;
              let parse_ok = FilenameMap.add file file_sig.File_sig.tolerable_errors parse_results.parse_ok in
              { parse_results with parse_ok; }
            end
        | Parse_fail converted ->
            let fail = (file, info, converted) in
            let parse_fails = fail :: parse_results.parse_fails in
            { parse_results with parse_fails; }
        | Parse_skip Skip_non_flow_file
        | Parse_skip Skip_resource_file ->
            let parse_skips = (file, info) :: parse_results.parse_skips in
            { parse_results with parse_skips; }
        end
      | docblock_errors, info ->
        let fail = (file, info, Docblock_errors docblock_errors) in
        let parse_fails = fail :: parse_results.parse_fails in
        { parse_results with parse_fails; }
      end
  | None ->
      let info = Docblock.default_info in
      let parse_skips = (file, info) :: parse_results.parse_skips in
      { parse_results with parse_skips; }

(* merge is just memberwise union/concat of results *)
let merge r1 r2 =
  {
    parse_ok = FilenameMap.union r1.parse_ok r2.parse_ok;
    parse_skips = r1.parse_skips @ r2.parse_skips;
    parse_fails = r1.parse_fails @ r2.parse_fails;
  }

let opt_or_alternate opt alternate =
  match opt with
    | Some x -> x
    | None -> alternate

(* types_mode and use_strict aren't special, they just happen to be the ones that needed to be
overridden *)
let get_defaults ~types_mode ~use_strict options =
  let types_mode = opt_or_alternate
    types_mode
    (* force types when --all is set, but otherwise forbid them unless the file
       has @flow in it. *)
    (if Options.all options then TypesAllowed else TypesForbiddenByDefault)
  in
  let use_strict = opt_or_alternate
    use_strict
    (Options.modules_are_use_strict options)
  in
  let profile = Options.should_profile options in
  let max_header_tokens = Options.max_header_tokens options in
  let lazy_mode = Options.is_lazy_mode options in
  let noflow fn =
    Files.is_untyped (Options.file_options options) (File_key.to_string fn)
  in
  types_mode, use_strict, profile, max_header_tokens, lazy_mode, noflow

(***************************** public ********************************)

let progress_fn ~total ~start ~length:_ =
  let finished = start in
  MonitorRPC.status_update
    ServerStatus.(Parsing_progress { total = Some total; finished })

let next_of_filename_set ?(with_progress=false) workers filenames =
  if with_progress
  then MultiWorkerLwt.next ~progress_fn workers (FilenameSet.elements filenames)
  else MultiWorkerLwt.next workers (FilenameSet.elements filenames)

let parse ~types_mode ~use_strict ~profile ~max_header_tokens ~lazy_mode ~noflow
  workers next
: results Lwt.t =
  let t = Unix.gettimeofday () in
  let reducer = reducer ~types_mode ~use_strict ~max_header_tokens ~lazy_mode ~noflow in
  let%lwt results = MultiWorkerLwt.call
    workers
    ~job: (List.fold_left reducer)
    ~neutral: empty_result
    ~merge
    ~next
  in
  if profile then
    let t2 = Unix.gettimeofday () in
    let ok_count = FilenameMap.cardinal results.parse_ok in
    let skip_count = List.length results.parse_skips in
    let fail_count = List.length results.parse_fails in
    Hh_logger.info "parsed %d files (%d ok, %d skipped, %d failed) in %f"
      (ok_count + skip_count + fail_count)
      ok_count skip_count fail_count
      (t2 -. t)
  else ();
  Lwt.return results

let reparse
  ~types_mode ~use_strict ~profile ~max_header_tokens ~lazy_mode ~noflow ?with_progress workers files =
  (* save old parsing info for files *)
  ParsingHeaps.oldify_batch files;
  let next = next_of_filename_set ?with_progress workers files in
  let%lwt results =
    parse ~types_mode ~use_strict ~profile ~max_header_tokens ~lazy_mode ~noflow workers next
  in
  let modified = results.parse_ok |> FilenameMap.keys |> FilenameSet.of_list in
  let modified = List.fold_left (fun acc (fail, _, _) ->
    FilenameSet.add fail acc
  ) modified results.parse_fails in
  let modified = List.fold_left (fun acc (skip, _) ->
    FilenameSet.add skip acc
  ) modified results.parse_skips in
  (* discard old parsing info for modified files *)
  ParsingHeaps.remove_old_batch modified;
  let unchanged = FilenameSet.diff files modified in
  (* restore old parsing info for unchanged files *)
  ParsingHeaps.revive_batch unchanged;
  SharedMem_js.collect `gentle;
  Lwt.return (modified, results)

let parse_with_defaults ?types_mode ?use_strict options workers next =
  let types_mode, use_strict, profile, max_header_tokens, lazy_mode, noflow =
    get_defaults ~types_mode ~use_strict options
  in
  parse ~types_mode ~use_strict ~profile ~max_header_tokens ~lazy_mode ~noflow workers next

let reparse_with_defaults ?types_mode ?use_strict ?with_progress options workers files =
  let types_mode, use_strict, profile, max_header_tokens, lazy_mode, noflow =
    get_defaults ~types_mode ~use_strict options
  in
  reparse
    ~types_mode ~use_strict ~profile ~max_header_tokens ~lazy_mode ~noflow ?with_progress
    workers files

let has_ast = ASTHeap.mem

let get_ast = ASTHeap.get

let get_docblock = DocblockHeap.get

let get_file_sig = FileSigHeap.get

let get_ast_unsafe file =
  try ASTHeap.find_unsafe file
  with Not_found -> raise (Ast_not_found (File_key.to_string file))

let get_docblock_unsafe file =
  try DocblockHeap.find_unsafe file
  with Not_found -> raise (Docblock_not_found (File_key.to_string file))

let get_file_sig_unsafe file =
  try FileSigHeap.find_unsafe file
  with Not_found -> raise (Requires_not_found (File_key.to_string file))

let remove_batch files =
  ParsingHeaps.remove_batch files
