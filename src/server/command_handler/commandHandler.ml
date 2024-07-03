(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result
open Loc_collections
open ServerEnv
open Utils_js
open Lsp
open Types_js_types

type ephemeral_parallelizable_result = ServerProt.Response.response * Hh_json.json option

type ephemeral_nonparallelizable_result =
  ServerEnv.env * ServerProt.Response.response * Hh_json.json option

type persistent_parallelizable_result = LspProt.response * LspProt.metadata

type persistent_nonparallelizable_result = ServerEnv.env * LspProt.response * LspProt.metadata

type 'a workload = profiling:Profiling_js.running -> env:ServerEnv.env -> 'a Lwt.t

(* Returns the result of calling `type_parse_artifacts`, along with a bool option indicating
 * whether the cache was hit -- None if no cache was available, Some true if it was hit, and Some
 * false if it was missed. *)
let type_parse_artifacts_with_cache
    ~options ~profiling ~type_parse_artifacts_cache master_cx file artifacts =
  match type_parse_artifacts_cache with
  | None ->
    let result =
      Type_contents.type_parse_artifacts ~options ~profiling master_cx file (Lazy.force artifacts)
    in
    (result, None)
  | Some cache ->
    let lazy_result =
      lazy
        (Type_contents.type_parse_artifacts
           ~options
           ~profiling
           master_cx
           file
           (Lazy.force artifacts)
        )
    in
    let (result, did_hit) =
      FilenameCache.with_cache_sync ~cond:(fun _ -> true) file lazy_result cache
    in
    (result, Some did_hit)

let add_cache_hit_data_to_json json_props did_hit =
  match did_hit with
  | None ->
    (* This means the cache was not available *)
    json_props
  | Some did_hit -> ("cached", Hh_json.JSON_Bool did_hit) :: json_props

(** Catch exceptions, stringify them, and return Error. Otherwise, return
    the unchanged result of calling `f`.

    Does NOT catch Lwt.Canceled, because that is used as a signal to restart the command.
    TODO: this should be a dedicated exception, but we have to make sure nothing
    swallows it. *)
let try_with f =
  (* NOT try%lwt, even though we catch Lwt.Canceled *)
  try f () with
  | Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    Error (Exception.to_string exn)

let try_with_lwt f =
  try%lwt f () with
  | Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    Lwt.return (Error (Exception.to_string exn))

(** Catch exceptions, stringify them, and return Error. Otherwise, return
    the unchanged result of calling `f`.

    Does NOT catch Lwt.Canceled, because that is used as a signal to restart the command.
    TODO: this should be a dedicated exception, but we have to make sure nothing
    swallows it. *)
let try_with_json : (unit -> ('a, string) result * 'json) -> ('a, string) result * 'json =
 fun f ->
  (* NOT try%lwt, even though we catch Lwt.Canceled *)
  try f () with
  | Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Exception.reraise exn
  | exn ->
    let exn = Exception.wrap exn in
    (Error (Exception.to_string exn), None)

let status_log errors =
  if Flow_errors_utils.ConcreteLocPrintableErrorSet.is_empty errors then
    Hh_logger.info "Status: OK"
  else
    Hh_logger.info "Status: Error";
  flush stdout

let convert_errors ~errors ~warnings ~suppressed_errors =
  if
    Flow_errors_utils.ConcreteLocPrintableErrorSet.is_empty errors
    && Flow_errors_utils.ConcreteLocPrintableErrorSet.is_empty warnings
    && suppressed_errors = []
  then
    ServerProt.Response.NO_ERRORS
  else
    ServerProt.Response.ERRORS { errors; warnings; suppressed_errors }

let json_of_parse_error =
  let int x = Hh_json.JSON_Number (string_of_int x) in
  let position p = Hh_json.JSON_Object [("line", int p.Loc.line); ("column", int p.Loc.column)] in
  let location loc =
    Hh_json.JSON_Object [("start", position loc.Loc.start); ("end", position loc.Loc._end)]
  in
  fun (loc, err) ->
    Hh_json.JSON_Object
      [("loc", location loc); ("message", Hh_json.JSON_String (Parse_error.PP.error err))]

let fold_json_of_parse_errors parse_errors acc =
  match parse_errors with
  | err :: _ ->
    ("parse_error", json_of_parse_error err)
    :: ("parse_error_count", Hh_json.JSON_Number (parse_errors |> List.length |> string_of_int))
    :: acc
  | [] -> acc

let file_input_of_text_document_identifier ~client t =
  let filename = Flow_lsp_conversions.lsp_DocumentIdentifier_to_flow_path t in
  Persistent_connection.get_file client filename

let file_input_of_text_document_identifier_opt ~client_id t =
  Base.Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
      file_input_of_text_document_identifier ~client t
  )

let file_input_of_text_document_position ~client t =
  let { Lsp.TextDocumentPositionParams.textDocument; _ } = t in
  file_input_of_text_document_identifier ~client textDocument

let file_input_of_text_document_position_opt ~client_id t =
  Base.Option.map (Persistent_connection.get_client client_id) ~f:(fun client ->
      file_input_of_text_document_position ~client t
  )

let file_key_of_file_input_without_env ~options ~libs file_input =
  let file_options = Options.file_options options in
  File_input.filename_of_file_input file_input
  |> Files.filename_from_string ~options:file_options ~consider_libdefs:true ~libs

let file_key_of_file_input ~options ~env file_input =
  file_key_of_file_input_without_env ~options ~libs:env.ServerEnv.libs file_input

(* This tries to simulate the logic from elsewhere which determines whether we would report
 * errors for a given file. The criteria are
 *
 * 1) The file must be either implicitly included (be in the same dir structure as .flowconfig)
 *    or explicitly included
 * 2) The file must not be ignored
 * 3) The file path must be a Flow file (e.g foo.js and not foo.php or foo/)
 * 4) The file must either have `// @flow` or all=true must be set in the .flowconfig or CLI
 *)
let check_that_we_care_about_this_file =
  let is_stdin file_path = String.equal file_path "-" in
  let check_file_not_ignored ~file_options ~env ~file_path () =
    if Files.wanted ~options:file_options ~include_libdef:true env.ServerEnv.libs file_path then
      Ok ()
    else
      Error "File is ignored"
  in
  let check_file_included ~options ~file_options ~file_path () =
    let file_is_implicitly_included =
      let root_str = spf "%s%s" (File_path.to_string (Options.root options)) Filename.dir_sep in
      String.starts_with ~prefix:root_str file_path
    in
    if file_is_implicitly_included then
      Ok ()
    else if Files.is_included file_options file_path then
      Ok ()
    else
      Error "File is not implicitly or explicitly included"
  in
  let check_is_flow_file ~file_options ~file_path () =
    if Files.is_flow_file ~options:file_options file_path then
      Ok ()
    else
      Error "File is not a Flow file"
  in
  let check_flow_pragma ~options ~content ~file_key () =
    if Options.all options || File_key.is_lib_file file_key then
      Ok ()
    else
      let (_, docblock) =
        Docblock_parser.(
          parse_docblock
            ~max_tokens:docblock_max_tokens
            ~file_options:(Options.file_options options)
            file_key
            content
        )
      in
      if Docblock.is_flow docblock then
        Ok ()
      else
        Error "File is missing @flow pragma and `all` is not set to `true`"
  in
  fun ~options ~env ~file_key ~content ->
    let file_path = File_key.to_string file_key in
    if is_stdin file_path then
      (* if we don't know the filename (stdin), assume it's ok *)
      Ok ()
    else if Files.is_in_flowlib (Options.file_options options) file_path then
      Ok ()
    else
      let file_path = Files.imaginary_realpath file_path in
      let file_options = Options.file_options options in
      Ok ()
      >>= check_file_not_ignored ~file_options ~env ~file_path
      >>= check_file_included ~options ~file_options ~file_path
      >>= check_is_flow_file ~file_options ~file_path
      >>= check_flow_pragma ~options ~content ~file_key

type ide_file_error =
  | Skipped of string
  | Failed of string

let json_props_of_skipped reason =
  let open Hh_json in
  [("skipped", JSON_Bool true); ("skip_reason", JSON_String reason)]

let json_of_skipped reason = Some (Hh_json.JSON_Object (json_props_of_skipped reason))

let of_file_input ~options ~env file_input =
  let file_key = file_key_of_file_input ~options ~env file_input in
  match File_input.content_of_file_input file_input with
  | Error msg -> Error (Failed msg)
  | Ok file_contents ->
    (match check_that_we_care_about_this_file ~options ~env ~file_key ~content:file_contents with
    | Error reason -> Error (Skipped reason)
    | Ok () -> Ok (file_key, file_contents))

let get_haste_name ~reader f =
  Parsing_heaps.get_file_addr f |> Base.Option.bind ~f:(Parsing_heaps.Reader.get_haste_name ~reader)

let mk_module_system_info =
  let is_package_file ~reader module_name =
    let dependency = Parsing_heaps.get_dependency (Modulename.String module_name) in
    match Option.bind dependency (Parsing_heaps.Reader.get_provider ~reader) with
    | Some addr -> Parsing_heaps.Reader.is_package_file ~reader addr
    | None -> false
  in
  fun ~options ~reader ->
    {
      Lsp_module_system_info.file_options = Options.file_options options;
      haste_module_system = Options.(module_system options = Haste);
      get_haste_name = get_haste_name ~reader;
      get_package_info = Parsing_heaps.Reader.get_package_info ~reader;
      is_package_file = is_package_file ~reader;
    }

let get_status ~options ~reader env =
  let lazy_stats = Rechecker.get_lazy_stats ~options env in
  let status_response =
    (* collate errors by origin *)
    let (errors, warnings, suppressed_errors) =
      if Options.include_suppressions options then
        ErrorCollator.get env
      else
        let (errors, warnings) = ErrorCollator.get_without_suppressed env in
        (errors, warnings, [])
    in
    let warnings =
      if Options.should_include_warnings options then
        warnings
      else
        Flow_errors_utils.ConcreteLocPrintableErrorSet.empty
    in
    (* TODO: check status.directory *)
    status_log errors;
    FlowEventLogger.status_response
      ~num_errors:(Flow_errors_utils.ConcreteLocPrintableErrorSet.cardinal errors);
    let to_printable =
      Flow_intermediate_error.to_printable_error
        ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
        ~strip_root:(Base.Option.some_if (Options.should_strip_root options) (Options.root options))
    in
    let suppressed_errors =
      Base.List.map suppressed_errors ~f:(fun (e, loc_set) -> (to_printable e, loc_set))
    in
    convert_errors ~errors ~warnings ~suppressed_errors
  in
  (status_response, lazy_stats)

let autoimport_options ~ac_options =
  let open Export_search in
  {
    default_options with
    max_results = 100;
    num_threads = Base.Int.max 1 (Sys_utils.nbr_procs - 2);
    weighted = ac_options.AutocompleteService_js.imports_ranked_usage;
  }

let search_exported_values ~exports ~ac_options before =
  Export_search.search_values ~options:(autoimport_options ~ac_options) before exports

let search_exported_types ~exports ~ac_options before =
  Export_search.search_types ~options:(autoimport_options ~ac_options) before exports

let json_of_autocomplete_result initial_json_props = function
  | None ->
    let err_str = "Couldn't parse file in parse_contents" in
    let json_data_to_log =
      let open Hh_json in
      JSON_Object
        (("errors", JSON_Array [JSON_String err_str])
        :: ("result", JSON_String "FAILURE_CHECK_CONTENTS")
        :: ("count", JSON_Number "0")
        :: initial_json_props
        )
    in
    (Error err_str, Some json_data_to_log)
  | Some (info, parse_errors, token_opt, ac_loc, ac_type_string, results_res) ->
    let open AutocompleteService_js in
    let json_props_to_log =
      ("ac_type", Hh_json.JSON_String ac_type_string)
      :: ("docblock", Docblock.json_of_docblock info)
      :: ( "token",
           match token_opt with
           | None -> Hh_json.JSON_Null
           | Some token -> Hh_json.JSON_String token
         )
      :: initial_json_props
    in
    let (response, json_props_to_log) =
      let open Hh_json in
      match results_res with
      | AcResult { result; errors_to_log } ->
        let { ServerProt.Response.Completion.items; is_incomplete = _ } = result in
        let result_string =
          match (items, errors_to_log) with
          | (_, []) -> "SUCCESS"
          | ([], _ :: _) -> "FAILURE"
          | (_ :: _, _ :: _) -> "PARTIAL"
        in
        let at_least_one_result_has_documentation =
          Base.List.exists items ~f:(fun ServerProt.Response.Completion.{ documentation; _ } ->
              Base.Option.is_some documentation
          )
        in
        ( Ok (token_opt, result, ac_loc, ac_type_string),
          ("result", JSON_String result_string)
          :: ("count", JSON_Number (items |> List.length |> string_of_int))
          :: ("errors", JSON_Array (Base.List.map ~f:(fun s -> JSON_String s) errors_to_log))
          :: ("documentation", JSON_Bool at_least_one_result_has_documentation)
          :: json_props_to_log
        )
      | AcEmpty reason ->
        ( Ok
            ( token_opt,
              { ServerProt.Response.Completion.items = []; is_incomplete = false },
              ac_loc,
              ac_type_string
            ),
          ("result", JSON_String "SUCCESS")
          :: ("count", JSON_Number "0")
          :: ("empty_reason", JSON_String reason)
          :: json_props_to_log
        )
      | AcFatalError error ->
        ( Error error,
          ("result", JSON_String "FAILURE")
          :: ("errors", JSON_Array [JSON_String error])
          :: json_props_to_log
        )
    in
    let json_props_to_log = fold_json_of_parse_errors parse_errors json_props_to_log in
    (response, Some (Hh_json.JSON_Object json_props_to_log))

let type_parse_artifacts_for_ac_with_cache
    ~options ~profiling ~type_parse_artifacts_cache master_cx file contents artifacts =
  let type_parse_artifacts =
    lazy
      (match Lazy.force artifacts with
      | (None, errs) -> Error errs
      | (Some parse_artifacts, _errs) ->
        let (Parse_artifacts { docblock; ast; requires; file_sig; _ }) = parse_artifacts in
        let (cx, aloc_ast) =
          Type_contents.compute_env_of_contents
            ~options
            ~profiling
            ~reader:(State_reader.create ())
            master_cx
            file
            docblock
            ast
            requires
            file_sig
        in
        Ok (contents, parse_artifacts, cx, aloc_ast))
  in
  let cond = function
    | Error _ -> false (* we can't tell there's a match *)
    | Ok (contents', _, _, _) -> contents = contents'
  in
  match type_parse_artifacts_cache with
  | Some cache ->
    let (result, did_hit) = FilenameCache.with_cache_sync ~cond file type_parse_artifacts cache in
    (result, Some did_hit)
  | _ -> (Lazy.force type_parse_artifacts, None)

let autocomplete_on_parsed
    ~filename
    ~contents
    ~trigger_character
    ~reader
    ~options
    ~env
    ~client
    ~profiling
    ~cursor
    ~imports
    ~imports_min_characters
    ~imports_ranked_usage
    ~imports_ranked_usage_boost_exact_match_min_length
    ~show_ranking_info =
  let cursor_loc =
    let (line, column) = cursor in
    Loc.cursor (Some filename) line column
  in
  (* Assuming input request at `foo.bar|`, [contents] will include `foo.AUTO332`.
   * We will refer to this form of contents as canonical. *)
  let (contents, broader_context, canon_token) =
    let (line, column) = cursor in
    Autocomplete_sigil.add (Some filename) contents line column
  in
  let canon_cursor =
    Base.Option.value_map ~default:cursor_loc ~f:Autocomplete_sigil.Canonical.cursor canon_token
  in
  Autocomplete_js.autocomplete_set_hooks ~cursor:canon_cursor;
  let initial_json_props =
    let open Hh_json in
    [
      ("ac_trigger", JSON_String (Base.Option.value trigger_character ~default:"None"));
      ("broader_context", JSON_String broader_context);
    ]
  in
  let type_parse_artifacts_cache =
    Base.Option.map client ~f:Persistent_connection.autocomplete_artifacts_cache
  in
  (* Parse the canonical contents into a canonical AST. *)
  let parse_result = lazy (Type_contents.parse_contents ~options ~profiling contents filename) in
  (* Perform type inference over the canonical AST. *)
  let (file_artifacts_result, did_hit) =
    type_parse_artifacts_for_ac_with_cache
      ~options
      ~profiling
      ~type_parse_artifacts_cache
      env.master_cx
      filename
      contents
      parse_result
  in
  let initial_json_props = add_cache_hit_data_to_json initial_json_props did_hit in
  let ac_typing_artifacts =
    match file_artifacts_result with
    | Error _ -> None
    | Ok (_contents, parse_artifacts, cx, aloc_ast) ->
      let (Parse_artifacts { docblock = info; file_sig; ast; parse_errors; _ }) = parse_artifacts in
      Some (info, file_sig, ast, parse_errors, cx, aloc_ast)
  in
  let ac_result =
    match ac_typing_artifacts with
    | None -> None
    | Some (info, file_sig, ast, parse_errors, cx, aloc_ast) ->
      let open AutocompleteService_js in
      let (token_opt, ac_loc, ac_type_string, results_res) =
        Profiling_js.with_timer profiling ~timer:"GetResults" ~f:(fun () ->
            let typing =
              AutocompleteService_js.mk_typing_artifacts
                ~layout_options:(Code_action_utils.layout_options options)
                ~module_system_info:(mk_module_system_info ~options ~reader)
                ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
                ~get_ast_from_shared_mem:(Parsing_heaps.Reader.get_ast ~reader)
                ~search_exported_values:(search_exported_values ~exports:env.ServerEnv.exports)
                ~search_exported_types:(search_exported_types ~exports:env.ServerEnv.exports)
                ~cx
                ~file_sig
                ~ast
                ~aloc_ast
                ~canonical:canon_token
            in
            let ac_options =
              {
                AutocompleteService_js.imports;
                imports_min_characters;
                imports_ranked_usage;
                imports_ranked_usage_boost_exact_match_min_length;
                show_ranking_info;
              }
            in
            autocomplete_get_results typing ac_options trigger_character cursor_loc
        )
      in
      Some (info, parse_errors, token_opt, ac_loc, ac_type_string, results_res)
  in
  (* Make sure hooks are unset *after* we've gotten the results to account for
   * on-demand type checking. *)
  Autocomplete_js.autocomplete_unset_hooks ();
  (initial_json_props, ac_result)

let autocomplete
    ~trigger_character
    ~reader
    ~options
    ~env
    ~client
    ~profiling
    ~input
    ~cursor
    ~imports
    ~imports_min_characters
    ~imports_ranked_usage
    ~imports_ranked_usage_boost_exact_match_min_length
    ~show_ranking_info =
  match of_file_input ~options ~env input with
  | Error (Failed e) -> (Error e, None)
  | Error (Skipped reason) ->
    let response =
      (None, { ServerProt.Response.Completion.items = []; is_incomplete = false }, None, "Skipped")
    in
    let extra_data = json_of_skipped reason in
    (Ok response, extra_data)
  | Ok (filename, contents) ->
    let (initial_json_props, ac_result) =
      autocomplete_on_parsed
        ~filename
        ~contents
        ~trigger_character
        ~reader
        ~options
        ~env
        ~client
        ~profiling
        ~cursor
        ~imports
        ~imports_min_characters
        ~imports_ranked_usage
        ~imports_ranked_usage_boost_exact_match_min_length
        ~show_ranking_info
    in
    json_of_autocomplete_result initial_json_props ac_result

let check_file ~options ~env ~profiling ~force file_input =
  let options = { options with Options.opt_all = Options.all options || force } in
  match of_file_input ~options ~env file_input with
  | Error (Failed _reason)
  | Error (Skipped _reason) ->
    ServerProt.Response.NOT_COVERED
  | Ok (file_key, content) ->
    let result =
      let ((_, parse_errs) as intermediate_result) =
        Type_contents.parse_contents ~options ~profiling content file_key
      in
      if not (Flow_error.ErrorSet.is_empty parse_errs) then
        Error parse_errs
      else
        Type_contents.type_parse_artifacts
          ~options
          ~profiling
          env.master_cx
          file_key
          intermediate_result
    in
    let (errors, warnings) =
      Type_contents.printable_errors_of_file_artifacts_result ~options ~env file_key result
    in
    convert_errors ~errors ~warnings ~suppressed_errors:[]

(* This returns result, json_data_to_log, where json_data_to_log is the json data from
 * getdef_get_result which we end up using *)
let get_def_of_check_result ~reader ~profiling ~check_result ~purpose (file, line, col) =
  Profiling_js.with_timer profiling ~timer:"GetResult" ~f:(fun () ->
      let loc = Loc.cursor (Some file) line col in
      let ( Parse_artifacts { ast; file_sig; parse_errors; _ },
            Typecheck_artifacts { cx; typed_ast; _ }
          ) =
        check_result
      in
      GetDef_js.get_def
        ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
        ~cx
        ~file_sig
        ~ast
        ~available_ast:(Typed_ast_utils.Typed_ast typed_ast)
        ~purpose
        loc
      |> fun result ->
      let open GetDef_js.Get_def_result in
      let json_props = fold_json_of_parse_errors parse_errors [] in
      match result with
      | Def (locs, _) ->
        (Ok (LocSet.elements locs), Some (("result", Hh_json.JSON_String "SUCCESS") :: json_props))
      | Partial (locs, _, msg) ->
        ( Ok (LocSet.elements locs),
          Some
            (("result", Hh_json.JSON_String "PARTIAL_FAILURE")
            :: ("error", Hh_json.JSON_String msg)
            :: json_props
            )
        )
      | Bad_loc msg ->
        ( Ok [],
          Some
            (("result", Hh_json.JSON_String "BAD_LOC")
            :: ("error", Hh_json.JSON_String msg)
            :: json_props
            )
        )
      | Def_error msg ->
        ( Error msg,
          Some
            (("result", Hh_json.JSON_String "FAILURE")
            :: ("error", Hh_json.JSON_String msg)
            :: json_props
            )
        )
  )

let infer_type_to_response
    ~reader ~json ~expanded ~exact_by_default ~strip_root loc documentation tys =
  let module Ty_debug = Ty_debug.Make (struct
    let aloc_to_loc = Some (Parsing_heaps.Reader.loc_of_aloc ~reader)
  end) in
  let tys =
    if json then
      let open Hh_json in
      let type_json t =
        let json_obj =
          [("type", JSON_String (Ty_printer.string_of_elt_single_line ~exact_by_default t))]
        in
        let json_obj =
          if expanded then
            ("expanded", Ty_debug.json_of_elt ~strip_root t) :: json_obj
          else
            json_obj
        in
        JSON_Object json_obj
      in
      let json =
        match tys with
        | Some { Ty.unevaluated; evaluated; refs = _ } ->
          let evaluated = Base.Option.value_map evaluated ~default:JSON_Null ~f:type_json in
          JSON_Object [("unevaluated", type_json unevaluated); ("evaluated", evaluated)]
        | None -> JSON_Null
      in
      ServerProt.Response.Infer_type_JSON json
    else
      ServerProt.Response.Infer_type_string
        (Base.Option.map tys ~f:(Ty_printer.string_of_type_at_pos_result ~exact_by_default))
  in
  ServerProt.Response.Infer_type_response { loc; tys; documentation }

let infer_type
    ~(options : Options.t)
    ~(reader : Parsing_heaps.Reader.reader)
    ~(env : ServerEnv.env)
    ~(profiling : Profiling_js.running)
    ~type_parse_artifacts_cache
    input : ServerProt.Response.infer_type_response * Hh_json.json option =
  let {
    ServerProt.Infer_type_options.input = file_input;
    line;
    char = column;
    verbose;
    omit_targ_defaults;
    wait_for_recheck = _;
    verbose_normalizer;
    max_depth;
    json;
    strip_root;
    expanded;
    no_typed_ast_for_imports;
  } =
    input
  in
  match of_file_input ~options ~env file_input with
  | Error (Failed e) -> (Error e, None)
  | Error (Skipped reason) ->
    (* TODO: wow, this is a shady way to return no result! *)
    let tys =
      if json then
        ServerProt.Response.Infer_type_JSON Hh_json.JSON_Null
      else
        ServerProt.Response.Infer_type_string None
    in
    let response =
      ServerProt.Response.Infer_type_response { loc = Loc.none; tys; documentation = None }
    in
    let extra_data = json_of_skipped reason in
    (Ok response, extra_data)
  | Ok (file_key, content) ->
    let options = { options with Options.opt_verbose = verbose } in
    let (file_artifacts_result, did_hit_cache) =
      let parse_result = lazy (Type_contents.parse_contents ~options ~profiling content file_key) in
      type_parse_artifacts_with_cache
        ~options
        ~profiling
        ~type_parse_artifacts_cache
        env.master_cx
        file_key
        parse_result
    in
    (match file_artifacts_result with
    | Error _parse_errors ->
      let err_str = "Couldn't parse file in parse_artifacts" in
      let json_props = add_cache_hit_data_to_json [] did_hit_cache in
      (Error err_str, Some (Hh_json.JSON_Object json_props))
    | Ok
        ( (Parse_artifacts { file_sig; ast; _ }, Typecheck_artifacts { cx; typed_ast; _ }) as
        check_result
        ) ->
      let ((loc, tys), type_at_pos_json_props) =
        Type_info_service.type_at_pos
          ~cx
          ~file_sig
          ~typed_ast
          ~omit_targ_defaults
          ~max_depth
          ~verbose_normalizer
          ~no_typed_ast_for_imports
          ~include_refs:(Some (Parsing_heaps.Reader.loc_of_aloc ~reader))
          file_key
          line
          column
      in
      let (getdef_loc_result, _) =
        try_with_json (fun () ->
            get_def_of_check_result
              ~reader
              ~profiling
              ~check_result
              ~purpose:Get_def_types.Purpose.JSDoc
              (file_key, line, column)
        )
      in
      let get_def_documentation =
        match getdef_loc_result with
        | Ok [getdef_loc] ->
          Find_documentation.jsdoc_of_getdef_loc
            ~ast
            ~get_ast_from_shared_mem:(Parsing_heaps.Reader.get_ast ~reader)
            getdef_loc
          |> Base.Option.bind ~f:Find_documentation.documentation_of_jsdoc
        | _ -> None
      in
      let hardcoded_documentation =
        Find_documentation.hardcoded_documentation_at_loc
          ast
          (Loc.cursor (Some file_key) line column)
      in
      let documentation =
        match (get_def_documentation, hardcoded_documentation) with
        | (None, None) -> None
        | (Some d, None)
        | (None, Some d) ->
          Some d
        | (Some d1, Some d2) -> Some (d1 ^ "\n\n" ^ d2)
      in
      let json_props =
        ("documentation", Hh_json.JSON_Bool (Base.Option.is_some documentation))
        :: add_cache_hit_data_to_json type_at_pos_json_props did_hit_cache
      in
      let exact_by_default = Options.exact_by_default options in
      let response =
        infer_type_to_response
          ~reader
          ~json
          ~expanded
          ~exact_by_default
          ~strip_root
          loc
          documentation
          tys
      in
      (Ok response, Some (Hh_json.JSON_Object json_props)))

let insert_type
    ~options
    ~env
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~file_input
    ~target
    ~verbose
    ~omit_targ_defaults
    ~location_is_strict
    ~ambiguity_strategy =
  let file_key = file_key_of_file_input ~options ~env file_input in
  let options = { options with Options.opt_verbose = verbose } in
  File_input.content_of_file_input file_input >>= fun file_content ->
  Code_action_service.insert_type
    ~options
    ~env
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~file_key
    ~file_content
    ~target
    ~omit_targ_defaults
    ~location_is_strict
    ~ambiguity_strategy

let autofix_exports
    ~options
    ~env
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~input =
  let file_key = file_key_of_file_input ~options ~env input in
  File_input.content_of_file_input input >>= fun file_content ->
  Code_action_service.autofix_exports
    ~options
    ~master_cx:env.ServerEnv.master_cx
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~file_key
    ~file_content

let autofix_missing_local_annot
    ~options
    ~env
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~input =
  let file_key = file_key_of_file_input ~options ~env input in
  File_input.content_of_file_input input >>= fun file_content ->
  Code_action_service.autofix_missing_local_annot
    ~options
    ~master_cx:env.ServerEnv.master_cx
    ~profiling
    ~loc_of_aloc
    ~get_ast_from_shared_mem
    ~get_haste_name
    ~get_type_sig
    ~file_key
    ~file_content

let collect_rage ~options ~reader ~env ~files =
  let items = [] in
  (* options *)
  let data = Printf.sprintf "lazy_mode=%s\n" (Options.lazy_mode options |> Bool.to_string) in
  let items = ("options", data) :: items in
  (* env: checked files *)
  let data =
    Printf.sprintf
      "%s\n\n%s\n"
      (CheckedSet.debug_counts_to_string env.checked_files)
      (CheckedSet.debug_to_string ~limit:200 env.checked_files)
  in
  let items = ("env.checked_files", data) :: items in
  (* env: dependency graph *)
  let dependency_to_string (file, deps) =
    let file = File_key.to_string file in
    let deps =
      Utils_js.FilenameSet.elements deps
      |> Base.List.map ~f:File_key.to_string
      |> ListUtils.first_upto_n 20 (fun t -> Some (Printf.sprintf " ...%d more" t))
      |> String.concat ","
    in
    file ^ ":" ^ deps ^ "\n"
  in
  let dependencies =
    Dependency_info.implementation_dependency_graph env.ServerEnv.dependency_info
    |> Utils_js.FilenameGraph.to_map
    |> Utils_js.FilenameMap.bindings
    |> Base.List.map ~f:dependency_to_string
    |> ListUtils.first_upto_n 200 (fun t -> Some (Printf.sprintf "[shown 200/%d]\n" t))
    |> String.concat ""
  in
  let data = "DEPENDENCIES:\n" ^ dependencies in
  let items = ("env.dependencies", data) :: items in
  (* env: errors *)
  let (errors, warnings) = ErrorCollator.get_without_suppressed env in
  let json =
    Flow_errors_utils.Json_output.json_of_errors_with_context
      ~strip_root:None
      ~stdin_file:None
      ~offset_kind:Offset_utils.Utf8
      ~suppressed_errors:[]
      ~errors
      ~warnings
      ()
  in
  let data = "ERRORS:\n" ^ Hh_json.json_to_multiline json in
  let items = ("env.errors", data) :: items in
  (* Checking if file hashes are up to date *)
  let items =
    Base.Option.value_map files ~default:items ~f:(fun files ->
        let buf = Buffer.create 1024 in
        Printf.bprintf
          buf
          "Does the content on the disk match the most recent version of the file?\n\n";
        List.iter
          (fun file ->
            (* TODO - this isn't exactly right. It could be something else, right? *)
            let file_key = File_key.SourceFile file in
            let file_state =
              if not (FilenameSet.mem file_key env.ServerEnv.files) then
                "FILE NOT PARSED BY FLOW (likely ignored implicitly or explicitly)"
              else
                match Sys_utils.cat_or_failed file with
                | None -> "ERROR! FAILED TO READ"
                | Some content ->
                  let reader = Abstract_state_reader.State_reader reader in
                  if Parsing_service_js.does_content_match_file_hash ~reader file_key content then
                    "OK"
                  else
                    "HASH OUT OF DATE"
            in
            Printf.bprintf buf "%s: %s\n" file file_state)
          files;
        ("file hash check", Buffer.contents buf) :: items
    )
  in
  items

let dump_types ~options ~env ~profiling ~evaluate_type_destructors file_input =
  let open Base.Result in
  let file_key = file_key_of_file_input ~options ~env file_input in
  File_input.content_of_file_input file_input >>= fun content ->
  let file_artifacts_result =
    let parse_result = Type_contents.parse_contents ~options ~profiling content file_key in
    Type_contents.type_parse_artifacts ~options ~profiling env.master_cx file_key parse_result
  in
  match file_artifacts_result with
  | Error _parse_errors -> Error "Couldn't parse file in parse_contents"
  | Ok (Parse_artifacts { file_sig; _ }, Typecheck_artifacts { cx; typed_ast; _ }) ->
    Ok (Type_info_service.dump_types ~evaluate_type_destructors cx file_sig typed_ast)

let coverage ~options ~env ~profiling ~type_parse_artifacts_cache ~force file_key content =
  let (file_artifacts_result, did_hit_cache) =
    let parse_result = lazy (Type_contents.parse_contents ~options ~profiling content file_key) in
    type_parse_artifacts_with_cache
      ~options
      ~profiling
      ~type_parse_artifacts_cache
      env.master_cx
      file_key
      parse_result
  in
  let extra_data =
    let json_props = add_cache_hit_data_to_json [] did_hit_cache in
    Hh_json.JSON_Object json_props
  in
  match file_artifacts_result with
  | Ok (_, Typecheck_artifacts { cx; typed_ast; obj_to_obj_map = _ }) ->
    let coverage =
      Profiling_js.with_timer profiling ~timer:"Coverage" ~f:(fun () ->
          Type_info_service.coverage ~cx ~typed_ast ~force file_key content
      )
    in
    (Ok coverage, Some extra_data)
  | Error _parse_errors -> (Error "Couldn't parse file in parse_contents", Some extra_data)

let batch_coverage ~options ~env ~batch =
  if Options.lazy_mode options then
    Error
      "Batch coverage cannot be run in lazy mode.\n\nRestart the Flow server with '--no-lazy' to enable this command."
  else
    let filter key = Base.List.exists ~f:(fun elt -> Files.is_prefix elt key) batch in
    let coverage_map =
      FilenameMap.filter
        (fun key _ -> (not (File_key.is_lib_file key)) && File_key.to_string key |> filter)
        env.coverage
    in
    let response =
      FilenameMap.fold (fun key coverage -> List.cons (key, coverage)) coverage_map []
    in
    Ok response

let serialize_graph graph =
  (* Convert from map/set to lists for serialization to client. *)
  FilenameMap.fold
    (fun f dep_fs acc ->
      let f = File_key.to_string f in
      let dep_fs = FilenameSet.fold (fun dep_f acc -> File_key.to_string dep_f :: acc) dep_fs [] in
      (f, dep_fs) :: acc)
    graph
    []

let output_dependencies ~env root strip_root types_only outfile =
  let strip_root =
    if strip_root then
      Files.relative_path root
    else
      fun x ->
    x
  in
  let dep_graph =
    if types_only then
      Dependency_info.sig_dependency_graph
    else
      Dependency_info.implementation_dependency_graph
  in
  let graph = serialize_graph (dep_graph env.ServerEnv.dependency_info |> FilenameGraph.to_map) in
  Hh_logger.info "printing dependency graph to %s\n" outfile;
  let%lwt out = Lwt_io.open_file ~mode:Lwt_io.Output outfile in
  let%lwt () = LwtUtils.output_graph out strip_root graph in
  let%lwt () = Lwt_io.close out in
  Lwt.return (Ok ())

let get_cycle ~env fn types_only =
  (* Re-calculate SCC *)
  let parsed = env.ServerEnv.files in
  let dependency_info = env.ServerEnv.dependency_info in
  let dependency_graph =
    if types_only then
      Dependency_info.sig_dependency_graph dependency_info
    else
      Dependency_info.implementation_dependency_graph dependency_info
  in
  Ok
    (let components = Sort_js.topsort ~roots:parsed dependency_graph in
     (* Get component for target file *)
     let component = List.find (Nel.mem ~equal:File_key.equal fn) components in
     (* Restrict dep graph to only in-cycle files *)
     Nel.fold_left
       (fun acc f ->
         Base.Option.fold (FilenameGraph.find_opt f dependency_graph) ~init:acc ~f:(fun acc deps ->
             let subdeps =
               FilenameSet.filter (fun f -> Nel.mem ~equal:File_key.equal f component) deps
             in
             if FilenameSet.is_empty subdeps then
               acc
             else
               FilenameMap.add f subdeps acc
         ))
       FilenameMap.empty
       component
     |> serialize_graph
    )

let find_module ~options ~reader (moduleref, filename) =
  let file = File_key.SourceFile filename in
  let resolved_module =
    Module_js.imported_module
      ~options
      ~reader:(Abstract_state_reader.State_reader reader)
      ~node_modules_containers:!Files.node_modules_containers
      file
      moduleref
  in
  let provider =
    match resolved_module with
    | Ok m -> Parsing_heaps.Reader.get_provider ~reader m
    | Error _ ->
      (* TODO: We reach this codepath for requires that might resolve to
       * builtin modules. During check we check the master context, which we
       * can also do here. *)
      None
  in
  match provider with
  | Some addr -> Some (Parsing_heaps.read_file_key addr)
  | None -> None

let get_def ~options ~reader ~env ~profiling ~type_parse_artifacts_cache (file_input, line, col) =
  match of_file_input ~options ~env file_input with
  | Error (Failed msg) -> (Error msg, None)
  | Error (Skipped reason) ->
    let json_props = ("result", Hh_json.JSON_String "SKIPPED") :: json_props_of_skipped reason in
    (Ok [], Some (Hh_json.JSON_Object json_props))
  | Ok (file_key, content) ->
    let (check_result, did_hit_cache) =
      match
        let parse_result =
          lazy (Type_contents.parse_contents ~options ~profiling content file_key)
        in
        type_parse_artifacts_with_cache
          ~options
          ~profiling
          ~type_parse_artifacts_cache
          env.master_cx
          file_key
          parse_result
      with
      | (Ok result, did_hit_cache) -> (Ok result, did_hit_cache)
      | (Error _parse_errors, did_hit_cache) ->
        (Error "Couldn't parse file in parse_contents", did_hit_cache)
    in
    (match check_result with
    | Error msg ->
      let json_props = [("error", Hh_json.JSON_String msg)] in
      let json_props = add_cache_hit_data_to_json json_props did_hit_cache in
      (Error msg, Some (Hh_json.JSON_Object json_props))
    | Ok check_result ->
      let (result, json_props) =
        get_def_of_check_result
          ~reader
          ~profiling
          ~check_result
          ~purpose:Get_def_types.Purpose.GoToDefinition
          (file_key, line, col)
      in
      let json =
        let json_props = Base.Option.value ~default:[] json_props in
        let json_props =
          match result with
          | Ok [] ->
            (* add file context when we return no result (100 bytes before and after) *)
            let offset = File_content.get_offset content { File_content.line; column = col } in
            let before = String.sub content (Int.max (offset - 100) 0) (Int.min 100 offset) in
            let after = String.sub content offset (Int.min 100 (String.length content - offset)) in
            let str = Printf.sprintf "%s|%s" before after in
            ("broader_context", Hh_json.JSON_String str) :: json_props
          | _ -> json_props
        in
        let json_props = add_cache_hit_data_to_json json_props did_hit_cache in
        Hh_json.JSON_Object json_props
      in
      (result, Some json))

let save_state ~saved_state_filename ~genv ~env ~profiling =
  let%lwt () = Saved_state.save ~saved_state_filename ~genv ~env ~profiling in
  Lwt.return (Ok ())

let auto_close_jsx ~options ~env ~profiling ~params ~client =
  let text_document = params.TextDocumentPositionParams.textDocument in
  let file_input = file_input_of_text_document_identifier ~client text_document in
  match of_file_input ~options ~env file_input with
  | Error (Failed e) -> (Error e, None)
  | Error (Skipped reason) ->
    let extra_data = json_of_skipped reason in
    (Ok None, extra_data)
  | Ok (filename, contents) ->
    let (parse_result, _) = Type_contents.parse_contents ~options ~profiling contents filename in
    begin
      match parse_result with
      | None -> (Ok None, None)
      | Some (Parse_artifacts { ast; _ }) ->
        let target_pos =
          Lsp.lsp_position_to_flow_position params.TextDocumentPositionParams.position
        in
        let edit = Auto_close_jsx.get_snippet ast target_pos in
        (Ok edit, None)
    end

let linked_editing_range ~options ~env ~profiling ~params ~client =
  let text_document = params.TextDocumentPositionParams.textDocument in
  let file_input = file_input_of_text_document_identifier ~client text_document in
  match of_file_input ~options ~env file_input with
  | Error (Failed e) -> (Error e, None)
  | Error (Skipped reason) ->
    let extra_data = json_of_skipped reason in
    (Ok None, extra_data)
  | Ok (filename, contents) ->
    let (parse_result, parse_errors) =
      Type_contents.parse_contents ~options ~profiling contents filename
    in
    if not (Flow_error.ErrorSet.is_empty parse_errors) then
      (* If there are parse errors, we can't necessarily match opening/closing tags in the way a user might expect. *)
      (Ok None, None)
    else (
      match parse_result with
      | None -> (Ok None, None)
      | Some (Parse_artifacts { ast; _ }) ->
        let result =
          let target_pos =
            Lsp.lsp_position_to_flow_position params.TextDocumentPositionParams.position
          in
          let target_loc = Loc.cursor (Some filename) target_pos.Loc.line target_pos.Loc.column in
          let linked_locs = Linked_editing_jsx.get_linked_locs ast target_loc in
          Base.Option.map linked_locs ~f:(fun linked_locs ->
              let ranges = Base.List.map linked_locs ~f:Lsp.loc_to_lsp_range in
              { LinkedEditingRange.ranges; wordPattern = None }
          )
        in
        (Ok result, None)
    )

let vscode_detailed_diagnostics ~options client =
  let client_config = Persistent_connection.client_config client in
  let lsp_initialize_params = Persistent_connection.lsp_initialize_params client in
  Persistent_connection.Client_config.detailed_error_rendering_merge_with_options
    ~flowconfig_enabled:(Options.vscode_detailed_diagnostics options)
    ~client_init_options_enabled:
      Lsp.Initialize.(lsp_initialize_params.initializationOptions.detailedErrorRendering)
    client_config

let rank_autoimports_by_usage ~options client =
  let client_config = Persistent_connection.client_config client in
  match Persistent_connection.Client_config.rank_autoimports_by_usage client_config with
  | Persistent_connection.Client_config.Default -> Options.autoimports_ranked_by_usage options
  | Persistent_connection.Client_config.True -> true
  | Persistent_connection.Client_config.False -> false

let handle_autocomplete
    ~trigger_character
    ~reader
    ~options
    ~profiling
    ~env
    ~input
    ~cursor
    ~imports
    ~imports_min_characters
    ~imports_ranked_usage
    ~imports_ranked_usage_boost_exact_match_min_length
    ~show_ranking_info =
  let (result, json_data) =
    try_with_json (fun () ->
        autocomplete
          ~trigger_character
          ~reader
          ~options
          ~env
          ~client:None
          ~profiling
          ~input
          ~cursor
          ~imports
          ~imports_min_characters
          ~imports_ranked_usage
          ~imports_ranked_usage_boost_exact_match_min_length
          ~show_ranking_info
    )
  in
  let result =
    Base.Result.map result ~f:(fun x ->
        match x with
        | (_, b, _, ac_type) -> (b, ac_type)
    )
  in
  Lwt.return (ServerProt.Response.AUTOCOMPLETE result, json_data)

let handle_autofix_exports ~options ~input ~profiling ~env ~reader =
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  let get_ast_from_shared_mem = Parsing_heaps.Reader.get_ast ~reader in
  let get_haste_name = get_haste_name ~reader in
  let get_type_sig = Parsing_heaps.Reader.get_type_sig ~reader in
  let result =
    try_with (fun () ->
        autofix_exports
          ~options
          ~env
          ~profiling
          ~input
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~get_haste_name
          ~get_type_sig
    )
  in
  Lwt.return (ServerProt.Response.AUTOFIX_EXPORTS result, None)

let handle_autofix_missing_local_annot ~options ~input ~profiling ~env ~reader =
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  let get_ast_from_shared_mem = Parsing_heaps.Reader.get_ast ~reader in
  let get_haste_name = get_haste_name ~reader in
  let get_type_sig = Parsing_heaps.Reader.get_type_sig ~reader in
  let result =
    try_with (fun () ->
        autofix_missing_local_annot
          ~options
          ~env
          ~profiling
          ~input
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~get_haste_name
          ~get_type_sig
    )
  in
  Lwt.return (ServerProt.Response.AUTOFIX_MISSING_LOCAL_ANNOT result, None)

let handle_check_file ~options ~force ~input ~profiling ~env =
  let response = check_file ~options ~env ~force ~profiling input in
  Lwt.return (ServerProt.Response.CHECK_FILE response, None)

let handle_coverage ~options ~force ~input ~profiling ~env =
  let (response, json_data) =
    try_with_json (fun () ->
        let options = { options with Options.opt_all = options.Options.opt_all || force } in
        match of_file_input ~options ~env input with
        | Error (Failed msg) -> (Error msg, None)
        | Error (Skipped reason) -> (Error reason, json_of_skipped reason)
        | Ok (file_key, file_contents) ->
          coverage
            ~options
            ~env
            ~profiling
            ~type_parse_artifacts_cache:None
            ~force
            file_key
            file_contents
    )
  in
  Lwt.return (ServerProt.Response.COVERAGE response, json_data)

let handle_batch_coverage ~options ~profiling:_ ~env ~batch =
  let response = batch_coverage ~options ~env ~batch in
  Lwt.return (ServerProt.Response.BATCH_COVERAGE response, None)

let handle_cycle ~fn ~types_only ~profiling:_ ~env =
  let response = get_cycle ~env fn types_only in
  Lwt.return (env, ServerProt.Response.CYCLE response, None)

let handle_dump_types ~options ~input ~evaluate_type_destructors ~profiling ~env =
  let response =
    try_with (fun () -> dump_types ~options ~env ~profiling ~evaluate_type_destructors input)
  in
  Lwt.return (ServerProt.Response.DUMP_TYPES response, None)

let handle_find_module ~options ~reader ~moduleref ~filename ~profiling:_ ~env:_ =
  let response = find_module ~options ~reader (moduleref, filename) in
  Lwt.return (ServerProt.Response.FIND_MODULE response, None)

let handle_force_recheck ~files ~focus ~missed_changes ~changed_mergebase ~profiling:_ =
  let fileset = SSet.of_list files in
  let metadata = { MonitorProt.missed_changes; changed_mergebase = Some changed_mergebase } in
  (* `flow force-recheck --focus a.js` not only marks a.js as a focused file, but it also
   * tells Flow that `a.js` has changed. In that case we push a.js to be rechecked and to be
   * focused *)
  if focus then
    ServerMonitorListenerState.push_files_to_force_focused_and_recheck fileset
  else
    ServerMonitorListenerState.push_files_to_recheck ~metadata fileset;
  (ServerProt.Response.FORCE_RECHECK, None)

let handle_get_def ~reader ~options ~input ~line ~char ~profiling ~env =
  let (result, json_data) =
    try_with_json (fun () ->
        get_def ~reader ~options ~env ~profiling ~type_parse_artifacts_cache:None (input, line, char)
    )
  in
  Lwt.return (ServerProt.Response.GET_DEF result, json_data)

let handle_graph_dep_graph ~root ~strip_root ~outfile ~types_only ~profiling:_ ~env =
  let%lwt response = output_dependencies ~env root strip_root types_only outfile in
  Lwt.return (env, ServerProt.Response.GRAPH_DEP_GRAPH response, None)

let handle_infer_type ~options ~reader ~profiling ~env input =
  let (result, json_data) =
    try_with_json (fun () ->
        infer_type ~options ~reader ~env ~profiling ~type_parse_artifacts_cache:None input
    )
  in
  Lwt.return (ServerProt.Response.INFER_TYPE result, json_data)

let handle_insert_type
    ~options
    ~file_input
    ~target
    ~verbose
    ~omit_targ_defaults
    ~location_is_strict
    ~ambiguity_strategy
    ~profiling
    ~env
    ~reader =
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  let get_ast_from_shared_mem = Parsing_heaps.Reader.get_ast ~reader in
  let get_haste_name = get_haste_name ~reader in
  let get_type_sig = Parsing_heaps.Reader.get_type_sig ~reader in
  let result =
    try_with (fun _ ->
        insert_type
          ~options
          ~env
          ~profiling
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~get_haste_name
          ~get_type_sig
          ~file_input
          ~target
          ~verbose
          ~omit_targ_defaults
          ~location_is_strict
          ~ambiguity_strategy
    )
  in
  Lwt.return (ServerProt.Response.INSERT_TYPE result, None)

let handle_rage ~reader ~options ~files ~profiling:_ ~env =
  let items = collect_rage ~options ~reader ~env ~files:(Some files) in
  Lwt.return (ServerProt.Response.RAGE items, None)

let handle_status ~reader ~options ~profiling:_ ~env =
  let (status_response, lazy_stats) = get_status ~options ~reader env in
  Lwt.return (env, ServerProt.Response.STATUS { status_response; lazy_stats }, None)

let handle_save_state ~options ~out ~genv ~profiling ~env =
  let%lwt saved_state_filename =
    match out with
    | `Scm ->
      (match%lwt Saved_state_scm_fetcher.output_filename options with
      | Ok file -> Lwt.return_ok (File_path.make file)
      | Error err -> Lwt.return_error err)
    | `File file -> Lwt.return_ok file
  in
  let%lwt result =
    match saved_state_filename with
    | Ok saved_state_filename ->
      let out_str = File_path.to_string saved_state_filename in
      (match%lwt
         try_with_lwt (fun () -> save_state ~saved_state_filename ~genv ~env ~profiling)
       with
      | Ok () -> Lwt.return_ok (Printf.sprintf "Created saved-state file `%s`\n%!" out_str)
      | Error err ->
        Lwt.return_error
          (Printf.sprintf "Failed to create saved-state file `%s`:\n%s\n%!" out_str err))
    | Error err ->
      Lwt.return_error (Printf.sprintf "Failed to determine saved-state output file: %s" err)
  in
  Lwt.return (env, ServerProt.Response.SAVE_STATE result, None)

let find_code_actions ~reader ~options ~env ~profiling ~params ~client =
  let CodeActionRequest.{ textDocument; range; context = { only; diagnostics } } = params in
  if not (Code_action_service.kind_is_supported only) then
    (* bail out early if we don't support any of the code actions requested *)
    (Ok [], None)
  else
    let file_input = file_input_of_text_document_identifier ~client textDocument in
    match of_file_input ~options ~env file_input with
    | Error (Failed msg) -> (Error msg, None)
    | Error (Skipped reason) ->
      let extra_data = json_of_skipped reason in
      (Ok [], extra_data)
    | Ok (file_key, file_contents) ->
      let (file_artifacts_result, _did_hit_cache) =
        let parse_result =
          lazy (Type_contents.parse_contents ~options ~profiling file_contents file_key)
        in
        let type_parse_artifacts_cache =
          Some (Persistent_connection.type_parse_artifacts_cache client)
        in
        type_parse_artifacts_with_cache
          ~options
          ~profiling
          ~type_parse_artifacts_cache
          env.master_cx
          file_key
          parse_result
      in
      (match file_artifacts_result with
      | Error _ -> (Ok [], None)
      | Ok
          ( Parse_artifacts { file_sig; tolerable_errors; ast; parse_errors; _ },
            Typecheck_artifacts { cx; typed_ast; _ }
          ) ->
        let uri = TextDocumentIdentifier.(textDocument.uri) in
        let loc = Lsp.lsp_range_to_flow_loc ~source:file_key range in
        let lsp_init_params = Persistent_connection.lsp_initialize_params client in
        let imports_ranked_usage = rank_autoimports_by_usage ~options client in
        let scope_info =
          Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:false ast
        in
        let code_actions =
          Code_action_service.code_actions_at_loc
            ~options
            ~lsp_init_params
            ~imports_ranked_usage
            ~env
            ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
            ~get_ast_from_shared_mem:(Parsing_heaps.Reader.get_ast ~reader)
            ~get_type_sig:(Parsing_heaps.Reader.get_type_sig ~reader)
            ~module_system_info:(mk_module_system_info ~options ~reader)
            ~cx
            ~file_sig
            ~tolerable_errors
            ~file_contents
            ~ast
            ~typed_ast
            ~scope_info
            ~parse_errors
            ~diagnostics
            ~only
            ~uri
            ~loc
        in
        let extra_data =
          match code_actions with
          | Error _ -> None
          | Ok code_actions ->
            let open Hh_json in
            let json_of_code_action = function
              | CodeAction.Command Command.{ title; command = _; arguments = _ }
              | CodeAction.Action CodeAction.{ title; kind = _; diagnostics = _; action = _ } ->
                JSON_String title
            in
            let actions = JSON_Array (Base.List.map ~f:json_of_code_action code_actions) in
            Some (JSON_Object [("actions", actions)])
        in
        (code_actions, extra_data))

let add_missing_imports ~reader ~options ~env ~profiling ~client textDocument =
  let file_input = file_input_of_text_document_identifier ~client textDocument in
  let file_key = file_key_of_file_input ~options ~env file_input in
  match File_input.content_of_file_input file_input with
  | Error msg -> Lwt.return (Error msg)
  | Ok file_contents ->
    let type_parse_artifacts_cache =
      Some (Persistent_connection.type_parse_artifacts_cache client)
    in
    let uri = TextDocumentIdentifier.(textDocument.uri) in
    let (file_artifacts_result, _did_hit_cache) =
      let parse_result =
        lazy (Type_contents.parse_contents ~options ~profiling file_contents file_key)
      in
      type_parse_artifacts_with_cache
        ~options
        ~profiling
        ~type_parse_artifacts_cache
        env.master_cx
        file_key
        parse_result
    in
    (match file_artifacts_result with
    | Error _ -> Lwt.return (Ok [])
    | Ok (Parse_artifacts { ast; _ }, Typecheck_artifacts { cx; typed_ast = _; obj_to_obj_map = _ })
      ->
      let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
      Lwt.return
        (Ok
           (Code_action_service.autofix_imports
              ~options
              ~env
              ~loc_of_aloc
              ~module_system_info:(mk_module_system_info ~options ~reader)
              ~cx
              ~ast
              ~uri
           )
        ))

let organize_imports ~options ~profiling ~client textDocument =
  let file_input = file_input_of_text_document_identifier ~client textDocument in
  let file_key = file_key_of_file_input_without_env ~options ~libs:SSet.empty file_input in
  match File_input.content_of_file_input file_input with
  | Error msg -> Error msg
  | Ok file_contents ->
    let (parse_artifacts, _parse_errors) =
      Type_contents.parse_contents ~options ~profiling file_contents file_key
    in
    (match parse_artifacts with
    | None -> Ok []
    | Some (Parse_artifacts { ast; _ }) -> Ok (Code_action_service.organize_imports ~options ~ast))

type command_handler =
  | Handle_immediately of (profiling:Profiling_js.running -> ephemeral_parallelizable_result)
      (** A command can be handled immediately if it is super duper fast and doesn't require the env.
          These commands will be handled as soon as we read them off the pipe. Almost nothing should ever
          be handled immediately *)
  | Handle_parallelizable of ephemeral_parallelizable_result workload
      (** A command is parallelizable if it passes four conditions
          1. It is fast. Running it in parallel will block the current recheck, so it needs to be really
             fast.
          2. It doesn't use the workers. Currently we can't deal with the recheck using the workers at the
             same time as a command using the workers
          3. It doesn't return a new env. It really should be just a read-only job
          4. It doesn't mind using slightly out of date data. During a recheck, it will be reading the
            oldified data *)
  | Handle_nonparallelizable of ephemeral_nonparallelizable_result workload
      (** A command is nonparallelizable if it can't be handled immediately or parallelized. *)

(* This command is parallelizable, but we will treat it as nonparallelizable if we've been told
 * to wait_for_recheck by the .flowconfig or CLI *)
let mk_parallelizable ~wait_for_recheck ~options f =
  let wait_for_recheck =
    Base.Option.value wait_for_recheck ~default:(Options.wait_for_recheck options)
  in
  if wait_for_recheck then
    Handle_nonparallelizable
      (fun ~profiling ~env ->
        let%lwt (response, json_data) = f ~profiling ~env in
        Lwt.return (env, response, json_data))
  else
    Handle_parallelizable f

(* This function is called as soon as we read an ephemeral command from the pipe. It decides whether
 * the command should be handled immediately or deferred as parallelizable or nonparallelizable.
 * This function does NOT run any handling code itself. *)
let get_ephemeral_handler genv command =
  let options = genv.options in
  let reader = State_reader.create () in
  match command with
  | ServerProt.Request.AUTOCOMPLETE
      {
        input;
        cursor;
        trigger_character;
        wait_for_recheck;
        imports;
        imports_ranked_usage;
        show_ranking_info;
      } ->
    let imports_min_characters = Options.autoimports_min_characters options in
    let imports_ranked_usage_boost_exact_match_min_length =
      Options.autoimports_ranked_by_usage_boost_exact_match_min_length options
    in
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_autocomplete
         ~trigger_character
         ~reader
         ~options
         ~input
         ~cursor
         ~imports
         ~imports_min_characters
         ~imports_ranked_usage
         ~imports_ranked_usage_boost_exact_match_min_length
         ~show_ranking_info
      )
  | ServerProt.Request.AUTOFIX_EXPORTS { input; verbose; wait_for_recheck } ->
    let options = { options with Options.opt_verbose = verbose } in
    mk_parallelizable ~wait_for_recheck ~options (handle_autofix_exports ~input ~options ~reader)
  | ServerProt.Request.AUTOFIX_MISSING_LOCAL_ANNOT { input; verbose; wait_for_recheck } ->
    let options = { options with Options.opt_verbose = verbose } in
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_autofix_missing_local_annot ~input ~options ~reader)
  | ServerProt.Request.CHECK_FILE { input; verbose; force; include_warnings; wait_for_recheck } ->
    let options =
      {
        options with
        Options.opt_verbose = verbose;
        opt_include_warnings = options.Options.opt_include_warnings || include_warnings;
      }
    in
    mk_parallelizable ~wait_for_recheck ~options (handle_check_file ~options ~force ~input)
  | ServerProt.Request.COVERAGE { input; force; wait_for_recheck } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_coverage ~options ~force ~input)
  | ServerProt.Request.BATCH_COVERAGE { batch; wait_for_recheck } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_batch_coverage ~options ~batch)
  | ServerProt.Request.CYCLE { filename; types_only } ->
    (* The user preference is to make this wait for up-to-date data *)
    let file_options = Options.file_options options in
    let fn =
      Files.filename_from_string
        ~options:file_options
        ~consider_libdefs:true
        ~libs:SSet.empty
        filename
    in
    Handle_nonparallelizable (handle_cycle ~fn ~types_only)
  | ServerProt.Request.DUMP_TYPES { input; evaluate_type_destructors; wait_for_recheck } ->
    let evaluate_type_destructors =
      if evaluate_type_destructors then
        Ty_normalizer_env.EvaluateAll
      else
        Ty_normalizer_env.EvaluateNone
    in
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_dump_types ~options ~input ~evaluate_type_destructors)
  | ServerProt.Request.FIND_MODULE { moduleref; filename; wait_for_recheck } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_find_module ~options ~reader ~moduleref ~filename)
  | ServerProt.Request.FORCE_RECHECK { files; focus; missed_changes; changed_mergebase } ->
    Handle_immediately (handle_force_recheck ~files ~focus ~missed_changes ~changed_mergebase)
  | ServerProt.Request.GET_DEF { input; line; char; wait_for_recheck } ->
    mk_parallelizable ~wait_for_recheck ~options (handle_get_def ~reader ~options ~input ~line ~char)
  | ServerProt.Request.GRAPH_DEP_GRAPH { root; strip_root; outfile; types_only } ->
    (* The user preference is to make this wait for up-to-date data *)
    Handle_nonparallelizable (handle_graph_dep_graph ~root ~strip_root ~types_only ~outfile)
  | ServerProt.Request.INFER_TYPE input ->
    mk_parallelizable
      ~wait_for_recheck:input.ServerProt.Infer_type_options.wait_for_recheck
      ~options
      (handle_infer_type ~options ~reader input)
  | ServerProt.Request.RAGE { files } ->
    mk_parallelizable ~wait_for_recheck:None ~options (handle_rage ~reader ~options ~files)
  | ServerProt.Request.INSERT_TYPE
      {
        input = file_input;
        target;
        wait_for_recheck;
        verbose;
        omit_targ_defaults;
        location_is_strict;
        ambiguity_strategy;
      } ->
    mk_parallelizable
      ~wait_for_recheck
      ~options
      (handle_insert_type
         ~file_input
         ~options
         ~target
         ~verbose
         ~omit_targ_defaults
         ~location_is_strict
         ~ambiguity_strategy
         ~reader
      )
  | ServerProt.Request.STATUS { include_warnings } ->
    let options =
      let open Options in
      { options with opt_include_warnings = options.opt_include_warnings || include_warnings }
    in

    (* `flow status` is often used by users to get all the current errors. After talking to some
     * coworkers and users, glevi decided that users would rather that `flow status` always waits
     * for the current recheck to finish. So even though we could technically make `flow status`
     * parallelizable, we choose to make it nonparallelizable *)
    Handle_nonparallelizable (handle_status ~reader ~options)
  | ServerProt.Request.SAVE_STATE { out } ->
    (* save-state can take awhile to run. Furthermore, you probably don't want to run this with out
     * of date data. So save-state is not parallelizable *)
    Handle_nonparallelizable (handle_save_state ~options ~out ~genv)

let send_command_summary profiling name =
  MonitorRPC.send_telemetry
    (LspProt.Command_summary { name; duration = Profiling_js.get_profiling_duration profiling });
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_end

let send_ephemeral_response ~profiling ~client_context ~cmd_str ~request_id result =
  match result with
  | Ok (ret, response, json_data) ->
    FlowEventLogger.ephemeral_command_success ~json_data ~client_context ~profiling;
    MonitorRPC.respond_to_request ~request_id ~response;
    Hh_logger.info "Finished %s" cmd_str;
    Ok ret
  | Error (exn_str, json_data) ->
    FlowEventLogger.ephemeral_command_failure ~client_context ~json_data;
    MonitorRPC.request_failed ~request_id ~exn_str;
    Error ()

let handle_ephemeral_uncaught_exception cmd_str exn =
  let exn_str = Exception.to_string exn in
  let json_data = Some Hh_json.(JSON_Object [("exn", JSON_String exn_str)]) in
  Hh_logger.error ~exn "Uncaught exception while handling a request (%s)" cmd_str;
  Error (exn_str, json_data)

(* This is the common code which wraps each command handler. It deals with stuff like logging and
 * catching exceptions *)
let wrap_ephemeral_handler handler ~genv ~request_id ~client_context ~workload ~cmd_str arg =
  Hh_logger.info "%s" cmd_str;
  MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;

  let should_print_summary = Options.should_profile genv.options in
  let%lwt (profiling, result) =
    Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
        try%lwt
          let%lwt result = handler ~genv ~request_id ~workload ~profiling arg in
          Lwt.return (Ok result)
        with
        | Lwt.Canceled as exn -> Exception.(reraise (wrap exn))
        | exn ->
          let exn = Exception.wrap exn in
          Lwt.return (handle_ephemeral_uncaught_exception cmd_str exn)
    )
  in
  send_command_summary profiling cmd_str;
  Lwt.return (send_ephemeral_response ~profiling ~client_context ~cmd_str ~request_id result)

let wrap_immediate_ephemeral_handler
    handler ~genv ~request_id ~client_context ~workload ~cmd_str arg =
  Hh_logger.info "%s" cmd_str;

  let should_print_summary = Options.should_profile genv.options in
  let (profiling, result) =
    Profiling_js.with_profiling_sync ~label:"Command" ~should_print_summary (fun profiling ->
        try Ok (handler ~genv ~request_id ~workload ~profiling arg) with
        | exn ->
          let exn = Exception.wrap exn in
          handle_ephemeral_uncaught_exception cmd_str exn
    )
  in
  send_ephemeral_response ~profiling ~client_context ~cmd_str ~request_id result

(* A few commands need to be handled immediately, as soon as they arrive from the monitor. An
 * `env` is NOT available, since we don't have the server's full attention *)
let handle_ephemeral_immediately_unsafe ~genv:_ ~request_id:_ ~workload ~profiling () =
  let (response, json_data) = workload ~profiling in
  ((), response, json_data)

let handle_ephemeral_immediately =
  wrap_immediate_ephemeral_handler handle_ephemeral_immediately_unsafe

(* If command running in serial (i.e. not in parallel with a recheck) is canceled, it kicks off a
 * recheck itself and then reruns itself
 *
 * While parallelizable commands can be run out of order (some might get deferred),
 * nonparallelizable commands always run in order. So that's why we don't defer commands here.
 *
 * Since this might run a recheck, `workload ~profiling ~env` MUST return the new env.
 *)
let rec run_command_in_serial ~genv ~env ~profiling ~workload =
  try%lwt workload ~profiling ~env with
  | Lwt.Canceled ->
    Hh_logger.info "Command successfully canceled. Running a recheck before restarting the command";
    let%lwt (recheck_profiling, env) = Rechecker.recheck_loop genv env in
    List.iter (fun from -> Profiling_js.merge ~into:profiling ~from) recheck_profiling;
    Hh_logger.info "Now restarting the command";
    run_command_in_serial ~genv ~env ~profiling ~workload

(* A command that is running in parallel with a recheck, if canceled, can't just run a recheck
 * itself. It needs to defer itself until later. *)
let run_command_in_parallel ~env ~profiling ~name ~workload ~mk_workload =
  try%lwt
    let%lwt (response, json_data) = workload ~profiling ~env in
    Lwt.return (response, json_data)
  with
  | Lwt.Canceled as exn ->
    let exn = Exception.wrap exn in
    Hh_logger.info
      "Command successfully canceled. Requeuing the command for after the next recheck.";
    ServerMonitorListenerState.defer_parallelizable_workload ~name (mk_workload ());
    Exception.reraise exn

let rec handle_parallelizable_ephemeral_unsafe
    ~client_context ~cmd_str ~genv ~request_id ~workload ~profiling env =
  let%lwt (response, json_data) =
    let mk_workload () =
      handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
    in
    run_command_in_parallel ~env ~profiling ~name:cmd_str ~workload ~mk_workload
  in
  Lwt.return ((), response, json_data)

and handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str =
  let parallelizable_workload_should_be_cancelled () = false in
  let parallelizable_workload_handler env =
    try%lwt
      let handler = handle_parallelizable_ephemeral_unsafe ~client_context ~cmd_str in
      let%lwt result =
        wrap_ephemeral_handler handler ~genv ~request_id ~client_context ~workload ~cmd_str env
      in
      match result with
      | Ok ()
      | Error () ->
        Lwt.return_unit
    with
    | Lwt.Canceled ->
      (* It's fine for parallelizable commands to be canceled - they'll be run again later *)
      Lwt.return_unit
  in
  { WorkloadStream.parallelizable_workload_should_be_cancelled; parallelizable_workload_handler }

let handle_nonparallelizable_ephemeral_unsafe ~genv ~request_id:_ ~workload ~profiling env =
  run_command_in_serial ~genv ~env ~profiling ~workload

let handle_nonparallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str =
  let workload_should_be_cancelled () = false in
  let workload_handler env =
    let%lwt result =
      wrap_ephemeral_handler
        handle_nonparallelizable_ephemeral_unsafe
        ~genv
        ~request_id
        ~client_context
        ~workload
        ~cmd_str
        env
    in
    match result with
    | Ok env -> Lwt.return env
    | Error () -> Lwt.return env
  in
  { WorkloadStream.workload_should_be_cancelled; workload_handler }

let enqueue_or_handle_ephemeral genv (request_id, command_with_context) =
  let { ServerCommandWithContext.client_logging_context = client_context; command } =
    command_with_context
  in
  let cmd_str = spf "%s: %s" request_id (ServerProt.Request.to_string command) in
  match get_ephemeral_handler genv command with
  | Handle_immediately workload ->
    let result =
      handle_ephemeral_immediately ~genv ~request_id ~client_context ~workload ~cmd_str ()
    in
    (match result with
    | Ok ()
    | Error () ->
      ())
  | Handle_parallelizable workload ->
    let workload =
      handle_parallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
    in
    ServerMonitorListenerState.push_new_parallelizable_workload ~name:cmd_str workload
  | Handle_nonparallelizable workload ->
    let workload =
      handle_nonparallelizable_ephemeral ~genv ~request_id ~client_context ~workload ~cmd_str
    in
    ServerMonitorListenerState.push_new_workload ~name:cmd_str workload

let with_error ?(stack : Utils.callstack option) ~(reason : string) (metadata : LspProt.metadata) :
    LspProt.metadata =
  let open LspProt in
  let stack =
    match stack with
    | Some stack -> stack
    | None -> Utils.Callstack ""
  in
  let error_info = Some (ExpectedError, reason, stack) in
  { metadata with error_info }

let handle_persistent_canceled ~id ~metadata ~client:_ ~profiling:_ =
  let e = { Error.code = Error.RequestCancelled; message = "cancelled"; data = None } in
  let response = ResponseMessage (id, ErrorResult (e, "")) in
  let metadata = with_error ~stack:(Utils.Callstack "") ~reason:"cancelled" metadata in
  (LspProt.LspFromServer (Some response), metadata)

let cancelled_request_id_opt = function
  | LspProt.LspToServer (RequestMessage (id, _))
    when IdSet.mem id !ServerMonitorListenerState.cancellation_requests ->
    Some id
  | _ -> None

let check_if_cancelled ~profiling ~client request metadata =
  request
  |> cancelled_request_id_opt
  |> Base.Option.map ~f:(fun id ->
         Hh_logger.info
           "Skipping canceled persistent request: %s"
           (LspProt.string_of_request request);

         (* We can't actually skip a canceled request...we need to send a response. But we can
          * skip the normal handler *)
         handle_persistent_canceled ~id ~metadata ~client ~profiling
     )

let handle_persistent_uncaught_exception request e =
  let exception_constructor = Exception.get_ctor_string e in
  let stack = Exception.get_backtrace_string e in
  LspProt.UncaughtException { request; exception_constructor; stack }

let send_persistent_response ~profiling ~client result =
  let server_profiling = Some profiling in
  let server_logging_context = Some (FlowEventLogger.get_context ()) in
  let (ret, lsp_response, metadata) = result in
  let metadata = { metadata with LspProt.server_profiling; server_logging_context } in
  let response = (lsp_response, metadata) in
  Persistent_connection.send_response response client;
  Hh_logger.info "Persistent response: %s" (LspProt.string_of_response lsp_response);
  ret

type 'a persistent_handling_result = 'a * LspProt.response * LspProt.metadata

let wrap_persistent_handler
    (type a b)
    (handler :
      genv:ServerEnv.genv ->
      workload:a ->
      client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      env ->
      b persistent_handling_result Lwt.t
      )
    ~(genv : ServerEnv.genv)
    ~(client_id : LspProt.client_id)
    ~(request : LspProt.request_with_metadata)
    ~(workload : a)
    ~(default_ret : b)
    (arg : env) : b Lwt.t =
  let (request, metadata) = request in
  match Persistent_connection.get_client client_id with
  | None ->
    Hh_logger.error "Unknown persistent client %d. Maybe connection went away?" client_id;
    Lwt.return default_ret
  | Some client ->
    Hh_logger.info "Persistent request: %s" (LspProt.string_of_request request);
    MonitorRPC.status_update ~event:ServerStatus.Handling_request_start;

    let should_print_summary = Options.should_profile genv.options in
    let%lwt (profiling, result) =
      Profiling_js.with_profiling_lwt ~label:"Command" ~should_print_summary (fun profiling ->
          match check_if_cancelled ~profiling ~client request metadata with
          | Some (response, json_data) -> Lwt.return (default_ret, response, json_data)
          | None ->
            (try%lwt handler ~genv ~workload ~client ~profiling arg with
            | Lwt.Canceled as e ->
              (* Don't swallow Lwt.Canceled. Parallelizable commands may be canceled and run again
               * later. *)
              Exception.(reraise (wrap e))
            | e ->
              let response = handle_persistent_uncaught_exception request (Exception.wrap e) in
              Lwt.return (default_ret, response, metadata))
      )
    in
    let ret = send_persistent_response ~profiling ~client result in
    (* we'll send this "Finishing_up" event only after sending the LSP response *)
    send_command_summary profiling (LspProt.string_of_request request);
    Lwt.return ret

let rec handle_parallelizable_persistent_unsafe
    ~request ~genv ~name ~workload ~client ~profiling env : unit persistent_handling_result Lwt.t =
  let mk_workload () =
    let client_id = Persistent_connection.get_id client in
    handle_parallelizable_persistent ~genv ~client_id ~request ~name ~workload
  in
  let workload = workload ~client in
  let%lwt (response, json_data) =
    run_command_in_parallel ~env ~profiling ~name ~workload ~mk_workload
  in
  Lwt.return ((), response, json_data)

and handle_parallelizable_persistent ~genv ~client_id ~request ~name ~workload :
    WorkloadStream.parallelizable_workload =
  let parallelizable_workload_should_be_cancelled () =
    Option.is_some (cancelled_request_id_opt (fst request))
  in
  let parallelizable_workload_handler env =
    try%lwt
      wrap_persistent_handler
        (handle_parallelizable_persistent_unsafe ~request ~name)
        ~genv
        ~client_id
        ~request
        ~workload
        ~default_ret:()
        env
    with
    | Lwt.Canceled ->
      (* It's fine for parallelizable commands to be canceled - they'll be run again later *)
      Lwt.return_unit
  in
  { WorkloadStream.parallelizable_workload_should_be_cancelled; parallelizable_workload_handler }

let handle_nonparallelizable_persistent_unsafe ~genv ~workload ~client ~profiling env =
  let workload = workload ~client in
  run_command_in_serial ~genv ~env ~profiling ~workload

let handle_nonparallelizable_persistent ~genv ~client_id ~request ~workload :
    WorkloadStream.workload =
  let workload_should_be_cancelled () = Option.is_some (cancelled_request_id_opt (fst request)) in
  let workload_handler env =
    wrap_persistent_handler
      handle_nonparallelizable_persistent_unsafe
      ~genv
      ~client_id
      ~request
      ~workload
      ~default_ret:env
      env
  in
  { WorkloadStream.workload_should_be_cancelled; workload_handler }

let did_open env client ~options (_files : (string * string) Nel.t) : ServerEnv.env Lwt.t =
  let (errors, warnings) = ErrorCollator.get_with_separate_warnings env in
  Persistent_connection.send_errors_if_subscribed
    ~client
    ~flowconfig_vscode_detailed_diagnostics:(Options.vscode_detailed_diagnostics options)
    ~errors_reason:LspProt.Env_change
    ~errors
    ~warnings;
  Lwt.return env

let did_close env client ~options : ServerEnv.env Lwt.t =
  let (errors, warnings) = ErrorCollator.get_with_separate_warnings env in
  Persistent_connection.send_errors_if_subscribed
    ~client
    ~flowconfig_vscode_detailed_diagnostics:(Options.vscode_detailed_diagnostics options)
    ~errors_reason:LspProt.Env_change
    ~errors
    ~warnings;
  Lwt.return env

let keyvals_of_json (json : Hh_json.json option) : (string * Hh_json.json) list =
  match json with
  | None -> []
  | Some (Hh_json.JSON_Object keyvals) -> keyvals
  | Some json -> [("json_data", json)]

let with_data ~(extra_data : Hh_json.json option) (metadata : LspProt.metadata) : LspProt.metadata =
  let open LspProt in
  let extra_data = metadata.extra_data @ keyvals_of_json extra_data in
  { metadata with extra_data }

(** This is commonly called by persistent handlers when something goes wrong and we need to return
  an error response *)
let mk_lsp_error_response ~id ~reason ?stack metadata =
  let metadata = with_error ?stack ~reason metadata in
  let (_, reason, Utils.Callstack stack) = Base.Option.value_exn metadata.LspProt.error_info in
  let message =
    match id with
    | Some id ->
      Hh_logger.error "Error: %s\n%s" reason stack;
      let friendly_message =
        "Flow encountered an unexpected error while handling this request. "
        ^ "See the Flow logs for more details."
      in
      let e = { Error.code = Error.UnknownErrorCode; message = friendly_message; data = None } in
      ResponseMessage (id, ErrorResult (e, stack))
    | None ->
      let text =
        Printf.sprintf "%s [%s]\n%s" reason (Error.show_code Error.UnknownErrorCode) stack
      in
      NotificationMessage
        (TelemetryNotification { LogMessage.type_ = MessageType.ErrorMessage; message = text })
  in
  (LspProt.LspFromServer (Some message), metadata)

let handle_persistent_subscribe ~options ~metadata ~client ~profiling:_ ~env =
  let (current_errors, current_warnings) = ErrorCollator.get_with_separate_warnings env in
  Persistent_connection.subscribe_client
    ~client
    ~flowconfig_vscode_detailed_diagnostics:(Options.vscode_detailed_diagnostics options)
    ~current_errors
    ~current_warnings;
  Lwt.return (env, LspProt.LspFromServer None, metadata)

(* A did_open notification can come in about N files, which is great. But sometimes we'll get
 * N did_open notifications in quick succession. Let's batch them up and run them all at once!
 *)
let (enqueue_did_open_files, handle_persistent_did_open_notification) =
  let pending = ref SMap.empty in
  let enqueue_did_open_files (files : (string * string) Nel.t) =
    (* Overwrite the older content with the newer content *)
    pending := Nel.fold_left (fun acc (fn, content) -> SMap.add fn content acc) !pending files
  in
  let get_and_clear_did_open_files () : (string * string) list =
    let ret = SMap.bindings !pending in
    pending := SMap.empty;
    ret
  in
  let handle_persistent_did_open_notification ~options ~metadata ~client ~profiling:_ ~env =
    let%lwt env =
      match get_and_clear_did_open_files () with
      | [] -> Lwt.return env
      | first :: rest -> did_open env client ~options (first, rest)
    in
    Lwt.return (env, LspProt.LspFromServer None, metadata)
  in
  (enqueue_did_open_files, handle_persistent_did_open_notification)

let handle_persistent_did_open_notification_no_op ~metadata ~client:_ ~profiling:_ =
  (LspProt.LspFromServer None, metadata)

let handle_persistent_did_change_notification ~params ~metadata ~client ~profiling:_ =
  let { Lsp.DidChange.textDocument; contentChanges } = params in
  let { VersionedTextDocumentIdentifier.uri; version = _ } = textDocument in
  let fn = Lsp_helpers.lsp_uri_to_path uri in
  match Persistent_connection.client_did_change client fn contentChanges with
  | Ok () -> (LspProt.LspFromServer None, metadata)
  | Error (reason, stack) -> mk_lsp_error_response ~id:None ~reason ~stack metadata

let handle_persistent_did_save_notification ~metadata ~client:_ ~profiling:_ =
  (LspProt.LspFromServer None, metadata)

let handle_persistent_did_close_notification ~options ~metadata ~client ~profiling:_ ~env =
  let%lwt env = did_close env client ~options in
  Lwt.return (env, LspProt.LspFromServer None, metadata)

let handle_persistent_did_close_notification_no_op ~metadata ~client:_ ~profiling:_ =
  (LspProt.LspFromServer None, metadata)

let handle_persistent_cancel_notification ~params ~metadata ~client:_ ~profiling:_ ~env =
  let id = params.CancelRequest.id in
  (* by the time this cancel request shows up in the queue, then it must already *)
  (* have had its effect if any on messages earlier in the queue, and so can be  *)
  (* removed. *)
  ServerMonitorListenerState.(cancellation_requests := IdSet.remove id !cancellation_requests);
  Lwt.return (env, LspProt.LspFromServer None, metadata)

let handle_persistent_did_change_configuration_notification ~params ~metadata ~client ~profiling:_ =
  let open Hh_json_helpers in
  let open Persistent_connection in
  let { Lsp.DidChangeConfiguration.settings } = params in
  (match settings with
  | Hh_json.JSON_Null ->
    (* a null notification means we should pull the configs we care about.
     * In this case, we should not update the client config will all the defaults. *)
    ()
  | _ ->
    let client_config = client_config client in
    let json = Some settings in
    let client_config =
      match Jget.val_opt json "detailedErrorRendering" with
      | Some (Hh_json.JSON_String "true")
      | Some (Hh_json.JSON_Bool true) ->
        { client_config with Client_config.detailed_error_rendering = Client_config.True }
      | Some (Hh_json.JSON_String "false")
      | Some (Hh_json.JSON_Bool false) ->
        { client_config with Client_config.detailed_error_rendering = Client_config.False }
      | Some _
      | None ->
        { client_config with Client_config.detailed_error_rendering = Client_config.Default }
    in
    let suggest = Jget.obj_opt json "suggest" in
    let client_config =
      match Jget.bool_opt suggest "autoImports" with
      | Some suggest_autoimports -> { client_config with Client_config.suggest_autoimports }
      | None -> client_config
    in
    let client_config =
      match Jget.val_opt suggest "rankAutoimportsByUsage" with
      | Some (Hh_json.JSON_String "true")
      | Some (Hh_json.JSON_Bool true) ->
        { client_config with Client_config.rank_autoimports_by_usage = Client_config.True }
      | Some (Hh_json.JSON_String "false")
      | Some (Hh_json.JSON_Bool false) ->
        { client_config with Client_config.rank_autoimports_by_usage = Client_config.False }
      | Some _
      | None ->
        { client_config with Client_config.rank_autoimports_by_usage = Client_config.Default }
    in
    let client_config =
      let show_suggest_ranking_info = Jget.bool_d suggest "showRankingInfo" ~default:false in
      { client_config with Client_config.show_suggest_ranking_info }
    in
    client_did_change_configuration client client_config);
  (LspProt.LspFromServer None, metadata)

let handle_persistent_get_def
    ~reader ~options ~id ~params ~file_input ~metadata ~client ~profiling ~env =
  let file_input =
    match file_input with
    | Some file_input -> file_input
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
       * a little more defensive. The only danger here is that the file contents may have changed *)
      file_input_of_text_document_position ~client params
  in
  let (line, char) = Flow_lsp_conversions.position_of_document_position params in
  let type_parse_artifacts_cache = Some (Persistent_connection.type_parse_artifacts_cache client) in
  let (result, extra_data) =
    get_def ~options ~reader ~env ~profiling ~type_parse_artifacts_cache (file_input, line, char)
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok locs ->
    let { TextDocumentPositionParams.textDocument; position = _ } = params in
    let default_uri = textDocument.TextDocumentIdentifier.uri in
    let locations =
      Base.List.map locs ~f:(Flow_lsp_conversions.loc_to_lsp_with_default ~default_uri)
    in
    let response = ResponseMessage (id, DefinitionResult locations) in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)

let handle_persistent_infer_type
    ~options ~reader ~id ~params ~file_input ~metadata ~client ~profiling ~env =
  let open TextDocumentPositionParams in
  let file_input =
    match file_input with
    | Some file_input -> file_input
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
       * a little more defensive. The only danger here is that the file contents may have changed *)
      file_input_of_text_document_position ~client params
  in
  let (line, column) = Flow_lsp_conversions.position_of_document_position params in
  (* if Some, would write to server logs *)
  let type_parse_artifacts_cache = Some (Persistent_connection.type_parse_artifacts_cache client) in
  let input =
    {
      ServerProt.Infer_type_options.input = file_input;
      line;
      char = column;
      verbose = None;
      omit_targ_defaults = false;
      wait_for_recheck = None;
      verbose_normalizer = false;
      max_depth = 50;
      json = false;
      strip_root = None;
      expanded = false;
      no_typed_ast_for_imports = false;
    }
  in
  let (result, extra_data) =
    infer_type ~options ~reader ~env ~profiling ~type_parse_artifacts_cache input
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok (ServerProt.Response.Infer_type_response { loc; tys; documentation }) ->
    (* loc may be the 'none' location; content may be None. *)
    (* If both are none then we'll return null; otherwise we'll return a hover *)
    let default_uri = params.textDocument.TextDocumentIdentifier.uri in
    let location = Flow_lsp_conversions.loc_to_lsp_with_default ~default_uri loc in
    let range =
      if loc = Loc.none then
        None
      else
        Some location.Lsp.Location.range
    in
    let contents =
      let (types, refs) =
        match tys with
        | ServerProt.Response.Infer_type_string (Some (result, refs)) ->
          ( [MarkedCode ("flow", result)],
            Base.Option.value_map refs ~default:[] ~f:(fun refs ->
                Base.List.map refs ~f:(fun (name, loc) ->
                    let { Loc.source; start = { Loc.line; column; _ }; _ } = loc in
                    match source with
                    | None -> []
                    | Some file ->
                      (* We'll use default_uri here as we don't expect this to fail *)
                      let location =
                        Flow_lsp_conversions.loc_to_lsp_with_default ~default_uri loc
                      in
                      let lib = Utils_js.ite (File_key.is_lib_file file) "(lib) " "" in
                      let basename =
                        File_path.basename (File_path.make (File_key.to_string file))
                      in
                      let str =
                        spf
                          "`%s` defined at [`%s%s:%d:%d`](%s#L%d,%d)"
                          name
                          lib
                          basename
                          line
                          column
                          (DocumentUri.to_string location.Location.uri)
                          line
                          column
                      in
                      [MarkedString str]
                )
            )
          )
        | _ -> ([], [])
      in
      let docs = Base.Option.to_list documentation |> List.map (fun doc -> MarkedString doc) in
      match Base.List.concat ([types] @ refs @ [docs]) with
      | [] -> [MarkedString "?"]
      | _ :: _ as contents -> contents
    in
    let r =
      match (range, tys, documentation) with
      | (None, ServerProt.Response.Infer_type_string None, None) -> None
      | (_, _, _) -> Some { Lsp.Hover.contents; range }
    in
    let response = ResponseMessage (id, HoverResult r) in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)

let handle_persistent_code_action_request
    ~reader ~options ~id ~params ~metadata ~client ~profiling ~env =
  let (result, extra_data) = find_code_actions ~reader ~options ~profiling ~env ~client ~params in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok code_actions ->
    Lwt.return
      (LspProt.LspFromServer (Some (ResponseMessage (id, CodeActionResult code_actions))), metadata)
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)

let handle_persistent_autocomplete_lsp
    ~reader ~options ~id ~params ~file_input ~metadata ~client ~profiling ~env =
  let client_config = Persistent_connection.client_config client in
  let lsp_init_params = Persistent_connection.lsp_initialize_params client in
  let is_snippet_supported = Lsp_helpers.supports_snippets lsp_init_params in
  let is_tags_supported = Lsp_helpers.supports_tags lsp_init_params in
  let is_preselect_supported = Lsp_helpers.supports_preselect lsp_init_params in
  let is_label_detail_supported =
    Lsp_helpers.supports_completion_item_label_details lsp_init_params
  in
  let is_insert_replace_supported =
    Lsp_helpers.supports_completion_item_insert_replace lsp_init_params
  in
  let { Completion.loc = lsp_loc; context } = params in
  let file_input =
    match file_input with
    | Some file_input -> file_input
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
       * a little more defensive. The only danger here is that the file contents may have changed *)
      file_input_of_text_document_position ~client lsp_loc
  in
  let (line, char) = Flow_lsp_conversions.position_of_document_position lsp_loc in
  let trigger_character =
    Base.Option.value_map
      ~f:(fun completionContext -> completionContext.Completion.triggerCharacter)
      ~default:None
      context
  in
  let imports =
    Persistent_connection.Client_config.suggest_autoimports client_config
    && Options.autoimports options
  in
  let imports_ranked_usage = rank_autoimports_by_usage ~options client in
  let imports_min_characters = Options.autoimports_min_characters options in
  let imports_ranked_usage_boost_exact_match_min_length =
    Options.autoimports_ranked_by_usage_boost_exact_match_min_length options
  in
  let show_ranking_info =
    Persistent_connection.Client_config.show_suggest_ranking_info client_config
  in
  let (result, extra_data) =
    autocomplete
      ~trigger_character
      ~reader
      ~options
      ~env
      ~client:(Some client)
      ~profiling
      ~input:file_input
      ~cursor:(line, char)
      ~imports
      ~imports_min_characters
      ~imports_ranked_usage
      ~imports_ranked_usage_boost_exact_match_min_length
      ~show_ranking_info
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok (token, completions, token_loc, ac_type) ->
    let file_key =
      match token_loc with
      | None -> None
      | Some token_loc -> token_loc.Loc.source
    in
    let (token_line, token_char) =
      match token_loc with
      | None -> (None, None)
      | Some token_loc -> (Some token_loc.Loc.start.Loc.line, Some token_loc.Loc.start.Loc.column)
    in
    let autocomplete_session_length =
      match (token_line, token_char, file_key) with
      | (None, _, _)
      | (_, None, _)
      | (_, _, None) ->
        None
      | (Some token_line, Some token_char, file_key) ->
        Some
          (Persistent_connection.autocomplete_session
             client
             ~ac_type
             (token_line, token_char, file_key)
          )
    in
    let typed_len =
      match token_char with
      | None -> None
      | Some token_char -> Some (char - token_char)
    in
    let metadata =
      with_data
        ~extra_data:
          (Some
             (Hh_json_helpers.Jprint.object_opt
                [
                  ("session_requests", Base.Option.map ~f:Hh_json.int_ autocomplete_session_length);
                  ("typed_length", Base.Option.map ~f:Hh_json.int_ typed_len);
                ]
             )
          )
        metadata
    in
    let result =
      Flow_lsp_conversions.flow_completions_to_lsp
        ?token
        ?autocomplete_session_length
        ?typed_len
        ~ac_type
        ~is_snippet_supported
        ~is_tags_supported
        ~is_preselect_supported
        ~is_label_detail_supported
        ~is_insert_replace_supported
        completions
    in
    let response = ResponseMessage (id, CompletionResult result) in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)

let handle_persistent_signaturehelp_lsp
    ~reader ~options ~id ~params ~file_input ~metadata ~client ~profiling ~env =
  let file_input =
    match file_input with
    | Some file_input -> file_input
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
         * a little more defensive. The only danger here is that the file contents may have changed *)
      file_input_of_text_document_position ~client params.SignatureHelp.loc
  in
  let (line, col) = Flow_lsp_conversions.position_of_document_position params.SignatureHelp.loc in
  let fn_content =
    match file_input with
    | File_input.FileContent (fn, content) -> Ok (fn, content)
    | File_input.FileName fn ->
      (try Ok (Some fn, Sys_utils.cat fn) with
      | e ->
        let e = Exception.wrap e in
        Error (Exception.get_ctor_string e, Utils.Callstack (Exception.get_backtrace_string e)))
  in
  match fn_content with
  | Error (reason, stack) -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason ~stack metadata)
  | Ok (filename, contents) ->
    let path =
      match filename with
      | Some filename -> filename
      | None -> "-"
    in
    let path = File_key.SourceFile path in
    let (file_artifacts_result, did_hit_cache) =
      let parse_result = lazy (Type_contents.parse_contents ~options ~profiling contents path) in
      let type_parse_artifacts_cache =
        Some (Persistent_connection.type_parse_artifacts_cache client)
      in
      type_parse_artifacts_with_cache
        ~options
        ~profiling
        ~type_parse_artifacts_cache
        env.master_cx
        path
        parse_result
    in
    let metadata =
      let json_props = add_cache_hit_data_to_json [] did_hit_cache in
      let json = Hh_json.JSON_Object json_props in
      with_data ~extra_data:(Some json) metadata
    in
    (match file_artifacts_result with
    | Error _parse_errors ->
      Lwt.return
        (mk_lsp_error_response
           ~id:(Some id)
           ~reason:"Couldn't parse file in parse_artifacts"
           metadata
        )
    | Ok (Parse_artifacts { ast; file_sig; _ }, Typecheck_artifacts { cx; typed_ast; _ }) ->
      let func_details =
        let cursor_loc = Loc.cursor (Some path) line col in
        Signature_help.find_signatures
          ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
          ~get_ast_from_shared_mem:(Parsing_heaps.Reader.get_ast ~reader)
          ~cx
          ~file_sig
          ~ast
          ~typed_ast
          cursor_loc
      in
      (match func_details with
      | Ok details ->
        let r = SignatureHelpResult (Flow_lsp_conversions.flow_signature_help_to_lsp details) in
        let response = ResponseMessage (id, r) in
        let has_any_documentation =
          match details with
          | None -> false
          | Some (details_list, _) ->
            Base.List.exists
              details_list
              ~f:
                ServerProt.Response.(
                  fun { func_documentation; param_tys; _ } ->
                    Base.Option.is_some func_documentation
                    || Base.List.exists param_tys ~f:(fun { param_documentation; _ } ->
                           Base.Option.is_some param_documentation
                       )
                )
        in
        let extra_data =
          Some (Hh_json.JSON_Object [("documentation", Hh_json.JSON_Bool has_any_documentation)])
        in
        Lwt.return (LspProt.LspFromServer (Some response), with_data ~extra_data metadata)
      | Error _ ->
        Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason:"Failed to normalize type" metadata)))

let get_file_artifacts ~options ~client ~profiling ~env pos :
    ((Types_js_types.file_artifacts * File_key.t) option, string) result * Hh_json.json option =
  let file_input = file_input_of_text_document_position ~client pos in
  match of_file_input ~options ~env file_input with
  | Error (Failed reason) -> (Error reason, None)
  | Error (Skipped reason) -> (Ok None, json_of_skipped reason)
  | Ok (file_key, content) ->
    let (file_artifacts_result, did_hit_cache) =
      let type_parse_artifacts_cache =
        Some (Persistent_connection.type_parse_artifacts_cache client)
      in
      let parse_result = lazy (Type_contents.parse_contents ~options ~profiling content file_key) in
      type_parse_artifacts_with_cache
        ~options
        ~profiling
        ~type_parse_artifacts_cache
        env.master_cx
        file_key
        parse_result
    in
    (match file_artifacts_result with
    | Error _parse_errors ->
      let err_str = "Couldn't parse file in parse_artifacts" in
      let json_props = add_cache_hit_data_to_json [] did_hit_cache in
      (Error err_str, Some (Hh_json.JSON_Object json_props))
    | Ok file_artifacts -> (Ok (Some (file_artifacts, file_key)), None))

let find_local_references ~loc_of_aloc ~file_artifacts ~kind file_key pos :
    ((Get_def_types.def_info * FindRefsTypes.find_refs_ok, string) result * Hh_json.json option)
    Lwt.t =
  let (parse_artifacts, typecheck_artifacts) = file_artifacts in
  let (line, col) = Flow_lsp_conversions.position_of_document_position pos in
  let%lwt refs_results =
    let results =
      FindRefs_js.find_local_refs
        ~loc_of_aloc
        ~file_key
        ~parse_artifacts
        ~typecheck_artifacts
        ~kind
        ~line
        ~col
    in
    Lwt.return results
  in
  let extra_data =
    match refs_results with
    | Ok (_def_info, FindRefsTypes.FoundReferences _) ->
      Some (Hh_json.JSON_Object [("result", Hh_json.JSON_String "SUCCESS")])
    | Ok (_def_info, FindRefsTypes.NoDefinition no_def_reason) ->
      Some
        (Hh_json.JSON_Object
           [
             ("result", Hh_json.JSON_String "BAD_LOC");
             ( "error",
               Hh_json.JSON_String (Base.Option.value ~default:"No reason given" no_def_reason)
             );
           ]
        )
    | Error msg ->
      Some
        (Hh_json.JSON_Object
           [("result", Hh_json.JSON_String "FAILURE"); ("error", Hh_json.JSON_String msg)]
        )
  in
  Lwt.return (refs_results, extra_data)

let map_local_find_references_results
    ~loc_of_aloc ~options ~client ~profiling ~env ~f text_doc_position =
  let (file_artifacts_opt, extra_parse_data) =
    get_file_artifacts ~options ~client ~profiling ~env text_doc_position
  in
  match file_artifacts_opt with
  | Ok (Some (file_artifacts, file_key)) ->
    let%lwt (local_refs, extra_data) =
      find_local_references
        ~loc_of_aloc
        ~file_artifacts
        ~kind:FindRefsTypes.FindReferences
        file_key
        text_doc_position
    in
    let mapped_refs =
      match local_refs with
      | Ok (_def_info, FindRefsTypes.FoundReferences refs) -> Ok (Base.List.filter_map ~f refs)
      | Ok (_def_info, FindRefsTypes.NoDefinition _) -> Ok []
      | Error _ as err -> err
    in
    Lwt.return (mapped_refs, extra_data)
  | Ok None -> Lwt.return (Ok [], extra_parse_data)
  | Error _ as err -> Lwt.return (err, extra_parse_data)

let handle_global_find_references
    ~loc_of_aloc
    ~options
    ~id
    ~metadata
    ~client
    ~profiling
    ~env
    ~kind
    ~request:_
    ~refs_to_lsp_result
    text_doc_position =
  let (file_artifacts_opt, extra_parse_data) =
    get_file_artifacts ~options ~client ~profiling ~env text_doc_position
  in
  let error_return reason =
    let metadata =
      with_data
        ~extra_data:
          (Some
             (Hh_json.JSON_Object
                [("result", Hh_json.JSON_String "FAILURE"); ("error", Hh_json.JSON_String reason)]
             )
          )
        metadata
    in
    let (resp, metadata) = mk_lsp_error_response ~id:(Some id) ~reason metadata in
    (env, resp, metadata)
  in
  match file_artifacts_opt with
  | Error reason -> Lwt.return (error_return reason)
  | Ok None ->
    let metadata = with_data ~extra_data:extra_parse_data metadata in
    Lwt.return
      ( env,
        LspProt.LspFromServer (Some (ResponseMessage (id, refs_to_lsp_result ~current_ast:None []))),
        metadata
      )
  | Ok (Some ((parse_artifacts, typecheck_artifacts), file_key)) ->
    let (Types_js_types.Parse_artifacts { ast; _ }) = parse_artifacts in
    let (line, col) = Flow_lsp_conversions.position_of_document_position text_doc_position in
    (match
       FindRefs_js.find_local_refs
         ~loc_of_aloc
         ~file_key
         ~parse_artifacts
         ~typecheck_artifacts
         ~kind
         ~line
         ~col
     with
    | Error reason ->
      (* If initial go-to-definition errors, we respond with error. *)
      Lwt.return (error_return reason)
    | Ok (Get_def_types.NoDefinition no_def_reason, _) ->
      (* If initial go-to-definition returns no result, we respond with empty results. *)
      let extra_data =
        Hh_json.JSON_Object
          [
            ("result", Hh_json.JSON_String "BAD_LOC");
            ( "error",
              Hh_json.JSON_String (Base.Option.value ~default:"No reason given" no_def_reason)
            );
          ]
      in
      let metadata = with_data ~extra_data:(Some extra_data) metadata in
      Lwt.return
        ( env,
          LspProt.LspFromServer
            (Some (ResponseMessage (id, refs_to_lsp_result ~current_ast:(Some ast) []))),
          metadata
        )
    | Ok (Get_def_types.PropertyDefinition (Get_def_types.PrivateNameProperty _), local_refs) ->
      (* We directly return the already computed find-ref result for private names, instead of
       * wasting time scheduling another recheck. *)
      let references =
        match local_refs with
        | FindRefsTypes.NoDefinition _ -> []
        | FindRefsTypes.FoundReferences refs -> refs
      in
      let extra_data = Hh_json.JSON_Object [("result", Hh_json.JSON_String "SUCCESS")] in
      let metadata = with_data ~extra_data:(Some extra_data) metadata in
      Lwt.return
        ( env,
          LspProt.LspFromServer
            (Some (ResponseMessage (id, refs_to_lsp_result ~current_ast:(Some ast) references))),
          metadata
        )
    | Ok (def_info, local_refs) ->
      (* The most interesting path for global find refs. We schedule a recheck, and a new
       * non-parallelizable command after the recheck to read the find ref *)
      let def_locs = GetDefUtils.all_locs_of_def_info def_info in
      let references_to_lsp_response result =
        match result with
        | Ok refs ->
          (* We replace all the find ref results from recheck on the current file with the ones
           * from local_refs. The local ones are more fresh, since it can see unsaved changes. *)
          let refs =
            Base.List.append
              (match local_refs with
              | FindRefsTypes.NoDefinition _ -> []
              | FindRefsTypes.FoundReferences refs -> refs)
              (Base.List.filter refs ~f:(fun (_, ref_loc) -> Loc.source ref_loc <> Some file_key))
          in
          let response = ResponseMessage (id, refs_to_lsp_result ~current_ast:(Some ast) refs) in
          let metadata =
            with_data
              ~extra_data:(Some (Hh_json.JSON_Object [("result", Hh_json.JSON_String "SUCCESS")]))
              metadata
          in
          (LspProt.LspFromServer (Some response), metadata)
        | Error reason ->
          let (_, resp, metadata) = error_return reason in
          (resp, metadata)
      in
      ServerMonitorListenerState.push_global_find_ref_request
        ~request:{ FindRefsTypes.def_info; kind }
        ~client
        ~references_to_lsp_response
        def_locs;
      Lwt.return (env, LspProt.LspFromServer None, LspProt.empty_metadata))

let handle_persistent_find_references
    ~loc_of_aloc ~options ~id ~params ~metadata ~client ~profiling ~env =
  let text_doc_position = params.FindReferences.loc in
  let ref_to_location (_, loc) = Flow_lsp_conversions.loc_to_lsp loc |> Base.Result.ok in
  handle_global_find_references
    ~loc_of_aloc
    ~options
    ~id
    ~metadata
    ~client
    ~profiling
    ~env
    ~kind:FindRefsTypes.FindReferences
    ~request:(LspProt.LspToServer (RequestMessage (id, FindReferencesRequest params)))
    ~refs_to_lsp_result:(fun ~current_ast:_ refs ->
      let result_compare (_, l1) (_, l2) = Loc.compare l1 l2 in
      let locs =
        refs
        |> Base.List.dedup_and_sort ~compare:result_compare
        |> Base.List.filter_map ~f:ref_to_location
      in
      FindReferencesResult locs)
    text_doc_position

let handle_persistent_document_highlight
    ~loc_of_aloc ~options ~id ~params ~metadata ~client ~profiling ~env =
  (* All the locs are implicitly in the same file *)
  let ref_to_highlight (_, loc) =
    Some { DocumentHighlight.range = Lsp.loc_to_lsp_range loc; kind = Some DocumentHighlight.Text }
  in
  let%lwt (result, extra_data) =
    map_local_find_references_results
      ~loc_of_aloc
      ~options
      ~client
      ~profiling
      ~env
      ~f:ref_to_highlight
      params
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok result ->
    let r = DocumentHighlightResult result in
    let response = ResponseMessage (id, r) in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)

let handle_persistent_rename ~reader ~options ~id ~params ~metadata ~client ~profiling ~env =
  let Rename.{ textDocument; position; newName } = params in
  let text_doc_position = TextDocumentPositionParams.{ textDocument; position } in
  handle_global_find_references
    ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
    ~options
    ~id
    ~metadata
    ~client
    ~profiling
    ~env
    ~kind:FindRefsTypes.Rename
    ~request:(LspProt.LspToServer (RequestMessage (id, RenameRequest params)))
    ~refs_to_lsp_result:(fun ~current_ast refs ->
      let (ref_map, files) =
        List.fold_left
          (fun (ref_map, files) (ref_kind, loc) ->
            ( Loc_collections.LocMap.add loc ref_kind ref_map,
              loc
              |> Loc.source
              |> Base.Option.value_map ~default:files ~f:(fun f -> FilenameSet.add f files)
            ))
          (Loc_collections.LocMap.empty, FilenameSet.empty)
          refs
      in
      let asts =
        files
        |> FilenameSet.elements
        |> Base.List.filter_map ~f:(fun filename ->
               match current_ast with
               | Some (ast_loc, _) when Loc.source ast_loc = Some filename -> current_ast
               | _ -> Parsing_heaps.Reader.get_ast ~reader filename
           )
      in
      let diff_of_ast ast =
        Flow_ast_differ.program
          ast
          (RenameMapper.rename ~global:true ~targets:ref_map ~new_name:newName ast)
      in
      let all_diffs = Base.List.bind asts ~f:diff_of_ast in
      let opts =
        Js_layout_generator.
          {
            default_opts with
            bracket_spacing = Options.format_bracket_spacing options;
            single_quotes = Options.format_single_quotes options;
          }
      in
      let changes =
        Base.List.fold_right
          (Replacement_printer.mk_loc_patch_ast_differ ~opts all_diffs)
          ~init:UriMap.empty
          ~f:(fun (loc, newText) acc ->
            match Flow_lsp_conversions.loc_to_lsp loc |> Base.Result.ok with
            | None -> acc
            | Some { Location.uri; range } ->
              let edits = UriMap.find_opt uri acc |> Base.Option.value ~default:[] in
              let edits = { TextEdit.range; newText } :: edits in
              UriMap.add uri edits acc
        )
      in
      RenameResult { WorkspaceEdit.changes })
    text_doc_position

let handle_persistent_coverage ~options ~id ~params ~file_input ~metadata ~client ~profiling ~env =
  let textDocument = params.TypeCoverage.textDocument in
  let file_input =
    match file_input with
    | Some file_input -> file_input
    | None ->
      (* We must have failed to get the client when we first tried. We could throw here, but this is
       * a little more defensive. The only danger here is that the file contents may have changed *)
      file_input_of_text_document_identifier ~client textDocument
  in
  match of_file_input ~options ~env file_input with
  | Error (Failed reason) -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)
  | Error (Skipped reason) ->
    let range = { start = { line = 0; character = 0 }; end_ = { line = 1; character = 0 } } in
    let r =
      TypeCoverageResult
        {
          TypeCoverage.coveredPercent = 0;
          uncoveredRanges = [{ TypeCoverage.range; message = None }];
          defaultMessage = "Use @flow to get type coverage for this file";
        }
    in
    let response = ResponseMessage (id, r) in
    let metadata =
      let extra_data = json_of_skipped reason in
      with_data ~extra_data metadata
    in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Ok (file_key, file_contents) ->
    let (result, extra_data) =
      (* 'true' makes it report "unknown" for all exprs in non-flow files *)
      let force = Options.all options in
      let type_parse_artifacts_cache =
        Some (Persistent_connection.type_parse_artifacts_cache client)
      in
      coverage ~options ~env ~profiling ~type_parse_artifacts_cache ~force file_key file_contents
    in
    let metadata = with_data ~extra_data metadata in
    (match result with
    | Ok all_locs ->
      (* Figure out the percentages *)
      let accum_coverage (covered, total) (_loc, cov) =
        let covered =
          match cov with
          | Coverage.Kind.Checked -> covered + 1
          | Coverage.Kind.Any
          | Coverage.Kind.Empty ->
            covered
        in
        (covered, total + 1)
      in
      let (covered, total) = Base.List.fold all_locs ~init:(0, 0) ~f:accum_coverage in
      let coveredPercent =
        if total = 0 then
          100
        else
          100 * covered / total
      in
      (* Figure out each individual uncovered span *)
      let uncovereds =
        Base.List.filter_map all_locs ~f:(fun (loc, cov) ->
            match cov with
            | Coverage.Kind.Checked -> None
            | Coverage.Kind.Any
            | Coverage.Kind.Empty ->
              Some loc
        )
      in
      (* Imagine a tree of uncovered spans based on range inclusion. *)
      (* This sorted list is a pre-order flattening of that tree. *)
      let sorted = Base.List.sort uncovereds ~compare:Loc.compare in
      (* We can use that sorted list to remove any span which contains another, so *)
      (* the user only sees actionable reports of the smallest causes of untypedness. *)
      (* The algorithm: accept a range if its immediate successor isn't contained by it. *)
      let f (candidate, acc) loc =
        if Loc.contains candidate loc then
          (loc, acc)
        else
          (loc, candidate :: acc)
      in
      let singles =
        match sorted with
        | [] -> []
        | first :: _ ->
          let (final_candidate, singles) = Base.List.fold sorted ~init:(first, []) ~f in
          final_candidate :: singles
      in
      (* Convert to LSP *)
      let loc_to_lsp loc = { TypeCoverage.range = Lsp.loc_to_lsp_range loc; message = None } in
      let uncoveredRanges = Base.List.map singles ~f:loc_to_lsp in
      (* Send the results! *)
      let r =
        TypeCoverageResult
          {
            TypeCoverage.coveredPercent;
            uncoveredRanges;
            defaultMessage = "Un-type checked code. Consider adding type annotations.";
          }
      in
      let response = ResponseMessage (id, r) in
      Lwt.return (LspProt.LspFromServer (Some response), metadata)
    | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata))

let handle_persistent_rage ~reader ~genv ~id ~metadata ~client:_ ~profiling:_ ~env =
  let root = File_path.to_string genv.ServerEnv.options.Options.opt_root in
  let items =
    collect_rage ~options:genv.ServerEnv.options ~reader ~env ~files:None
    |> Base.List.map ~f:(fun (title, data) -> { Lsp.Rage.title = Some (root ^ ":" ^ title); data })
  in
  let response = ResponseMessage (id, RageResult items) in
  Lwt.return (LspProt.LspFromServer (Some response), metadata)

let handle_persistent_ping ~id ~metadata ~client:_ ~profiling:_ ~env:_ =
  let start_server_status =
    match metadata.LspProt.start_server_status with
    | None -> None
    | Some status -> Some (ServerStatus.string_of_status ~terse:true status)
  in
  let response = ResponseMessage (id, PingResult { Lsp.Ping.start_server_status }) in
  Lwt.return (LspProt.LspFromServer (Some response), metadata)

let handle_persistent_log_command ~id ~metadata ~arguments:_ ~client:_ ~profiling:_ =
  (* don't need to do anything, since everything we need to log is already in `metadata` *)
  (LspProt.LspFromServer (Some (ResponseMessage (id, ExecuteCommandResult ()))), metadata)

let send_workspace_edit ~client ~id ~metadata ~on_response ~on_error label edit =
  let req_id =
    let prefix =
      match id with
      | Lsp.NumberId id -> string_of_int id
      | Lsp.StringId id -> id
    in
    Lsp.StringId (spf "%s:applyEdit" prefix)
  in
  let request =
    RequestMessage (req_id, ApplyWorkspaceEditRequest { ApplyWorkspaceEdit.label; edit })
  in
  let handler = ApplyWorkspaceEditHandler on_response in
  Persistent_connection.push_outstanding_handler client req_id (handler, on_error);
  (LspProt.LspFromServer (Some request), metadata)

let handle_persistent_add_missing_imports_command
    ~reader ~options ~id ~metadata ~textDocument ~client ~profiling ~env =
  let%lwt edits = add_missing_imports ~reader ~options ~env ~profiling ~client textDocument in
  match edits with
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)
  | Ok [] ->
    (* nothing to do, return immediately *)
    Lwt.return
      (LspProt.LspFromServer (Some (ResponseMessage (id, ExecuteCommandResult ()))), metadata)
  | Ok edits ->
    (* send a workspace/applyEdit command to the client. when it replies, we'll reply to the command *)
    let on_response _result () =
      (* respond to original executeCommand *)
      let response = ResponseMessage (id, ExecuteCommandResult ()) in
      Persistent_connection.send_response (LspProt.LspFromServer (Some response), metadata) client
    in
    let on_error _ () = (* TODO send error to client *) () in
    let label = Some "Add missing imports" in
    let uri = TextDocumentIdentifier.(textDocument.uri) in
    let edit = { WorkspaceEdit.changes = Lsp.UriMap.singleton uri edits } in
    Lwt.return (send_workspace_edit ~client ~id ~metadata ~on_response ~on_error label edit)

let handle_persistent_organize_imports_command
    ~options ~id ~metadata ~textDocument ~client ~profiling =
  match organize_imports ~options ~profiling ~client textDocument with
  | Error reason -> mk_lsp_error_response ~id:(Some id) ~reason metadata
  | Ok [] ->
    (* nothing to do, return immediately *)
    (LspProt.LspFromServer (Some (ResponseMessage (id, ExecuteCommandResult ()))), metadata)
  | Ok edits ->
    (* send a workspace/applyEdit command to the client. when it replies, we'll reply to the command *)
    let on_response _result () =
      (* respond to original executeCommand *)
      let response = ResponseMessage (id, ExecuteCommandResult ()) in
      Persistent_connection.send_response (LspProt.LspFromServer (Some response), metadata) client
    in
    let on_error _ () = (* TODO send error to client *) () in
    let label = Some "Organize imports" in
    let uri = TextDocumentIdentifier.(textDocument.uri) in
    let edit = { WorkspaceEdit.changes = Lsp.UriMap.singleton uri edits } in
    send_workspace_edit ~client ~id ~metadata ~on_response ~on_error label edit

let handle_persistent_auto_close_jsx ~options ~id ~params ~metadata ~client ~profiling ~env =
  let (result, extra_data) = auto_close_jsx ~options ~env ~profiling ~params ~client in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)
  | Ok text_opt ->
    Lwt.return
      (LspProt.LspFromServer (Some (ResponseMessage (id, AutoCloseJsxResult text_opt))), metadata)

let handle_persistent_linked_editing_range ~options ~id ~params ~metadata ~client ~profiling ~env =
  let (result, extra_data) = linked_editing_range ~options ~env ~profiling ~params ~client in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Error reason -> Lwt.return (mk_lsp_error_response ~id:(Some id) ~reason metadata)
  | Ok result ->
    Lwt.return
      ( LspProt.LspFromServer (Some (ResponseMessage (id, LinkedEditingRangeResult result))),
        metadata
      )

let handle_persistent_rename_file_imports
    ~reader ~options ~id ~params ~metadata ~client ~profiling:_ ~env =
  let text_document_identifier = { TextDocumentIdentifier.uri = params.RenameFiles.oldUri } in
  let file_input = file_input_of_text_document_identifier ~client text_document_identifier in
  let (result, extra_data) =
    let file_key = file_key_of_file_input ~options ~env file_input in
    (* This only works for haste modules right now *)
    let old_haste_name = Module_js.exported_module ~options file_key ~package_info:None in
    let new_flowpath =
      Flow_lsp_conversions.lsp_DocumentIdentifier_to_flow_path
        { TextDocumentIdentifier.uri = params.RenameFiles.newUri }
    in
    (* The type or contents of the file isn't changing, just the path *)
    let new_file_key = File_key.map (fun _ -> new_flowpath) file_key in
    let new_haste_name = Module_js.exported_module ~options new_file_key ~package_info:None in
    match (old_haste_name, new_haste_name) with
    | (Some old_haste_name, Some new_haste_name) ->
      let edits =
        RenameModule.get_rename_edits ~reader ~options ~old_haste_name ~new_haste_name file_key
      in
      (edits, None)
    | (_, _) -> (Error "Error converting file names to Haste paths", None)
  in
  let metadata = with_data ~extra_data metadata in
  match result with
  | Ok result ->
    let r = RenameFileImportsResult result in
    let response = ResponseMessage (id, r) in
    Lwt.return (LspProt.LspFromServer (Some response), metadata)
  | Error reason ->
    let (res, meta) = mk_lsp_error_response ~id:(Some id) ~reason metadata in
    Lwt.return (res, meta)

let handle_persistent_malformed_command ~id ~metadata ~client:_ ~profiling:_ =
  mk_lsp_error_response ~id:(Some id) ~reason:"Malformed command" metadata

let handle_persistent_unsupported ?id ~unhandled ~metadata ~client:_ ~profiling:_ () =
  let message = Printf.sprintf "Unhandled method %s" (Lsp_fmt.message_name_to_string unhandled) in
  let response =
    match id with
    | Some id ->
      let e = { Error.code = Error.MethodNotFound; message; data = None } in
      ResponseMessage (id, ErrorResult (e, ""))
    | None ->
      NotificationMessage
        (TelemetryNotification { LogMessage.type_ = MessageType.ErrorMessage; message })
  in
  (LspProt.LspFromServer (Some response), metadata)

let handle_result_from_client ~id ~metadata ~(result : Lsp.lsp_result) ~client ~profiling:_ =
  (match Persistent_connection.pop_outstanding_handler client id with
  | Some (handler, handle_error) ->
    (match (result, handler) with
    | (ApplyWorkspaceEditResult result, ApplyWorkspaceEditHandler handle) -> handle result ()
    | (ErrorResult (e, msg), _) -> handle_error (e, msg) ()
    | _ -> ())
  | None -> ());
  (LspProt.LspFromServer None, metadata)

(* What should we do if we get multiple requests for the same URI? Each request wants the most
 * up-to-date live errors, so if we have 10 pending requests then we would want to send the same
 * response to each. And we could do that, but it might have some weird side effects:
 *
 * 1. Logging would make this look faster than it is, since we're doing a single check to respond
 *    to N requests
 * 2. The LSP process would have to integrate N responses into its store of errors
 *
 * So instead we just respond that the first N-1 requests were canceled and send a response to the
 * Nth request. Since it's very cheap to cancel a request, this shouldn't delay the LSP process
 * getting a response *)
let handle_live_errors_request =
  (* How do we know that we're responding to the latest request for a URI? Keep track of the
   * latest metadata object *)
  let uri_to_latest_metadata_map = ref SMap.empty in
  let is_latest_metadata uri metadata =
    match SMap.find_opt uri !uri_to_latest_metadata_map with
    | Some latest_metadata -> latest_metadata = metadata
    | None -> false
  in
  fun ~options ~uri ~metadata ->
    (* Immediately store the latest metadata *)
    uri_to_latest_metadata_map := SMap.add uri metadata !uri_to_latest_metadata_map;
    fun ~client ~profiling ~env ->
      if not (is_latest_metadata uri metadata) then
        (* A more recent request for live errors has come in for this file. So let's cancel
         * this one and let the later one handle it *)
        Lwt.return
          ( LspProt.(
              LiveErrorsResponse
                (Error
                   {
                     live_errors_failure_kind = Canceled_error_response;
                     live_errors_failure_reason = "Subsumed by a later request";
                     live_errors_failure_uri = Lsp.DocumentUri.of_string uri;
                   }
                )
            ),
            metadata
          )
      else
        (* This is the most recent live errors request we've received for this file. All the
         * older ones have already been responded to or canceled *)
        let file_path = Lsp_helpers.lsp_uri_to_path (Lsp.DocumentUri.of_string uri) in
        let%lwt ret =
          let file_input = Persistent_connection.get_file client file_path in
          match file_input with
          | File_input.FileName _ ->
            (* Maybe we've received a didClose for this file? Or maybe we got a request for a file
             * that wasn't open in the first place (that would be a bug). *)
            Lwt.return
              ( LspProt.(
                  LiveErrorsResponse
                    (Error
                       {
                         live_errors_failure_kind = Errored_error_response;
                         live_errors_failure_reason =
                           spf "Cannot get live errors for %s: File not open" file_path;
                         live_errors_failure_uri = Lsp.DocumentUri.of_string uri;
                       }
                    )
                ),
                metadata
              )
          | File_input.FileContent (_, content) ->
            let%lwt (live_errors, live_warnings, metadata) =
              let file_key = file_key_of_file_input ~options ~env file_input in
              match check_that_we_care_about_this_file ~options ~env ~file_key ~content with
              | Ok () ->
                let file_key =
                  let file_options = Options.file_options options in
                  Files.filename_from_string
                    ~options:file_options
                    ~consider_libdefs:true
                    ~libs:env.ServerEnv.libs
                    file_path
                in
                let (result, did_hit_cache) =
                  let ((_, parse_errs) as intermediate_result) =
                    Type_contents.parse_contents ~options ~profiling content file_key
                  in
                  if not (Flow_error.ErrorSet.is_empty parse_errs) then
                    (Error parse_errs, None)
                  else
                    let type_parse_artifacts_cache =
                      Some (Persistent_connection.type_parse_artifacts_cache client)
                    in
                    type_parse_artifacts_with_cache
                      ~options
                      ~profiling
                      ~type_parse_artifacts_cache
                      env.master_cx
                      file_key
                      (lazy intermediate_result)
                in
                let (live_errors, live_warnings) =
                  Type_contents.printable_errors_of_file_artifacts_result
                    ~options
                    ~env
                    file_key
                    result
                in
                let metadata =
                  let json_props = add_cache_hit_data_to_json [] did_hit_cache in
                  let json = Hh_json.JSON_Object json_props in
                  with_data ~extra_data:(Some json) metadata
                in
                Lwt.return (live_errors, live_warnings, metadata)
              | Error reason ->
                Hh_logger.info "Not reporting live errors for file %S: %s" file_path reason;

                let metadata =
                  let extra_data = json_of_skipped reason in
                  with_data ~extra_data metadata
                in

                (* If the LSP requests errors for a file for which we wouldn't normally emit errors
                 * then just return empty sets *)
                Lwt.return
                  ( Flow_errors_utils.ConcreteLocPrintableErrorSet.empty,
                    Flow_errors_utils.ConcreteLocPrintableErrorSet.empty,
                    metadata
                  )
            in
            let live_errors_uri = Lsp.DocumentUri.of_string uri in
            let live_diagnostics =
              Flow_lsp_conversions.diagnostics_of_flow_errors
                ~unsaved_content:(Some (File_path.make file_path, content))
                ~vscode_detailed_diagnostics:(vscode_detailed_diagnostics ~options client)
                ~errors:live_errors
                ~warnings:live_warnings
              |> Lsp.UriMap.find_opt live_errors_uri
              |> Base.Option.value ~default:[]
            in
            Lwt.return
              ( LspProt.LiveErrorsResponse (Ok { LspProt.live_diagnostics; live_errors_uri }),
                metadata
              )
        in
        (* If we've successfully run and there isn't a more recent request for this URI,
         * then remove the entry from the map *)
        if is_latest_metadata uri metadata then
          uri_to_latest_metadata_map := SMap.remove uri !uri_to_latest_metadata_map;
        Lwt.return ret

type persistent_command_handler =
  | Handle_persistent_immediately of
      (client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      persistent_parallelizable_result
      )
      (** A command can be handled immediately if it is super duper fast and doesn't require the env.
          These commands will be handled as soon as we read them off the pipe. Almost nothing should ever
          be handled immediately *)
  | Handle_parallelizable_persistent of
      (client:Persistent_connection.single_client -> persistent_parallelizable_result workload)
      (** A command is parallelizable if it passes four conditions
          1. It is fast. Running it in parallel will block the current recheck, so it needs to be really
            fast.
          2. It doesn't use the workers. Currently we can't deal with the recheck using the workers at the
            same time as a command using the workers
          3. It doesn't return a new env. It really should be just a read-only job
          4. It doesn't mind using slightly out of date data. During a recheck, it will be reading the
            oldified data *)
  | Handle_nonparallelizable_persistent of
      (client:Persistent_connection.single_client -> persistent_nonparallelizable_result workload)
      (** A command is nonparallelizable if it can't be handled immediately or parallelized. *)

(* This command is parallelizable, but we will treat it as nonparallelizable if we've been told
 * to wait_for_recheck by the .flowconfig *)
let mk_parallelizable_persistent ~options f =
  let wait_for_recheck = Options.wait_for_recheck options in
  if wait_for_recheck then
    Handle_nonparallelizable_persistent
      (fun ~client ~profiling ~env ->
        let%lwt (msg, metadata) = f ~client ~profiling ~env in
        Lwt.return (env, msg, metadata))
  else
    Handle_parallelizable_persistent f

(* get_persistent_handler can do a tiny little bit of work, but it's main job is just returning the
 * persistent command's handler.
 *)
let get_persistent_handler ~genv ~client_id ~request:(request, metadata) :
    persistent_command_handler =
  let open LspProt in
  let options = genv.ServerEnv.options in
  let reader = State_reader.create () in
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  match request with
  | LspToServer (RequestMessage (id, _))
    when IdSet.mem id !ServerMonitorListenerState.cancellation_requests ->
    (* We don't do any work, we just immediately tell the monitor that this request was already
       * canceled *)
    Handle_persistent_immediately (handle_persistent_canceled ~id ~metadata)
  | Subscribe ->
    (* This mutates env, so it can't run in parallel *)
    Handle_nonparallelizable_persistent (handle_persistent_subscribe ~options ~metadata)
  | LspToServer (NotificationMessage (DidOpenNotification params)) ->
    let { Lsp.DidOpen.textDocument } = params in
    let { Lsp.TextDocumentItem.text; uri; languageId = _; version = _ } = textDocument in
    let fn = Lsp_helpers.lsp_uri_to_path uri in
    let files = Nel.one (fn, text) in
    let did_anything_change =
      match Persistent_connection.get_client client_id with
      | None -> false
      | Some client ->
        (* We want to create a local copy of this file immediately, so we can respond to requests
              * about this file *)
        Persistent_connection.client_did_open client ~files
    in
    if did_anything_change then (
      enqueue_did_open_files files;

      (* This mutates env, so it can't run in parallel *)
      Handle_nonparallelizable_persistent
        (handle_persistent_did_open_notification ~options ~metadata)
    ) else
      (* It's a no-op, so we can respond immediately *)
      Handle_persistent_immediately (handle_persistent_did_open_notification_no_op ~metadata)
  | LspToServer (NotificationMessage (DidChangeNotification params)) ->
    (* This just updates our copy of the file in question. We want to do this immediately *)
    Handle_persistent_immediately (handle_persistent_did_change_notification ~params ~metadata)
  | LspToServer (NotificationMessage (DidSaveNotification _params)) ->
    (* No-op can be handled immediately *)
    Handle_persistent_immediately (handle_persistent_did_save_notification ~metadata)
  | LspToServer (NotificationMessage (DidCloseNotification params)) ->
    let { Lsp.DidClose.textDocument } = params in
    let fn = Lsp_helpers.lsp_textDocumentIdentifier_to_filename textDocument in
    let filenames = Nel.one fn in
    let did_anything_change =
      match Persistent_connection.get_client client_id with
      | None -> false
      | Some client ->
        (* Close this file immediately in case another didOpen comes soon *)
        Persistent_connection.client_did_close client ~filenames
    in
    if did_anything_change then
      (* This mutates env, so it can't run in parallel *)
      Handle_nonparallelizable_persistent
        (handle_persistent_did_close_notification ~options ~metadata)
    else
      (* It's a no-op, so we can respond immediately *)
      Handle_persistent_immediately (handle_persistent_did_close_notification_no_op ~metadata)
  | LspToServer (NotificationMessage (CancelRequestNotification params)) ->
    (* The general idea here is this:
       *
       * 1. As soon as we get a cancel notification, add the ID to the canceled requests set.
       * 2. When a request comes in or runs with the canceled ID, cancel that request and immediately
       *    respond that the request has been canceled.
       * 3. When we go to run a request that has been canceled, skip it's normal handler and instead
       *    respond that the request has been canceled.
       * 4. When the nonparallelizable cancel notification workload finally runs, remove the ID from
       *    the set. We're guaranteed that the canceled request will not show up later *)
    let id = params.CancelRequest.id in
    ServerMonitorListenerState.(cancellation_requests := IdSet.add id !cancellation_requests);
    Handle_nonparallelizable_persistent (handle_persistent_cancel_notification ~params ~metadata)
  | LspToServer (NotificationMessage (DidChangeConfigurationNotification params)) ->
    Handle_persistent_immediately
      (handle_persistent_did_change_configuration_notification ~params ~metadata)
  | LspToServer (RequestMessage (id, DefinitionRequest params)) ->
    (* Grab the file contents immediately in case of any future didChanges *)
    let file_input = file_input_of_text_document_position_opt ~client_id params in
    mk_parallelizable_persistent
      ~options
      (handle_persistent_get_def ~reader ~options ~id ~params ~file_input ~metadata)
  | LspToServer (RequestMessage (id, HoverRequest params)) ->
    (* Grab the file contents immediately in case of any future didChanges *)
    let file_input = file_input_of_text_document_position_opt ~client_id params in
    mk_parallelizable_persistent
      ~options
      (handle_persistent_infer_type ~options ~reader ~id ~params ~file_input ~metadata)
  | LspToServer (RequestMessage (id, CodeActionRequest params)) ->
    mk_parallelizable_persistent
      ~options
      (handle_persistent_code_action_request ~reader ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, CompletionRequest params)) ->
    (* Grab the file contents immediately in case of any future didChanges *)
    let loc = params.Completion.loc in
    let file_input = file_input_of_text_document_position_opt ~client_id loc in
    mk_parallelizable_persistent
      ~options
      (handle_persistent_autocomplete_lsp ~reader ~options ~id ~params ~file_input ~metadata)
  | LspToServer (RequestMessage (id, SignatureHelpRequest params)) ->
    (* Grab the file contents immediately in case of any future didChanges *)
    let loc = params.SignatureHelp.loc in
    let file_input = file_input_of_text_document_position_opt ~client_id loc in
    mk_parallelizable_persistent
      ~options
      (handle_persistent_signaturehelp_lsp ~reader ~options ~id ~params ~file_input ~metadata)
  | LspToServer (RequestMessage (id, DocumentHighlightRequest params)) ->
    mk_parallelizable_persistent
      ~options
      (handle_persistent_document_highlight ~loc_of_aloc ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, FindReferencesRequest params)) ->
    Handle_nonparallelizable_persistent
      (handle_persistent_find_references ~loc_of_aloc ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, RenameRequest params)) ->
    Handle_nonparallelizable_persistent
      (handle_persistent_rename ~reader ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, TypeCoverageRequest params)) ->
    (* Grab the file contents immediately in case of any future didChanges *)
    let textDocument = params.TypeCoverage.textDocument in
    let file_input = file_input_of_text_document_identifier_opt ~client_id textDocument in
    mk_parallelizable_persistent
      ~options
      (handle_persistent_coverage ~options ~id ~params ~file_input ~metadata)
  | LspToServer (RequestMessage (id, RageRequest)) ->
    (* Whoever is waiting for the rage results probably doesn't want to wait for a recheck *)
    mk_parallelizable_persistent ~options (handle_persistent_rage ~reader ~genv ~id ~metadata)
  | LspToServer (RequestMessage (id, PingRequest)) ->
    mk_parallelizable_persistent ~options (handle_persistent_ping ~id ~metadata)
  | LspToServer (RequestMessage (id, ExecuteCommandRequest params)) ->
    let ExecuteCommand.{ command = Command.Command command; arguments } = params in
    let extra_data =
      let open Hh_json in
      let arguments_json =
        match arguments with
        | None -> JSON_Null
        | Some jsons -> JSON_Array jsons
      in
      Some (JSON_Object [("command", JSON_String command); ("arguments", arguments_json)])
    in
    let metadata = with_data ~extra_data metadata in
    (match command with
    | "log" ->
      let extra_data =
        let open Hh_json in
        match arguments with
        | Some (JSON_String "textDocument/completion" :: _) ->
          (* add extra metadata to completion logs that we don't want to have to include
             on every result. *)
          (match Persistent_connection.get_client client_id with
          | None -> None
          | Some client ->
            let props =
              [("rank_autoimports_by_usage", JSON_Bool (rank_autoimports_by_usage ~options client))]
            in
            Some (JSON_Object props))
        | _ -> None
      in
      let metadata = with_data ~extra_data metadata in
      Handle_persistent_immediately (handle_persistent_log_command ~id ~arguments ~metadata)
    | "source.addMissingImports" ->
      (match arguments with
      | Some [json] ->
        let textDocument = Lsp_fmt.parse_textDocumentIdentifier (Some json) in
        mk_parallelizable_persistent
          ~options
          (handle_persistent_add_missing_imports_command
             ~reader
             ~options
             ~id
             ~metadata
             ~textDocument
          )
      | _ -> Handle_persistent_immediately (handle_persistent_malformed_command ~id ~metadata))
    | "source.organizeImports" ->
      (match arguments with
      | Some [json] ->
        let textDocument = Lsp_fmt.parse_textDocumentIdentifier (Some json) in
        Handle_persistent_immediately
          (handle_persistent_organize_imports_command ~options ~id ~metadata ~textDocument)
      | _ -> Handle_persistent_immediately (handle_persistent_malformed_command ~id ~metadata))
    | _ -> Handle_persistent_immediately (handle_persistent_malformed_command ~id ~metadata))
  | LspToServer (RequestMessage (id, AutoCloseJsxRequest params)) ->
    mk_parallelizable_persistent
      ~options
      (handle_persistent_auto_close_jsx ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, LinkedEditingRangeRequest params)) ->
    mk_parallelizable_persistent
      ~options
      (handle_persistent_linked_editing_range ~options ~id ~params ~metadata)
  | LspToServer (RequestMessage (id, RenameFileImportsRequest params)) ->
    mk_parallelizable_persistent
      ~options
      (handle_persistent_rename_file_imports ~reader ~options ~id ~params ~metadata)
  | LspToServer (ResponseMessage (id, result)) ->
    Handle_persistent_immediately (handle_result_from_client ~id ~result ~metadata)
  | LspToServer unhandled ->
    let id =
      match unhandled with
      | RequestMessage (id, _) -> Some id
      | _ -> None
    in
    (* We can reject unsupported stuff immediately *)
    Handle_persistent_immediately (handle_persistent_unsupported ?id ~unhandled ~metadata ())
  | LiveErrorsRequest uri ->
    let uri = Lsp.DocumentUri.to_string uri in
    (* We can handle live errors even during a recheck *)
    mk_parallelizable_persistent ~options (handle_live_errors_request ~options ~uri ~metadata)

let wrap_immediate_persistent_handler
    (type a b)
    (handler :
      genv:ServerEnv.genv ->
      workload:a ->
      client:Persistent_connection.single_client ->
      profiling:Profiling_js.running ->
      unit ->
      b persistent_handling_result
      )
    ~(genv : ServerEnv.genv)
    ~(client_id : LspProt.client_id)
    ~(request : LspProt.request_with_metadata)
    ~(workload : a)
    ~(default_ret : b)
    (arg : unit) : b =
  let (request, metadata) = request in
  match Persistent_connection.get_client client_id with
  | None ->
    Hh_logger.error "Unknown persistent client %d. Maybe connection went away?" client_id;
    default_ret
  | Some client ->
    Hh_logger.info "Persistent request: %s" (LspProt.string_of_request request);
    let should_print_summary = Options.should_profile genv.options in
    let (profiling, result) =
      Profiling_js.with_profiling_sync ~label:"Command" ~should_print_summary (fun profiling ->
          match check_if_cancelled ~profiling ~client request metadata with
          | Some (response, json_data) -> (default_ret, response, json_data)
          | None ->
            (try handler ~genv ~workload ~client ~profiling arg with
            | e ->
              let response = handle_persistent_uncaught_exception request (Exception.wrap e) in
              (default_ret, response, metadata))
      )
    in
    send_persistent_response ~profiling ~client result

let handle_persistent_immediately_unsafe ~genv:_ ~workload ~client ~profiling () =
  let (response, json_data) = workload ~client ~profiling in
  ((), response, json_data)

let handle_persistent_immediately ~genv ~client_id ~request ~workload =
  wrap_immediate_persistent_handler
    handle_persistent_immediately_unsafe
    ~genv
    ~client_id
    ~request
    ~workload
    ~default_ret:()
    ()

let enqueue_persistent
    (genv : ServerEnv.genv) (client_id : LspProt.client_id) (request : LspProt.request_with_metadata)
    : unit =
  let name = request |> fst |> LspProt.string_of_request in
  match get_persistent_handler ~genv ~client_id ~request with
  | Handle_persistent_immediately workload ->
    handle_persistent_immediately ~genv ~client_id ~request ~workload
  | Handle_parallelizable_persistent workload ->
    let workload = handle_parallelizable_persistent ~genv ~client_id ~request ~name ~workload in
    ServerMonitorListenerState.push_new_parallelizable_workload ~name workload
  | Handle_nonparallelizable_persistent workload ->
    let workload = handle_nonparallelizable_persistent ~genv ~client_id ~request ~workload in
    ServerMonitorListenerState.push_new_workload ~name workload
