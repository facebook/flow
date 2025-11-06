(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Types_js_types
open Utils_js

(* Find the first occurrence of the identifier by name and its type from typed_ast *)
let find_identifier_and_type target_name typed_ast =
  let exception Found of ALoc.t * Type.t in
  let visitor =
    object
      inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

      method on_loc_annot x = x

      method on_type_annot x = x

      method! t_identifier id =
        let ((aloc, t), { Flow_ast.Identifier.name = id_name; _ }) = id in
        if id_name = target_name then raise (Found (aloc, t));
        id
    end
  in
  match visitor#program typed_ast with
  | exception Found (aloc, t) -> Some (aloc, t)
  | _ -> None

let type_of_name_from_artifacts
    ~doc_at_loc ~reader ~profiling ~check_result ~expand_component_props ~actual_name (aloc, type_)
    =
  let (Parse_artifacts { file_sig; ast; _ }, Typecheck_artifacts { cx; typed_ast; _ }) =
    check_result
  in
  let file_key = Context.file cx in
  let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
  let documentation =
    let { Loc.start = { Loc.line; column; _ }; _ } = loc in
    doc_at_loc ~reader ~profiling ~check_result ~ast file_key line column
  in
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors =
        Ty_normalizer_env.EvaluateCustom
          (function
          | Type.ReactCheckComponentConfig _ -> expand_component_props
          | _ -> false);
      optimize_types = true;
      omit_targ_defaults_option = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 10;
      toplevel_is_type_identifier_reference = false;
    }
  in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast) in
  match Ty_normalizer_flow.from_type genv type_ with
  | Ok ty ->
    let refs = Ty.symbols_of_elt ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader) ty in
    let r = { Ty.unevaluated = ty; evaluated = None; refs = Some refs } in
    let (type_, refs) =
      Ty_printer.string_of_type_at_pos_result ~exact_by_default:true ~ts_syntax:false r
    in
    let response =
      { ServerProt.Response.InferTypeOfName.loc; type_; refs; actual_name; documentation }
    in
    Ok response
  | Error e -> Error ("normalizer error " ^ Ty_normalizer.error_to_string e)

let get_server_exports env =
  match env.ServerEnv.exports with
  | Some exports -> Ok exports
  | None -> Error "No server exports found (make sure you're not using '--no-autoimport')"

let type_of_name_from_index
    ~doc_at_loc
    ~options
    ~reader
    ~env
    ~profiling
    ~expand_component_props
    ~exact_match_only
    target_name
    file_key =
  let open Base.Result.Let_syntax in
  let%bind exports = get_server_exports env in
  let { Export_search_types.results; is_incomplete = _ } =
    Export_search.search_both_values_and_types
      ~options:
        {
          Fuzzy_path.default_options with
          Fuzzy_path.max_results = 100;
          num_threads = Base.Int.max 1 (Sys_utils.nbr_procs - 2);
          weighted = true;
        }
      target_name
      exports
  in
  let%bind (actual_name, source, kind) =
    match results with
    | {
        Export_search_types.search_result =
          {
            (* NOTE the target name might change due to fuzzy finding *)
            Export_search_types.name = actual_name;
            source = Export_index.(File_key _ | Global) as source;
            kind;
          };
        _;
      }
      :: _ ->
      if exact_match_only && actual_name <> target_name then
        Error (spf "Could not find exact match for `%s`" target_name)
      else
        Ok (actual_name, source, kind)
    | { Export_search_types.search_result = { Export_search_types.source; _ }; _ } :: _ ->
      Error (spf "Result found in unsupported source '%s'" (Export_index.show_source source))
    | [] -> Error "No results found"
  in
  (* Create contents of the form
   *
   *   import NAME from FILE;
   *   import {NAME} from FILE;
   *   import type NAME from FILE;
   *   import type {NAME} from FILE;
   *
   * dependending on the kind of export that was selected. Note that this import
   * does not appear in the current file. This means that the necessary dependencies
   * might not have even been merged.
   *)
  let%bind contents_body =
    let open Export_index in
    match (source, kind) with
    | (File_key s, _) ->
      let thing =
        match kind with
        | DefaultType -> spf "type %s" actual_name
        | Default -> actual_name
        | Named -> spf "{%s}" actual_name
        | NamedType -> spf "type {%s}" actual_name
        | Namespace -> spf "%s" actual_name
      in
      Ok (spf "import %s from '%s';" thing (File_key.to_string s))
    | (Global, (DefaultType | NamedType)) -> Ok (spf "declare var _: %s;" actual_name)
    | (Global, (Default | Named | Namespace)) -> Ok (spf "%s;" actual_name)
    | (Builtin _, _) -> Error "(handled earlier)"
  in
  let contents = "/* @flow */ " ^ contents_body in
  let parse_result = Type_contents.parse_contents ~options ~profiling contents file_key in
  match
    Type_contents.type_parse_artifacts
      ~options
      ~profiling
      env.ServerEnv.master_cx
      file_key
      parse_result
  with
  | Error _errors -> Error "Parse or typing errors on index"
  | Ok check_result ->
    let (Parse_artifacts _, Typecheck_artifacts { typed_ast; _ }) = check_result in
    begin
      match find_identifier_and_type actual_name typed_ast with
      | Some (loc, type_) ->
        let result =
          type_of_name_from_artifacts
            ~doc_at_loc
            ~reader
            ~profiling
            ~check_result
            ~expand_component_props
            ~actual_name
            (loc, type_)
        in
        (* Use def-loc of type in the response, but pass `loc` above to
         * `type_of_name_from_artifacts` so that we properly compute documentation *)
        let def_loc = Parsing_heaps.Reader.loc_of_aloc ~reader (TypeUtil.def_loc_of_t type_) in
        Base.Result.map result ~f:(fun r ->
            { r with ServerProt.Response.InferTypeOfName.loc = def_loc }
        )
      | None -> Error "Unexpected: no type found for identifier in phony program"
    end

let type_of_name
    ~(options : Options.t)
    ~(reader : Parsing_heaps.Reader.reader)
    ~(env : ServerEnv.env)
    ~(profiling : Profiling_js.running)
    ~doc_at_loc
    file_key
    input
    check_result : ServerProt.Response.infer_type_of_name_response =
  let {
    ServerProt.Type_of_name_options.input = _file_input;
    name = target_name;
    verbose = _;
    expand_component_props;
    exact_match_only;
    wait_for_recheck = _;
    strip_root = _;
  } =
    input
  in
  let (Parse_artifacts _, Typecheck_artifacts { typed_ast; _ }) = check_result in
  match find_identifier_and_type target_name typed_ast with
  | Some r ->
    (* Identifier exists in current file. Use type from TAST. *)
    type_of_name_from_artifacts
      ~doc_at_loc
      ~reader
      ~profiling
      ~check_result
      ~expand_component_props
      ~actual_name:target_name
      r
  | None ->
    (* Identifier does not exist in current file. Look up the index. *)
    type_of_name_from_index
      ~doc_at_loc
      ~options
      ~reader
      ~env
      ~profiling
      ~expand_component_props
      ~exact_match_only
      target_name
      file_key
