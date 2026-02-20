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

(* Extract per-prop documentation from flattened component props *)
let extract_prop_docs ~reader ~ast (ty : Ty.elt) :
    ServerProt.Response.InferTypeOfName.prop_doc list option =
  let get_ast_from_shared_mem file_key = Parsing_heaps.Reader.get_ast ~reader file_key in
  let get_props = function
    | Ty.Type (Ty.Component { regular_props = Ty.FlattenedComponentProps { props; _ }; _ }) ->
      Some props
    | Ty.Decl (Ty.NominalComponentDecl { props = Ty.FlattenedComponentProps { props; _ }; _ }) ->
      Some props
    | _ -> None
  in
  match get_props ty with
  | None -> None
  | Some props ->
    (* Collect all def_locs, then batch-lookup JSDoc in a single AST traversal per file *)
    let prop_locs =
      Base.List.filter_map props ~f:(fun (Ty.FlattenedComponentProp { def_locs; _ }) ->
          match def_locs with
          | aloc :: _ -> Some (Parsing_heaps.Reader.loc_of_aloc ~reader aloc)
          | [] -> None
      )
    in
    let jsdoc_map =
      Find_documentation.jsdocs_of_getdef_locs ~ast ~get_ast_from_shared_mem prop_locs
    in
    let docs =
      Base.List.filter_map props ~f:(fun (Ty.FlattenedComponentProp { name; def_locs; _ }) ->
          let prop_name = Reason.display_string_of_name name in
          let description =
            match def_locs with
            | aloc :: _ ->
              let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
              Loc_sig.LocS.LMap.find_opt loc jsdoc_map
              |> Base.Option.bind ~f:Find_documentation.documentation_of_jsdoc
            | [] -> None
          in
          match description with
          | Some description -> Some { ServerProt.Response.InferTypeOfName.prop_name; description }
          | None -> None
      )
    in
    if Base.List.is_empty docs then
      None
    else
      Some docs

(* Member lookup: find a member's type and def_locs from a Ty.elt *)
let lookup_member_in_component_props member_name props =
  let target = Reason.OrdinaryName member_name in
  Base.List.find_map props ~f:(fun (Ty.FlattenedComponentProp { name; t; def_locs; _ }) ->
      if name = target then
        Some (t, def_locs)
      else
        None
  )

let lookup_member_in_obj_props member_name obj_props =
  let target = Reason.OrdinaryName member_name in
  Base.List.find_map obj_props ~f:(function
      | Ty.NamedProp { name; prop; def_locs; _ } when name = target ->
        let t =
          match prop with
          | Ty.Field { t; _ } -> t
          | Ty.Method ft -> Ty.Fun ft
          | Ty.Get t
          | Ty.Set t ->
            t
        in
        Some (t, def_locs)
      | _ -> None
      )

let rec lookup_member_in_elt member_name ty_elt =
  match ty_elt with
  (* Component value types *)
  | Ty.Type (Ty.Component { regular_props = Ty.FlattenedComponentProps { props; _ }; _ }) ->
    lookup_member_in_component_props member_name props
  (* Component declaration types *)
  | Ty.Decl (Ty.NominalComponentDecl { props = Ty.FlattenedComponentProps { props; _ }; _ }) ->
    lookup_member_in_component_props member_name props
  (* Object types *)
  | Ty.Type (Ty.Obj { Ty.obj_props; _ }) -> lookup_member_in_obj_props member_name obj_props
  (* Inline interfaces *)
  | Ty.Type (Ty.InlineInterface { Ty.if_props; _ }) ->
    lookup_member_in_obj_props member_name if_props
  (* Type alias with body — unwrap and recurse *)
  | Ty.Decl (Ty.TypeAliasDecl { type_ = Some body; _ }) ->
    lookup_member_in_elt member_name (Ty.Type body)
  | _ -> None

(* Generate a type summary string from a Ty.t for ref expansion *)
let rec summarize_ty t =
  let type_str = Ty_printer.string_of_t_single_line ~exact_by_default:true ~ts_syntax:false t in
  match t with
  | Ty.Obj { Ty.obj_props; _ } ->
    let n = Base.List.length obj_props in
    if n > 5 then
      Some (spf "(%d fields)" n)
    else
      Some (spf "= %s" type_str)
  | Ty.Union _ ->
    if String.length type_str > 60 then
      (* Find a good truncation point near a '|' boundary *)
      let truncation_point =
        try
          let idx = ref 57 in
          while !idx > 0 && type_str.[!idx] <> '|' do
            decr idx
          done;
          if !idx > 0 then
            !idx - 1
          else
            57
        with
        | _ -> 57
      in
      Some (spf "= %s | ..." (String.sub type_str 0 truncation_point))
    else
      Some (spf "= %s" type_str)
  | Ty.Fun _ ->
    if String.length type_str > 80 then
      None
    else
      Some (spf "= %s" type_str)
  | Ty.Generic (_, Ty.ClassKind, _) -> Some "(class)"
  | Ty.Generic (_, Ty.InterfaceKind, _) -> Some "(interface)"
  | Ty.Generic (_, Ty.EnumKind, _) -> Some "(enum)"
  | Ty.Generic (_, Ty.ComponentKind, _) -> Some "(component)"
  | _ ->
    if String.length type_str > 60 then
      None
    else
      Some (spf "= %s" type_str)

(* Generate a type summary from a Ty.elt *)
and summarize_ty_elt ty_elt =
  match ty_elt with
  | Ty.Decl (Ty.ClassDecl _) -> Some "(class)"
  | Ty.Decl (Ty.InterfaceDecl _) -> Some "(interface)"
  | Ty.Decl (Ty.NominalComponentDecl _) -> Some "(component)"
  | Ty.Decl (Ty.EnumDecl _) -> Some "(enum)"
  | Ty.Decl (Ty.TypeAliasDecl { tparams = Some _; _ }) -> Some "(generic type)"
  | Ty.Decl (Ty.TypeAliasDecl { type_ = Some body; tparams = None; _ }) -> summarize_ty body
  | Ty.Type t -> summarize_ty t
  | _ -> None

(* Compute a type summary for a ref's body Type.t *)
let compute_ref_summary ~genv body_type =
  (* Strip the RTypeAlias reason to force the normalizer to expand the body
     instead of creating another Generic node *)
  let stripped =
    TypeUtil.mod_reason_of_t (Reason.replace_desc_reason (Reason.RCustom "ref_expansion")) body_type
  in
  (* Use compact options: disable ref body collection to avoid recursion,
     use shallow depth *)
  let compact_genv =
    {
      genv with
      Ty_normalizer_env.ref_type_bodies = None;
      options = { genv.Ty_normalizer_env.options with Ty_normalizer_env.max_depth = Some 3 };
    }
  in
  match Ty_normalizer_flow.from_type compact_genv stripped with
  | Error _ -> None
  | Ok ty_elt -> summarize_ty_elt ty_elt

(* Augment refs with type summaries from collected ref type bodies *)
let augment_refs_with_summaries ~genv ~ref_type_bodies_tbl refs =
  Base.Option.map refs ~f:(fun refs ->
      Base.List.map refs ~f:(fun (name, loc) ->
          let summary =
            match Hashtbl.find_opt ref_type_bodies_tbl name with
            | Some body_type -> compute_ref_summary ~genv body_type
            | None -> None
          in
          (name, loc, summary)
      )
  )

let mk_normalizer_genv ~expand_component_props ~check_result =
  let (Parse_artifacts { file_sig; _ }, Typecheck_artifacts { cx; typed_ast; _ }) = check_result in
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      expand_enum_members = true;
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
  let ref_type_bodies_tbl = Hashtbl.create 16 in
  let base_genv =
    Ty_normalizer_flow.mk_genv ~options ~cx ~file_sig ~typed_ast_opt:(Some typed_ast)
  in
  let genv = { base_genv with Ty_normalizer_env.ref_type_bodies = Some ref_type_bodies_tbl } in
  (genv, ref_type_bodies_tbl)

let format_ty_elt_response
    ~reader ~genv ~ref_type_bodies_tbl ~loc ~documentation ~ast ~actual_name ~source ty_elt =
  let refs = Ty.symbols_of_elt ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader) ty_elt in
  let r = { Ty.unevaluated = ty_elt; evaluated = None; refs = Some refs } in
  let (type_str, refs) =
    Ty_printer.string_of_type_at_pos_result ~exact_by_default:true ~ts_syntax:false r
  in
  let refs = augment_refs_with_summaries ~genv ~ref_type_bodies_tbl refs in
  let prop_docs = extract_prop_docs ~reader ~ast ty_elt in
  ServerProt.Response.InferTypeOfName.
    { loc; type_ = type_str; refs; actual_name; documentation; prop_docs; source }

let type_of_name_from_artifacts
    ~doc_at_loc
    ~reader
    ~profiling
    ~check_result
    ~expand_component_props
    ~actual_name
    ~source
    (aloc, type_) =
  let (Parse_artifacts { ast; _ }, Typecheck_artifacts { cx; _ }) = check_result in
  let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
  let documentation =
    let { Loc.start = { Loc.line; column; _ }; _ } = loc in
    doc_at_loc ~reader ~profiling ~check_result ~ast (Context.file cx) line column
  in
  let (genv, ref_type_bodies_tbl) = mk_normalizer_genv ~expand_component_props ~check_result in
  match Ty_normalizer_flow.from_type genv type_ with
  | Ok ty_elt ->
    Ok
      (format_ty_elt_response
         ~reader
         ~genv
         ~ref_type_bodies_tbl
         ~loc
         ~documentation
         ~ast
         ~actual_name
         ~source
         ty_elt
      )
  | Error e -> Error ("normalizer error " ^ Ty_normalizer.error_to_string e)

let member_doc_from_def_locs ~reader ~ast def_locs =
  let get_ast_from_shared_mem file_key = Parsing_heaps.Reader.get_ast ~reader file_key in
  match def_locs with
  | aloc :: _ ->
    let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
    Find_documentation.jsdoc_of_getdef_loc ~ast ~get_ast_from_shared_mem loc
    |> Base.Option.bind ~f:Find_documentation.documentation_of_jsdoc
  | [] -> None

let type_of_name_member
    ~reader ~profiling:_ ~check_result ~doc_at_loc:_ ~full_name ~member_name ~source (_, type_) =
  let (Parse_artifacts { file_sig; ast; _ }, Typecheck_artifacts { cx; typed_ast; _ }) =
    check_result
  in
  let (genv, ref_type_bodies_tbl) = mk_normalizer_genv ~expand_component_props:true ~check_result in
  match Ty_normalizer_flow.from_type genv type_ with
  | Error e -> Error ("normalizer error " ^ Ty_normalizer.error_to_string e)
  | Ok ty_elt ->
    let format_member_ty member_ty def_locs =
      let (loc, documentation) =
        match def_locs with
        | aloc :: _ ->
          let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
          let documentation = member_doc_from_def_locs ~reader ~ast def_locs in
          (loc, documentation)
        | [] -> (Loc.none, None)
      in
      Ok
        (format_ty_elt_response
           ~reader
           ~genv
           ~ref_type_bodies_tbl
           ~loc
           ~documentation
           ~ast
           ~actual_name:full_name
           ~source
           (Ty.Type member_ty)
        )
    in
    (* First try Ty-layer lookup (components, objects, type aliases) *)
    (match lookup_member_in_elt member_name ty_elt with
    | Some (member_ty, def_locs) -> format_member_ty member_ty def_locs
    | None ->
      (* Fall back to Ty_members.extract for classes/interfaces *)
      (match
         Ty_members.extract
           ~force_instance:true
           ~allowed_prop_names:[Reason.OrdinaryName member_name]
           ~cx
           ~typed_ast_opt:(Some typed_ast)
           ~file_sig
           type_
       with
      | Ok { Ty_members.members; _ } ->
        (match NameUtils.Map.find_opt (Reason.OrdinaryName member_name) members with
        | Some Ty_members.{ ty = member_ty; def_locs; _ } -> format_member_ty member_ty def_locs
        | None -> Error (Printf.sprintf "member '%s' not found on type" member_name))
      | Error _ -> Error (Printf.sprintf "member '%s' not found on type" member_name)))

let get_server_exports env =
  match env.ServerEnv.exports with
  | Some exports -> Ok exports
  | None -> Error "No server exports found (make sure you're not using '--no-autoimport')"

let find_first_match ~exact_match_only target_name results =
  Base.List.find results ~f:(function
      | {
          Export_search_types.search_result =
            {
              (* NOTE the target name might change due to fuzzy finding *)
              Export_search_types.name = actual_name;
              source = Export_index.(File_key _ | Global);
              kind = _;
            };
          _;
        } ->
        (not exact_match_only) || actual_name = target_name
      | _ -> false
      )

let resolve_name_from_index ~options ~env ~profiling ~exact_match_only target_name file_key =
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
    match find_first_match ~exact_match_only target_name results with
    | Some
        {
          Export_search_types.search_result =
            { Export_search_types.name = actual_name; kind; source };
          _;
        } ->
      Ok (actual_name, source, kind)
    | None -> Error (Printf.sprintf "'%s' not found" target_name)
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
    | (Builtin _, _) -> Error "builtin lookup not supported"
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
    (match find_identifier_and_type actual_name typed_ast with
    | Some (aloc, type_) -> Ok (actual_name, source, check_result, aloc, type_)
    | None -> Error "Unexpected: no type found for identifier in phony program")

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
  let%bind (actual_name, source, check_result, aloc, type_) =
    resolve_name_from_index ~options ~env ~profiling ~exact_match_only target_name file_key
  in
  let result =
    type_of_name_from_artifacts
      ~doc_at_loc
      ~reader
      ~profiling
      ~check_result
      ~expand_component_props
      ~actual_name
      ~source
      (aloc, type_)
  in
  (* Use def-loc of type in the response, but pass `loc` above to
   * `type_of_name_from_artifacts` so that we properly compute documentation *)
  let def_loc = Parsing_heaps.Reader.loc_of_aloc ~reader (TypeUtil.def_loc_of_t type_) in
  Base.Result.map result ~f:(fun r -> { r with ServerProt.Response.InferTypeOfName.loc = def_loc })

let type_of_name_single
    ~(options : Options.t)
    ~(reader : Parsing_heaps.Reader.reader)
    ~(env : ServerEnv.env)
    ~(profiling : Profiling_js.running)
    ~doc_at_loc
    ~expand_component_props
    ~exact_match_only
    ~target_name
    file_key
    check_result : ServerProt.Response.infer_type_of_name_response =
  match String.split_on_char '.' target_name with
  | [] -> Error "empty name"
  | [_] ->
    (* No dot — existing logic *)
    let (Parse_artifacts _, Typecheck_artifacts { typed_ast; _ }) = check_result in
    (match find_identifier_and_type target_name typed_ast with
    | Some r ->
      (* Identifier exists in current file. Use type from TAST. *)
      type_of_name_from_artifacts
        ~doc_at_loc
        ~reader
        ~profiling
        ~check_result
        ~expand_component_props
        ~actual_name:target_name
        ~source:(Export_index.File_key file_key)
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
        file_key)
  | base_name :: member_parts ->
    let member_name = String.concat "." member_parts in
    let full_name = target_name in
    let resolve_member_from_check_result base_check_result base_source (aloc, type_) =
      type_of_name_member
        ~reader
        ~profiling
        ~check_result:base_check_result
        ~doc_at_loc
        ~full_name
        ~member_name
        ~source:base_source
        (aloc, type_)
    in
    (* Try to find the base name in the current file first *)
    let (Parse_artifacts _, Typecheck_artifacts { typed_ast; _ }) = check_result in
    (match find_identifier_and_type base_name typed_ast with
    | Some (aloc, type_) ->
      resolve_member_from_check_result check_result (Export_index.File_key file_key) (aloc, type_)
    | None ->
      (* Base name not in current file — look it up via the index, then do member lookup *)
      let open Base.Result.Let_syntax in
      let%bind (actual_base_name, source, index_check_result, aloc, type_) =
        resolve_name_from_index ~options ~env ~profiling ~exact_match_only base_name file_key
      in
      let actual_full_name = actual_base_name ^ "." ^ member_name in
      type_of_name_member
        ~reader
        ~profiling
        ~check_result:index_check_result
        ~doc_at_loc
        ~full_name:actual_full_name
        ~member_name
        ~source
        (aloc, type_))

let type_of_name
    ~(options : Options.t)
    ~(reader : Parsing_heaps.Reader.reader)
    ~(env : ServerEnv.env)
    ~(profiling : Profiling_js.running)
    ~doc_at_loc
    file_key
    input
    check_result : ServerProt.Response.infer_type_of_name_response list =
  let {
    ServerProt.Type_of_name_options.input = _file_input;
    names;
    verbose = _;
    expand_component_props;
    exact_match_only;
    wait_for_recheck = _;
    strip_root = _;
  } =
    input
  in
  Base.List.map names ~f:(fun target_name ->
      type_of_name_single
        ~options
        ~reader
        ~env
        ~profiling
        ~doc_at_loc
        ~expand_component_props
        ~exact_match_only
        ~target_name
        file_key
        check_result
  )
