(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let json_data_of_result str acc = ("result", Hh_json.JSON_String str) :: acc

let json_data_of_error str acc = ("error", Hh_json.JSON_String str) :: acc

let json_data_of_loc loc acc = ("loc", Reason.json_of_loc ~offset_table:None loc) :: acc

let json_data_of_type key str acc = (key, Hh_json.JSON_String str) :: acc

let json_data_of_type_opt key str_opt acc =
  ( key,
    Base.Option.value_map str_opt ~default:Hh_json.JSON_Null ~f:(fun str -> Hh_json.JSON_String str)
  )
  :: acc

let json_data_of_locs_opt key locs acc =
  ( key,
    match locs with
    | [] -> Hh_json.JSON_Null
    | _ -> Hh_json.JSON_Array (List.map (Reason.json_of_loc ~offset_table:None) locs)
  )
  :: acc

let type_at_pos
    ~cx
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~max_depth
    ~verbose_normalizer
    ~no_typed_ast_for_imports
    ~include_refs
    ~include_refinement_info
    file
    line
    col =
  let loc = Loc.cursor (Some file) line col in
  let (refining_locs, refinement_invalidated) =
    match include_refinement_info with
    | None -> ([], [])
    | Some loc_of_aloc ->
      let open Loc_collections in
      let contains_cursor aloc _ = Loc.contains (loc_of_aloc aloc) loc in
      let refining_locs =
        ALocMap.filter contains_cursor (Context.refined_locations cx)
        |> ALocMap.values
        |> Base.List.fold ~init:ALocSet.empty ~f:ALocSet.union
        |> ALocSet.elements
        |> List.map loc_of_aloc
      in
      let refinement_invalidated =
        ALocMap.filter contains_cursor (Context.aggressively_invalidated_locations cx)
        |> ALocMap.values
        |> Base.List.fold ~init:ALocMap.empty ~f:ALocMap.union
        |> ALocMap.elements
        |> List.map (fun (loc, reason) -> (loc_of_aloc loc, reason))
      in
      (refining_locs, refinement_invalidated)
  in
  let (json_data, loc, ty) =
    let open Query_types in
    let result =
      type_at_pos_type
        ~cx
        ~file_sig
        ~omit_targ_defaults
        ~verbose_normalizer
        ~max_depth
        ~typed_ast
        ~no_typed_ast_for_imports
        ~include_refs
        loc
    in
    match result with
    | FailureNoMatch -> (json_data_of_result "FAILURE_NO_MATCH" [], Loc.none, None)
    | FailureUnparseable (loc, gt, msg) ->
      let json_data =
        []
        |> json_data_of_result "FAILURE_UNPARSEABLE"
        |> json_data_of_error msg
        |> json_data_of_loc loc
        |> json_data_of_type "type" (Type.string_of_ctor gt)
      in
      (json_data, loc, None)
    | Success (loc, ({ Ty.unevaluated; evaluated; refs = _ } as tys)) ->
      let json_data =
        []
        |> json_data_of_result "SUCCESS"
        |> json_data_of_loc loc
        |> json_data_of_type
             "type"
             ((* TODO use Ty_debug.json_of_t after making it faster using count_calls *)
              let exact_by_default = Context.exact_by_default cx in
              Ty_printer.string_of_elt
                ~exact_by_default
                ~ts_syntax:(Context.ts_syntax cx)
                unevaluated
             )
        |> json_data_of_type_opt
             "type_evaluated"
             (let exact_by_default = Context.exact_by_default cx in
              Base.Option.map
                evaluated
                ~f:(Ty_printer.string_of_elt ~exact_by_default ~ts_syntax:(Context.ts_syntax cx))
             )
        |> json_data_of_locs_opt "refining_locs" refining_locs
        |> json_data_of_locs_opt "refinement_invalidated" (List.map fst refinement_invalidated)
      in
      (json_data, loc, Some tys)
  in
  ((loc, ty, refining_locs, refinement_invalidated), json_data)

let batched_type_at_pos_special_comment_regex = Str.regexp "^ *\\^\\?$"

let batched_type_at_pos_from_special_comments
    ~cx
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~max_depth
    ~verbose_normalizer
    ~no_typed_ast_for_imports
    ~loc_of_aloc
    file =
  let (_, { Flow_ast.Program.all_comments; _ }) = typed_ast in
  let handle_comment (comment_loc, { Flow_ast.Comment.kind; text; on_newline = _ }) =
    if
      kind = Flow_ast.Comment.Line
      && Str.string_match batched_type_at_pos_special_comment_regex text 0
    then
      let { Loc.start = { Loc.line; column }; _ } = loc_of_aloc comment_loc in
      let line = line - 1 in
      (* To compute the column, +2 for ^?, -2 for starting // *)
      let column = column + String.length text in
      let ((ty_loc, tys, refining_locs, invalidation_info), json_data) =
        type_at_pos
          ~cx
          ~file_sig
          ~typed_ast
          ~omit_targ_defaults
          ~max_depth
          ~verbose_normalizer
          ~no_typed_ast_for_imports
          ~include_refs:(Some loc_of_aloc)
          ~include_refinement_info:(Some loc_of_aloc)
          file
          line
          column
      in
      let cursor_loc = Loc.cursor (Some file) line column in
      Some
        ((cursor_loc, ty_loc, tys, refining_locs, invalidation_info), Hh_json.JSON_Object json_data)
    else
      None
  in
  let (friendly_results, json_data_list) =
    Base.List.filter_map all_comments ~f:handle_comment |> Base.List.unzip
  in
  (friendly_results, Hh_json.JSON_Array json_data_list)

let dump_types ~evaluate_type_destructors ~for_tool cx file_sig typed_ast =
  (* Print type using Flow type syntax *)
  match for_tool with
  | Some depth -> Query_types.dump_types_for_tool cx typed_ast depth
  | None ->
    let exact_by_default = Context.exact_by_default cx in
    let printer =
      Ty_printer.string_of_elt_single_line ~exact_by_default ~ts_syntax:(Context.ts_syntax cx)
    in
    Query_types.dump_types ~printer ~evaluate_type_destructors cx file_sig typed_ast

let coverage ~cx ~typed_ast ~force file content =
  let should_check =
    if force then
      true
    else
      (* We can't just use the docblock that parse_contents returns because parse_contents modifies
       * it and we want the original docblock. Fortunately this is a pure function, and pretty fast,
       * so recomputing it isn't a problem. *)
      let (_, docblock) =
        Docblock_parser.(
          parse_docblock
            ~max_tokens:docblock_max_tokens
            ~file_options:(Context.file_options cx)
            file
            content
        )
      in
      Docblock.is_flow docblock
  in
  Coverage.covered_types cx ~should_check typed_ast
