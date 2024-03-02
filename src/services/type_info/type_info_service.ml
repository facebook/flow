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

let type_at_pos
    ~cx
    ~file_sig
    ~typed_ast
    ~omit_targ_defaults
    ~max_depth
    ~verbose_normalizer
    ~no_typed_ast_for_imports
    file
    line
    col =
  let loc = Loc.cursor (Some file) line col in
  let (json_data, loc, ty) =
    let open Query_types in
    let file = Context.file cx in
    let result =
      type_at_pos_type
        ~cx
        ~file
        ~file_sig
        ~omit_targ_defaults
        ~verbose_normalizer
        ~max_depth
        ~typed_ast
        ~no_typed_ast_for_imports
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
    | Success (loc, ({ Ty.unevaluated; evaluated } as tys)) ->
      let json_data =
        []
        |> json_data_of_result "SUCCESS"
        |> json_data_of_loc loc
        |> json_data_of_type
             "type"
             ((* TODO use Ty_debug.json_of_t after making it faster using count_calls *)
              let exact_by_default = Context.exact_by_default cx in
              Ty_printer.string_of_elt ~exact_by_default unevaluated
             )
        |> json_data_of_type_opt
             "type_evaluated"
             (let exact_by_default = Context.exact_by_default cx in
              Base.Option.map evaluated ~f:(Ty_printer.string_of_elt ~exact_by_default)
             )
      in
      (json_data, loc, Some tys)
  in
  ((loc, ty), json_data)

let dump_types ~evaluate_type_destructors cx file_sig typed_ast =
  (* Print type using Flow type syntax *)
  let exact_by_default = Context.exact_by_default cx in
  let printer = Ty_printer.string_of_elt_single_line ~exact_by_default in
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
            ~file_options:(Context.metadata cx).Context.file_options
            file
            content
        )
      in
      Docblock.is_flow docblock
  in
  Coverage.covered_types cx ~should_check typed_ast
