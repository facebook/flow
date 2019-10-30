(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type patch = (int * int * string) list

(* Location patches retain all the information needed to send edits over the LSP *)
type loc_patch = (Loc.t * string) list

let show_patch p : string =
  ListUtils.to_string
    ""
    (fun (s, e, p) -> Printf.sprintf "Start: <%d> End: <%d> Patch: <%s>\n" s e p)
    p

let with_content_of_file_input file f =
  match File_input.content_of_file_input file with
  | Ok contents -> f contents
  | Error _ ->
    let file_name = File_input.filename_of_file_input file in
    let error_msg =
      Printf.sprintf "Replacement_printer: Input file, \"%s\", couldn't be read." file_name
    in
    Utils_js.assert_false error_msg

let mk_loc_patch_ast_differ
    (diff : Flow_ast_differ.node Flow_ast_differ.change list) (ast : (Loc.t, Loc.t) Ast.program) :
    loc_patch =
  let attached_comments = Some (Flow_prettier_comments.attach_comments ast) in
  Ast_diff_printer.edits_of_changes attached_comments diff

let mk_patch_ast_differ
    (diff : Flow_ast_differ.node Flow_ast_differ.change list)
    (ast : (Loc.t, Loc.t) Ast.program)
    (content : string) : patch =
  let offset_table = Offset_utils.make ~kind:Offset_utils.Utf8 content in
  let offset loc = Offset_utils.offset offset_table loc in
  mk_loc_patch_ast_differ diff ast
  |> Core_list.map ~f:(fun (loc, text) -> Loc.(offset loc.start, offset loc._end, text))

let mk_patch_ast_differ_unsafe diff ast file =
  with_content_of_file_input file @@ mk_patch_ast_differ diff ast

let print (patch : patch) (content : string) : string =
  let patch_sorted =
    List.sort (fun (start_one, _, _) (start_two, _, _) -> compare start_one start_two) patch
  in
  let file_end = String.length content in
  (* Apply the spans to the original text *)
  let (result_string_minus_end, last_span) =
    List.fold_left
      (fun (file, last) (start, _end, text) ->
        let file_curr =
          Printf.sprintf "%s%s%s" file (String.sub content last (start - last)) text
        in
        (file_curr, _end))
      ("", 0)
      patch_sorted
  in
  let last_span_to_end_size = file_end - last_span in
  let result_string =
    if last_span_to_end_size = 0 then
      Printf.sprintf "%s" result_string_minus_end
    else
      Printf.sprintf
        "%s%s"
        result_string_minus_end
        (String.sub content last_span last_span_to_end_size)
  in
  result_string

let print_unsafe patch file = with_content_of_file_input file @@ print patch
