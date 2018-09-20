(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module IO = Lwt_io
module S = Lwt_stream
module L = Utils_js.LocMap
module D = Mapper_differ
module J = Js_layout_generator
module F = Ast.Function

type patch = (int * int * string) list

let file_info file_path =
  let%lwt file = IO.open_file ~mode:IO.input file_path in
  let lines_stream = IO.read_lines file in
  let%lwt lines, line_counts, _ =
    S.fold
      (fun s (lines, acc, prev_offset) ->
        let size = String.length s in
        let new_offset = prev_offset + size in
        (s :: lines, (size, prev_offset, new_offset) :: acc, new_offset + 1) )
      lines_stream
      ([], [(0, 0, 0)], 0)
  in
  let%lwt _ = Lwt_io.close file in
  Lwt.return @@ (lines, line_counts)

let mk_patch (diff : Mapper_differ.t) (ast : (Loc.t, Loc.t) Ast.program)
    (file_path : string) : patch Lwt.t =
  let%lwt _, line_counts = file_info file_path in
  let line_counts_arr = Array.of_list (List.rev line_counts) in
  let offset {Loc.line; column; _} =
    let _, line_start, _ = line_counts_arr.(line) in
    line_start + column
  in
  let attached_comments = Flow_prettier_comments.attach_comments ast in
  J.with_attached_comments := Some attached_comments ;
  let spans =
    L.fold
      (fun loc value acc ->
        let {Loc.start; _end; _} = loc in
        let node_string =
          ( match value with
          | D.Statement (_, node) -> J.statement node
          | D.Expression (_, node) -> J.expression node
          | D.Type (_, node) -> J.type_ node
          | D.Return (F.Available annot) -> J.type_annotation annot
          | D.Return (F.Missing _) -> Layout.Empty
          | D.ClassElement (_, node) -> (
            match node with
            | Ast.Class.Body.Method meth -> J.class_method meth
            | Ast.Class.Body.Property prop -> J.class_property prop
            | Ast.Class.Body.PrivateField field -> J.class_private_field field
            ) )
          |> Pretty_printer.print ~skip_endline:true ~source_maps:None
          |> Source.contents
        in
        let node_string =
          match value with
          | D.Return (F.Available _) -> node_string ^ " "
          | _ -> node_string
        in
        (offset start, offset _end, node_string) :: acc )
      diff []
  in
  J.with_attached_comments := None ;
  Lwt.return
  @@ List.sort
       (fun (start_one, _, _) (start_two, _, _) -> compare start_one start_two)
       spans

let print (patch : patch) (file_path : string) : string Lwt.t =
  let%lwt lines, line_counts = file_info file_path in
  let _, _, file_end = List.hd line_counts in
  let file_string = String.concat "\n" (List.rev lines) in
  (* Apply the spans to the original text *)
  let result_string_minus_end, last_span =
    List.fold_left
      (fun (file, last) (start, _end, text) ->
        let file_curr =
          Printf.sprintf "%s%s%s" file
            (String.sub file_string last (start - last))
            text
        in
        (file_curr, _end) )
      ("", 0) patch
  in
  let last_span_to_end_size = file_end - last_span in
  let result_string =
    if last_span_to_end_size = 0 then
      Printf.sprintf "%s\n" result_string_minus_end
    else
      Printf.sprintf "%s%s\n" result_string_minus_end
        (String.sub file_string last_span last_span_to_end_size)
  in
  Lwt.return result_string
