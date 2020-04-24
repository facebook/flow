(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Codemod_report

let print_results ~strip_root ~report result : unit =
  match report with
  | StringReporter r ->
    Utils_js.print_endlinef ">>> Launching report...\n\n%s\n" (r ~strip_root result)
  | UnitReporter r ->
    Utils_js.print_endlinef ">>> Launching report...";
    r ~strip_root result

let print_ast_file_dry ~strip_root file =
  let file_path = File_key.to_string file in
  let file_input = File_input.FileName file_path in
  match Diff_heaps.get_diff file with
  | Some (_x :: _xs as diff) ->
    let source = Replacement_printer.print_unsafe diff file_input in
    let file_path = Reason.string_of_source ~strip_root file in
    Utils_js.print_endlinef ">>> %s (#changes: %d)" file_path (List.length diff);
    Utils_js.print_endlinef "%s" source
  | Some []
  | None ->
    ()

let print_ast_file_real file =
  let file_path = File_key.to_string file in
  let file_input = File_input.FileName file_path in
  match Diff_heaps.get_diff file with
  | Some (_x :: _xs as diff) ->
    let source = Replacement_printer.print_unsafe diff file_input in
    let%lwt chan = Lwt_io.open_file ~mode:Lwt_io.output file_path in
    let%lwt () = Lwt_io.fprint chan source in
    let%lwt () = Lwt_io.close chan in
    Lwt.return (Some file)
  | Some []
  | None ->
    Lwt.return None

let max_files_open = 1024

(* Returns None Lwt.t if called in dry_run mode. Otherwise, returns (Some list) Lwt.t
   where list contains the files that were changed. *)
let print_asts ~strip_root ~write files : File_key.t list option Lwt.t =
  let print_dry () =
    files |> List.sort File_key.compare |> List.iter (print_ast_file_dry ~strip_root);
    Lwt.return None
  in
  let print_real () =
    let buckets = ListUtils.bucket_n max_files_open files in
    let%lwt changed_files =
      Lwt_list.fold_left_s
        (fun acc files ->
          let%lwt changed_files = Lwt_list.filter_map_p print_ast_file_real files in
          Lwt.return (List.rev_append changed_files acc))
        []
        buckets
    in
    Lwt.return (Some changed_files)
  in
  if write then
    print_real ()
  else
    print_dry ()
