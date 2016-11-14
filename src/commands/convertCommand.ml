(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open CommandUtils
open Sys_utils
open Utils_js
(* conversion *)

let dts_ext = ".d.ts"

let convert_file error_flags outpath file =
  let base = Filename.chop_suffix (Filename.basename file) dts_ext in
  let outpath = match outpath with
    | None -> Filename.dirname file | Some p -> p in
  let outfile = Filename.concat outpath base ^ ".js" in
  Printf.printf "converting %S -> %S\n%!" file outfile;
  let content = cat file in
  let ast, errors = Parser_dts.program_file ~fail:false content file in
  if errors = []
  then (
    let oc = open_out outfile in
    if
      let fmt = Format.formatter_of_out_channel oc in
      call_succeeds (Printer_dts.program fmt) ast
    then
      let () = Printf.printf "No errors!\n\n" in
      close_out oc;
      0, 1, 1
    else
      let () = Printf.printf "No errors!\n\n" in
      Printf.printf "Conversion was not successful!\n\n";
      close_out oc;
      Sys.remove outfile;
      0, 0, 1
  ) else (
    let n = List.length errors in
    Printf.printf "%d errors:\n" n;
    let flow_errors = List.map (fun e ->
      Errors.parse_error_to_flow_error e
    ) errors in
    let root = Path.dummy_path in
    Errors.print_error_summary
      ~flags:error_flags
      ~strip_root:false
      ~root
      flow_errors;
    n, 0, 1
  )

  (* Printer_dts.program *)

let find_files_recursive path =
  Find.find ~filter:(fun x -> Filename.check_suffix x dts_ext) [Path.make path]

let find_files path =
  Array.fold_left (fun acc f ->
    if Filename.check_suffix f dts_ext
    then (Filename.concat path f) :: acc
    else acc
  ) [] (Sys.readdir path)

let sum f = List.fold_left (fun n i -> n + f i) 0

(* sum_triple adds triples (3-tuples) in a list obtained by applying
   function f on a list of inputs (similar to sum function which adds
   integers instead of triples) *)
let sum_triple f = List.fold_left (
  fun (x,y,z) i ->
    let a, b, c = f i
    in (a+x, b+y, c+z)) (0, 0, 0)

let convert_dir outpath path recurse error_flags =
  let dts_files = if recurse
    then find_files_recursive path
    else find_files path in
  (* List.fold_left (convert_file outpath) dts_files *)
  sum_triple (convert_file error_flags outpath) dts_files

let convert path recurse error_flags outpath =
  let nerrs, successful_converts, total_files  =
    if Filename.check_suffix path dts_ext then (
      let outpath = match outpath with
        | None -> Some (Filename.dirname path)
        | _ -> outpath
      in
      convert_file error_flags outpath path
    ) else (
      if recurse && outpath != None then
        failwith "output path not available when recursive";
      convert_dir outpath path recurse error_flags
    ) in
  print_endlinef "Total Errors: %d\nTotal Files: %d\nSuccessful Conversions: %d"
    nerrs total_files successful_converts;
  let num_failed_conversions = total_files - successful_converts in
  if num_failed_conversions = 0
  then FlowExitStatus.(exit No_error)
  else begin
    let msg = spf "Failed to convert %d files" num_failed_conversions in
    FlowExitStatus.(exit ~msg Unknown_error)
  end

(* command wiring *)

let spec = {
  CommandSpec.
  name = "convert";
  doc = "";
  usage = Printf.sprintf
    "Usage: %s convert [DIR]\n\n\
      Convert *.d.ts in DIR if supplied, or current directory.\n\
      foo.d.ts is written to foo.js\n"
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> error_flags
    |> flag "--output" (optional string)
        ~doc:"Output path (not available when recursive)"
    |> flag "--r" no_arg
        ~doc:"Recurse into subdirectories"
    |> anon "dir" (optional string)
        ~doc:"Directory (default: current directory)"
  )
}

let main error_flags outpath recurse dir () =
  let path = match dir with
  | None -> "."
  | Some path -> path
  in
  if ! Sys.interactive
  then ()
  else
    let opts = FlowConfig.Opts.default_options in
    let sharedmem_config = { SharedMem_js.
      global_size = opts.FlowConfig.Opts.shm_global_size;
      heap_size = opts.FlowConfig.Opts.shm_heap_size;
      dep_table_pow = opts.FlowConfig.Opts.shm_dep_table_pow;
      hash_table_pow = opts.FlowConfig.Opts.shm_hash_table_pow;
      shm_dirs = opts.FlowConfig.Opts.shm_dirs;
      shm_min_avail = opts.FlowConfig.Opts.shm_min_avail;
      log_level = opts.FlowConfig.Opts.shm_log_level;
    } in
    let _handle = SharedMem_js.init sharedmem_config in
    convert path recurse error_flags outpath

let command = CommandSpec.command spec main
