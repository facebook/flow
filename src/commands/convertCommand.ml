(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(* conversion *)

let dts_ext = ".d.ts"
let dts_ext_find_pattern = "*.d.ts"

let convert_file outpath file =
  let base = Filename.chop_suffix (Filename.basename file) dts_ext in
  let outpath = match outpath with
    | None -> Filename.dirname file | Some p -> p in
  let outfile = Filename.concat outpath base ^ ".js" in
  Printf.printf "converting %S -> %S\n%!" file outfile;
  let content = cat file in
  let ast, errors = Parser_dts.program_file ~fail:false content file in
  if errors = []
  then (
    Printf.printf "...no errors!\n%!";
    let oc = open_out outfile in
    Printer_dts.program (Format.formatter_of_out_channel oc) ast;
    close_out oc;
    0
  ) else (
    let n = List.length errors in
    Printf.printf "%d errors:\n" n;
    List.iter (fun e ->
      let e = Errors_js.parse_error_to_hack_error e in
      Errors_js.print_error_color e
    ) errors;
    n
  )

  (* Printer_dts.program *)

let find_files_recursive path =
  Find.find_with_name [Path.mk_path path] dts_ext_find_pattern

let find_files path =
  Array.fold_left (fun acc f ->
    if Filename.check_suffix f dts_ext
    then (Filename.concat path f) :: acc
    else acc
  ) [] (Sys.readdir path)

let sum f = List.fold_left (fun n i -> n + f i) 0

let convert_dir outpath path recurse =
  let dts_files = if recurse
    then find_files_recursive path
    else find_files path in
  (* List.fold_left (convert_file outpath) dts_files *)
  sum (convert_file outpath) dts_files

let convert path recurse outpath =
  let nerrs = if Filename.check_suffix path dts_ext then (
    let outpath = match outpath with
      | None -> Some (Filename.dirname path)
      | _ -> outpath
    in
    convert_file outpath path
  ) else (
    if recurse && outpath != None then
      failwith "output path not available when recursive";
    convert_dir outpath path recurse
  ) in
  Printf.printf "%d total errors\n%!" nerrs

(* command wiring *)

type env = {
  path : string;
  recurse : bool;
  outpath : string option;
}

let parse_args () =
  let outpath = ref None in
  let recurse = ref false in
  let options = CommandUtils.sort_opts [
    "--output", Arg.String (fun s -> outpath := Some s),
      " Output path (not available when recursive)";
    "--r", CommandUtils.arg_set_unit recurse,
      " Recurse into subdirectories";
  ] in
  let usage = Printf.sprintf "Usage: %s convert [DIR]\n\n\
  Convert *.d.ts in DIR if supplied, or current directory.\n\
  foo.d.ts is written to foo.js" Sys.argv.(0) in
  let args = ClientArgs.parse_without_command options usage "convert" in
  let path = match args with
    | [] -> "."
    | [path] -> path
    | _ ->
      Arg.usage options usage;
      exit 2
  in
  { path; recurse = !recurse; outpath = !outpath }

let die str =
  let oc = stderr in
  output_string oc str;
  close_out oc;
  exit 2

let main { path; recurse; outpath; } =
  if ! Sys.interactive
  then ()
  else
    SharedMem.init();
    Errors.try_
      (fun () -> convert path recurse outpath)
      (fun l -> die (Errors.to_string (Errors.to_absolute l)))

let run () = main (parse_args ())
