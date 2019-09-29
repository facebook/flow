(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Config = Flowtestgen_config
open Printf

let runtime_error_file = "runtime_error.txt"

let type_error_file = "type_error.txt"

let early_type_error_file = "early_type_error.txt"

let no_error_file = "no_error.txt"

let no_error_count = ref 0

let type_error_count = ref 0

let early_type_error_count = ref 0

let runtime_error_count = ref 0

let runtime_error_out =
  if Config.(config.log_to_console) then
    stdout
  else
    open_out runtime_error_file

let type_error_out =
  if Config.(config.log_to_console) then
    stdout
  else
    open_out type_error_file

let early_type_error_out =
  if Config.(config.log_to_console) then
    stdout
  else
    open_out early_type_error_file

let no_error_out =
  if Config.(config.log_to_console) then
    stdout
  else
    open_out no_error_file

let reset () =
  no_error_count := 0;
  type_error_count := 0;
  runtime_error_count := 0

let log_no_error code =
  no_error_count := !no_error_count + 1;
  fprintf no_error_out "// Good program ==========\n%s\n%!" code

let log_type_error code msg =
  type_error_count := !type_error_count + 1;
  printf "TYPE ERROR.\n%!";
  fprintf type_error_out "//====================\n%s\n%!" code;
  fprintf type_error_out "/*\nType Error: \n%s\n*/\n%!" msg

let log_early_type_error code msg =
  early_type_error_count := !early_type_error_count + 1;
  printf "EARLY TYPE ERROR.\n%!";
  fprintf early_type_error_out "//====================\n%s\n%!" code;
  fprintf early_type_error_out "/*\nType Error: \n%s\n*/\n%!" msg

let log_runtime_error code msg =
  runtime_error_count := !runtime_error_count + 1;
  printf "RUNTIME ERROR.\n%!";
  fprintf runtime_error_out "//====================\n%s\n%!" code;
  fprintf runtime_error_out "/*\nRuntime Error: \n%s\n*/\n%!" msg

let print_stats () =
  let early_type_count_str =
    sprintf "%d early type errors written to %s\n%!" !early_type_error_count early_type_error_file
  in
  fprintf early_type_error_out "// %s\n%!" early_type_count_str;
  printf "%s%!" early_type_count_str;

  (* print type error message *)
  let type_count_str =
    sprintf "%d type errors written to %s\n%!" !type_error_count type_error_file
  in
  fprintf type_error_out "// %s\n%!" type_count_str;
  printf "%s%!" type_count_str;

  (* print runtime error message *)
  let runtime_count_str =
    sprintf "%d runtime errors written to %s\n%!" !runtime_error_count runtime_error_file
  in
  fprintf runtime_error_out "// %s\n%!" runtime_count_str;
  printf "%s%!" runtime_count_str;

  (* Print no error message *)
  let noerror_count_str =
    sprintf "%d programs without error written to %s.\n%!" !no_error_count no_error_file
  in
  fprintf no_error_out "// %s\n%!" noerror_count_str;
  printf "%s%!" noerror_count_str

let close () =
  if not Config.(config.log_to_console) then close_out early_type_error_out;
  if not Config.(config.log_to_console) then close_out type_error_out;
  if not Config.(config.log_to_console) then close_out runtime_error_out;
  if not Config.(config.log_to_console) then close_out no_error_out
