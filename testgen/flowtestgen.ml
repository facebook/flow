(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*  This program generates programs that can type check but have
 *  runtime error
 *)
module Utils = Flowtestgen_utils;;
module Config = Flowtestgen_config;;
module FRandom = Utils.FRandom;;
module Syntax = Syntax_base;;
open Printf;;

let runtime_error_file = "runtime_error.txt";;
let type_error_file = "type_error.txt";;
let no_error_file = "no_error.txt";;

let check_file (filename : string) (cmd : string) =
  if not (Sys.file_exists filename) then begin
    printf "%s does not exist in the directory.\n" filename;
    printf "Creating %s ...\n" filename;
    if Sys.command cmd > 0 then
      failwith ("Failed to create " ^ filename)
  end;;

let sys_init () =
  printf "Checking required libraries...\n";
  check_file ".flowconfig" "flow init";
  check_file "package.json" "npm init -f";
  check_file
    "./node_modules/.bin/babel"
    "npm install babel-cli babel-preset-flow";
  check_file
    ".babelrc"
    "echo '{\"presets\": [\"flow\"]}' | > .babelrc";;

let move_func (prog : Syntax.t list) =
  let is_func s = match s with
      | Syntax.Stmt (Ast.Statement.FunctionDeclaration _) -> true
      | _ -> false in

  let all_func = List.filter is_func prog in
  let all_non_func = List.filter (fun p -> not (is_func p)) prog in
  all_func @ all_non_func

(* Main entry functions for generating code *)
let mk_code engine prog_num =
  engine#gen_prog prog_num
  |> (List.map (fun (slist, env) ->
      (* We add type assertions at the end *)
      let prog = slist |> move_func in
      Printf.sprintf "%s\n%!" ((Syntax.combine_syntax prog) ^ (Ruleset_base.str_of_env env))))

(* Generate some ASTs from scratch and then type check them. *)
let main () =
  sys_init ();
  Random.self_init ();
  FRandom.init_all_hist ();
  let type_error_count = ref 0 in
  let runtime_error_count = ref 0 in
  let no_error_count = ref 0 in
  let runtime_error_oc =
    if Config.(config.log_to_console) then
      stdout
    else
      open_out runtime_error_file in
  let flow_error_oc =
    if Config.(config.log_to_console) then
      stdout
    else
      open_out type_error_file in
  let no_error_oc =
    if Config.(config.log_to_console) then
      stdout
    else
      open_out no_error_file in
  printf "Generating programs...\n%!";
  let base_engine = new Ruleset_base.ruleset_base in
  let depth_engine = new Ruleset_depth.ruleset_depth in
  let func_engine = new Ruleset_func.ruleset_func in
  let optional_engine = new Ruleset_optional.ruleset_optional in
  let exact_engine = new Ruleset_exact.ruleset_exact in
  let union_engine = new Ruleset_union.ruleset_union in
  let rtest_engine = new Ruleset_rtest.ruleset_rtest in
  ignore base_engine;
  ignore depth_engine;
  ignore func_engine;
  ignore optional_engine;
  ignore exact_engine;
  ignore union_engine;
  ignore rtest_engine;
  let engine = func_engine in
  let all_prog = mk_code engine Config.(config.num_prog) in
  printf "Generated %d programs.\n%!" (List.length all_prog);
  List.iter (fun content ->
    let type_run_result =
      if Utils.is_typecheck (engine#get_name ())
      then Utils.type_check content
      else None in
    match type_run_result with
    | None ->
      (match Utils.test_code content with
       | None -> no_error_count := !no_error_count + 1;
         fprintf no_error_oc "// Good program ==========\n%s\n%!" content;
       | Some test_error_msg ->
         printf "RUNTIME ERROR.\n%!";
         runtime_error_count := !runtime_error_count + 1;
         fprintf runtime_error_oc
           "//===================\n%s\n%!" content;
         fprintf runtime_error_oc "/*\nError: \n%s\n*/\n%!" test_error_msg)
    | Some type_error_msg ->
      printf "TYPE ERROR.\n%!";
      type_error_count := !type_error_count + 1;
      fprintf flow_error_oc "//===================\n%s\n%!" content;
      fprintf flow_error_oc "/*\nError: \n%s\n*/\n%!" type_error_msg) all_prog;
  printf "Done!\n%!";

  (* print type error message *)
  let type_count_str =
    sprintf
      "%d type errors written to %s\n%!"
      !type_error_count
      type_error_file in
  fprintf flow_error_oc "// %s\n%!" type_count_str;
  if not Config.(config.log_to_console) then
    close_out flow_error_oc;
  printf "%s%!" type_count_str;

  (* print runtime error message *)
  let runtime_count_str =
    sprintf
      "%d runtime errors written to %s\n%!"
      !runtime_error_count
      runtime_error_file in
  fprintf runtime_error_oc "// %s\n%!" runtime_count_str;
  if not Config.(config.log_to_console) then
    close_out runtime_error_oc;
  printf "%s%!" runtime_count_str;

  (* Print no error message *)
  let noerror_count_str =
    sprintf
      "%d programs without error written to %s.\n%!"
      !no_error_count
      no_error_file in
  fprintf no_error_oc "// %s\n%!" noerror_count_str;
  if not Config.(config.log_to_console) then
    close_out no_error_oc;
  printf "%s%!" noerror_count_str;;

main ();;
