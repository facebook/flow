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
module Logging = Flowtestgen_logging;;
module FRandom = Utils.FRandom;;
module Syntax = Syntax_base;;
open Printf;;

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
  printf "Generating programs...\n%!";
  let base_engine =
    if Config.(config.random)
    then new Ruleset_base.ruleset_base
    else new Ruleset_base.ruleset_random_base in
  let depth_engine =
    if Config.(config.random)
    then new Ruleset_depth.ruleset_depth
    else new Ruleset_depth.ruleset_random_depth in
  let func_engine =
    if Config.(config.random)
    then new Ruleset_func.ruleset_func
    else new Ruleset_func.ruleset_random_func in
  let optional_engine = 
    if Config.(config.random)
    then new Ruleset_optional.ruleset_optional
    else new Ruleset_optional.ruleset_random_optional in
  let exact_engine = 
    if Config.(config.random)
    then new Ruleset_exact.ruleset_exact
    else new Ruleset_exact.ruleset_random_exact in
  let union_engine = 
    if Config.(config.random)
    then new Ruleset_union.ruleset_union
    else new Ruleset_union.ruleset_random_union in
  ignore base_engine;
  ignore depth_engine;
  ignore func_engine;
  ignore optional_engine;
  ignore exact_engine;
  ignore union_engine;
  let engine = union_engine in
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
       | None -> Logging.log_no_error content
       | Some test_error_msg -> Logging.log_runtime_error content test_error_msg)
    | Some type_error_msg -> Logging.log_type_error content type_error_msg) all_prog;
  printf "Done!\n%!";
  Logging.print_stats ();
  Logging.close ();;

main ();;
