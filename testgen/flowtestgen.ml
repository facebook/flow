(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*  This program generates programs that can type check but have
 *  runtime error
 *)
module Utils = Flowtestgen_utils
module Config = Flowtestgen_config
module Logging = Flowtestgen_logging
module Syntax = Syntax_base
open Printf

let check_file (filename : string) (cmd : string) =
  if not (Sys.file_exists filename) then (
    printf "%s does not exist in the directory.\n" filename;
    printf "Creating %s ...\n" filename;
    if Sys.command cmd > 0 then failwith ("Failed to create " ^ filename)
  )

let sys_init () =
  printf "Checking required libraries...\n";
  check_file ".flowconfig" "flow init";
  check_file "package.json" "npm init -f";
  check_file "./node_modules/.bin/flow-remove-types" "npm install flow-remove-types"

(*
  check_file
    "./node_modules/.bin/babel"
    "npm install babel-cli babel-preset-flow";
  check_file
    ".babelrc"
    "echo '{\"presets\": [\"flow\"]}' | > .babelrc";;
     *)

let move_func (prog : Syntax.t list) =
  let is_func s =
    match s with
    | Syntax.Stmt (Flow_ast.Statement.FunctionDeclaration _) -> true
    | _ -> false
  in
  let all_func = List.filter is_func prog in
  let all_non_func = List.filter (fun p -> not (is_func p)) prog in
  all_func @ all_non_func

(* Main entry functions for generating code *)
let mk_code engine prog_num =
  engine#gen_prog prog_num
  |> Core_list.map ~f:(fun (slist, env) ->
         (* We add type assertions at the end *)
         let prog = slist |> move_func in
         Printf.sprintf "%s\n%!" (Syntax.combine_syntax prog ^ Ruleset_base.str_of_env env))

(* Generate some ASTs from scratch and then type check them. *)
let main () =
  sys_init ();
  Random.self_init ();
  printf "Generating programs...\n%!";
  let base_engine =
    if Config.(config.random) then
      new Ruleset_base.ruleset_random_base
    else
      new Ruleset_base.ruleset_base
  in
  let depth_engine =
    if Config.(config.random) then
      new Ruleset_depth.ruleset_random_depth
    else
      new Ruleset_depth.ruleset_depth
  in
  let func_engine =
    if Config.(config.random) then
      new Ruleset_func.ruleset_random_func
    else
      new Ruleset_func.ruleset_func
  in
  let optional_engine =
    if Config.(config.random) then
      new Ruleset_optional.ruleset_random_optional
    else
      new Ruleset_optional.ruleset_optional
  in
  let exact_engine =
    if Config.(config.random) then
      new Ruleset_exact.ruleset_random_exact
    else
      new Ruleset_exact.ruleset_exact
  in
  let union_engine =
    if Config.(config.random) then
      new Ruleset_union.ruleset_random_union
    else
      new Ruleset_union.ruleset_union
  in
  ignore base_engine;
  ignore depth_engine;
  ignore func_engine;
  ignore optional_engine;
  ignore exact_engine;
  ignore union_engine;
  let engine = depth_engine in
  let all_prog = mk_code engine Config.(config.num_prog) in
  printf "Generated %d programs.\n%!" (List.length all_prog);

  (* Filter out all the programs that don't type check *)
  let type_check_progs =
    List.filter
      (fun content ->
        let result =
          if Utils.is_typecheck (engine#get_name ()) then
            Utils.type_check content
          else
            None
        in
        match result with
        | None -> true
        | Some msg ->
          Logging.log_type_error content msg;
          false)
      all_prog
  in
  (* run all the type check programs *)
  let batch_result = Utils.batch_run type_check_progs in
  List.iter2
    (fun content msg ->
      match msg with
      | None -> Logging.log_no_error content
      | Some msg -> Logging.log_runtime_error content msg)
    type_check_progs
    batch_result;
  printf "Done!\n%!";
  Logging.print_stats ();
  Logging.close ()

;;
main ()
