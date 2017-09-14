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
open Printf;;

(* Whether the program runs successfully. None means no error
   message and the run is successful *)
type run_result = string option;;

let assert_func = "
// from http://tinyurl.com/y93dykzv
const util = require('util');
function assert_type(actual: any, expected: any) {
    if(typeof(actual) != 'object' || typeof(expected) != 'object') {
        if(Array.isArray(expected)) {
            if(expected.indexOf(actual) === -1) {
                var message = '';
                var expected_str = expected.toString();

                var actual_str = 'null';
                if(actual != null) {
                    actual_str = actual.toString();
                }
                message = message.concat('Not contain: ',
                                         'Actual : ',
                                         actual_str,
                                         ' != Expected: ',
                                         expected_str);
                console.log(message);
                throw new Error(message);
            }
        } else if(actual != expected) {
            var expected_str = 'null';
            if(expected != null) {
                expected_str = expected.toString();
            }

            var actual_str = 'null';
            if(actual != null) {
                actual_str = actual.toString();
            }
            var message = '';
            message = message.concat('Not equal: ',
                                     'Actual : ',
                                     actual_str,
                                     ' != Expected: ',
                                     expected_str);
            console.log(message);
            throw new Error(message);
        }
    } else {
        for(var prop in expected) {
            if(expected.hasOwnProperty(prop)) {
                if(!actual.hasOwnProperty(prop)) {
                    var message = '';
                    message = message.concat('Missing property: ', prop.toString());
                    console.log(message);
                    throw new Error(message);
                }
                assert_type(actual[prop], expected[prop]);
            }
        }
    }
}
  
function check_opt_prop(actual : any, expected : any) {
    if(actual === undefined) {
        return;
    }
    assert_type(actual, expected);
}
\n\n
";;

let runtime_error_file = "runtime_error.txt";;
let type_error_file = "type_error.txt";;
let no_error_file = "no_error.txt";;


(* Run a system command with a given code as input *)
let run_cmd
    (code : string)
    (cmd : string)
    (exit_code_handler : (int -> string -> run_result)) : run_result =
  let content = code in
  let ic, oc = Unix.open_process cmd in
  let out_str = sprintf "/* @flow */\n%s\n" (assert_func ^ content) in
  fprintf oc "%s" out_str;
  close_out oc;
  let lines = Utils.read_all ic in
  close_in ic;
  let exit_code = match (Unix.close_process (ic, oc)) with
    | Unix.WEXITED code -> code
    | _ -> failwith "Command exited abnormally." in
  exit_code_handler exit_code (String.concat "\n" lines);;

(* Exit code handler for flow type checking *)
let type_check_exit_handler
    (exit_code : int)
    (output : string) : run_result =
  if exit_code = 0 then None
  else
    let error_type =
      exit_code
      |> FlowExitStatus.error_type
      |> FlowExitStatus.to_string in
    let msg = error_type ^ ":\n" ^ output ^ "\n" in
    Some msg;;

(* type check a piece of code.
 * Return true if this code doesn't have type error.
 *
 * We decided to run Flow using shell command, because it is much
 * easier than using the APIs. If the performance starts to hurt,
 * we will change it to using the APIs.
 *)
let type_check (code : string) : run_result =
  (* Check if we have .flowconfig file *)
  let cmd = "flow check-contents" in
  run_cmd code cmd type_check_exit_handler;;


(* Exit handler for running the program *)
let run_exit_handler
    (exit_code : int)
    (output : string) : run_result =
  if exit_code = 0 then None
  else
    let msg = "Failed to run program:\n" ^ output ^ "\n" in
    Some msg;;

(* Run the code and see if it has runtime error *)
let test_code (code : string) : run_result =
  (* Check if we have flow-remove-types *)
  let exe = "./node_modules/.bin/babel" in
  run_cmd code (exe ^ " --presets flow | node") run_exit_handler;;

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
  printf "Generating programs...\n";
  let all_prog = Codegen.mk_code Config.(config.num_prog) Config.(config.random) in
  printf "Generated %d programs.\n" (List.length all_prog);
  List.iter (fun content ->
    let type_run_result =
      if Config.(config.type_check)
      then type_check content
      else None in
    match type_run_result with
    | None ->
      (match test_code content with
       | None -> no_error_count := !no_error_count + 1;
         fprintf no_error_oc "// Good program ==========\n%s\n" content;
       | Some test_error_msg ->
         printf "RUNTIME ERROR.\n";
         runtime_error_count := !runtime_error_count + 1;
         fprintf runtime_error_oc
           "//===================\n%s\n" content;
         fprintf runtime_error_oc "/*\nError: \n%s\n*/\n" test_error_msg)
    | Some type_error_msg ->
      printf "TYPE ERROR.\n";
      type_error_count := !type_error_count + 1;
      fprintf flow_error_oc "//===================\n%s\n" content;
      fprintf flow_error_oc "/*\nError: \n%s\n*/\n" type_error_msg) all_prog;
  printf "Done!\n";

  (* print type error message *)
  let type_count_str =
    sprintf
      "%d type errors written to %s\n"
      !type_error_count
      type_error_file in
  fprintf flow_error_oc "// %s\n" type_count_str;
  if not Config.(config.log_to_console) then
    close_out flow_error_oc;
  printf "%s" type_count_str;

  (* print runtime error message *)
  let runtime_count_str =
    sprintf
      "%d runtime errors written to %s\n"
      !runtime_error_count
      runtime_error_file in
  fprintf runtime_error_oc "// %s\n" runtime_count_str;
  if not Config.(config.log_to_console) then
    close_out runtime_error_oc;
  printf "%s" runtime_count_str;

  (* Print no error message *)
  let noerror_count_str =
    sprintf
      "%d programs without error written to %s.\n"
      !no_error_count
      no_error_file in
  fprintf no_error_oc "// %s\n" noerror_count_str;
  if not Config.(config.log_to_console) then
    close_out no_error_oc;
  printf "%s" noerror_count_str;;

main ();;
