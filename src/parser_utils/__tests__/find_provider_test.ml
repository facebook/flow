(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Test_utils

let indent_len str =
  let len = String.length str in
  let i = ref 0 in
  while !i < len && str.[!i] = ' ' do
    incr i
  done;
  !i

let dedent_trim str =
  let lines = String.split_on_char '\n' str in
  let lines = List.filter (fun line -> String.trim line <> "") lines in
  let min_indent = List.fold_left (fun acc line -> min acc (indent_len line)) max_int lines in
  let lines =
    List.map
      (fun line ->
        let len = String.length line in
        String.sub line min_indent (len - min_indent))
      lines
  in
  String.concat "\n" lines

let parse contents =
  let parse_options =
    Some
      {
        Parser_env.default_parse_options with
        Parser_env.enums = true;
        Parser_env.esproposal_class_instance_fields = true;
        Parser_env.esproposal_class_static_fields = true;
        Parser_env.esproposal_export_star_as = true;
      }
  in
  let (ast, _errors) = Parser_flow.program ~parse_options (dedent_trim contents) in
  ast

let print_providers prov =
  match prov with
  | None -> "[]"
  | Some provider_locs ->
    Utils_js.spf
      "[%s]"
      ( Loc_collections.LocSet.elements provider_locs
      |> Base.List.map ~f:(Loc.debug_to_string ~include_source:false)
      |> String.concat "], [" )

let print_providers_of_def prov =
  match prov with
  | None -> "[]"
  | Some provider_locs ->
    Utils_js.spf
      "[%s]"
      ( Base.List.map
          ~f:(fun r -> Loc.debug_to_string ~include_source:false (Reason.poly_loc_of_reason r))
          provider_locs
      |> String.concat "], [" )

let mk_provider_test var contents expected_msg ctxt =
  let ast = parse contents in
  let env = Provider_api.LocProviders.find_providers ast in
  let msg = Provider_api.LocProviders.get_providers_for_toplevel_var var env in
  assert_equal
    ~ctxt
    ~printer:(fun x -> x)
    ~msg:"Results don't match!"
    expected_msg
    (print_providers msg)

let mk_provider_loc_test loc contents expected_msg ctxt =
  let ast = parse contents in
  let (scope, _) = Provider_api.LocProviders.find_providers ast in
  let msg = Provider_api.LocProviders.providers_of_def scope loc in
  assert_equal
    ~ctxt
    ~printer:(fun x -> x)
    ~msg:"Results don't match!"
    expected_msg
    (print_providers_of_def msg)

let tests =
  "find_providers"
  >::: [
         "empty_to_null_to_init"
         >:: mk_provider_test
               "x"
               "
         var x;
         x = null;
         x = 42;
         "
               "[(2, 0) to (2, 1)], [(3, 0) to (3, 1)]";
         "null_to_init2"
         >:: mk_provider_test
               "x"
               "
let x = null;
x = 42;
x = 100;
         "
               "[(1, 4) to (1, 5)], [(2, 0) to (2, 1)]";
         "empty_to_null_to_init_loc"
         >:: mk_provider_loc_test
               (mk_loc (4, 0) (4, 1))
               "
         var x;
         x = null;
         x = 42;
         x = 10;
         "
               "[(2, 0) to (2, 1)], [(3, 0) to (3, 1)]";
         "write_before_declare"
         >:: mk_provider_test
               "x"
               "
         x = 42;
         var x = true;
         "
               "[(2, 4) to (2, 5)]";
         "write_before_declare_2"
         >:: mk_provider_test
               "x"
               "
         x = 42;
         var x = null;
         "
               "[(1, 0) to (1, 1)], [(2, 4) to (2, 5)]";
         "shadow_let"
         >:: mk_provider_test
               "x"
               "
         let x;
         {
           let x;
           x = 42;
         }
         x = null;
         x = 42;
         "
               "[(6, 0) to (6, 1)], [(7, 0) to (7, 1)]";
         "shadow_let_2"
         >:: mk_provider_test
               "x"
               "
         let x;
         {
           x = 42;
           let x;
           x = 42;
         }
         x = null;
         x = 42;
         "
               "[(7, 0) to (7, 1)], [(8, 0) to (8, 1)]";
         "shadow_var"
         >:: mk_provider_test
               "x"
               "
         var x;
         {
           var x;
           x = 42;
         }
         x = null;
         x = 42;
         "
               "[(4, 2) to (4, 3)]";
         "inner_scope"
         >:: mk_provider_test
               "x"
               "
         let x;
         {
           x = 42;
         }
         "
               "[(3, 2) to (3, 3)]";
         "annotated"
         >:: mk_provider_test
               "x"
               "
         var x: ?number;
         x = null;
         x = 42;
         "
               "[(1, 4) to (1, 5)]";
         "function"
         >:: mk_provider_test
               "f"
               "
         function f() {

         }
         "
               "[(1, 9) to (1, 10)]";
         "function_parameters"
         >:: mk_provider_test "x" "
         function f(x) {

         }
         " "[]";
         "if"
         >:: mk_provider_test
               "x"
               "
         var x = null;
         var condition;
         if (condition) {
           x = 42;
         } else {
           x = 100;
         }
         x = true;
         "
               "[(1, 4) to (1, 5)], [(4, 2) to (4, 3)], [(6, 2) to (6, 3)]";
         "if_onearmed"
         >:: mk_provider_test
               "x"
               "
         var x = null;
         var condition;
         if (condition) {
           x = 42;
         }
         x = true;
         "
               "[(1, 4) to (1, 5)], [(4, 2) to (4, 3)]";
         "switch"
         >:: mk_provider_test
               "x"
               "
         var x = null;
         var condition;
         switch (condition) {
           case 'a':
             x = 1;
             break;
           case 'b':
             x = true;
             break;
           case 'c':
             x = 'hi';
             break;
           default:
             x = (x => x);
         }
         x = 100;
         "
               "[(1, 4) to (1, 5)], [(5, 4) to (5, 5)], [(8, 4) to (8, 5)], [(11, 4) to (11, 5)], [(14, 4) to (14, 5)]";
         "try"
         >:: mk_provider_test
               "x"
               "
         var x = null;
         try {
           x = 100;
         } catch (e) {
           x = true
         } finally {
           x = 'hi';
         }
         x = 42;
         "
               "[(1, 4) to (1, 5)], [(3, 2) to (3, 3)], [(5, 2) to (5, 3)], [(7, 2) to (7, 3)]";
         "try_inscope"
         >:: mk_provider_test "e" "
         try {
         } catch (e) {
         }
         " "[]";
         "fun_var1"
         >:: mk_provider_test "ac" "
var ac = 42;
function ac() {}
         " "[(1, 4) to (1, 6)]";
         "fun_var2"
         >:: mk_provider_test "ac" "
function ac() {}
var ac = 42;
         " "[(1, 9) to (1, 11)]";
         "undeclared" >:: mk_provider_test "x" "
x = 10
         " "[(1, 0) to (1, 1)]";
       ]
