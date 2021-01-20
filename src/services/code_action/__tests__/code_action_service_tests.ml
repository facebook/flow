(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Code_action_service.For_tests

let source_modulename path = Modulename.Filename (File_key.SourceFile path)

let string_opt = function
  | Some x -> x
  | None -> "(None)"

let tests =
  "path_of_modulename"
  >::: [
         ( "removes_js_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               (Some "/path/to/root")
               (source_modulename "/path/to/root/foo/bar.js")
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/bar") path );
         ( "removes_index_js" >:: fun ctxt ->
           let path =
             path_of_modulename
               (Some "/path/to/root")
               (source_modulename "/path/to/root/foo/index.js")
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo") path );
         ( "leaves_json_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               (Some "/path/to/root")
               (source_modulename "/path/to/root/foo/bar.json")
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/bar.json") path );
         ( "leaves_index_json_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               (Some "/path/to/root")
               (source_modulename "/path/to/root/foo/index.json")
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/index.json") path );
       ]
