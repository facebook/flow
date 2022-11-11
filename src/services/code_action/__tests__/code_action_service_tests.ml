(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

let node_resolver_dirnames = ["node_modules"]

let reader = State_reader.create ()

let add_package fn pkg =
  let file_key = File_key.JsonFile fn in
  let module_name = None in
  let hash = Xx.init 0L |> Xx.digest in
  let (_ : Modulename.Set.t) =
    Parsing_heaps.From_saved_state.add_package file_key hash module_name (Ok pkg)
  in
  ()

let tests =
  "path_of_modulename"
  >::: [
         ( "removes_js_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root")
               (File_key.SourceFile "/path/to/root/foo/bar.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/bar") path
         );
         ( "removes_index_js" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root")
               (File_key.SourceFile "/path/to/root/foo/index.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo") path
         );
         ( "leaves_json_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root")
               (File_key.SourceFile "/path/to/root/foo/bar.json")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/bar.json") path
         );
         ( "leaves_index_json_extension" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root")
               (File_key.SourceFile "/path/to/root/foo/index.json")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./foo/index.json") path
         );
         ( "removes_node_modules_in_parent" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a/b")
               (File_key.SourceFile "/path/to/root/a/node_modules/module/index.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "module") path
         );
         ( "removes_node_modules_in_self" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a")
               (File_key.SourceFile "/path/to/root/a/node_modules/module/index.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "module") path
         );
         ( "does_not_remove_node_modules_in_child" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a")
               (File_key.SourceFile "/path/to/root/a/b/node_modules/module/index.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "./b/node_modules/module") path
         );
         ( "does_not_remove_node_modules_in_cousin" >:: fun ctxt ->
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a/c")
               (File_key.SourceFile "/path/to/root/a/b/node_modules/module/index.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "../b/node_modules/module") path
         );
         ( "supports_package_json_main" >:: fun ctxt ->
           let pkg = Package_json.create ~name:None ~main:(Some "main.js") ~haste_commonjs:false in
           add_package "/path/to/root/node_modules/pkg_with_main/package.json" pkg;
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a/c")
               (File_key.SourceFile "/path/to/root/node_modules/pkg_with_main/main.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "pkg_with_main") path
         );
         ( "supports_package_json_relative_main" >:: fun ctxt ->
           let pkg =
             Package_json.create ~name:None ~main:(Some "./main.js") ~haste_commonjs:false
           in
           add_package "/path/to/root/node_modules/pkg_with_relative_main/package.json" pkg;
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a/c")
               (File_key.SourceFile "/path/to/root/node_modules/pkg_with_relative_main/main.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "pkg_with_relative_main") path
         );
         ( "supports_package_json_nested_main" >:: fun ctxt ->
           let pkg =
             Package_json.create ~name:None ~main:(Some "dist/main.js") ~haste_commonjs:false
           in
           add_package "/path/to/root/node_modules/pkg_with_nested_main/package.json" pkg;
           let path =
             path_of_modulename
               ~node_resolver_dirnames
               ~reader
               (Some "/path/to/root/a/c")
               (File_key.SourceFile "/path/to/root/node_modules/pkg_with_nested_main/dist/main.js")
               None
           in
           assert_equal ~ctxt ~printer:string_opt (Some "pkg_with_nested_main") path
         );
       ]
