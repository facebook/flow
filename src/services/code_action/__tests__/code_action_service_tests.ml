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

(** Creates a mutator so we can write some files to sharedmem, and
    cleans up at the end. *)
let with_transaction iter_files f =
  Transaction.with_transaction_sync "test" (fun transaction ->
      let _reader = Mutator_state_reader.create transaction in
      let mutator = Parsing_heaps.Saved_state_mutator.create transaction iter_files in
      f mutator
  )

let reader = State_reader.create ()

let add_package mutator file_key pkg =
  let file_opt = None in
  let module_name = None in
  let hash = Xx.init 0L |> Xx.digest in
  let (_ : Modulename.Set.t) =
    Parsing_heaps.Saved_state_mutator.add_package mutator file_key file_opt hash module_name (Ok pkg)
  in
  ()

let with_package fn pkg f =
  let file_key = File_key.JsonFile fn in
  let file_set = Utils_js.FilenameSet.singleton file_key in
  let iter_files f = f file_key in
  let () =
    with_transaction iter_files @@ fun (_master_mutator, mutator) ->
    add_package mutator file_key pkg
  in
  let finally () =
    with_transaction iter_files @@ fun (master_mutator, mutator) ->
    let dirty_modules = Parsing_heaps.Saved_state_mutator.clear_not_found mutator file_key in
    ignore (dirty_modules : Modulename.Set.t);
    Parsing_heaps.Saved_state_mutator.record_not_found master_mutator file_set
  in
  Fun.protect ~finally f

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
           let fn = "/path/to/root/node_modules/pkg_with_main/package.json" in
           let pkg = Package_json.create ~name:None ~main:(Some "main.js") ~haste_commonjs:false in
           with_package fn pkg @@ fun () ->
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
           let fn = "/path/to/root/node_modules/pkg_with_relative_main/package.json" in
           let pkg =
             Package_json.create ~name:None ~main:(Some "./main.js") ~haste_commonjs:false
           in
           with_package fn pkg @@ fun () ->
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
           let fn = "/path/to/root/node_modules/pkg_with_nested_main/package.json" in
           let pkg =
             Package_json.create ~name:None ~main:(Some "dist/main.js") ~haste_commonjs:false
           in
           with_package fn pkg @@ fun () ->
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
