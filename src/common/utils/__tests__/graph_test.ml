(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module StringGraph = Graph.Make (SSet) (SMap)

let smap_equal = SMap.equal SSet.equal

let string_of_sset set = set |> SSet.elements |> String.concat ", " |> Printf.sprintf "[%s]"

let string_of_smap map =
  map
  |> SMap.elements
  |> List.map (fun (key, value) -> Printf.sprintf "  %s: %s" key (string_of_sset value))
  |> String.concat "\n"

let assert_ssets_equal ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:SSet.equal ~printer:string_of_sset expected actual

let assert_smaps_equal ~ctxt expected actual =
  assert_equal ~ctxt ~cmp:smap_equal ~printer:string_of_smap expected actual

let map =
  SMap.of_list
    [("foo", SSet.of_list ["bar"; "baz"]); ("bar", SSet.of_list ["baz"]); ("baz", SSet.of_list [])]

let reverse_map =
  SMap.of_list
    [("foo", SSet.of_list []); ("bar", SSet.of_list ["foo"]); ("baz", SSet.of_list ["foo"; "bar"])]

let graph = StringGraph.of_map map

let tests =
  "graph"
  >::: [
         ( "basic_construction" >:: fun ctxt ->
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           assert_smaps_equal ~ctxt map result_forward_map;
           assert_smaps_equal ~ctxt reverse_map result_backward_map );
         ( "find" >:: fun ctxt ->
           let result = StringGraph.find "foo" graph in
           let expected = SMap.find "foo" map in
           assert_ssets_equal ~ctxt expected result );
         ( "find_opt" >:: fun ctxt ->
           let result = StringGraph.find_opt "foo" graph in
           let result = Option.value_exn result in
           let expected = SMap.find "foo" map in
           assert_ssets_equal ~ctxt expected result );
         ( "find_opt_none" >:: fun ctxt ->
           let result = StringGraph.find_opt "qux" graph in
           let expected = None in
           assert_equal ~ctxt expected result );
         ( "find_backward" >:: fun ctxt ->
           let result = StringGraph.find_backward "baz" graph in
           let expected = SMap.find "baz" reverse_map in
           assert_ssets_equal ~ctxt expected result );
         ( "find_backward_opt" >:: fun ctxt ->
           let result = StringGraph.find_backward_opt "baz" graph in
           let result = Option.value_exn result in
           let expected = SMap.find "baz" reverse_map in
           assert_ssets_equal ~ctxt expected result );
         ( "find_backward_opt_none" >:: fun ctxt ->
           let result = StringGraph.find_backward_opt "qux" graph in
           let expected = None in
           assert_equal ~ctxt expected result );
         ( "add" >:: fun ctxt ->
           let update_map = SMap.of_list [("qux", SSet.of_list ["foo"; "baz"])] in
           let graph = StringGraph.update_from_map graph update_map ~to_remove:SSet.empty in
           let result_forward_map = StringGraph.to_map graph in
           let expected_forward_map = SMap.union map update_map in
           let result_backward_map = StringGraph.to_backward_map graph in
           let expected_backward_map =
             reverse_map
             |> SMap.add "foo" (SSet.singleton "qux")
             |> SMap.add "baz" (SSet.of_list ["qux"; "foo"; "bar"])
             |> SMap.add "qux" SSet.empty
           in
           assert_smaps_equal ~ctxt expected_forward_map result_forward_map;
           assert_smaps_equal ~ctxt expected_backward_map result_backward_map );
         ( "remove" >:: fun ctxt ->
           let graph =
             StringGraph.update_from_map graph SMap.empty ~to_remove:(SSet.singleton "bar")
           in
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           let expected_forward_map =
             SMap.of_list [("foo", SSet.of_list ["baz"]); ("baz", SSet.of_list [])]
           in
           let expected_backward_map =
             SMap.of_list [("foo", SSet.of_list []); ("baz", SSet.of_list ["foo"])]
           in
           assert_smaps_equal ~ctxt expected_forward_map result_forward_map;
           assert_smaps_equal ~ctxt expected_backward_map result_backward_map );
         ( "remove_all" >:: fun ctxt ->
           let graph =
             StringGraph.update_from_map graph SMap.empty ~to_remove:(SSet.of_list (SMap.keys map))
           in
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           let expected = SMap.empty in
           assert_smaps_equal ~ctxt expected result_forward_map;
           assert_smaps_equal ~ctxt expected result_backward_map );
         ( "remove_nonexistent" >:: fun ctxt ->
           let graph =
             StringGraph.update_from_map graph SMap.empty ~to_remove:(SSet.singleton "fake")
           in
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           assert_smaps_equal ~ctxt map result_forward_map;
           assert_smaps_equal ~ctxt reverse_map result_backward_map );
         ( "modify" >:: fun ctxt ->
           let update_map = SMap.of_list [("foo", SSet.empty)] in
           let graph = StringGraph.update_from_map graph update_map ~to_remove:SSet.empty in
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           let expected_forward_map = SMap.add "foo" SSet.empty map in
           let expected_backward_map =
             SMap.of_list [("foo", SSet.empty); ("bar", SSet.empty); ("baz", SSet.singleton "bar")]
           in
           assert_smaps_equal ~ctxt expected_forward_map result_forward_map;
           assert_smaps_equal ~ctxt expected_backward_map result_backward_map );
         ( "modify_complex" >:: fun ctxt ->
           let update_map =
             SMap.of_list [("foo", SSet.of_list ["qux"]); ("qux", SSet.of_list ["baz"])]
           in
           let to_remove = SSet.singleton "bar" in
           let graph = StringGraph.update_from_map graph update_map ~to_remove in
           let result_forward_map = StringGraph.to_map graph in
           let result_backward_map = StringGraph.to_backward_map graph in
           let expected_forward_map =
             SMap.of_list
               [
                 ("foo", SSet.of_list ["qux"]);
                 ("baz", SSet.of_list []);
                 ("qux", SSet.of_list ["baz"]);
               ]
           in
           let expected_backward_map =
             SMap.of_list
               [
                 ("foo", SSet.of_list []);
                 ("baz", SSet.of_list ["qux"]);
                 ("qux", SSet.of_list ["foo"]);
               ]
           in
           assert_smaps_equal ~ctxt expected_forward_map result_forward_map;
           assert_smaps_equal ~ctxt expected_backward_map result_backward_map );
       ]
