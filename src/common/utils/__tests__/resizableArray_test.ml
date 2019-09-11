(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests =
  "resizable_array"
  >::: [
         ( "basic"
         >:: fun ctxt ->
         let arr = ResizableArray.make 1 in
         ResizableArray.push arr "foo";
         ResizableArray.push arr "bar";
         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
         assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar") );
         ( "zero_initial_size"
         >:: fun ctxt ->
         let arr = ResizableArray.make 0 in
         ResizableArray.push arr "foo";
         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 1 );
         ( "size"
         >:: fun ctxt ->
         let arr = ResizableArray.make 4 in
         assert_equal ~ctxt (ResizableArray.size arr) 0;
         ResizableArray.push arr "foo";
         assert_equal ~ctxt (ResizableArray.size arr) 1 );
         ( "set"
         >:: fun ctxt ->
         let arr = ResizableArray.make 4 in
         ResizableArray.push arr "foo0";
         ResizableArray.push arr "foo1";
         ResizableArray.set arr 0 "bar0";
         ResizableArray.set arr 1 "bar1";
         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "bar0");
         assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar1") );
         ( "out_of_bounds_get"
         >:: fun ctxt ->
         let arr = ResizableArray.make 4 in
         assert_equal ~ctxt (ResizableArray.get arr 0) None;
         assert_equal ~ctxt (ResizableArray.get arr (-1)) None;
         assert_equal ~ctxt (ResizableArray.get arr (-10)) None;
         assert_equal ~ctxt (ResizableArray.get arr 3) None;
         assert_equal ~ctxt (ResizableArray.get arr 4) None;
         assert_equal ~ctxt (ResizableArray.get arr 8) None );
         ( "out_of_bounds_set"
         >:: fun _ctxt ->
         let arr = ResizableArray.make 4 in
         assert_raises (ResizableArray.Out_of_bounds_set "Index: 0, size: 0") (fun () ->
             ResizableArray.set arr 0 "foo") );
         ( "expand"
         >:: fun ctxt ->
         let arr = ResizableArray.make 1 in
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 1;
         ResizableArray.push arr "foo0";
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 1;
         ResizableArray.push arr "foo1";
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 2;
         ResizableArray.push arr "foo2";
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 4;
         ResizableArray.push arr "foo3";
         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 4;
         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo0");
         assert_equal ~ctxt (ResizableArray.get arr 1) (Some "foo1");
         assert_equal ~ctxt (ResizableArray.get arr 2) (Some "foo2");
         assert_equal ~ctxt (ResizableArray.get arr 3) (Some "foo3") );
         ( "shrink"
         >:: fun ctxt ->
         let arr = ResizableArray.make 8 in
         ResizableArray.push arr "foo";
         ResizableArray.push arr "bar";

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 8;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         ResizableArray.shrink arr;

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 2;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
         assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar") );
         ( "shrink_noop"
         >:: fun ctxt ->
         let arr = ResizableArray.make 2 in
         ResizableArray.push arr "foo";
         ResizableArray.push arr "bar";

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 2;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         ResizableArray.shrink arr;

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 2;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
         assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar") );
         ( "to_hashtbl"
         >:: fun ctxt ->
         let arr = ResizableArray.make 2 in
         ResizableArray.push arr "foo";
         ResizableArray.push arr "bar";

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 2;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         let tbl = ResizableArray.to_hashtbl arr in
         assert_equal ~ctxt (Hashtbl.find tbl "foo") 0;
         assert_equal ~ctxt (Hashtbl.find tbl "bar") 1;
         assert_equal ~ctxt (Hashtbl.length tbl) 2 );
         ( "to_hashtbl_bigger_array"
         >:: fun ctxt ->
         let arr = ResizableArray.make 8 in
         ResizableArray.push arr "foo";
         ResizableArray.push arr "bar";

         assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 8;
         assert_equal ~ctxt (ResizableArray.size arr) 2;

         let tbl = ResizableArray.to_hashtbl arr in
         assert_equal ~ctxt (Hashtbl.find tbl "foo") 0;
         assert_equal ~ctxt (Hashtbl.find tbl "bar") 1;
         assert_equal ~ctxt (Hashtbl.length tbl) 2 );
       ]
