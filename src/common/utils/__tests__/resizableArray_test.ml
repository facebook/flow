(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let tests = "resizable_array" >::: [
  "basic" >:: begin fun ctxt ->
    let arr = ResizableArray.make 1 in
    ResizableArray.push arr "foo";
    ResizableArray.push arr "bar";
    assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
    assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar");
  end;
  "zero_initial_size" >:: begin fun ctxt ->
    let arr = ResizableArray.make 0 in
    ResizableArray.push arr "foo";
    assert_equal ~ctxt (ResizableArray.get arr 0) (Some "foo");
    assert_equal ~ctxt (ResizableArray.underlying_array_size_do_not_use arr) 1;
  end;
  "size" >:: begin fun ctxt ->
    let arr = ResizableArray.make 4 in
    assert_equal ~ctxt (ResizableArray.size arr) 0;
    ResizableArray.push arr "foo";
    assert_equal ~ctxt (ResizableArray.size arr) 1;
  end;
  "set" >:: begin fun ctxt ->
    let arr = ResizableArray.make 4 in
    ResizableArray.push arr "foo0";
    ResizableArray.push arr "foo1";
    ResizableArray.set arr 0 "bar0";
    ResizableArray.set arr 1 "bar1";
    assert_equal ~ctxt (ResizableArray.get arr 0) (Some "bar0");
    assert_equal ~ctxt (ResizableArray.get arr 1) (Some "bar1");
  end;
  "out_of_bounds_get" >:: begin fun ctxt ->
    let arr = ResizableArray.make 4 in
    assert_equal ~ctxt (ResizableArray.get arr 0) None;
    assert_equal ~ctxt (ResizableArray.get arr (-1)) None;
    assert_equal ~ctxt (ResizableArray.get arr (-10)) None;
    assert_equal ~ctxt (ResizableArray.get arr 3) None;
    assert_equal ~ctxt (ResizableArray.get arr 4) None;
    assert_equal ~ctxt (ResizableArray.get arr 8) None;
  end;
  "out_of_bounds_set" >:: begin fun _ctxt ->
    let arr = ResizableArray.make 4 in
    assert_raises (ResizableArray.Out_of_bounds_set "Index: 0, size: 0") (fun () ->
      ResizableArray.set arr 0 "foo"
    );
  end;
  "expand" >:: begin fun ctxt ->
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
    assert_equal ~ctxt (ResizableArray.get arr 3) (Some "foo3");
  end;
]
