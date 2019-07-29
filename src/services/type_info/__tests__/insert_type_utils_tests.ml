(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ty

let stylize = (new Insert_type_utils.stylize_ty_mapper ())#on_t Loc.none

let tests = "ty_simplifier" >::: [
  "stylize_union_number_with_number_literal" >:: begin fun ctxt ->
    let t_in = Union (Num None, NumLit "1", []) in
    let t_exp = Num None in
    assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in);
  end;
  "stylize_union_string_with_string_literal" >:: begin fun ctxt ->
    let t_in = Union (StrLit "foo", Str None, []) in
    let t_exp = Str None in
    assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in);
  end;
  "stylize_union_true_and_false" >:: begin fun ctxt ->
    let t_in = Union (BoolLit true, BoolLit false, []) in
    let t_exp = Bool None in
    assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in);
  end;
  "stylize_union_true_and_bool" >:: begin fun ctxt ->
    let t_in = Union (BoolLit true, Bool None, []) in
    let t_exp = Bool None in
    assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in);
  end;
  "stylize_union_string_number_literals" >:: begin fun ctxt ->
    let t_in = Union (Str None, NumLit "1", [NumLit "2"]) in
    let t_exp = Union (NumLit "1", NumLit "2", [Str None]) in
    assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in);
  end;
]
