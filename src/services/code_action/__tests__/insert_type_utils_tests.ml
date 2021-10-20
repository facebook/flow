(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ty

let stylize = (new Insert_type_utils.stylize_ty_mapper ())#on_t Loc.none

let tests =
  "insert_type_utils"
  >::: [
         ( "stylize_union_number_with_number_literal" >:: fun ctxt ->
           let t_in = Union (Num None, NumLit "1", []) in
           let t_exp = Num None in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_string_with_string_literal" >:: fun ctxt ->
           let t_in = Union (StrLit (Reason.OrdinaryName "foo"), Str None, []) in
           let t_exp = Str None in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_true_and_false" >:: fun ctxt ->
           let t_in = Union (BoolLit true, BoolLit false, []) in
           let t_exp = Bool None in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_true_and_bool" >:: fun ctxt ->
           let t_in = Union (BoolLit true, Bool None, []) in
           let t_exp = Bool None in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_string_number_literals" >:: fun ctxt ->
           let t_in = Union (Str None, NumLit "1", [NumLit "2"]) in
           let t_exp = Union (NumLit "1", NumLit "2", [Str None]) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         (* These tests just document that sorting is working in a sane order *)
         ( "sort_types_numeric_literals" >:: fun ctxt ->
           let t_in = Union (NumLit "5", NumLit "11", [NumLit "1"; NumLit "2"]) in
           let t_exp = Union (NumLit "1", NumLit "2", [NumLit "5"; NumLit "11"]) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_top_any" >:: fun ctxt ->
           let t_in = Union (Top, Any (Annotated ALoc.none), []) in
           let t_exp = Top in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_bot_any" >:: fun ctxt ->
           let t_in = Union (Bot EmptyType, Any (Annotated ALoc.none), []) in
           let t_exp = Any (Annotated ALoc.none) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_any_first" >:: fun ctxt ->
           let t_in =
             Union (Void, Any (Annotated ALoc.none), [Null; Str None; NumLit "5"; Bool None])
           in
           let t_exp =
             Union (Any (Annotated ALoc.none), Void, [Null; Bool None; NumLit "5"; Str None])
           in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
       ]
