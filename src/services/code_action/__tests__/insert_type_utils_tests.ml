(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
           let t_in = Union (false, Num, NumLit "1", []) in
           let t_exp = Num in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_string_with_string_literal" >:: fun ctxt ->
           let t_in = Union (false, StrLit (Reason.OrdinaryName "foo"), Str, []) in
           let t_exp = Str in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_true_and_false" >:: fun ctxt ->
           let t_in = Union (false, BoolLit true, BoolLit false, []) in
           let t_exp = Bool in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_true_and_bool" >:: fun ctxt ->
           let t_in = Union (false, BoolLit true, Bool, []) in
           let t_exp = Bool in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         ( "stylize_union_string_number_literals" >:: fun ctxt ->
           let t_in = Union (false, Str, NumLit "1", [NumLit "2"]) in
           let t_exp = Union (false, NumLit "1", NumLit "2", [Str]) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (stylize t_in)
         );
         (* These tests just document that sorting is working in a sane order *)
         ( "sort_types_numeric_literals" >:: fun ctxt ->
           let t_in = Union (false, NumLit "5", NumLit "11", [NumLit "1"; NumLit "2"]) in
           let t_exp = Union (false, NumLit "1", NumLit "2", [NumLit "5"; NumLit "11"]) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_top_any" >:: fun ctxt ->
           let t_in = Union (false, Top, Any (Annotated ALoc.none), []) in
           let t_exp = Top in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_bot_any" >:: fun ctxt ->
           let t_in = Union (false, Bot EmptyType, Any (Annotated ALoc.none), []) in
           let t_exp = Any (Annotated ALoc.none) in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
         ( "sort_types_any_first" >:: fun ctxt ->
           let t_in =
             Union (false, Void, Any (Annotated ALoc.none), [Null; Str; NumLit "5"; Bool])
           in
           let t_exp =
             Union (false, Any (Annotated ALoc.none), Void, [Null; Bool; NumLit "5"; Str])
           in
           assert_equal ~ctxt ~printer:Ty.show t_exp (Insert_type.simplify t_in)
         );
       ]
