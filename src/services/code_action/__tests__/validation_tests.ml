(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ty
open Insert_type_utils.Validator
module Error = Insert_type_utils.Error

let tests =
  "validation_tests"
  >::: [
         (* Valid types *)
         ( "Any_annotated" >:: fun ctxt ->
           let t = Any Annotated in
           let (_, errs) = validate_type ~size_limit:1000 t in
           assert_equal ~ctxt ~printer:(fun _ -> "unit") (List.length errs) 0 );
         ( "Any_bound_function_this" >:: fun ctxt ->
           let t = Any (Unsound BoundFunctionThis) in
           let (_, errs) = validate_type ~size_limit:1000 t in
           assert_equal ~ctxt ~printer:(fun _ -> "unit") (List.length errs) 0 );
         (* Invalid type (number | any(unsound)) - raises exception *)
         ( "Any_unsound_unresolved_type" >:: fun ctxt ->
           let t = Union (Num None, Any (Unsound UnresolvedType), []) in
           let (_, errs) = validate_type ~size_limit:1000 t in
           assert_equal ~ctxt ~printer:(fun _ -> "unit") errs [Error.Any_Unsound UnresolvedType] );
         (* Type too big - raises exception *)
         ( "Type_too_big" >:: fun ctxt ->
           let t = Union (Num None, Num None, []) in
           let (_, errs) = validate_type ~size_limit:2 t in
           assert_equal
             ~ctxt
             ~printer:(fun _ -> "unit")
             errs
             [Error.TooBig { size_limit = 2; size = Some 3 }] );
       ]
