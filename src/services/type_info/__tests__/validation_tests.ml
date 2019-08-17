(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ty

let tests = "validation_tests" >::: [
  (* Valid types *)
  "Any_annotated" >:: begin fun ctxt ->
    let t = Any Annotated in
    let v = Insert_type_utils.validate_type ~size_limit:1000 t in
    assert_equal ~ctxt ~printer:(fun _ -> "unit") v ();
  end;
  "Any_bound_function_this" >:: begin fun ctxt ->
    let t = Any (Unsound BoundFunctionThis) in
    let v = Insert_type_utils.validate_type ~size_limit:1000 t in
    assert_equal ~ctxt ~printer:(fun _ -> "unit") v ();
  end;

  (* Invalid type (number | any(unsound)) - raises exception *)
  "Any_unsound_unresolved_type" >:: begin fun _ctxt ->
    let open Insert_type_utils in
    let t = Union (Num None, Any (Unsound UnresolvedType), []) in
    assert_raises (Fatal (Any_Unsound UnresolvedType)) (fun _ ->
      validate_type ~size_limit:1000 t
    );
  end;

  (* Type too big - raises exception *)
  "Type_too_big" >:: begin fun _ctxt ->
    let open Insert_type_utils in
    let t = Union (Num None, Num None, []) in
    assert_raises (Fatal (TooBig { size_limit = 2; size = Some 3 })) (fun _ ->
      validate_type ~size_limit:2 t
    );
  end;
]
