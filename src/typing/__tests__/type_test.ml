(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Type

let assert_forced_to_any s =
  match Constraint.ForcingState.force ~on_error:AnyT.error s with
  | AnyT _ -> ()
  | _ -> failwith "Invalid type"

let forcing_state_tests =
  [
    ( "invalid_self_recursive" >:: fun _ ->
      let rec s_lazy =
        lazy
          (Constraint.ForcingState.of_lazy_t
             ~error_reason:Reason.(locationless_reason RNull)
             (lazy (Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s_lazy)))
          )
      in
      let s = Lazy.force s_lazy in
      assert_forced_to_any s
    );
    ( "invalid_mutually_recursive" >:: fun _ ->
      let rec s1_lazy =
        lazy
          (Constraint.ForcingState.of_lazy_t
             ~error_reason:Reason.(locationless_reason RNull)
             (lazy (Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s2_lazy)))
          )
      and s2_lazy =
        lazy
          (Constraint.ForcingState.of_lazy_t
             ~error_reason:Reason.(locationless_reason RNull)
             (lazy (Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s1_lazy)))
          )
      in
      let s1 = Lazy.force s1_lazy in
      let s2 = Lazy.force s2_lazy in
      assert_forced_to_any s1;
      assert_forced_to_any s2
    );
    ( "invalid_self_recursive_mapped" >:: fun _ ->
      let rec s_lazy =
        lazy
          (Constraint.ForcingState.of_lazy_t
             ~error_reason:Reason.(locationless_reason RNull)
             (lazy (Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s_lazy)))
          )
      in
      let s = Lazy.force s_lazy in
      let s' = Constraint.ForcingState.copy ~on_error:AnyT.error ~visit_for_copier:ignore s in
      assert_forced_to_any s'
    );
    ( "invalid_self_recursive_force_twice" >:: fun _ ->
      let rec s_lazy =
        lazy
          (Constraint.ForcingState.of_lazy_t
             ~error_reason:Reason.(locationless_reason RNull)
             ( lazy
               (let (_ : Type.t) =
                  Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s_lazy)
                in
                Constraint.ForcingState.force ~on_error:AnyT.error (Lazy.force s_lazy)
               )
               )
          )
      in
      let s = Lazy.force s_lazy in
      let s' = Constraint.ForcingState.copy ~on_error:AnyT.error ~visit_for_copier:ignore s in
      assert_forced_to_any s'
    );
  ]

let tests = "type" >::: ["forcing_state" >::: forcing_state_tests]
