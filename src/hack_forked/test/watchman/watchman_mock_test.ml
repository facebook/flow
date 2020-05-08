(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel

module Watchman_changes_comparator = struct
  type t = Watchman.changes

  let to_string changes =
    Watchman.(
      let pushed_to_string = function
        | Changed_merge_base (hg_rev, changes, clock) ->
          let changes = String.concat ~sep:", " (SSet.elements changes) in
          Printf.sprintf "Changed_merge_base(%s, %s, %s)" hg_rev changes clock
        | State_enter (name, json) ->
          Printf.sprintf
            "State_enter(%s, %s)"
            name
            (Asserter.Hh_json_json_option_comparator.to_string json)
        | State_leave (name, json) ->
          Printf.sprintf
            "State_leave(%s, %s)"
            name
            (Asserter.Hh_json_json_option_comparator.to_string json)
        | Files_changed s ->
          Printf.sprintf "Watchman_push files [%s]" (String.concat ~sep:", " @@ SSet.elements s)
      in
      match changes with
      | Watchman_unavailable -> "Watchman_unavailable"
      | Watchman_pushed changes -> pushed_to_string changes
      | Watchman_synchronous s ->
        Printf.sprintf
          "Watchman_synchronous [%s]"
          (String.concat ~sep:", " @@ List.map ~f:pushed_to_string s))

  let pushed_is_equal exp actual =
    Watchman.(
      match (exp, actual) with
      | (Files_changed exp, Files_changed actual) -> SSet.equal exp actual
      | ( Changed_merge_base (hg_rev_exp, changes_exp, clock_exp),
          Changed_merge_base (hg_rev_actual, changes_actual, clock_actual) ) ->
        String.equal hg_rev_exp hg_rev_actual
        && SSet.equal changes_exp changes_actual
        && String.equal clock_exp clock_actual
      | (State_enter (state_exp, json_exp), State_enter (state_actual, json_actual))
      | (State_leave (state_exp, json_exp), State_leave (state_actual, json_actual)) ->
        let json_equals = Asserter.Hh_json_json_option_comparator.is_equal json_exp json_actual in
        String.equal state_exp state_actual && json_equals
      | (_, _) -> false)

  let is_equal exp actual =
    Watchman.(
      match (exp, actual) with
      | (Watchman_unavailable, Watchman_unavailable) -> true
      | (Watchman_pushed exp, Watchman_pushed actual) -> pushed_is_equal exp actual
      | (Watchman_synchronous exp, Watchman_synchronous actual) ->
        List.for_all2_exn ~f:pushed_is_equal exp actual
      | _ -> false)
end

module Watchman_changes_asserter = Asserter.Make_asserter (Watchman_changes_comparator)

(**
 * Tests for Watchman.Mocking module.
 *
 * Tests require Watchman_Mock to be injected.
 *)

let test_mock_basic () =
  Watchman.Mocking.init_returns None;
  let%lwt result = Watchman.init Watchman.Testing.test_settings () in
  let result = Base.Option.map result ~f:Watchman.Mocking.print_env in
  Asserter.String_asserter.assert_option_equals None result "init_returns";
  Watchman.Mocking.init_returns (Some "hello");
  let%lwt result = Watchman.init Watchman.Testing.test_settings () in
  let result = Base.Option.map result ~f:Watchman.Mocking.print_env in
  Asserter.String_asserter.assert_option_equals (Some "hello") result "init_returns";
  let expected_changes =
    Watchman.Watchman_synchronous [Watchman.Files_changed (SSet.singleton "some_file.php")]
  in
  Watchman.Mocking.get_changes_returns expected_changes;
  let%lwt (_, actual_changes) =
    let%lwt env = Watchman.Testing.get_test_env () in
    Watchman.get_changes (Watchman.Watchman_alive env)
  in
  Watchman_changes_asserter.assert_equals expected_changes actual_changes "get_changes_returns";
  Lwt.return true

let tests = [("test_mock_basic", (fun () -> test_mock_basic () |> Lwt_main.run))]

let () = Unit_test.run_all tests
