(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let parse_state_enter_response () =
  let json =
    "{
  \"subscription\":  \"mysubscriptionname\",
  \"root\":          \"/path/to/root\",
  \"state-enter\":   \"mystate\",
  \"clock\":         \"c:1446410081:18462:7:137\",
  \"metadata\": {
    \"foo\": \"bar\"
  }
  }"
  in
  let json = Hh_json.json_of_string ~strict:true json in
  let (_, response) =
    Watchman.Testing.transform_asynchronous_get_changes_response
      (Watchman.Testing.get_test_env ())
      (Some json)
  in
  match response with
  | Watchman.State_enter ("mystate", _) -> true
  | _ -> false

let parse_state_leave_response () =
  let json =
    "{
  \"subscription\":  \"mysubscriptionname\",
  \"root\":          \"/path/to/root\",
  \"state-leave\":   \"mystate\",
  \"clock\":         \"c:1446410081:18462:7:137\",
  \"metadata\": {
    \"foo\": \"bar\"
  }
  }"
  in
  let json = Hh_json.json_of_string ~strict:true json in
  let (_, response) =
    Watchman.Testing.transform_asynchronous_get_changes_response
      (Watchman.Testing.get_test_env ())
      (Some json)
  in
  match response with
  | Watchman.State_leave ("mystate", _) -> true
  | _ -> false

let tests =
  [
    ("parse_state_enter_response", parse_state_enter_response);
    ("parse_state_leave_response", parse_state_leave_response);
  ]

let () = Unit_test.run_all tests
