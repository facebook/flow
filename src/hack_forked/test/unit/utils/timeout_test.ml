(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let test_basic_no_timeout () =
  Timeout.with_timeout ~timeout:1 ~on_timeout:(fun _ -> false) ~do_:(fun _ -> true)

let test_basic_with_timeout () =
  Timeout.with_timeout
    ~timeout:1
    ~on_timeout:(fun _ -> true)
    ~do_:(fun _timeout ->
      let _ = Unix.select [] [] [] 2.0 in
      false)

let test_basic_nested_no_timeout () =
  Timeout.with_timeout
    ~timeout:1
    ~on_timeout:(fun _ -> false)
    ~do_:(fun _timeout ->
      Timeout.with_timeout ~timeout:1 ~on_timeout:(fun _ -> false) ~do_:(fun _ -> true))

let test_basic_nested_inner_timeout () =
  Timeout.with_timeout
    ~timeout:3
    ~on_timeout:(fun _ -> false)
    ~do_:(fun _timeout ->
      Timeout.with_timeout
        ~timeout:1
        ~on_timeout:(fun _ -> true)
        ~do_:(fun _timeout ->
          let _ = Unix.select [] [] [] 2.0 in
          false))

let test_basic_nested_outer_timeout () =
  Timeout.with_timeout
    ~timeout:1
    ~on_timeout:(fun _ -> true)
    ~do_:(fun _timeout ->
      Timeout.with_timeout
        ~timeout:3
        ~on_timeout:(fun _ -> false)
        ~do_:(fun _timeout ->
          let _ = Unix.select [] [] [] 2.0 in
          false))

let tests =
  [
    ("test_basic_no_timeout", test_basic_no_timeout);
    ("test_basic_with_timeout", test_basic_with_timeout);
    ("test_basic_nested_no_timeout", test_basic_nested_no_timeout);
    ("test_basic_nested_inner_timeout", test_basic_nested_inner_timeout);
    ("test_basic_nested_outer_timeout", test_basic_nested_outer_timeout);
  ]

let () = Unit_test.run_all tests
