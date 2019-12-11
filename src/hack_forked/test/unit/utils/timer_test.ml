(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* schedule_timer_to_append ~result X will schedule a 0.Xs timer with a callback that appends X to
 * the result and asserts that the callback was not called too early and not called too late.
 *
 * For example
 *   schedule_timer_to_append ~result 5 // schedules a 0.5s timer and will add 5 to the result
 *   schedule_timer_to_append ~result 1 // schedules a 0.1s timer and will add 1 to the result
 *)
let schedule_timer_to_append ~result id =
  let start = Unix.gettimeofday () in
  let interval = 0.1 *. float_of_int id in
  (* Negative intervals get called right away *)
  let expected_interval = max 0.0 interval in
  (* When this test is run in isolation on a powerful development server, the callback is invoked
   * within ~ 0.063ms of the target. However, when run with hundreds of other tests, I've seen it
   * fire a full 1.87ms after the target. On Sandcastle, I've seen it fire 22.6ms after the target.
   * So let's set the maximum delay to 400ms, to make this test more robust *)
  let maximum_delay = 0.4 in
  let callback () =
    let stop = Unix.gettimeofday () in
    let actual_interval = stop -. start in
    if actual_interval < expected_interval then
      failwith
        (Printf.sprintf
           "Timer fired too early! Expected interval of %f but got %f"
           expected_interval
           actual_interval);
    let delta = actual_interval -. expected_interval in
    if delta > maximum_delay then
      failwith
        (Printf.sprintf
           "Expected timer to fire within %fs of target, but it fired %fs after"
           maximum_delay
           delta);
    result := !result @ [id]
  in
  Timer.set_timer ~interval ~callback

let test_adding_in_order () =
  let result = ref [] in
  schedule_timer_to_append ~result 1 |> ignore;
  schedule_timer_to_append ~result 2 |> ignore;
  schedule_timer_to_append ~result 3 |> ignore;

  Unix.sleep 1;

  Asserter.Int_asserter.assert_list_equals
    [1; 2; 3]
    !result
    "Expected ids to be added to list in the timers' firing order";
  true

let test_adding_in_reverse_order () =
  let result = ref [] in
  schedule_timer_to_append ~result 3 |> ignore;
  schedule_timer_to_append ~result 2 |> ignore;
  schedule_timer_to_append ~result 1 |> ignore;

  Unix.sleep 1;

  Asserter.Int_asserter.assert_list_equals
    [1; 2; 3]
    !result
    "Expected ids to be added to list in the timers' firing order";
  true

(* Negative and 0 intervals should have their callback immediately invoked *)
let test_negative_interval () =
  let result = ref [] in
  schedule_timer_to_append ~result (-1) |> ignore;
  schedule_timer_to_append ~result 0 |> ignore;

  (* No sleep needed *)
  Asserter.Int_asserter.assert_list_equals
    [-1; 0]
    !result
    "Expected ids to be added to list in the order the timers are created";
  true

let test_single_exception_in_sync_callback () =
  try
    Timer.set_timer ~interval:0.0 ~callback:(fun () -> failwith "Boom") |> ignore;
    failwith "Expected set_timer to invoke callback immediately and throw"
  with Failure msg -> msg = "Boom"

let test_single_exception_in_async_callback () =
  Timer.set_timer ~interval:0.1 ~callback:(fun () -> failwith "Boom") |> ignore;
  try
    Unix.sleep 1;
    failwith "Expected timer callback to throw when it fires"
  with Failure msg -> msg = "Boom"

let test_mult_exception_in_async_callback () =
  Timer.set_timer ~interval:0.1 ~callback:(fun () -> failwith "BoomA") |> ignore;
  Timer.set_timer ~interval:0.1 ~callback:(fun () -> failwith "BoomB") |> ignore;
  Timer.set_timer ~interval:0.1 ~callback:(fun () -> failwith "BoomC") |> ignore;
  try
    Unix.sleep 1;
    failwith "Expected the various timer callbacks to throw"
  with Failure msg -> msg = "BoomA" || msg = "BoomB" || msg = "BoomC"

let test_cancelling_current_timer () =
  let result = ref [] in
  let timer1 = schedule_timer_to_append ~result 1 in
  schedule_timer_to_append ~result 2 |> ignore;
  schedule_timer_to_append ~result 3 |> ignore;

  Timer.cancel_timer timer1;

  Unix.sleep 1;

  Asserter.Int_asserter.assert_list_equals
    [2; 3]
    !result
    "Expected cancelled timer #1 to be missing from list";
  true

let test_cancelling_next_timer () =
  let result = ref [] in
  schedule_timer_to_append ~result 1 |> ignore;
  let timer2 = schedule_timer_to_append ~result 2 in
  schedule_timer_to_append ~result 3 |> ignore;

  Timer.cancel_timer timer2;

  Unix.sleep 1;

  Asserter.Int_asserter.assert_list_equals
    [1; 3]
    !result
    "Expected cancelled timer #2 to be missing from list";
  true

let test_cancelling_finished_timer () =
  let result = ref [] in
  let timer = schedule_timer_to_append ~result 0 in
  Timer.cancel_timer timer;

  Asserter.Int_asserter.assert_list_equals
    [0]
    !result
    "Expected cancelling a finished timer to have no effect";
  true

let tests =
  [
    ("test_adding_in_order", test_adding_in_order);
    ("test_adding_in_reverse_order", test_adding_in_reverse_order);
    ("test_negative_interval", test_negative_interval);
    ("test_single_exception_in_sync_callback", test_single_exception_in_sync_callback);
    ("test_single_exception_in_async_callback", test_single_exception_in_async_callback);
    ("test_mult_exception_in_async_callback", test_mult_exception_in_async_callback);
    ("test_cancelling_current_timer", test_cancelling_current_timer);
    ("test_cancelling_next_timer", test_cancelling_next_timer);
    ("test_cancelling_finished_timer", test_cancelling_finished_timer);
  ]

let () = Unit_test.run_all tests
