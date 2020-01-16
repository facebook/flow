(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module StringCache = Cache.Make (SMap)

(* Like `>::` except it expects the function to return `unit Lwt.t` rather than `unit` *)
let ( %>:: ) name f = name >:: fun ctxt -> LwtInit.run_lwt (fun () -> f ctxt)

module LazyEvaluationTracker : sig
  type 'a t

  val make : 'a -> 'a t

  val get : 'a t -> 'a Lazy.t

  val was_evaluated : 'a t -> bool
end = struct
  type 'a t = {
    lazy_val: 'a Lazy.t;
    mutable was_evaluated: bool;
  }

  let make value =
    let rec result =
      {
        was_evaluated = false;
        lazy_val =
          lazy
            ( result.was_evaluated <- true;
              value );
      }
    in
    result

  let get { lazy_val; _ } = lazy_val

  let was_evaluated { was_evaluated; _ } = was_evaluated
end

let make_cache () = StringCache.make ~max_size:3

let tests =
  "cache"
  >::: [
         ( "basic_miss" %>:: fun ctxt ->
           let cache = make_cache () in
           let eval_tracker = LazyEvaluationTracker.make (Lwt.return 42) in
           let%lwt (result, did_hit) =
             StringCache.with_cache "foo" (LazyEvaluationTracker.get eval_tracker) cache
           in
           assert_equal ~ctxt 42 result;
           assert_equal ~ctxt true (LazyEvaluationTracker.was_evaluated eval_tracker);
           assert_equal ~ctxt false did_hit;
           Lwt.return_unit );
         ( "basic_hit" %>:: fun ctxt ->
           let cache = make_cache () in
           let eval_tracker = LazyEvaluationTracker.make (Lwt.return 42) in
           let%lwt (first_result, first_did_hit) =
             StringCache.with_cache "foo" (lazy (Lwt.return 42)) cache
           in
           let%lwt (second_result, second_did_hit) =
             StringCache.with_cache "foo" (LazyEvaluationTracker.get eval_tracker) cache
           in
           assert_equal ~ctxt 42 first_result;
           assert_equal ~ctxt 42 second_result;
           assert_equal ~ctxt false (LazyEvaluationTracker.was_evaluated eval_tracker);
           assert_equal ~ctxt false first_did_hit;
           assert_equal ~ctxt true second_did_hit;
           Lwt.return_unit );
         ( "eviction" %>:: fun ctxt ->
           let cache = make_cache () in
           let%lwt _ = StringCache.with_cache "foo" (lazy (Lwt.return 1)) cache in
           (* Without the sleeps I (nmote) observed a spurious failure of this test, since
            * eviction is based on last time of access. A few 1 ms sleeps should be more than
            * enough time to avoid spurious failures, but not enough to noticeably affect the
            * runtime of the tests. *)
           Unix.sleepf 0.001;
           let%lwt _ = StringCache.with_cache "bar" (lazy (Lwt.return 2)) cache in
           Unix.sleepf 0.001;
           let%lwt _ = StringCache.with_cache "baz" (lazy (Lwt.return 3)) cache in
           Unix.sleepf 0.001;

           (* This will evict something *)
           let%lwt _ = StringCache.with_cache "qux" (lazy (Lwt.return 4)) cache in

           (* "foo" should have been evicted *)
           let eval_tracker = LazyEvaluationTracker.make (Lwt.return 1) in
           let%lwt (result, did_hit) =
             StringCache.with_cache "foo" (LazyEvaluationTracker.get eval_tracker) cache
           in
           assert_equal ~ctxt 1 result;
           assert_equal ~ctxt true (LazyEvaluationTracker.was_evaluated eval_tracker);
           assert_equal ~ctxt false did_hit;
           Lwt.return_unit );
         ( "eviction_last_access" %>:: fun ctxt ->
           let cache = make_cache () in
           let%lwt _ = StringCache.with_cache "foo" (lazy (Lwt.return 1)) cache in
           Unix.sleepf 0.001;
           let%lwt _ = StringCache.with_cache "bar" (lazy (Lwt.return 2)) cache in
           Unix.sleepf 0.001;
           let%lwt _ = StringCache.with_cache "baz" (lazy (Lwt.return 3)) cache in
           Unix.sleepf 0.001;

           (* This accesses "foo" which updates the access time *)
           let%lwt _ = StringCache.with_cache "foo" (lazy (Lwt.return 1)) cache in

           (* This will evict something *)
           let%lwt _ = StringCache.with_cache "qux" (lazy (Lwt.return 4)) cache in

           (* "bar" should have been evicted *)
           let eval_tracker = LazyEvaluationTracker.make (Lwt.return 2) in
           let%lwt (result, did_hit) =
             StringCache.with_cache "bar" (LazyEvaluationTracker.get eval_tracker) cache
           in
           assert_equal ~ctxt 2 result;
           assert_equal ~ctxt true (LazyEvaluationTracker.was_evaluated eval_tracker);
           assert_equal ~ctxt false did_hit;
           Lwt.return_unit );
         ( "clear" %>:: fun ctxt ->
           let cache = make_cache () in
           let eval_tracker = LazyEvaluationTracker.make (Lwt.return 42) in
           let%lwt (first_result, first_did_hit) =
             StringCache.with_cache "foo" (lazy (Lwt.return 42)) cache
           in
           StringCache.clear cache;
           let%lwt (second_result, second_did_hit) =
             StringCache.with_cache "foo" (LazyEvaluationTracker.get eval_tracker) cache
           in
           assert_equal ~ctxt 42 first_result;
           assert_equal ~ctxt 42 second_result;
           assert_equal ~ctxt true (LazyEvaluationTracker.was_evaluated eval_tracker);
           assert_equal ~ctxt false first_did_hit;
           assert_equal ~ctxt false second_did_hit;
           Lwt.return_unit );
         ( "remove_entry" %>:: fun ctxt ->
           let cache = make_cache () in
           let%lwt _ = StringCache.with_cache "foo" (lazy (Lwt.return 42)) cache in
           let%lwt _ = StringCache.with_cache "bar" (lazy (Lwt.return 43)) cache in
           StringCache.remove_entry "foo" cache;

           let foo_eval_tracker = LazyEvaluationTracker.make (Lwt.return 42) in
           let bar_eval_tracker = LazyEvaluationTracker.make (Lwt.return 43) in
           let%lwt (foo_result, foo_did_hit) =
             StringCache.with_cache "foo" (LazyEvaluationTracker.get foo_eval_tracker) cache
           in
           let%lwt (bar_result, bar_did_hit) =
             StringCache.with_cache "bar" (LazyEvaluationTracker.get bar_eval_tracker) cache
           in
           assert_equal ~ctxt 42 foo_result;
           assert_equal ~ctxt 43 bar_result;
           assert_equal ~ctxt true (LazyEvaluationTracker.was_evaluated foo_eval_tracker);
           assert_equal ~ctxt false (LazyEvaluationTracker.was_evaluated bar_eval_tracker);
           assert_equal ~ctxt false foo_did_hit;
           assert_equal ~ctxt true bar_did_hit;
           Lwt.return_unit );
       ]
