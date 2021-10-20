(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type key = Digest.t

module IntVal = struct
  type t = int

  let prefix = Prefix.make ()

  let description = "IntVal"
end

let expect ~msg bool =
  if bool then
    ()
  else (
    print_endline msg;
    Printexc.(get_callstack 100 |> print_raw_backtrace stderr);
    exit 1
  )

let expect_equals ~name value expected =
  let str x =
    match x with
    | None -> "None"
    | Some n -> Printf.sprintf "(Some '%d')" n
  in
  expect
    ~msg:(Printf.sprintf "Expected key %s to equal %s, got %s" name (str expected) (str value))
    (value = expected)

let test_cache_behavior
    (module IntHeap : SharedMem.WithCache with type key = string and type value = int) () =
  let expect_cache_size expected_l1 expected_l2 =
    let actual_l1 = IntHeap.DebugCache.DebugL1.get_size () in
    expect
      ~msg:(Printf.sprintf "Expected L1 cacke size of %d, got %d" expected_l1 actual_l1)
      (actual_l1 = expected_l1);
    let actual_l2 = IntHeap.DebugCache.DebugL2.get_size () in
    expect
      ~msg:(Printf.sprintf "Expected L2 cacke size of %d, got %d" expected_l2 actual_l2)
      (actual_l2 = expected_l2)
  in
  expect_cache_size 0 0;

  (* Fill the L1 cache. *)
  for i = 1 to 1000 do
    IntHeap.add (Printf.sprintf "%d" i) i;
    expect_cache_size i i
  done;

  (* Make sure the L1 cache does not grow past capacity - L2 will. *)
  for i = 1001 to 2000 do
    IntHeap.add (Printf.sprintf "%d" i) i;
    expect_cache_size 1000 i
  done;

  (* L2 will be collected and resized. *)
  for i = 2001 to 3000 do
    IntHeap.add (Printf.sprintf "%d" i) i;
    expect_cache_size 1000 (i - 1000)
  done;

  (* Delete entries and watch both cache sizes shrink. *)
  for i = 3000 downto 2001 do
    IntHeap.remove_batch (IntHeap.KeySet.singleton (Printf.sprintf "%d" i));
    expect_cache_size (max (i - 2001) 0) (max (i - 1001) 0)
  done

(* Cannot test beyond this point. The LFU cache collection, that
   occurred when the index hit 2000, deleted half of the keys at
   random; we don't know which ones specifically. *)

module TestWithCache = SharedMem.WithCache (StringKey) (IntVal)

let tests () =
  let list = [("test_cache_behavior", test_cache_behavior (module TestWithCache))] in
  let setup_test (name, test) =
    ( name,
      fun () ->
        let num_workers = 0 in
        let config = { SharedMem.heap_size = 409600; hash_table_pow = 12; log_level = 0 } in
        match SharedMem.init ~num_workers config with
        | Ok handle ->
          ignore (handle : SharedMem.handle);
          test ();
          true
        | Error () -> false
    )
  in
  List.map setup_test list

let () = Unit_test.run_all (tests ())
