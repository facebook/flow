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

let test_local_changes
    (module IntHeap : SharedMem.NoCache with type t = int and type key = string) () =
  let expect_value ~name expected =
    expect_equals ~name (IntHeap.get name) expected;
    let mem = expected <> None in
    let str =
      if mem then
        "be"
      else
        "not be"
    in
    expect ~msg:(Printf.sprintf "Expected key %s to %s a member" name str) (IntHeap.mem name = mem)
  in
  let expect_add key value =
    IntHeap.add key value;
    expect_value key (Some value)
  in
  let expect_remove key =
    IntHeap.remove_batch (IntHeap.KeySet.singleton key);
    expect_value key None
  in
  let test () =
    expect_value "Filled" (Some 0);
    expect_value "Empty" None;

    IntHeap.LocalChanges.push_stack ();

    expect_value "Filled" (Some 0);
    expect_value "Empty" None;

    (* For local changes we allow overriding values *)
    expect_add "Filled" 1;
    expect_add "Empty" 2;

    expect_remove "Filled";
    expect_remove "Empty";

    expect_add "Filled" 1;
    expect_add "Empty" 2;

    IntHeap.LocalChanges.pop_stack ();
    expect_value "Filled" (Some 0);
    expect_value "Empty" None;

    (* Commit changes are reflected in the shared memory *)
    IntHeap.LocalChanges.push_stack ();

    expect_add "Filled" 1;
    expect_add "Empty" 2;

    IntHeap.LocalChanges.commit_all ();
    IntHeap.LocalChanges.pop_stack ();
    expect_value "Filled" (Some 1);
    expect_value "Empty" (Some 2);

    IntHeap.LocalChanges.push_stack ();

    expect_remove "Filled";
    expect_remove "Empty";

    IntHeap.LocalChanges.pop_stack ();
    expect_value "Filled" (Some 1);
    expect_value "Empty" (Some 2);

    IntHeap.LocalChanges.push_stack ();

    expect_remove "Filled";
    expect_remove "Empty";

    IntHeap.LocalChanges.commit_all ();
    IntHeap.LocalChanges.pop_stack ();
    expect_value "Filled" None;
    expect_value "Empty" None;

    IntHeap.LocalChanges.push_stack ();

    expect_add "Filled" 0;
    expect_add "Empty" 2;
    IntHeap.LocalChanges.commit_batch (IntHeap.KeySet.singleton "Filled");
    IntHeap.LocalChanges.revert_batch (IntHeap.KeySet.singleton "Empty");
    expect_value "Filled" (Some 0);
    expect_value "Empty" None;

    IntHeap.LocalChanges.pop_stack ();
    expect_value "Filled" (Some 0);
    expect_value "Empty" None
  in
  IntHeap.add "Filled" 0;
  test ();

  IntHeap.(remove_batch @@ KeySet.singleton "Filled");
  IntHeap.add "Empty" 2;
  IntHeap.LocalChanges.push_stack ();
  IntHeap.add "Filled" 0;
  IntHeap.(remove_batch @@ KeySet.singleton "Empty");
  test ();
  IntHeap.LocalChanges.pop_stack ()

let test_cache_behavior
    (module IntHeap : SharedMem.WithCache with type t = int and type key = string) () =
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

module TestNoCache = SharedMem.NoCache (SharedMem.Immediate) (StringKey) (IntVal)
module TestWithCache = SharedMem.WithCache (SharedMem.Immediate) (StringKey) (IntVal)

let tests () =
  let list =
    [
      ("test_local_changes_no_cache", test_local_changes (module TestNoCache));
      ("test_local_changes_with_cache", test_local_changes (module TestWithCache));
      ("test_cache_behavior", test_cache_behavior (module TestWithCache));
    ]
  in
  let setup_test (name, test) =
    ( name,
      fun () ->
        let num_workers = 0 in
        let handle =
          SharedMem.init
            ~num_workers
            {
              SharedMem.global_size = 16;
              heap_size = 409600;
              dep_table_pow = 2;
              hash_table_pow = 12;
              shm_dirs = [];
              shm_min_avail = 0;
              log_level = 0;
              sample_rate = 0.0;
            }
        in
        ignore (handle : SharedMem.handle);
        test ();
        true )
  in
  List.map setup_test list

let () = Unit_test.run_all (tests ())
