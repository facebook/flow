(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TestHeap =
  SharedMem.NoCache
    (StringKey)
    (struct
      type t = string

      let prefix = Prefix.make ()

      let description = "test"
    end)

let expect ~msg bool =
  if bool then
    ()
  else (
    print_endline msg;
    exit 1
  )

let add = TestHeap.add

let get = TestHeap.get

let mem = TestHeap.mem

let mem_old = TestHeap.mem_old

let oldify k = TestHeap.oldify_batch (SSet.singleton k)

let revive k = TestHeap.revive_batch (SSet.singleton k)

let remove k = TestHeap.remove_batch (SSet.singleton k)

let remove_old k = TestHeap.remove_old_batch (SSet.singleton k)

let expect_equals ~name value expected =
  expect
    ~msg:(Printf.sprintf "Expected SharedMem.%s to equal %d, got %d" name expected value)
    (value = expected)

let expect_stats ~nonempty ~used =
  SharedMem.(
    let expected = { nonempty_slots = nonempty; used_slots = used; slots = 8 } in
    let { nonempty_slots; used_slots; slots } = hash_stats () in
    expect_equals ~name:"nonempty_slots" nonempty_slots expected.nonempty_slots;
    expect_equals ~name:"used_slots" used_slots expected.used_slots;
    expect_equals ~name:"slots" slots expected.slots)

let expect_heap_size count =
  (* Currently a single element takes 64 bytes *)
  let heap_space_per_element = 64 in
  expect_equals ~name:"heap_size" (SharedMem.heap_size ()) (count * heap_space_per_element)

let expect_mem key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to be in hashtable" key) @@ mem key

let expect_not_mem key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to not be in hashtable" key) @@ not (mem key)

let expect_mem_old key =
  expect ~msg:(Printf.sprintf "Expected oldified key '%s' to be in hashtable" key) @@ mem_old key

let expect_not_mem_old key =
  expect ~msg:(Printf.sprintf "Expected oldified key '%s' to not be in hashtable" key)
  @@ not (mem_old key)

let expect_get key expected =
  let value = Base.Option.value_exn (get key) in
  expect
    ~msg:(Printf.sprintf "Expected key '%s' to have value '%s', got '%s" key expected value)
    (value = expected)

let expect_gentle_collect expected =
  expect
    ~msg:
      (Printf.sprintf
         "Expected gentle collection to be %sneeded"
         ( if expected then
           ""
         else
           "not " ))
    (SharedMem.should_collect `gentle = expected)

let expect_aggressive_collect expected =
  expect
    ~msg:
      (Printf.sprintf
         "Expected aggressive collection to be %sneeded"
         ( if expected then
           ""
         else
           "not " ))
    (SharedMem.should_collect `aggressive = expected)

let test_ops () =
  expect_stats ~nonempty:0 ~used:0;
  expect_not_mem "0";

  add "0" "";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";

  oldify "0";
  expect_stats ~nonempty:2 ~used:1;
  expect_not_mem "0";
  expect_mem_old "0";

  remove_old "0";
  expect_stats ~nonempty:2 ~used:0;
  expect_not_mem "0";
  expect_not_mem_old "0"

let test_hashtbl_full_hh_add () =
  expect_stats ~nonempty:0 ~used:0;

  add "0" "";
  add "1" "";
  add "2" "";
  add "3" "";
  add "4" "";
  add "5" "";
  add "6" "";
  add "7" "";

  expect_stats ~nonempty:8 ~used:8;

  try
    add "8" "";
    expect ~msg:"Expected the hash table to be full" false
  with SharedMem.Hash_table_full -> ()

let test_hashtbl_full_hh_move () =
  expect_stats ~nonempty:0 ~used:0;

  add "0" "";
  add "1" "";
  add "2" "";
  add "3" "";
  add "4" "";
  add "5" "";
  add "7" "";
  add "8" "";

  expect_stats ~nonempty:8 ~used:8;

  try
    oldify "0";
    expect ~msg:"Expected the hash table to be full" false
  with SharedMem.Hash_table_full -> ()

(**
 * An important property to remember about the shared hash table is if a key
 * is set, it cannot be overwritten. If you want to associate a key with a new
 * value, then we need to first remove/move the key and then add the new value.
 *)
let test_no_overwrite () =
  expect_stats ~nonempty:0 ~used:0;

  add "0" "Foo";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";
  expect_get "0" "Foo";

  add "0" "Bar";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";
  expect_get "0" "Foo";

  remove "0";
  expect_stats ~nonempty:1 ~used:0;
  expect_not_mem "0";

  add "0" "Bar";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";
  expect_get "0" "Bar"

let test_reuse_slots () =
  expect_stats ~nonempty:0 ~used:0;

  add "1" "";
  expect_mem "1";
  expect_stats ~nonempty:1 ~used:1;

  (* If we reuse a previously used slot, the number of nonempty slots
   * stays the same
   *)
  remove "1";
  expect_not_mem "1";
  expect_stats ~nonempty:1 ~used:0;
  add "1" "Foo";
  expect_mem "1";
  expect_get "1" "Foo";
  expect_stats ~nonempty:1 ~used:1;

  (* Oldifying will use a new slot for the old key *)
  oldify "1";
  expect_not_mem "1";
  expect_mem_old "1";
  expect_stats ~nonempty:2 ~used:1;

  (* Reviving will reuse the original slot *)
  revive "1";
  expect_mem "1";
  expect_stats ~nonempty:2 ~used:1

(* Test basic garbage collection works *)
let test_gc_collect () =
  expect_stats ~nonempty:0 ~used:0;
  expect_heap_size 0;
  add "0" "0";
  add "1" "1";

  (* no memory is wasted *)
  expect_gentle_collect false;
  expect_aggressive_collect false;
  expect_heap_size 2;
  expect_mem "0";
  expect_mem "1";
  remove "1";
  expect_heap_size 2;

  (* Garbage collection should remove the space taken by the removed element *)
  SharedMem.collect `gentle;
  expect_heap_size 1;
  expect_mem "0"

(* Test aggresive garbage collection versus gentle *)
let test_gc_aggressive () =
  expect_stats ~nonempty:0 ~used:0;
  add "0" "0";
  add "1" "1";
  expect_heap_size 2;

  (* Since latest heap size is zero,
      now it should gc, but theres nothing to gc,
      so the heap will stay the same *)
  expect_gentle_collect false;
  SharedMem.collect `gentle;
  expect_heap_size 2;
  remove "1";
  add "2" "2";
  expect_heap_size 3;

  (* Gentle garbage collection shouldn't catch this *)
  expect_gentle_collect false;
  SharedMem.collect `gentle;
  expect_heap_size 3;

  (* Aggressive garbage collection should run *)
  expect_aggressive_collect true;
  SharedMem.collect `aggressive;
  expect_heap_size 2

let test_heapsize_decrease () =
  expect_stats ~nonempty:0 ~used:0;
  add "0" "0";
  add "1" "1";
  add "2" "2";
  add "3" "3";
  expect_heap_size 4;
  remove "2";
  remove "1";
  remove "0";
  add "4" "4";
  add "5" "5";
  expect_heap_size 6;
  (* This runs because 6 >= 2*3 *)
  SharedMem.collect `gentle;
  expect_heap_size 3;
  add "0" "0";
  add "1" "1";
  remove "4";
  remove "5";
  expect_heap_size 5;
  (* Aggressive collection should kick in,
   * because 5 >= 1.2*3 *)
  SharedMem.collect `aggressive;
  expect_heap_size 3;
  ()

let hash_table_pow = 3

let test_full () =
  (* add 2^3 hash table entries *)
  assert (hash_table_pow = 3);
  add "1" "";
  add "2" "";
  add "3" "";
  add "4" "";
  add "5" "";
  add "6" "";
  add "7" "";
  add "8" "";
  let passed = ref false in
  (try add "9" "" with SharedMem.Hash_table_full -> passed := true);
  assert !passed

let tests () =
  let list =
    [
      ("test_ops", test_ops);
      ("test_hashtbl_full_hh_add", test_hashtbl_full_hh_add);
      ("test_hashtbl_full_hh_move", test_hashtbl_full_hh_move);
      ("test_no_overwrite", test_no_overwrite);
      ("test_reuse_slots", test_reuse_slots);
      ("test_gc_collect", test_gc_collect);
      ("test_gc_aggressive", test_gc_aggressive);
      ("test_heapsize_decrease", test_heapsize_decrease);
      ("test_full", test_full);
    ]
  in
  let setup_test (name, test) =
    ( name,
      fun () ->
        let num_workers = 0 in
        let handle =
          SharedMem.init ~num_workers { SharedMem.heap_size = 1024; hash_table_pow; log_level = 0 }
        in
        ignore (handle : SharedMem.handle);
        test ();
        true )
  in
  List.map setup_test list

let () =
  EventLogger.init_fake ();
  Unit_test.run_all (tests ())
