(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type key = OpaqueDigest.t

external hh_add : key -> string -> unit = "hh_add"

external hh_mem : key -> bool = "hh_mem"

external hh_mem_status : key -> int = "hh_mem_status"

external hh_remove : key -> unit = "hh_remove"

external hh_move : key -> key -> unit = "hh_move"

external hh_get : key -> string = "hh_get_and_deserialize"

external hh_collect : unit -> unit = "hh_collect"

external heap_size : unit -> int = "hh_used_heap_size"

let expect ~msg bool =
  if bool then
    ()
  else (
    print_endline msg;
    exit 1
  )

let to_key = OpaqueDigest.string

let add key value = hh_add (to_key key) value

let mem key = hh_mem (to_key key)

let get_status key = hh_mem_status (to_key key)

let remove key = hh_remove (to_key key)

let move k1 k2 = hh_move (to_key k1) (to_key k2)

let get key = hh_get (to_key key)

let gentle_collect () = if SharedMem.should_collect `gentle then hh_collect ()

let aggressive_collect () = if SharedMem.should_collect `aggressive then hh_collect ()

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
  expect_equals ~name:"heap_size" (heap_size ()) (count * heap_space_per_element)

let expect_mem key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to be in hashtable" key) @@ mem key

let expect_not_mem key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to not be in hashtable" key) @@ not (mem key)

let expect_absent key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to be absent from hashtable" key)
  @@ (get_status key = -1)

let expect_removed key =
  expect ~msg:(Printf.sprintf "Expected key '%s' to be removed from hashtable" key)
  @@ (get_status key = -2)

let expect_get key expected =
  let value = get key in
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
  expect_absent "0";

  add "0" "";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";

  move "0" "1";
  expect_stats ~nonempty:2 ~used:1;
  expect_not_mem "0";
  expect_removed "0";
  expect_mem "1";

  remove "1";
  expect_stats ~nonempty:2 ~used:0;
  expect_not_mem "1";
  expect_removed "1"

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
  move "0" "1";
  move "1" "2";
  move "2" "3";
  move "3" "4";
  move "4" "5";
  move "5" "6";
  move "6" "7";

  expect_stats ~nonempty:8 ~used:1;

  try
    move "7" "8";
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
  expect_removed "0";

  add "0" "Bar";
  expect_stats ~nonempty:1 ~used:1;
  expect_mem "0";
  expect_get "0" "Bar"

let test_reuse_slots () =
  expect_stats ~nonempty:0 ~used:0;

  add "0" "0";
  add "1" "1";
  expect_mem "0";
  expect_mem "1";
  expect_stats ~nonempty:2 ~used:2;

  (* If we reuse a previously used slot, the number of nonempty slots
   * stays the same
   *)
  remove "1";
  expect_not_mem "1";
  expect_removed "1";
  expect_stats ~nonempty:2 ~used:1;
  add "1" "Foo";
  expect_mem "1";
  expect_get "1" "Foo";
  expect_stats ~nonempty:2 ~used:2;

  (* If we move to a previously used slot, nonempty slots stays the same *)
  remove "1";
  expect_not_mem "1";
  expect_removed "1";
  expect_stats ~nonempty:2 ~used:1;
  move "0" "1";
  expect_not_mem "0";
  expect_removed "0";
  expect_mem "1";
  expect_get "1" "0";
  expect_stats ~nonempty:2 ~used:1;

  (* Moving to a brand new key will increase number of nonempty slots *)
  move "1" "2";
  expect_not_mem "1";
  expect_removed "1";
  expect_mem "2";
  expect_get "2" "0";
  expect_stats ~nonempty:3 ~used:1

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
  gentle_collect ();
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
  gentle_collect ();
  expect_heap_size 2;
  remove "1";
  add "2" "2";
  expect_heap_size 3;

  (* Gentle garbage collection shouldn't catch this *)
  expect_gentle_collect false;
  gentle_collect ();
  expect_heap_size 3;

  (* Aggressive garbage collection should run *)
  expect_aggressive_collect true;
  aggressive_collect ();
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
  gentle_collect ();
  expect_heap_size 3;
  add "0" "0";
  add "1" "1";
  remove "4";
  remove "5";
  expect_heap_size 5;
  (* Aggressive collection should kick in,
   * because 5 >= 1.2*3 *)
  aggressive_collect ();
  expect_heap_size 3;
  ()

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
              SharedMem.heap_size = 1024;
              hash_table_pow = 3;
              shm_dirs = [];
              shm_min_avail = 0;
              log_level = 0;
            }
        in
        ignore (handle : SharedMem.handle);
        test ();
        true )
  in
  List.map setup_test list

let () = Unit_test.run_all (tests ())
