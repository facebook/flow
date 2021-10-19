(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* patternlint-disable flow-prefer-sharedmem-js *)

open OUnit2
open SharedMem
open NewAPI

module H1 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = heap_string
    end)

module H2 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = heap_string addr_tbl
    end)

module H3 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = heap_string addr_tbl addr_tbl
    end)

let assert_heap_size wsize =
  let bsize = wsize * (Sys.word_size / 8) in
  assert (SharedMem.heap_size () = bsize)

let collect_test _ctxt =
  let foo = "foo" in
  let bar = "bar" in
  let tbl1 = [| bar |] in
  let tbl2 = [| tbl1 |] in

  (* calculate size for heap writes *)
  let foo_size = header_size + string_size foo in
  let bar_size = header_size + string_size bar in
  let tbl1_size = header_size + addr_tbl_size tbl1 in
  let tbl2_size = header_size + addr_tbl_size tbl2 in
  let size = foo_size + bar_size + tbl1_size + tbl2_size in

  (* write four objects into the heap
   * 1. the string "foo"
   * 2. the string "bar"
   * 3. an addr tbl containing a single reference to (2)
   * 4. an addr tbl containing a single reference to (3)
   *
   * add (1) and (4) to the hash table
   *)
  let foo_key = "foo_key" in
  let tbl2_key = "tbl2_key" in
  alloc size (fun chunk ->
      let foo_addr = write_string chunk foo in
      let tbl2_addr = write_addr_tbl (write_addr_tbl write_string) chunk tbl2 in
      H1.add foo_key foo_addr;
      H3.add tbl2_key tbl2_addr);
  assert_heap_size size;

  (* all objects reachable via live roots foo, tbl2 *)
  SharedMem.compact ();
  assert_heap_size size;

  (* foo dead, tbl2 -> tbl1 -> bar kept alive *)
  H1.remove_batch (SSet.singleton foo_key);
  SharedMem.compact ();
  assert_heap_size (size - foo_size);

  (* confirm tbl2 -> tbl1 -> bar pointers still valid *)
  let () =
    let tbl2_addr = Base.Option.value_exn (H3.get tbl2_key) in
    let tbl2' = read_addr_tbl (read_addr_tbl read_string) tbl2_addr in
    assert (tbl2' = tbl2)
  in

  (* tbl2 dead, tbl1, bar no longer reachable *)
  H3.remove_batch (SSet.singleton tbl2_key);
  SharedMem.compact ();
  assert_heap_size 0

let tests = "heap_tests" >::: ["collect" >:: collect_test]

let () =
  let config = { heap_size = 1024 * 1024 * 1024; hash_table_pow = 14; log_level = 0 } in
  ignore (init ~num_workers:0 config : (handle, unit) result);
  run_test_tt_main tests
