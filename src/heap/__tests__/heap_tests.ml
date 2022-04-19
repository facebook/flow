(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module Ent =
  NoCacheAddr
    (StringKey)
    (struct
      type t = heap_string entity
    end)

let assert_heap_size wsize =
  let bsize = wsize * (Sys.word_size / 8) in
  assert (SharedMem.heap_size () = bsize)

let assert_null_committed entity = assert (Option.is_none (entity_read_committed entity))

let assert_committed f entity data = assert (data = f (Option.get (entity_read_committed entity)))

let assert_latest f entity data = assert (data = f (Option.get (entity_read_latest entity)))

let assert_latest_opt f entity data = assert (data = Option.map f (entity_read_latest entity))

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
      assert (foo_addr = H1.add foo_key foo_addr);
      assert (tbl2_addr = H3.add tbl2_key tbl2_addr)
  );
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

let entities_test _ctxt =
  let foo = "foo" in
  let bar = "bar" in

  (* write foo, bar, and ent=foo to heap *)
  let size = (3 * header_size) + string_size foo + string_size bar + entity_size in
  let (foo, bar, ent) =
    alloc size (fun chunk ->
        let foo = write_string chunk foo in
        let bar = write_string chunk bar in
        let ent = write_entity chunk (Some foo) in
        (foo, bar, ent)
    )
  in

  (* uncommitted *)
  assert_null_committed ent;
  assert_latest Fun.id ent foo;

  (* commit foo *)
  commit_transaction ();
  assert_committed Fun.id ent foo;
  assert_latest Fun.id ent foo;

  (* advance ent=bar, foo still committed *)
  entity_advance ent (Some bar);
  assert_committed Fun.id ent foo;
  assert_latest Fun.id ent bar;

  (* commit bar *)
  commit_transaction ();
  assert_committed Fun.id ent bar;
  assert_latest Fun.id ent bar;

  (* clean up *)
  compact ();
  assert_heap_size 0

let entities_compact_test _ctxt =
  let foo = "foo" in
  let bar = "bar" in
  let foo_size = header_size + string_size foo in
  let bar_size = header_size + string_size bar in
  let size = foo_size + bar_size + header_size + entity_size in
  let (bar, ent) =
    alloc size (fun chunk ->
        let foo = write_string chunk foo in
        let bar = write_string chunk bar in
        let ent = write_entity chunk (Some foo) in
        (bar, ent)
    )
  in

  (* keep ent alive *)
  let key = "ent_key" in
  assert (ent = Ent.add key ent);

  commit_transaction ();
  entity_advance ent (Some bar);

  (* compact before commit: both foo, bar reachable *)
  compact ();
  assert_heap_size size;
  let ent = Option.get (Ent.get key) in
  assert_committed read_string ent "foo";
  assert_latest read_string ent "bar";

  (* compact after commit: foo unreachable *)
  commit_transaction ();
  compact ();
  assert_heap_size (size - foo_size);
  let ent = Option.get (Ent.get key) in
  assert_committed read_string ent "bar";
  assert_latest read_string ent "bar";

  Ent.remove_batch (SSet.singleton key);
  compact ();
  assert_heap_size 0

let entities_rollback_test _ctxt =
  (* init *)
  commit_transaction ();

  let foo = "foo" in
  let bar = "bar" in
  let foo_size = header_size + string_size foo in
  let bar_size = header_size + string_size bar in
  let size = foo_size + bar_size + header_size + entity_size in
  let (foo, bar, ent) =
    alloc size (fun chunk ->
        let foo = write_string chunk foo in
        let bar = write_string chunk bar in
        let ent = write_entity chunk (Some foo) in
        (foo, bar, ent)
    )
  in
  assert_latest Fun.id ent foo;

  entity_rollback ent;
  assert_latest_opt Fun.id ent None;

  (* advance ent -> foo, commit *)
  entity_advance ent (Some foo);
  commit_transaction ();

  (* rollback after commit: no change *)
  entity_rollback ent;
  assert_latest Fun.id ent foo;

  (* advance ent -> bar, rollback *)
  entity_advance ent (Some bar);
  entity_rollback ent;
  assert_latest Fun.id ent foo;

  (* clean up *)
  compact ();
  assert_heap_size 0

let slot_taken_test _ =
  let foo = "foo" in
  let bar = "bar" in
  let key = "key" in
  let size = (2 * header_size) + string_size foo + string_size bar in
  alloc size (fun chunk ->
      let foo = write_string chunk foo in
      let bar = write_string chunk bar in
      assert (foo = H1.add key foo);
      (* add returns foo, because already taken *)
      assert (foo = H1.add key bar);
      (* add returns bar, because slot was freed via delete *)
      H1.remove key;
      assert (bar = H1.add key bar)
  );
  (* clean up *)
  H1.remove key;
  compact ();
  assert_heap_size 0

let tests =
  "heap_tests"
  >::: [
         "collect" >:: collect_test;
         "entities" >:: entities_test;
         "entities_compact" >:: entities_compact_test;
         "entities_rollback" >:: entities_rollback_test;
         "slot_taken" >:: slot_taken_test;
       ]

let () =
  let config = { heap_size = 1024 * 1024 * 1024; hash_table_pow = 14; log_level = 0 } in
  ignore (init ~num_workers:0 config : (handle, unit) result);
  run_test_tt_main tests
