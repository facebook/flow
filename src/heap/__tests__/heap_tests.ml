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
open Prepare_syntax

module H1 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `string ]
    end)

module H2 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `string ] tbl
    end)

module H3 =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `string ] tbl tbl
    end)

module Ent =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `string ] entity
    end)

module Files =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `file ]
    end)

module HasteModules =
  NoCacheAddr
    (StringKey)
    (struct
      type t = [ `haste_module ]
    end)

let assert_heap_size wsize =
  let bsize = wsize * (Sys.word_size / 8) in
  assert (SharedMem.heap_size () = bsize)

let assert_null_committed entity = assert (Option.is_none (entity_read_committed entity))

let assert_committed f entity data = assert (data = f (Option.get (entity_read_committed entity)))

let assert_latest f entity data = assert (data = f (Option.get (entity_read_latest entity)))

let assert_latest_opt f entity data = assert (data = Option.map f (entity_read_latest entity))

let skip_list_test workers _ctxt =
  let num_workers = List.length workers in
  let files_per_worker = 1000 in
  let mk_filename par_id seq_id = Printf.sprintf "s%dp%d" seq_id par_id in

  let file_set = alloc prepare_write_sklist in

  let create_files () par_id =
    let filenames = Array.init files_per_worker (mk_filename par_id) in
    let prepare_create_file key =
      let+ filename = prepare_write_string key
      and+ parse = prepare_write_entity
      and+ haste = prepare_write_entity
      and+ file = prepare_write_file Source_file in
      let file = file filename (parse None) (haste None) None in
      assert (file == Files.add key file)
    in
    alloc (prepare_iter prepare_create_file filenames)
  in

  let add_files () par_id =
    (* create nodes and add to set *)
    let files =
      Array.init files_per_worker (fun i -> Option.get (Files.get (mk_filename par_id i)))
    in
    let prepare_add_file file =
      let+ node = prepare_write_sknode () in
      let node = node file in
      assert (file_set_add file_set node);
      assert (file_set_mem file_set file)
    in
    alloc (prepare_iter prepare_add_file files)
  in

  let remove_files () par_id =
    for i = 0 to files_per_worker - 1 do
      let file = Option.get (Files.get (mk_filename par_id i)) in
      assert (file_set_remove file_set file)
    done
  in

  let setup_add_delete () par_id = if par_id mod 2 == 0 then add_files () par_id in

  let add_delete m () par_id =
    if par_id mod 2 == m then
      remove_files () par_id
    else
      add_files () par_id
  in

  let teardown_add_delete () par_id = if par_id mod 2 == 0 then remove_files () par_id in

  let run_par =
    let merge () () = () in
    let neutral = () in
    let mk_next () =
      let n = ref 0 in
      fun () ->
        let i = !n in
        incr n;
        if i < num_workers then
          Bucket.Job i
        else
          Bucket.Done
    in
    (fun job -> MultiWorkerLwt.call (Some workers) ~job ~merge ~neutral ~next:(mk_next ()))
  in

  let check_list expected_size =
    let prev = ref "" in
    let count = ref 0 in
    let f file =
      let filename = read_string (get_file_name file) in
      assert (!prev < filename);
      prev := filename;
      incr count
    in
    sklist_iter f file_set;
    assert (!count = expected_size)
  in

  let rec fill_then_empty_loop n =
    if n > 0 then (
      Lwt.bind (run_par add_files) @@ fun () ->
      check_list (num_workers * files_per_worker);
      Lwt.bind (run_par remove_files) @@ fun () ->
      check_list 0;
      fill_then_empty_loop (n - 1)
    ) else
      Lwt.return_unit
  in

  let mixed_add_delete_loop n =
    let rec loop n =
      if n > 0 then
        Lwt.bind (run_par (add_delete 0)) @@ fun () ->
        Lwt.bind (run_par (add_delete 1)) @@ fun () -> loop (n - 1)
      else
        Lwt.return_unit
    in
    Lwt.bind (run_par setup_add_delete) @@ fun () ->
    Lwt.bind (loop n) @@ fun () ->
    Lwt.bind (run_par teardown_add_delete) @@ fun () ->
    check_list 0;
    Lwt.return_unit
  in

  Lwt_main.run
    ( Lwt.bind (run_par create_files) @@ fun () ->
      Lwt.bind (fill_then_empty_loop 10) @@ fun () -> mixed_add_delete_loop 10
    );

  (* Clean up *)
  for w = 0 to num_workers - 1 do
    for i = 0 to files_per_worker - 1 do
      Files.remove (mk_filename w i)
    done
  done;
  SharedMem.compact ();
  assert_heap_size 0

let collect_test _ctxt =
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
  let ((foo_size, _) as prepare_foo) = prepare_write_string "foo" in
  let ((size, _) as prepare) =
    let+ foo = prepare_foo
    and+ tbl2 =
      prepare_write_addr_tbl [| prepare_write_addr_tbl [| prepare_write_string "bar" |] |]
    in
    assert (foo = H1.add foo_key foo);
    assert (tbl2 = H3.add tbl2_key tbl2)
  in
  alloc prepare;
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
    let tbl2 = Base.Option.value_exn (H3.get tbl2_key) in
    let tbl2 = read_addr_tbl (read_addr_tbl read_string) tbl2 in
    assert (tbl2 = [| [| "bar" |] |])
  in

  (* tbl2 dead, tbl1, bar no longer reachable *)
  H3.remove_batch (SSet.singleton tbl2_key);
  SharedMem.compact ();
  assert_heap_size 0

let entities_test _ctxt =
  (* write foo, bar, and ent=foo to heap *)
  let (foo, bar, ent) =
    let prepare =
      let+ foo = prepare_write_string "foo"
      and+ bar = prepare_write_string "bar"
      and+ ent = prepare_write_entity in
      (foo, bar, ent (Some foo))
    in
    alloc prepare
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
  let ((foo_size, _) as prepare_foo) = prepare_write_string "foo" in
  let ((size, _) as prepare) =
    let+ foo = prepare_foo and+ bar = prepare_write_string "bar" and+ ent = prepare_write_entity in
    (bar, ent (Some foo))
  in
  let (bar, ent) = alloc prepare in

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

  let (foo, bar, ent) =
    let prepare =
      let+ foo = prepare_write_string "foo"
      and+ bar = prepare_write_string "bar"
      and+ ent = prepare_write_entity in
      (foo, bar, ent (Some foo))
    in
    alloc prepare
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
  let key = "key" in
  let () =
    let prepare =
      let+ foo = prepare_write_string "foo" and+ bar = prepare_write_string "bar" in
      assert (foo = H1.add key foo);
      (* add returns foo, because already taken *)
      assert (foo = H1.add key bar);
      (* add returns bar, because slot was freed via delete *)
      H1.remove key;
      assert (bar = H1.add key bar)
    in
    alloc prepare
  in
  (* clean up *)
  H1.remove key;
  compact ();
  assert_heap_size 0

let add_provider_barrier_test _ =
  (* A file module points to a linked list of provider files. When we add a new
   * file to this list, we replace the head pointer.
   *
   * This test ensures that we use a write barrier so the marking pass is able
   * to reach all live objects. *)
  let key = "key" in

  (* First, we create a file (foo_f) and a file module (foo_m), and add file_f
   * to the provider list of foo_m. We add foo_m as a root, so foo_m is the only
   * thing keeping foo_f alive. *)
  let () =
    let prepare =
      let+ foo = prepare_write_string "foo"
      and+ foo_provider = prepare_write_entity
      and+ foo_dependents = prepare_write_sklist
      and+ foo_m = prepare_write_haste_module
      and+ haste_info = prepare_write_haste_info
      and+ parse_ent = prepare_write_entity
      and+ haste_ent = prepare_write_entity
      and+ foo_f = prepare_write_file Source_file in
      let foo_m = foo_m foo (foo_provider None) foo_dependents in
      let haste_info = haste_info foo_m in
      let foo_f = foo_f foo (parse_ent None) (haste_ent (Some haste_info)) None in
      add_haste_provider foo_m foo_f haste_info;
      assert (HasteModules.add key foo_m == foo_m)
    in
    alloc prepare
  in

  (* Start a GC. With a work budget of 1, this mark slice will certainly not
   * visit `foo_m`. *)
  assert (not (collect_slice ~force:true 1));

  (* Here, we allocate a new file (bar_f) during the marking pass and add it to
   * foo_m's providers list. That is, we change this:
   *
   *     foo_m -> foo_f
   *
   * to this:
   *
   *     foo_m -> bar_f -> foo_f
   *
   * We "allocate black" meaning that `bar_f` will not be scanned for pointers.
   * We need a write barrier when we modify the list head pointer, otherwise we
   * would never visit and mark `foo_f`. *)
  let () =
    let foo_m = Option.get (HasteModules.get key) in
    let prepare =
      let+ bar = prepare_write_string "bar"
      and+ haste_info = prepare_write_haste_info
      and+ parse_ent = prepare_write_entity
      and+ haste_ent = prepare_write_entity
      and+ bar_f = prepare_write_file Source_file in
      let haste_info = haste_info foo_m in
      let bar_f = bar_f bar (parse_ent None) (haste_ent (Some haste_info)) None in
      add_haste_provider foo_m bar_f haste_info
    in
    alloc prepare
  in

  (* Finish the current GC pass. *)
  collect_full ();

  (* Iterate through all file providers. The files in this list should be kept
   * alive by the module. If we failed to mark the files, then the read_header
   * call in `get_file_kind` will assert. *)
  let foo_m = Option.get (HasteModules.get key) in
  let foo_providers = get_haste_all_providers_exclusive foo_m in
  List.iter (fun f -> ignore (get_file_kind f)) foo_providers

let entity_barrier_test _ =
  (* Similar to `add_provider_barrier_test`, we also need a write barrier when
   * advancing entities. *)
  let ent_key = "ent" in
  let tbl_key = "tbl" in

  (* Add an entity with latest value "foo" *)
  let () =
    let prepare =
      let+ foo = prepare_write_string "foo" and+ ent = prepare_write_entity in
      ent (Some foo)
    in
    let ent = alloc prepare in
    assert (ent = Ent.add ent_key ent)
  in

  (* Commit initial transaction. The entity version will be 0, and next version
   * will be 2. *)
  commit_transaction ();

  (* Start the marking pass, but don't mark `foo` *)
  assert (not (collect_slice ~force:true 1));

  (* Allocate a new object referencing `foo`. Because we allocate black, we will
   * not scan this object for pointers.
   *
   * Also, advance the entity to None. Note that `foo` is still accessible by
   * using entity_read_committed, but `foo` will become inaccessible when we
   * commit.
   *
   * Advancing the entity will update the ent's version from 0 to 3. *)
  let () =
    let ent = Option.get (Ent.get ent_key) in
    let addr = Option.get (entity_read_latest ent) in
    let tbl = alloc (prepare_write_addr_tbl [| prepare_const addr |]) in
    assert (tbl = H2.add tbl_key tbl);
    entity_advance ent None
  in

  (* Once we commit the transaction, the global next_version will advance to 4.
   * At this point, `foo` is unreachable from the ent. *)
  commit_transaction ();

  (* Finish marking. When we visit the entity, we will not mark `foo` because
   * it's not reachable. *)
  collect_full ();

  (* Read `foo` from the tbl object allocated during the last GC. This should
   * succeed and the string should be considered live. If we failed to mark foo,
   * this would fail. *)
  let tbl = read_addr_tbl Fun.id (Option.get (H2.get tbl_key)) in
  assert (String.equal "foo" (read_string tbl.(0)))

let compare_string_test _ =
  let (empty1, empty2, foo1, foo2, foot, quux) =
    let prepare =
      let+ empty1 = prepare_write_string ""
      and+ empty2 = prepare_write_string ""
      and+ foo1 = prepare_write_string "foo"
      and+ foo2 = prepare_write_string "foo"
      and+ foot = prepare_write_string "foot"
      and+ quux = prepare_write_string "quux" in
      (empty1, empty2, foo1, foo2, foot, quux)
    in
    alloc prepare
  in

  assert (compare_string empty1 empty1 = 0);
  assert (compare_string foo1 foo1 = 0);
  assert (compare_string empty1 empty2 = 0);
  assert (compare_string foo1 foo2 = 0);
  assert (compare_string empty1 foo1 < 0);
  assert (compare_string foo1 quux < 0);
  assert (compare_string foo1 empty1 > 0);
  assert (compare_string quux foo1 > 0);
  assert (compare_string foo1 foot < 0);
  assert (compare_string foot foo1 > 0);

  (* clean up *)
  compact ();
  assert_heap_size 0

let tests workers =
  "heap_tests"
  >::: [
         "collect" >:: collect_test;
         "entities" >:: entities_test;
         "entities_compact" >:: entities_compact_test;
         "entities_rollback" >:: entities_rollback_test;
         "slot_taken" >:: slot_taken_test;
         "skip_list" >:: skip_list_test workers;
         "add_provider_barrier" >:: add_provider_barrier_test;
         "entity_barrier" >:: entity_barrier_test;
         "compare_string" >:: compare_string_test;
       ]

let () =
  Random.init 0;

  let entry =
    let restore () ~worker_id:_ = () in
    WorkerController.register_entry_point ~restore
  in
  Daemon.check_entry_point ();

  let num_workers = 4 in
  let config = { heap_size = 10 * 1024 * 1024; hash_table_pow = 20; log_level = 0 } in
  let heap_handle = Result.get_ok (init ~num_workers config) in
  let workers =
    MultiWorkerLwt.make
      ~call_wrapper:None
      ~saved_state:()
      ~entry
      ~nbr_procs:num_workers
      ~gc_control:(Gc.get ())
      ~heap_handle
  in
  run_test_tt_main (tests workers)
