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

module Files =
  NoCacheAddr
    (StringKey)
    (struct
      type t = file
    end)

module FileModules =
  NoCacheAddr
    (StringKey)
    (struct
      type t = file_module
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

  let file_set = alloc (header_size + sklist_size) write_sklist in

  let create_files () par_id =
    let filenames = Array.init files_per_worker (mk_filename par_id) in
    let size =
      let f acc fn = acc + (4 * header_size) + (2 * entity_size) + string_size fn + file_size in
      Array.fold_left f 0 filenames
    in
    let add_file chunk key =
      let filename = write_string chunk key in
      let parse = write_entity chunk None in
      let haste = write_entity chunk None in
      let file = write_file chunk Source_file filename parse haste None in
      assert (file == Files.add key file)
    in
    alloc size (fun chunk -> Array.iter (add_file chunk) filenames)
  in

  let add_files () par_id =
    let files =
      Array.init files_per_worker (fun i -> Option.get (Files.get (mk_filename par_id i)))
    in

    (* prepare for writing nodes *)
    let size_acc = ref 0 in
    let write_fns =
      let f file =
        let (size, write) = prepare_write_sknode () in
        size_acc := header_size + size + !size_acc;
        (fun chunk -> write chunk file)
      in
      Array.map f files
    in

    (* create nodes and add to set *)
    let f chunk file write =
      let node = write chunk in
      assert (file_set_add file_set node);
      assert (file_set_mem file_set file)
    in
    alloc !size_acc (fun chunk -> Array.iter2 (f chunk) files write_fns)
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

let add_provider_barrier_test _ =
  (* A file module points to a linked list of provider files. When we add a new
   * file to this list, we replace the head pointer.
   *
   * This test ensures that we use a write barrier so the marking pass is able
   * to reach all live objects. *)
  let foo = "foo" in
  let bar = "bar" in
  let key = "key" in

  (* First, we create a file (foo_f) and a file module (foo_m), and add file_f
   * to the provider list of foo_m. We add foo_m as a root, so foo_m is the only
   * thing keeping foo_f alive. *)
  let size =
    (7 * header_size)
    + (3 * entity_size)
    + string_size foo
    + sklist_size
    + file_module_size
    + file_size
  in
  alloc size (fun chunk ->
      let foo = write_string chunk foo in
      let foo_provider = write_entity chunk None in
      let foo_dependents = write_sklist chunk in
      let foo_m = write_file_module chunk foo_provider foo_dependents in
      let parse = write_entity chunk None in
      let haste_info = write_entity chunk None in
      let foo_f = write_file chunk Source_file foo parse haste_info (Some foo_m) in
      add_file_provider foo_m foo_f;
      assert (FileModules.add key foo_m == foo_m)
  );

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
  let size = (4 * header_size) + (2 * entity_size) + string_size bar + file_size in
  alloc size (fun chunk ->
      let foo_m = Option.get (FileModules.get key) in
      let bar = write_string chunk bar in
      let parse = write_entity chunk None in
      let haste_info = write_entity chunk None in
      let bar_f = write_file chunk Source_file bar parse haste_info (Some foo_m) in
      add_file_provider foo_m bar_f
  );

  (* Finish the current GC pass. *)
  collect_full ();

  (* Iterate through all file providers. The files in this list should be kept
   * alive by the module. If we failed to mark the files, then the read_header
   * call in `get_file_kind` will assert. *)
  let foo_m = Option.get (FileModules.get key) in
  let foo_providers = get_file_all_providers_exclusive foo_m in
  List.iter (fun f -> ignore (get_file_kind f)) foo_providers

let entity_barrier_test _ =
  (* Similar to `add_provider_barrier_test`, we also need a write barrier when
   * advancing entities. *)
  let foo = "foo" in
  let ent_key = "ent" in
  let tbl_key = "tbl" in

  (* Add an entity with latest value "foo" *)
  let size = (2 * header_size) + string_size foo + entity_size in
  alloc size (fun chunk ->
      let foo = write_string chunk foo in
      let ent = write_entity chunk (Some foo) in
      assert (ent = Ent.add ent_key ent)
  );

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
  let ent = Option.get (Ent.get ent_key) in
  let tbl = [| Option.get (entity_read_latest ent) |] in
  let size = header_size + addr_tbl_size tbl in
  alloc size (fun chunk ->
      let tbl = write_addr_tbl (fun _ addr -> addr) chunk tbl in
      assert (tbl = H2.add tbl_key tbl);
      entity_advance ent None
  );

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
