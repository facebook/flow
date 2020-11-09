(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module SigContextHeap =
  SharedMem_js.WithCache
    (File_key)
    (struct
      type t = Context.sig_t

      let description = "SigContext"
    end)

let master_sig : Context.sig_t option option ref = ref None

let add_sig_context = Expensive.wrap SigContextHeap.add

let add_sig ~audit cx =
  WorkerCancel.with_no_cancellations (fun () ->
      let cx_file = Context.file cx in
      if cx_file = File_key.Builtins then master_sig := None;
      add_sig_context ~audit cx_file (Context.sig_cx cx))

module SigHashHeap =
  SharedMem_js.NoCache
    (File_key)
    (struct
      type t = Xx.hash

      let description = "SigHash"
    end)

module LeaderHeap =
  SharedMem_js.WithCache
    (File_key)
    (struct
      type t = File_key.t

      let description = "Leader"
    end)

let oldify_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.oldify_batch files;
      SigContextHeap.oldify_batch files;
      SigHashHeap.oldify_batch files)

let remove_old_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.remove_old_batch files;
      SigContextHeap.remove_old_batch files;
      SigHashHeap.remove_old_batch files;
      SharedMem_js.collect `gentle)

let revive_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.revive_batch files;
      SigContextHeap.revive_batch files;
      SigHashHeap.revive_batch files)

module Init_master_context_mutator : sig
  val add_master_sig : (Context.t -> unit) Expensive.t
end = struct
  let add_master_sig ~audit cx = add_sig ~audit cx
end

let currently_oldified_files : FilenameSet.t ref option ref = ref None

module Merge_context_mutator : sig
  type master_mutator

  type worker_mutator

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val add_merge_on_diff :
    (worker_mutator -> Context.t -> File_key.t Nel.t -> Xx.hash -> unit) Expensive.t

  val add_merge_on_exn :
    (worker_mutator -> options:Options.t -> File_key.t Nel.t -> unit) Expensive.t

  val revive_files : master_mutator -> Utils_js.FilenameSet.t -> unit

  val unrevived_files : master_mutator -> Utils_js.FilenameSet.t
end = struct
  type master_mutator = Utils_js.FilenameSet.t ref

  type worker_mutator = unit

  let commit oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing context heaps";
        remove_old_merge_batch oldified_files;
        currently_oldified_files := None);
    Lwt.return_unit

  let rollback oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back context heaps";
        revive_merge_batch oldified_files;
        currently_oldified_files := None);
    Lwt.return_unit

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        let master_mutator = ref files in
        let worker_mutator = () in
        currently_oldified_files := Some master_mutator;

        let commit () = commit !master_mutator in
        let rollback () = rollback !master_mutator in
        oldify_merge_batch files;
        Transaction.add ~singleton:"Merge_context" ~commit ~rollback transaction;

        (master_mutator, worker_mutator))

  (* While merging, we must keep LeaderHeap, SigContextHeap, and SigHashHeap in
     sync, sometimes creating new entries and sometimes reusing old entries. *)

  (* Add a sig only if it has not changed meaningfully, and return the result of
     that check. *)
  let add_merge_on_diff ~audit () leader_cx component_files xx =
    let leader_f = Context.file leader_cx in
    (* Ideally we'd assert that leader_f is a member of the oldified files, but it's a little too
     * expensive to send the set of oldified files to the worker *)
    let diff =
      match SigHashHeap.get_old leader_f with
      | None -> true
      | Some xx_old -> xx <> xx_old
    in
    WorkerCancel.with_no_cancellations (fun () ->
        if diff then (
          Nel.iter
            (fun f ->
              (* Ideally we'd assert that f is a member of the oldified files too *)
              LeaderHeap.add f leader_f)
            component_files;
          add_sig_context ~audit leader_f (Context.sig_cx leader_cx);
          SigHashHeap.add leader_f xx
        ))

  let add_merge_on_exn ~audit () ~options component =
    (* Ideally we'd assert that leader_f is a member of the oldified files, but it's a little too
     * expensive to send the set of oldified files to the worker *)
    let leader_f = Nel.hd component in
    (* This context is only used to add *something* to the sighash when we encounter an unexpected
     * exception during typechecking. It doesn't really matter what we choose, so we might as well
     * make it the empty map. *)
    let aloc_tables = FilenameMap.empty in
    let sig_cx = Context.make_sig () in
    let ccx = Context.make_ccx sig_cx aloc_tables in
    let cx =
      let metadata = Context.metadata_of_options options in
      let rev_table = lazy (ALoc.make_empty_reverse_table ()) in
      let module_ref = Files.module_ref leader_f in
      Context.make ccx metadata leader_f rev_table module_ref Context.Merging
    in
    WorkerCancel.with_no_cancellations (fun () ->
        let module_refs =
          Base.List.map
            ~f:(fun f ->
              let module_ref = Files.module_ref f in
              let module_t = Type.AnyT.locationless (Type.AnyError None) in
              Context.add_module cx module_ref module_t;

              (* Ideally we'd assert that f is a member of the oldified files too *)
              LeaderHeap.add f leader_f;
              module_ref)
            (Nel.to_list component)
        in
        let xx = Merge_js.ContextOptimizer.sig_context cx module_refs in
        add_sig_context ~audit leader_f sig_cx;
        SigHashHeap.add leader_f xx)

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files !oldified_files));
    WorkerCancel.with_no_cancellations (fun () ->
        oldified_files := FilenameSet.diff !oldified_files files;
        revive_merge_batch files)

  (* WARNING: Only call this function at the end of merge!!! Calling it during merge will return
     meaningless results.

     Initially, `oldified_files` contains the set of files to be merged (see Merge_stream). During
     merge, we call `revive_files` for files whose signatures have not changed. So the remaining
     `oldified_files` at the end of merge must contain the set of files whose signatures are new or
     have changed. In principle, we could maintain this state separately in Merge_stream, but it
     seems wasteful to do so. *)
  let unrevived_files oldified_files = !oldified_files
end

module type READER = sig
  type reader

  val find_sig : reader:reader -> File_key.t -> Context.sig_t

  val find_leader : reader:reader -> File_key.t -> File_key.t
end

let find_sig ~get_sig ~reader:_ file =
  let cx_opt =
    if file = File_key.Builtins then (
      match !master_sig with
      | Some cx_opt -> cx_opt
      | None ->
        let cx_opt = get_sig file in
        master_sig := Some cx_opt;
        cx_opt
    ) else
      get_sig file
  in
  match cx_opt with
  | Some cx -> cx
  | None -> raise (Key_not_found ("SigContextHeap", File_key.to_string file))

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val sig_hash_changed : reader:reader -> File_key.t -> bool
end = struct
  type reader = Mutator_state_reader.t

  let find_sig = find_sig ~get_sig:SigContextHeap.get

  let find_leader ~reader:_ file =
    match LeaderHeap.get file with
    | Some leader -> leader
    | None -> raise (Key_not_found ("LeaderHeap", File_key.to_string file))

  let sig_hash_changed ~reader:_ f =
    match SigHashHeap.get f with
    | None -> false
    | Some xx ->
      (match SigHashHeap.get_old f with
      | None -> true
      | Some xx_old -> xx <> xx_old)
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_oldified file =
    match !currently_oldified_files with
    | None -> false
    | Some oldified_files -> FilenameSet.mem file !oldified_files

  let find_sig ~reader file =
    if should_use_oldified file then
      find_sig ~get_sig:SigContextHeap.get_old ~reader file
    else
      find_sig ~get_sig:SigContextHeap.get ~reader file

  let find_leader ~reader:_ file =
    let leader =
      if should_use_oldified file then
        LeaderHeap.get_old file
      else
        LeaderHeap.get file
    in
    match leader with
    | Some leader -> leader
    | None -> raise (Key_not_found ("LeaderHeap", File_key.to_string file))
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let find_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_sig ~reader
    | State_reader reader -> Reader.find_sig ~reader

  let find_leader ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_leader ~reader
    | State_reader reader -> Reader.find_leader ~reader
end
