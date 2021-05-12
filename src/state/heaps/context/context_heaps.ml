(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module MasterContextHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = Context.master_context

      let description = "MasterContext"
    end)

module SigContextHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = Context.sig_t

      let description = "SigContext"
    end)

let master_cx_ref : Context.master_context option ref = ref None

let add_sig_context = Expensive.wrap SigContextHeap.add

let add_master ~audit master_cx =
  WorkerCancel.with_no_cancellations (fun () ->
      master_cx_ref := None;
      let master_context =
        { Context.master_sig_cx = Context.sig_cx master_cx; builtins = Context.builtins master_cx }
      in
      (Expensive.wrap MasterContextHeap.add) ~audit File_key.Builtins master_context)

module SigHashHeap =
  SharedMem.NoCache
    (File_key)
    (struct
      type t = Xx.hash

      let description = "SigHash"
    end)

module LeaderHeap =
  SharedMem.WithCache
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
      SigHashHeap.remove_old_batch files)

let revive_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.revive_batch files;
      SigContextHeap.revive_batch files;
      SigHashHeap.revive_batch files)

module Init_master_context_mutator : sig
  val add_master : (Context.t -> unit) Expensive.t
end = struct
  let add_master ~audit cx = add_master ~audit cx
end

let currently_oldified_files : FilenameSet.t ref option ref = ref None

module Merge_context_mutator : sig
  type master_mutator

  type worker_mutator

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val add_leader : worker_mutator -> File_key.t -> File_key.t -> unit

  val add_merge_on_diff :
    (worker_mutator -> Context.t -> File_key.t Nel.t -> Xx.hash -> bool) Expensive.t

  val add_merge_on_exn :
    (options:Options.t -> worker_mutator -> File_key.t Nel.t -> bool) Expensive.t

  val revive_files : master_mutator -> Utils_js.FilenameSet.t -> unit
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

  let add_leader () leader_f f = LeaderHeap.add f leader_f

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
    let has_old_sig_cx = lazy (SigContextHeap.mem_old leader_f) in
    WorkerCancel.with_no_cancellations (fun () ->
        (* The component might not have an old sig context, but is still marked as unchanged.
         * This can happen when we load sig hashes from the saved state. Normally, if a component is
         * unchanged, we skip writing the sig cx and instead just revive the old one, but in this
         * case there is no old one so we have to write it. *)
        if diff || not (Lazy.force has_old_sig_cx) then (
          (* Ideally we'd assert that each file is a member of the oldified files too *)
          Nel.iter (add_leader () leader_f) component_files;
          add_sig_context ~audit leader_f (Context.sig_cx leader_cx);
          SigHashHeap.add leader_f xx
        ));
    diff

  let add_merge_on_exn ~audit ~options () component =
    (* Ideally we'd assert that leader_f is a member of the oldified files, but it's a little too
     * expensive to send the set of oldified files to the worker *)
    let leader_f = Nel.hd component in
    let ccx = Context.make_ccx () in
    let cx =
      let metadata = Context.metadata_of_options options in
      (* This context is only used to add *something* to the sighash when we encounter an unexpected
       * exception during typechecking. It doesn't really matter what we choose, so we might as well
       * make it the empty table. *)
      let aloc_table = lazy (ALoc.make_table leader_f) in
      let module_ref = Files.module_ref leader_f in
      Context.make ccx metadata leader_f aloc_table (Reason.OrdinaryName module_ref) Context.Merging
    in
    let module_refs =
      Nel.map
        (fun f ->
          let module_ref = Files.module_ref f in
          let module_t = Type.AnyT.locationless (Type.AnyError None) in
          Context.add_module cx (Reason.OrdinaryName module_ref) module_t;
          module_ref)
        component
    in
    let (xx, _) =
      let no_lowers _ r = Type.Unsoundness.merged_any r in
      Context_optimizer.reduce_context cx ~no_lowers (Nel.to_list module_refs)
    in
    add_merge_on_diff ~audit () cx component xx

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files !oldified_files));
    WorkerCancel.with_no_cancellations (fun () ->
        oldified_files := FilenameSet.diff !oldified_files files;
        revive_merge_batch files)
end

module type READER = sig
  type reader

  val find_sig : reader:reader -> File_key.t -> Context.sig_t

  val find_leader : reader:reader -> File_key.t -> File_key.t

  val sig_hash_opt : reader:reader -> File_key.t -> Xx.hash option

  val find_master : reader:reader -> Context.master_context
end

let find_sig ~get_sig ~reader:_ file =
  assert (file <> File_key.Builtins);
  match get_sig file with
  | Some cx -> cx
  | None -> raise (Key_not_found ("SigContextHeap", File_key.to_string file))

let find_master ~reader:_ =
  match !master_cx_ref with
  | Some master_cx -> master_cx
  | None ->
    begin
      match MasterContextHeap.get File_key.Builtins with
      | Some master_cx ->
        master_cx_ref := Some master_cx;
        master_cx
      | None -> raise (Key_not_found ("MasterContextHeap", "master context"))
    end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val sig_hash_changed : reader:reader -> File_key.t -> bool

  val leader_mem_old : reader:reader -> File_key.t -> bool
end = struct
  type reader = Mutator_state_reader.t

  let find_sig = find_sig ~get_sig:SigContextHeap.get

  let find_leader ~reader:_ file =
    match LeaderHeap.get file with
    | Some leader -> leader
    | None -> raise (Key_not_found ("LeaderHeap", File_key.to_string file))

  let find_master ~reader = find_master ~reader

  let sig_hash_opt ~reader:_ = SigHashHeap.get

  let leader_mem_old ~reader:_ = LeaderHeap.mem_old

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

  let sig_hash_opt ~reader:_ file =
    if should_use_oldified file then
      SigHashHeap.get_old file
    else
      SigHashHeap.get file

  let find_master ~reader = find_master ~reader
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

  let sig_hash_opt ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.sig_hash_opt ~reader
    | State_reader reader -> Reader.sig_hash_opt ~reader

  let find_master ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_master ~reader
    | State_reader reader -> Reader.find_master ~reader
end

module From_saved_state = struct
  let add_sig_hash = SigHashHeap.add
end
