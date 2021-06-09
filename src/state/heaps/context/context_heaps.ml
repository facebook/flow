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

let master_cx_ref : Context.master_context option ref = ref None

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
      SigHashHeap.oldify_batch files)

let remove_old_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.remove_old_batch files;
      SigHashHeap.remove_old_batch files)

let revive_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.revive_batch files;
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

  val add_merge_on_diff : worker_mutator -> File_key.t Nel.t -> Xx.hash -> bool

  val add_merge_on_exn : worker_mutator -> File_key.t Nel.t -> bool

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

  (* While merging, we must keep LeaderHeap and SigHashHeap in sync, sometimes
   * creating new entries and sometimes reusing old entries. *)

  let add_merge_on_diff () component xx =
    let leader_f = Nel.hd component in
    let diff =
      match SigHashHeap.get_old leader_f with
      | None -> true
      | Some xx_old -> xx <> xx_old
    in
    let has_old_state = lazy (LeaderHeap.mem_old leader_f) in
    WorkerCancel.with_no_cancellations (fun () ->
        (* The component might not have old data in the heap, but is still marked as unchanged.
         * This can happen when we load sig hashes from the saved state. Normally, if a component is
         * unchanged, we skip writing the sig cx and instead just revive the old one, but in this
         * case there is no old one so we have to write it. *)
        if diff || not (Lazy.force has_old_state) then (
          (* Ideally we'd assert that each file is a member of the oldified files too *)
          Nel.iter (fun f -> LeaderHeap.add f leader_f) component;
          SigHashHeap.add leader_f xx
        ));
    diff

  let add_merge_on_exn () component =
    let leader_f = Nel.hd component in
    WorkerCancel.with_no_cancellations (fun () ->
        Nel.iter (fun f -> LeaderHeap.add f leader_f) component);
    true

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files !oldified_files));
    WorkerCancel.with_no_cancellations (fun () ->
        oldified_files := FilenameSet.diff !oldified_files files;
        revive_merge_batch files)
end

module type READER = sig
  type reader

  val find_leader : reader:reader -> File_key.t -> File_key.t

  val sig_hash_opt : reader:reader -> File_key.t -> Xx.hash option

  val find_master : reader:reader -> Context.master_context
end

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
