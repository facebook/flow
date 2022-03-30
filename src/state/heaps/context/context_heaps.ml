(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
      (Expensive.wrap MasterContextHeap.add) ~audit File_key.Builtins master_context
  )

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
      SigHashHeap.oldify_batch files
  )

let remove_old_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.remove_old_batch files;
      SigHashHeap.remove_old_batch files
  )

let revive_merge_batch files =
  WorkerCancel.with_no_cancellations (fun () ->
      LeaderHeap.revive_batch files;
      SigHashHeap.revive_batch files
  )

module Init_master_context_mutator : sig
  val add_master : (Context.t -> unit) Expensive.t
end = struct
  let add_master ~audit cx = add_master ~audit cx
end

let currently_oldified_files : FilenameSet.t ref = ref FilenameSet.empty

module Merge_context_mutator : sig
  type master_mutator

  type worker_mutator

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val add_merge_on_diff : worker_mutator -> File_key.t Nel.t -> Xx.hash -> bool

  val add_merge_on_exn : worker_mutator -> File_key.t Nel.t -> bool

  val revive_files : master_mutator -> Utils_js.FilenameSet.t -> unit
end = struct
  type master_mutator = unit

  type worker_mutator = unit

  let commit () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing context heaps";
        remove_old_merge_batch !currently_oldified_files;
        currently_oldified_files := FilenameSet.empty
    );
    Lwt.return_unit

  let rollback () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back context heaps";
        revive_merge_batch !currently_oldified_files;
        currently_oldified_files := FilenameSet.empty
    );
    Lwt.return_unit

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_files := files;

        oldify_merge_batch files;
        Transaction.add ~singleton:"Merge_context" ~commit ~rollback transaction;

        ((), ())
    )

  (* While merging, we must keep LeaderHeap and SigHashHeap in sync, sometimes
   * creating new entries and sometimes reusing old entries. *)

  let add_merge_on_diff () component xx =
    let leader_f = Nel.hd component in
    let diff =
      match SigHashHeap.get_old leader_f with
      | None -> true
      | Some xx_old -> xx <> xx_old
    in
    WorkerCancel.with_no_cancellations (fun () ->
        if diff then (
          (* Ideally we'd assert that each file is a member of the oldified files too *)
          Nel.iter (fun f -> LeaderHeap.add f leader_f) component;
          SigHashHeap.add leader_f xx
        )
    );
    diff

  let add_merge_on_exn () component =
    let leader_f = Nel.hd component in
    WorkerCancel.with_no_cancellations (fun () ->
        Nel.iter (fun f -> LeaderHeap.add f leader_f) component
    );
    true

  let revive_files () files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files !currently_oldified_files));
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_files := FilenameSet.diff !currently_oldified_files files;
        revive_merge_batch files
    )
end

module type READER = sig
  type reader

  val find_leader : reader:reader -> File_key.t -> File_key.t

  val find_leader_opt : reader:reader -> File_key.t -> File_key.t option

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
end = struct
  type reader = Mutator_state_reader.t

  let find_leader_opt ~reader:_ file = LeaderHeap.get file

  let find_leader ~reader file =
    match find_leader_opt ~reader file with
    | Some leader -> leader
    | None -> raise (Key_not_found ("LeaderHeap", File_key.to_string file))

  let find_master ~reader = find_master ~reader

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

  let should_use_oldified file = FilenameSet.mem file !currently_oldified_files

  let find_leader_opt ~reader:_ file =
    if should_use_oldified file then
      LeaderHeap.get_old file
    else
      LeaderHeap.get file

  let find_leader ~reader file =
    match find_leader_opt ~reader file with
    | Some leader -> leader
    | None -> raise (Key_not_found ("LeaderHeap", File_key.to_string file))

  let find_master ~reader = find_master ~reader
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let find_leader_opt ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_leader_opt ~reader
    | State_reader reader -> Reader.find_leader_opt ~reader

  let find_leader ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_leader ~reader
    | State_reader reader -> Reader.find_leader ~reader

  let find_master ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_master ~reader
    | State_reader reader -> Reader.find_master ~reader
end
