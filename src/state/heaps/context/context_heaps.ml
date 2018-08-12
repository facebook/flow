(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module SigContextHeap = SharedMem_js.WithCache (File_key) (struct
  type t = Context.sig_t
  let prefix = Prefix.make()
  let description = "SigContext"
  let use_sqlite_fallback () = false
end)

let master_sig: Context.sig_t option option ref = ref None

let add_sig_context = Expensive.wrap SigContextHeap.add

let add_sig ~audit cx =
  let cx_file = Context.file cx in
  if cx_file = File_key.Builtins then master_sig := None;
  add_sig_context ~audit cx_file (Context.sig_cx cx)

let find_sig file =
  let cx_opt =
    if file = File_key.Builtins then
      match !master_sig with
      | Some cx_opt -> cx_opt
      | None ->
        let cx_opt = SigContextHeap.get file in
        master_sig := Some cx_opt;
        cx_opt
    else SigContextHeap.get file
  in
  match cx_opt with
  | Some cx -> cx
  | None -> raise (Key_not_found ("SigContextHeap", File_key.to_string file))

module SigHashHeap = SharedMem_js.NoCache (File_key) (struct
  type t = Xx.hash
  let prefix = Prefix.make()
  let description = "SigHash"
  let use_sqlite_fallback () = false
end)

module LeaderHeap = SharedMem_js.WithCache (File_key) (struct
  type t = File_key.t
  let prefix = Prefix.make()
  let description = "Leader"
  let use_sqlite_fallback () = false
end)

let find_leader file =
  match LeaderHeap.get file with
  | Some leader -> leader
  | None -> raise (Key_not_found ("LeaderHeap", (File_key.to_string file)))

let sig_hash_changed f =
  match SigHashHeap.get f with
  | None -> false
  | Some xx ->
    match SigHashHeap.get_old f with
    | None -> true
    | Some xx_old ->
      File_key.check_suffix f Files.flow_ext || xx <> xx_old

let oldify_merge_batch files =
  LeaderHeap.oldify_batch files;
  SigContextHeap.oldify_batch files;
  SigHashHeap.oldify_batch files

let remove_old_merge_batch files =
  LeaderHeap.remove_old_batch files;
  SigContextHeap.remove_old_batch files;
  SigHashHeap.remove_old_batch files;
  SharedMem_js.collect `gentle

let revive_merge_batch files =
  LeaderHeap.revive_batch files;
  SigContextHeap.revive_batch files;
  SigHashHeap.revive_batch files

module Init_master_context_mutator: sig
  val add_master_sig: (Context.t -> unit) Expensive.t
end = struct
  let add_master_sig ~audit cx =
    add_sig ~audit cx
end

module Merge_context_mutator: sig
  type master_mutator
  type worker_mutator
  val create: Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator
  val add_merge_on_diff:
    (worker_mutator -> Context.t -> File_key.t Nel.t -> Xx.hash -> unit) Expensive.t
  val add_merge_on_exn:
    (worker_mutator -> options:Options.t -> File_key.t Nel.t -> unit) Expensive.t
  val revive_files: master_mutator -> Utils_js.FilenameSet.t -> unit
end = struct
  type master_mutator = Utils_js.FilenameSet.t ref
  type worker_mutator = unit

  let commit oldified_files =
    Hh_logger.debug "Committing context heaps";
    remove_old_merge_batch oldified_files;
    Lwt.return_unit

  let rollback oldified_files =
    Hh_logger.debug "Rolling back context heaps";
    revive_merge_batch oldified_files;
    Lwt.return_unit

  let create transaction files =
    let master_mutator = ref files in
    let worker_mutator = () in

    let commit () = commit (!master_mutator) in
    let rollback () = rollback (!master_mutator) in

    oldify_merge_batch files;
    Transaction.add ~singleton:"Merge_context" ~commit ~rollback transaction;

    master_mutator, worker_mutator


  (* While merging, we must keep LeaderHeap, SigContextHeap, and SigHashHeap in
     sync, sometimes creating new entries and sometimes reusing old entries. *)

  (* Add a sig only if it has not changed meaningfully, and return the result of
     that check. *)
  let add_merge_on_diff ~audit () leader_cx component_files xx =
    let leader_f = Context.file leader_cx in
    (* Ideally we'd assert that leader_f is a member of the oldified files, but it's a little too
     * expensive to send the set of oldified files to the worker *)
    let diff = match SigHashHeap.get_old leader_f with
    | None -> true
    | Some xx_old ->
      File_key.check_suffix leader_f Files.flow_ext || xx <> xx_old
    in
    if diff then (
      Nel.iter (fun f ->
        (* Ideally we'd assert that f is a member of the oldified files too *)
        LeaderHeap.add f leader_f
      ) component_files;
      add_sig_context ~audit leader_f (Context.sig_cx leader_cx);
      SigHashHeap.add leader_f xx
    )

  let add_merge_on_exn ~audit () ~options component =
    let leader_f = Nel.hd component in
    (* Ideally we'd assert that leader_f is a member of the oldified files, but it's a little too
     * expensive to send the set of oldified files to the worker *)
    let sig_cx = Context.make_sig () in
    let cx =
      let metadata = Context.metadata_of_options options in
      let module_ref = Files.module_ref leader_f in
      Context.make sig_cx metadata leader_f module_ref
    in
    let module_refs = List.map (fun f ->
      let module_ref = Files.module_ref f in
      let module_t = Type.Locationless.AnyT.t in
      Context.add_module cx module_ref module_t;
      (* Ideally we'd assert that f is a member of the oldified files too *)
      LeaderHeap.add f leader_f;
      module_ref
    ) (Nel.to_list component) in
    let xx = Merge_js.ContextOptimizer.sig_context cx module_refs in
    add_sig_context ~audit leader_f sig_cx;
    SigHashHeap.add leader_f xx

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files (!oldified_files)));
    oldified_files := FilenameSet.diff (!oldified_files) files;
    revive_merge_batch files
end
