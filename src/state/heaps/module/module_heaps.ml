(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* map from module name to filename *)
module NameHeap = SharedMem_js.WithCache (Modulename.Key) (struct
  type t = File_key.t
  let prefix = Prefix.make()
  let description = "Name"
  let use_sqlite_fallback () = false
end)

let get_file = Expensive.wrap NameHeap.get
let module_exists = NameHeap.mem

let get_file_unsafe ~audit m =
  match get_file ~audit m with
  | Some file -> file
  | None -> failwith
      (Printf.sprintf "file name not found for module %s" (Modulename.to_string m))

module Commit_modules_mutator: sig
  type t
  val create: Transaction.t -> is_init:bool -> t
  val remove_and_replace:
    t ->
    workers:MultiWorkerLwt.worker list option ->
    to_remove:Modulename.Set.t ->
    to_replace:(Modulename.t *  File_key.t) list ->
    unit Lwt.t
end = struct
  type t' = {
    is_init: bool;
    changed_files: Modulename.Set.t;
  }
  type t = t' ref

  let commit mutator =
    Hh_logger.debug "Committing NameHeap";
    if not mutator.is_init
    then NameHeap.remove_old_batch mutator.changed_files;
    Lwt.return_unit

  let rollback mutator =
    Hh_logger.debug "Rolling back NameHeap";
    if not mutator.is_init
    then NameHeap.revive_batch mutator.changed_files;
    Lwt.return_unit

  let create transaction ~is_init =
    let mutator = ref { changed_files = Modulename.Set.empty; is_init; } in
    let commit () = commit (!mutator) in
    let rollback () = rollback (!mutator) in
    Transaction.add ~commit ~rollback transaction;
    mutator

  let remove_and_replace mutator ~workers ~to_remove ~to_replace =
    (* During init we don't need to worry about oldifying, reviving, or removing old entries *)
    if not !mutator.is_init
    then begin
      (* Verify there are no files we're both trying to remove and replace
       * - Note, to_replace may be a VERY LARGE list so avoid non-tail-recursive calls *)
      let to_replace_set =
        List.fold_left (fun set (f, _) -> Modulename.Set.add f set) Modulename.Set.empty to_replace
      in

      (* to_remove_set and to_replace_set should be disjoint sets *)
      let changed_files = Modulename.Set.union to_remove to_replace_set in
      mutator := { !mutator with changed_files; };

      (* Save the old data *)
      NameHeap.oldify_batch changed_files;
    end;

    (* Remove *)
    NameHeap.remove_batch to_remove;

    (* Replace *)
    MultiWorkerLwt.call
      workers
      ~job: (fun () to_replace -> List.iter (fun (m, f) -> NameHeap.add m f) to_replace)
      ~neutral: ()
      ~merge: (fun () () -> ())
      ~next: (MultiWorkerLwt.next workers to_replace)
end
