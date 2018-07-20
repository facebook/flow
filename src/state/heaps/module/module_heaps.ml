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

(* Subset of a file's context, with the important distinction that module
   references in the file have been resolved to module names. *)
(** TODO [perf] Make resolved_requires tighter. For info:
    (1) checked? We know that requires and phantom dependents for unchecked
    files are empty.

    (2) parsed? We only care about the module provided by an unparsed file, but
    that's probably guessable.
**)
type resolved_requires = {
  resolved_modules: Modulename.t SMap.t; (* map from module references in file
                                            to module names they resolve to *)
  phantom_dependents: SSet.t; (* set of paths that were looked up but not found
                                 when resolving module references in the file:
                                 when the paths come into existence, the module
                                 references need to be re-resolved. *)
}


(****************** shared dependency map *********************)

(* map from filename to resolved requires *)
module ResolvedRequiresHeap = SharedMem_js.WithCache (File_key) (struct
  type t = resolved_requires
  let prefix = Prefix.make()
  let description = "ResolvedRequires"
  let use_sqlite_fallback () = false
end)

let get_resolved_requires_unsafe = Expensive.wrap (fun f ->
  match ResolvedRequiresHeap.get f with
  | Some resolved_requires -> resolved_requires
  | None -> failwith
      (Printf.sprintf "resolved requires not found for file %s" (File_key.to_string f))
)

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

module Resolved_requires_mutator: sig
  type t
  val create: Transaction.t -> Utils_js.FilenameSet.t -> t
  val add_resolved_requires: t -> File_key.t -> resolved_requires -> unit
end = struct
  type t = unit

  let commit files =
    Hh_logger.debug "Committing ResolvedRequiresHeap";
    ResolvedRequiresHeap.remove_old_batch files

  let rollback files =
    Hh_logger.debug "Rolling back ResolvedRequiresHeap";
    ResolvedRequiresHeap.revive_batch files

  let create transaction oldified_files =
    ResolvedRequiresHeap.oldify_batch oldified_files;
    Transaction.add
      ~commit:(fun () -> Lwt.return (commit oldified_files))
      ~rollback:(fun () -> Lwt.return (rollback oldified_files))
      transaction

  (* This function runs on a worker process. Ideally, we'd assert that file is a member of
   * oldified_files, but for init and large rechecks this would involve sending a very large
   * set to the workers, which is really slow. *)
  let add_resolved_requires () file resolved_requires =
    ResolvedRequiresHeap.add file resolved_requires
end

module FromSavedState = struct
  let add_resolved_requires = ResolvedRequiresHeap.add
end
