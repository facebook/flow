(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(********************************** Name Heap *********************************)
(* Maps module names to the filenames which provide those modules             *)

module NameHeap =
  SharedMem.WithCache
    (Modulename.Key)
    (struct
      type t = File_key.t

      let description = "Name"
    end)

(*************************** Resolved Requires Heap ***************************)
(* Maps filenames to which other modules they require                         *)

(* Subset of a file's context, with the important distinction that module
   references in the file have been resolved to module names. *)

(** TODO [perf] Make resolved_requires tighter. For info:
    (1) checked? We know that requires and phantom dependents for unchecked
    files are empty.

    (2) parsed? We only care about the module provided by an unparsed file, but
    that's probably guessable.
 **)
type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  (* map from module references in file
     to module names they resolve to *)
  phantom_dependents: SSet.t;
  (* set of paths that were looked up but not found
     when resolving module references in the file:
     when the paths come into existence, the module
     references need to be re-resolved. *)
  hash: Xx.hash; (* An easy way to compare two resolved_requires to see if they've changed *)
}

let mk_resolved_requires ~resolved_modules ~phantom_dependents =
  let state = Xx.init 0L in
  SMap.iter
    (fun reference modulename ->
      Xx.update state reference;
      Xx.update state (Modulename.to_string modulename))
    resolved_modules;
  SSet.iter (Xx.update state) phantom_dependents;
  { resolved_modules; phantom_dependents; hash = Xx.digest state }

module ResolvedRequiresHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = resolved_requires

      let description = "ResolvedRequires"
    end)

(********************************** Info Heap *********************************)

type info = {
  module_name: Modulename.t;
  checked: bool;  (** in flow? *)
  parsed: bool;  (** if false, it's a tracking record only *)
}

(** Maps filenames to info about a module, including the module's name.
    note: currently we may have many files for one module name. this is an issue. *)
module InfoHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = info

      let description = "Info"
    end)

(*********************************** Mutators *********************************)

let currently_oldified_nameheap_modulenames : Modulename.Set.t ref option ref = ref None

module Commit_modules_mutator : sig
  type t

  val create : Transaction.t -> is_init:bool -> t

  val remove_and_replace :
    t ->
    workers:MultiWorkerLwt.worker list option ->
    to_remove:Modulename.Set.t ->
    to_replace:(Modulename.t * File_key.t) list ->
    unit Lwt.t
end = struct
  type t = {
    is_init: bool;
    changed_files: Modulename.Set.t ref;
  }

  let commit mutator =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing NameHeap";
        if not mutator.is_init then NameHeap.remove_old_batch !(mutator.changed_files);
        currently_oldified_nameheap_modulenames := None);
    Lwt.return_unit

  let rollback mutator =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back NameHeap";
        if not mutator.is_init then NameHeap.revive_batch !(mutator.changed_files);
        currently_oldified_nameheap_modulenames := None);
    Lwt.return_unit

  let create transaction ~is_init =
    WorkerCancel.with_no_cancellations (fun () ->
        let changed_files = ref Modulename.Set.empty in
        currently_oldified_nameheap_modulenames := Some changed_files;
        let mutator = { changed_files; is_init } in
        let commit () = commit mutator in
        let rollback () = rollback mutator in
        Transaction.add ~singleton:"Commit_modules" ~commit ~rollback transaction;
        mutator)

  let remove_and_replace mutator ~workers ~to_remove ~to_replace =
    (* During init we don't need to worry about oldifying, reviving, or removing old entries *)
    if not mutator.is_init then (
      (* Verify there are no files we're both trying to remove and replace
       * - Note, to_replace may be a VERY LARGE list so avoid non-tail-recursive calls *)
      let to_replace_set =
        List.fold_left (fun set (f, _) -> Modulename.Set.add f set) Modulename.Set.empty to_replace
      in
      (* to_remove_set and to_replace_set should be disjoint sets *)
      let changed_files = Modulename.Set.union to_remove to_replace_set in
      mutator.changed_files := changed_files;

      (* Save the old data *)
      NameHeap.oldify_batch changed_files
    );

    (* Remove *)
    NameHeap.remove_batch to_remove;

    (* Replace *)
    MultiWorkerLwt.call
      workers
      ~job:(fun () to_replace -> List.iter (fun (m, f) -> NameHeap.add m f) to_replace)
      ~neutral:()
      ~merge:(fun () () -> ())
      ~next:(MultiWorkerLwt.next workers to_replace)
end

let currently_oldified_resolved_requires : Utils_js.FilenameSet.t ref =
  ref Utils_js.FilenameSet.empty

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires : t -> File_key.t -> resolved_requires -> bool
end = struct
  type t = unit

  (* We actually may have multiple Resolved_requires_mutator's in a single transaction. So we need to
   * assert that they never interfere with each other *)
  let active_files = currently_oldified_resolved_requires

  let commit files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing ResolvedRequiresHeap";
        active_files := Utils_js.FilenameSet.diff !active_files files;
        ResolvedRequiresHeap.remove_old_batch files)

  let rollback files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back ResolvedRequiresHeap";
        active_files := Utils_js.FilenameSet.diff !active_files files;
        ResolvedRequiresHeap.revive_batch files)

  let create transaction oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        if
          not
            (Utils_js.FilenameSet.is_empty
               (Utils_js.FilenameSet.inter oldified_files !active_files))
        then
          failwith "Multiple Resolved_requires_mutator's operating on the same files";
        active_files := Utils_js.FilenameSet.union oldified_files !active_files;

        ResolvedRequiresHeap.oldify_batch oldified_files;
        Transaction.add
          ~commit:(fun () -> Lwt.return (commit oldified_files))
          ~rollback:(fun () -> Lwt.return (rollback oldified_files))
          transaction)

  (* This function runs on a worker process. Ideally, we'd assert that file is a member of
   * oldified_files, but for init and large rechecks this would involve sending a very large
   * set to the workers, which is really slow.
   *
   * It returns true if the resolved requires changed and false otherwise *)
  let add_resolved_requires () file resolved_requires =
    ResolvedRequiresHeap.add file resolved_requires;

    (* Check to see if the resolved requires changed at all with this addition *)
    match ResolvedRequiresHeap.get_old file with
    | None -> true
    | Some old_resolve_requires -> old_resolve_requires.hash <> resolved_requires.hash
end

let currently_oldified_infoheap_files : Utils_js.FilenameSet.t option ref = ref None

module Introduce_files_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_info : t -> File_key.t -> info -> unit
end = struct
  type t = unit

  let commit oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing InfoHeap";
        InfoHeap.remove_old_batch oldified_files;
        currently_oldified_infoheap_files := None);
    Lwt.return_unit

  let rollback oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back InfoHeap";
        InfoHeap.revive_batch oldified_files;
        currently_oldified_infoheap_files := None);
    Lwt.return_unit

  let create transaction oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_infoheap_files := Some oldified_files;
        InfoHeap.oldify_batch oldified_files;
        let commit () = commit oldified_files in
        let rollback () = rollback oldified_files in
        Transaction.add ~singleton:"Introduce_files" ~commit ~rollback transaction)

  (* Ideally we'd assert that file is in oldified_files, but passing through the oldified_files set
   * to the worker process which calls add_info is kind of expensive *)
  let add_info () file info = InfoHeap.add file info
end

(*********************************** Readers **********************************)

module type READER = sig
  type reader

  val get_file : reader:reader -> (Modulename.t -> File_key.t option) Expensive.t

  val get_file_unsafe : reader:reader -> (Modulename.t -> File_key.t) Expensive.t

  val module_exists : reader:reader -> Modulename.t -> bool

  val get_resolved_requires_unsafe : reader:reader -> (File_key.t -> resolved_requires) Expensive.t

  (** given a filename, returns module info *)
  val get_info_unsafe : reader:reader -> (File_key.t -> info) Expensive.t

  val get_info : reader:reader -> (File_key.t -> info option) Expensive.t

  val is_tracked_file : reader:reader -> File_key.t -> bool
end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_info : reader:reader -> (File_key.t -> info option) Expensive.t
end = struct
  type reader = Mutator_state_reader.t

  let get_file ~reader:_ = Expensive.wrap NameHeap.get

  let module_exists ~reader:_ = NameHeap.mem

  let get_file_unsafe ~reader ~audit m =
    match get_file ~reader ~audit m with
    | Some file -> file
    | None -> failwith (Printf.sprintf "file name not found for module %s" (Modulename.to_string m))

  let get_resolved_requires_unsafe ~reader:_ =
    Expensive.wrap (fun f ->
        match ResolvedRequiresHeap.get f with
        | Some resolved_requires -> resolved_requires
        | None ->
          failwith (Printf.sprintf "resolved requires not found for file %s" (File_key.to_string f)))

  let get_info ~reader:_ = Expensive.wrap InfoHeap.get

  let get_old_info ~reader:_ = Expensive.wrap InfoHeap.get_old

  let get_info_unsafe ~reader ~audit f =
    match get_info ~reader ~audit f with
    | Some info -> info
    | None -> failwith (Printf.sprintf "module info not found for file %s" (File_key.to_string f))

  let is_tracked_file ~reader:_ = InfoHeap.mem
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_old_nameheap key =
    match !currently_oldified_nameheap_modulenames with
    | None -> false
    | Some oldified_modulenames -> Modulename.Set.mem key !oldified_modulenames

  let should_use_old_resolved_requires f =
    Utils_js.FilenameSet.mem f !currently_oldified_resolved_requires

  let should_use_old_infoheap f =
    match !currently_oldified_infoheap_files with
    | None -> false
    | Some oldified_files -> Utils_js.FilenameSet.mem f oldified_files

  let get_file ~reader:_ ~audit key =
    if should_use_old_nameheap key then
      Expensive.wrap NameHeap.get_old ~audit key
    else
      Expensive.wrap NameHeap.get ~audit key

  let module_exists ~reader:_ key =
    if should_use_old_nameheap key then
      NameHeap.mem_old key
    else
      NameHeap.mem key

  let get_file_unsafe ~reader ~audit m =
    match get_file ~reader ~audit m with
    | Some file -> file
    | None -> failwith (Printf.sprintf "file name not found for module %s" (Modulename.to_string m))

  let get_resolved_requires_unsafe ~reader:_ =
    Expensive.wrap (fun f ->
        let resolved_requires =
          if should_use_old_resolved_requires f then
            ResolvedRequiresHeap.get_old f
          else
            ResolvedRequiresHeap.get f
        in
        match resolved_requires with
        | Some resolved_requires -> resolved_requires
        | None ->
          failwith (Printf.sprintf "resolved requires not found for file %s" (File_key.to_string f)))

  let get_info ~reader:_ ~audit f =
    if should_use_old_infoheap f then
      Expensive.wrap InfoHeap.get_old ~audit f
    else
      Expensive.wrap InfoHeap.get ~audit f

  let get_info_unsafe ~reader ~audit f =
    match get_info ~reader ~audit f with
    | Some info -> info
    | None -> failwith (Printf.sprintf "module info not found for file %s" (File_key.to_string f))

  let is_tracked_file ~reader:_ f =
    if should_use_old_infoheap f then
      InfoHeap.mem_old f
    else
      InfoHeap.mem f
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_file ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file ~reader
    | State_reader reader -> Reader.get_file ~reader

  let module_exists ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.module_exists ~reader
    | State_reader reader -> Reader.module_exists ~reader

  let get_file_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_unsafe ~reader
    | State_reader reader -> Reader.get_file_unsafe ~reader

  let get_resolved_requires_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_resolved_requires_unsafe ~reader
    | State_reader reader -> Reader.get_resolved_requires_unsafe ~reader

  let get_info ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_info ~reader
    | State_reader reader -> Reader.get_info ~reader

  let get_info_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_info_unsafe ~reader
    | State_reader reader -> Reader.get_info_unsafe ~reader

  let is_tracked_file ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.is_tracked_file ~reader
    | State_reader reader -> Reader.is_tracked_file ~reader
end

(******************** APIs for saving/loading saved state *********************)

module From_saved_state = struct
  let add_resolved_requires = ResolvedRequiresHeap.add
end
