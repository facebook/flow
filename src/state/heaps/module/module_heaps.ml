(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  file_key: File_key.t;
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
[@@deriving show]

let mk_resolved_requires file_key ~resolved_modules ~phantom_dependents =
  let state = Xx.init 0L in
  SMap.iter
    (fun reference modulename ->
      Xx.update state reference;
      Xx.update state (Modulename.to_string modulename))
    resolved_modules;
  SSet.iter (Xx.update state) phantom_dependents;
  { file_key; resolved_modules; phantom_dependents; hash = Xx.digest state }

module ResolvedRequiresHeap =
  SharedMem.NoCacheTag
    (File_key)
    (struct
      type t = resolved_requires

      let description = "ResolvedRequires"
    end)
    (struct
      let value = SharedMem.Serialized_resolved_requires
    end)

(*********************************** Mutators *********************************)

let currently_oldified_resolved_requires : Utils_js.FilenameSet.t ref =
  ref Utils_js.FilenameSet.empty

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires : t -> File_key.t -> resolved_requires -> bool
end = struct
  type t = unit

  let commit () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing ResolvedRequiresHeap";
        ResolvedRequiresHeap.remove_old_batch !currently_oldified_resolved_requires;
        currently_oldified_resolved_requires := Utils_js.FilenameSet.empty
    );
    Lwt.return_unit

  let rollback () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back ResolvedRequiresHeap";
        ResolvedRequiresHeap.revive_batch !currently_oldified_resolved_requires;
        currently_oldified_resolved_requires := Utils_js.FilenameSet.empty
    );
    Lwt.return_unit

  let create transaction oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_resolved_requires := oldified_files;
        ResolvedRequiresHeap.oldify_batch oldified_files;
        Transaction.add ~commit ~rollback transaction
    )

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

(*********************************** Readers **********************************)

module type READER = sig
  type reader

  val get_resolved_requires_unsafe : reader:reader -> (File_key.t -> resolved_requires) Expensive.t
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t = struct
  type reader = Mutator_state_reader.t

  let get_resolved_requires_unsafe ~reader:_ =
    Expensive.wrap (fun f ->
        match ResolvedRequiresHeap.get f with
        | Some resolved_requires -> resolved_requires
        | None ->
          failwith (Printf.sprintf "resolved requires not found for file %s" (File_key.to_string f))
    )
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_old_resolved_requires f =
    Utils_js.FilenameSet.mem f !currently_oldified_resolved_requires

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
          failwith (Printf.sprintf "resolved requires not found for file %s" (File_key.to_string f))
    )
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_resolved_requires_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_resolved_requires_unsafe ~reader
    | State_reader reader -> Reader.get_resolved_requires_unsafe ~reader
end

(******************** APIs for saving/loading saved state *********************)

module From_saved_state = struct
  let add_resolved_requires = ResolvedRequiresHeap.add
end

let iter_resolved_requires = ResolvedRequiresHeap.iter
