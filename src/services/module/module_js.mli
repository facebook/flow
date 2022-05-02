(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* export and import functions for the module system *)
val exported_module : options:Options.t -> File_key.t -> Docblock.t -> string option

type resolution_acc = { mutable paths: SSet.t }

val imported_module :
  options:Options.t ->
  reader:Abstract_state_reader.t ->
  node_modules_containers:SSet.t SMap.t ->
  File_key.t ->
  ?resolution_acc:resolution_acc ->
  string ->
  Modulename.t

(* repick providers for dirty modules *)
val commit_modules :
  transaction:Transaction.t ->
  workers:MultiWorkerLwt.worker list option ->
  options:Options.t ->
  (* dirty modules *)
  Modulename.Set.t ->
  (* changed modules and duplicate providers *)
  (Modulename.Set.t * (File_key.t * File_key.t Nel.t) SMap.t) Lwt.t

(* filenames to error sets *)

(* resolve and add requires from context to store *)
val add_parsed_resolved_requires :
  mutator:Parsing_heaps.Resolved_requires_mutator.t ->
  reader:Mutator_state_reader.t ->
  options:Options.t ->
  node_modules_containers:SSet.t SMap.t ->
  File_key.t ->
  bool

val add_package : string -> (Package_json.t, 'a) result -> unit

type package_incompatible_reason =
  (* Didn't exist before, now it exists *)
  | New
  (* Was valid, now is invalid *)
  | Became_invalid
  (* Was invalid, now is valid *)
  | Became_valid
  (* The `name` property changed from the former to the latter *)
  | Name_changed of string option * string option
  (* The `main` property changed from the former to the latter *)
  | Main_changed of string option * string option
  | Unknown

val string_of_package_incompatible_reason : package_incompatible_reason -> string

type package_incompatible_return =
  | Compatible
  | Incompatible of package_incompatible_reason

val package_incompatible :
  reader:State_reader.t -> string -> (Package_json.t, _) result -> package_incompatible_return

(***************************************************)

val clear_filename_cache : unit -> unit
