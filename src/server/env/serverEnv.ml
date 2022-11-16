(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
  options: Options.t;
  workers: MultiWorkerLwt.worker list option;
}

(*****************************************************************************)
(* The environment constantly maintained by the server *)
(*****************************************************************************)

(* Do not change these to contain `Loc.t`s. Because these errors are stored between rechecks, it
 * is critical that they contain `ALoc.t`s, so that we can update the concrete locations when we
 * render the errors, without having to retypecheck the files that generated those errors. *)
type errors = {
  (* errors are stored in a map from file path to error set, so that the errors
     from checking particular files can be cleared during recheck. *)
  local_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  (* duplicate providers found during commit_modules are stored separately so
     they can be cleared easily *)
  duplicate_providers: (File_key.t * File_key.t Nel.t) SMap.t;
  (* errors encountered during merge have to be stored separately so
     dependencies can be cleared during merge. *)
  merge_errors: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  (* warnings are stored in a map from file path to error set, so that the warnings
     from checking particular files can be cleared during recheck. *)
  warnings: Flow_error.ErrorSet.t Utils_js.FilenameMap.t;
  (* error suppressions in the code *)
  suppressions: Error_suppressions.t;
}

type collated_errors = {
  collated_errorset: Errors.ConcreteLocPrintableErrorSet.t;
  collated_warning_map: Errors.ConcreteLocPrintableErrorSet.t Utils_js.FilenameMap.t;
  collated_suppressed_errors: (Loc.t Errors.printable_error * Loc_collections.LocSet.t) list;
}

type env = {
  files: Utils_js.FilenameSet.t;  (** All the files that we at least parse (includes libs). *)
  dependency_info: Dependency_info.t;
  checked_files: CheckedSet.t;  (** All the current files we typecheck. *)
  package_json_files: Utils_js.FilenameSet.t;  (** package.json files *)
  ordered_libs: string list;  (** The lib files, in their merge order *)
  libs: SSet.t;  (** The lib files as a set *)
  unparsed: Utils_js.FilenameSet.t;  (** The files which didn't parse (skipped or errored) *)
  errors: errors;
  coverage: Coverage_response.file_coverage Utils_js.FilenameMap.t;
  collated_errors: collated_errors option ref;
  connections: Persistent_connection.t;
  exports: Export_search.t;
}
