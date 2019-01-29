(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : Options.t;
    workers          : MultiWorkerLwt.worker list option;
  }

(*****************************************************************************)
(* The environment constantly maintained by the server *)
(*****************************************************************************)

type errors = {
  (* errors are stored in a map from file path to error set, so that the errors
     from checking particular files can be cleared during recheck. *)
  local_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  (* errors encountered during merge have to be stored separately so
     dependencies can be cleared during merge. *)
  merge_errors: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  (* error suppressions in the code *)
  suppressions: Error_suppressions.t;
  (* lint severity settings in the code *)
  severity_cover_set: ExactCover.lint_severity_cover Utils_js.FilenameMap.t;
}

type collated_errors = {
  collated_errorset: Errors.ConcreteLocErrorSet.t;
  collated_warning_map: Errors.ConcreteLocErrorSet.t Utils_js.FilenameMap.t;
  collated_suppressed_errors: (Loc.t Errors.error * Utils_js.LocSet.t) list;
}

type env = {
    (* All the files that we at least parse. *)
    files: Utils_js.FilenameSet.t;
    (* All the files that we at least parse. *)
    dependency_info: Dependency_info.t;
    (* All the current files we typecheck. *)
    checked_files: CheckedSet.t;
    ordered_libs: string list; (* The lib files, in their merge order *)
    libs: SSet.t; (* a subset of `files` *)
    (* The files which didn't parse (skipped or errored) *)
    unparsed: Utils_js.FilenameSet.t;
    errors: errors;
    collated_errors: collated_errors option ref;
    connections: Persistent_connection.t;
}
