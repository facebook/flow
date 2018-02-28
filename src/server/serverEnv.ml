(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The "static" environment, initialized first and then doesn't change *)
(*****************************************************************************)

type genv = {
    options          : Options.t;
    workers          : MultiWorker.worker list option;
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
  suppressions: Error_suppressions.t Utils_js.FilenameMap.t;
  (* lint severity settings in the code *)
  severity_cover_set: ExactCover.lint_severity_cover Utils_js.FilenameMap.t;
}

type collated_errors = {
  collated_errorset: Errors.ErrorSet.t;
  collated_warning_map: Errors.ErrorSet.t Utils_js.FilenameMap.t;
  collated_suppressed_errors: (Errors.error * Utils_js.LocSet.t) list;
}

type env = {
    (* All the files that we at least parse. *)
    files: Utils_js.FilenameSet.t;
    (* All the current files we typecheck. *)
    checked_files: CheckedSet.t;
    libs: SSet.t; (* a subset of `files` *)
    errors: errors;
    collated_errors: collated_errors option ref;
    connections: Persistent_connection.t;
}
