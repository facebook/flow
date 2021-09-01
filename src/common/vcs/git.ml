(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Vcs_utils

type error_status = Vcs_utils.error_status =
  | Not_installed of { path: string }
  | Errored of string

let git ?cwd args = exec ?cwd "git" args

(** [merge_base a b] returns the hash of the common ancestor of [a] and [b].

    See https://git-scm.com/docs/git-merge-base for caveats. In particular,
    if [a] and [b] have multiple common ancestors, which one git returns
    is undefined (we don't pass [--all]). We also don't pass [--fork-point],
    so we may return "upstream" changes if history was rewritten. *)
let merge_base ?cwd a b =
  match%lwt git ?cwd ["merge-base"; a; b] with
  | Ok stdout ->
    let result =
      try Ok (String.sub stdout 0 40) with
      | Invalid_argument _ -> Error (Errored "Malformed merge-base")
    in
    Lwt.return result
  | Error _ as err -> Lwt.return err

let files_changed_since ?cwd hash =
  match%lwt git ?cwd ["diff"; "-z"; "--name-only"; hash] with
  | Ok stdout -> Lwt.return (Ok (split_null_terminated_lines stdout))
  | Error _ as err -> Lwt.return err

let files_changed_since_mergebase_with ?cwd commit =
  match%lwt merge_base ?cwd commit "HEAD" with
  | Error _ as err -> Lwt.return err
  | Ok mergebase ->
    (match%lwt files_changed_since ?cwd mergebase with
    | Ok changes -> Lwt.return (Ok (mergebase, changes))
    | Error _ as err -> Lwt.return err)
