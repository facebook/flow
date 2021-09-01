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

let hg ?cwd args =
  let env = [| "NOSCMLOG=1"; "HGPLAIN=1" |] in
  exec ?cwd ~env "hg" args

(** [merge_base a b] returns the hash of the common ancestor of [a] and [b]. *)
let merge_base ?cwd a b =
  let revset = Printf.sprintf "ancestor(%s,%s)" a b in
  match%lwt hg ?cwd ["log"; "-T"; "{node}"; "-r"; revset] with
  | Ok stdout ->
    let result =
      try Ok (String.sub stdout 0 40) with
      | Invalid_argument _ -> Error (Errored "Malformed merge-base")
    in
    Lwt.return result
  | Error _ as err -> Lwt.return err

let files_changed_since ?cwd hash =
  match%lwt hg ?cwd ["status"; "--print0"; "-n"; "--rev"; hash] with
  | Ok stdout -> Lwt.return (Ok (split_null_terminated_lines stdout))
  | Error _ as err -> Lwt.return err

let files_changed_since_mergebase_with ?cwd commit =
  match%lwt merge_base ?cwd "." commit with
  | Error _ as err -> Lwt.return err
  | Ok mergebase ->
    (match%lwt files_changed_since ?cwd mergebase with
    | Ok changes -> Lwt.return (Ok (mergebase, changes))
    | Error _ as err -> Lwt.return err)
