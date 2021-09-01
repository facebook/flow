(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let spf = Printf.sprintf

type error_status =
  | Not_installed of { path: string }
  | Errored of string

let exec ?env ?cwd cmd args =
  let%lwt { LwtSysUtils.status; stdout; stderr } = LwtSysUtils.exec ?env ?cwd cmd args in
  match status with
  | Unix.WEXITED 0 -> Lwt.return (Ok stdout)
  | Unix.WEXITED 127 ->
    let path = Option.value (Sys_utils.getenv_path ()) ~default:"(not set)" in
    Lwt.return (Error (Not_installed { path }))
  | Unix.WEXITED code ->
    let msg = spf "%s exited code %d, stderr = %S" cmd code stderr in
    Lwt.return (Error (Errored msg))
  | Unix.WSIGNALED signal ->
    let signal = PrintSignal.string_of_signal signal in
    let msg = spf "%s signaled with %s signal" cmd signal in
    Lwt.return (Error (Errored msg))
  | Unix.WSTOPPED signal ->
    let signal = PrintSignal.string_of_signal signal in
    let msg = spf "%s stopped with %s signal" cmd signal in
    Lwt.return (Error (Errored msg))

(** Splits a string consisting of null-terminated lines into a list of strings.
    Since each line is null-terminated, it does NOT return an empty string
    at the end like simply splitting on NUL would. *)
let split_null_terminated_lines content =
  let lines = String.split_on_char '\x00' content in
  match List.rev lines with
  | "" :: rest -> List.rev rest
  | _ -> lines
