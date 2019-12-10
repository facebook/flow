(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* A modified maybe monad
 * Most of the time, I prefer to use exceptions, I like things to blow up
 * if something went wrong.
 * However, in the case of dfind, exceptions are painful. We don't want things
 * to blow-up, we want to carry-on whatever happens.
 * So this monad never fails, it logs very nasty errors, for example, it will
 * log the fact that a watch couldn't be created, when the file still exists.
 *)
(*****************************************************************************)

let log = ref stderr

let set_log oc = log := oc

type 'a t = 'a option

let ( >>= ) x f =
  match x with
  | None -> None
  | Some x -> f x

let return x = Some x

let handle_file_exn path = function
  | Fsnotify.Error (_, Unix.ENOENT) ->
    () (* The file got deleted in the mean time ... we don't care *)
  | Fsnotify.Error (reason, _) ->
    (* This is bad ... *)
    Printf.fprintf !log "Error: could not add watch to %s [%s]\n" path reason
  | _ when Sys.file_exists path ->
    (* Logging this makes the system very noisy. There are too many
     * cases where a file has been removed etc ...
     *)
    ()
  | _ -> ()

(* Calls (f path), never fails, logs the nasty exceptions *)
let call f path =
  try f path
  with e ->
    handle_file_exn path e;
    None

let wrap f x = return (f x)
