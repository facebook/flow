(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type mutator = {
  commit: unit -> unit;
  rollback: unit -> unit;
}

type t = mutator list ref

let add ~commit ~rollback transaction = transaction := { commit; rollback } :: !transaction

let commit transaction = List.iter (fun mutator -> mutator.commit ()) !transaction

let rollback transaction = List.iter (fun mutator -> mutator.rollback ()) !transaction

let with_transaction f =
  let transaction = ref [] in
  let%lwt result =
    try%lwt f transaction with
    | exn ->
      let exn = Exception.wrap exn in
      let () = rollback transaction in
      Exception.reraise exn
  in
  let () = commit transaction in
  Lwt.return result
