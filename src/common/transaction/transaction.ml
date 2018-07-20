(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


type mutator = {
  commit: unit -> unit Lwt.t;
  rollback: unit -> unit Lwt.t;
}

type t = {
  mutable mutators: mutator list
}

let add ~commit ~rollback transaction =
  transaction.mutators <- { commit; rollback; } :: transaction.mutators

let commit transaction =
  Lwt_list.iter_s (fun mutator -> mutator.commit ()) transaction.mutators

let rollback transaction =
  Lwt_list.iter_s (fun mutator -> mutator.rollback ()) transaction.mutators

external reraise : exn -> 'a = "%reraise"

let with_transaction f =
  let transaction = { mutators = []; } in
  let%lwt result =
    try%lwt f transaction
    with exn ->
      let%lwt () = rollback transaction in
      reraise exn
  in
  let%lwt () = commit transaction in
  Lwt.return result
