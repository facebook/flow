(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is documented in state_reader.ml *)

type t = unit

let commit () =
  SharedMem.commit_transaction ();
  Lwt.return_unit

let rollback () = Lwt.return_unit

let create transaction = Transaction.add ~commit ~rollback transaction
