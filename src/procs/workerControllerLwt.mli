(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Call in a sub-process *)
val call : WorkerController.worker -> ('a -> 'b) -> 'a -> 'b Lwt.t
