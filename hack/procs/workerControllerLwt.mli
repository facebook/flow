(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* Call in a sub-process *)
val call: WorkerController.worker -> ('a -> 'b) -> 'a -> 'b Lwt.t
