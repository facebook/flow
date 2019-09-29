(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* Building the environment *)
(*****************************************************************************)
let make_genv options handle =
  let workers =
    let num_workers = Options.max_workers options in
    if num_workers > 0 then
      Some (ServerWorker.make ~n:num_workers handle)
    else
      None
  in
  { ServerEnv.options; workers }
