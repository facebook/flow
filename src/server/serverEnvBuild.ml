(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
  { ServerEnv.options; workers; }
