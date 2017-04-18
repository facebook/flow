(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Building the environment *)
(*****************************************************************************)
let make_genv options handle =
  let multicore = Options.max_workers options > 0 in
  let workers =
    if multicore then
      Some (ServerWorker.make options handle)
    else
      None
  in
  { ServerEnv.options; workers; }
