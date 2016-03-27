(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ServerEnv

let go class_ find_children env genv oc =
  let res_list =
    MethodJumps.get_inheritance env.tcopt class_ ~find_children
      env.files_info genv.workers in
  Marshal.to_channel oc res_list [];
  flush oc;
  ()
