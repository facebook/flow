(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env = {
  ns_uses: string SMap.t;
  ns_fun_uses: string SMap.t;
  ns_const_uses: string SMap.t;
  ns_name: string option;
}

let empty = {
  ns_uses = SMap.empty;
  ns_fun_uses = SMap.empty;
  ns_const_uses = SMap.empty;
  ns_name = None
}
