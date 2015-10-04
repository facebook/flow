(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let init _ _ = ()
let init_done _ = ()
let load_read_end _ = ()
let load_recheck_end _ = ()
let load_script_done _ = ()
let load_failed _ = ()
let master_exception _ = ()
let worker_exception _ = ()
let out_of_date _ = ()
let killed _ = ()
let lock_lost _ _ = ()
let lock_stolen _ _ = ()
let client_startup _ = ()
let client_begin_work _ = ()
let client_finish _ = ()
let check_response _ = ()
let build_differs _ _ _ = ()
let build_same _ _ = ()
