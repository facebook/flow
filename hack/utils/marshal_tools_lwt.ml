(**
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 module Marshal_tools_lwt = Marshal_tools.MarshalToolsFunctor (struct
   type 'a result = 'a Lwt.t
   type fd = Lwt_unix.file_descr

   let return = Lwt.return
   let fail = Lwt.fail
   let (>>=) = Lwt.(>>=)

   let write fd ~buffer ~offset ~size = Lwt_unix.write fd buffer offset size
   let read fd ~buffer ~offset ~size = Lwt_unix.read fd buffer offset size
   let log str = Lwt_log_core.ign_error str
 end)

 include Marshal_tools_lwt
