(**
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 val to_fd_with_preamble: Lwt_unix.file_descr -> 'a -> unit Lwt.t
 val from_fd_with_preamble: Lwt_unix.file_descr -> 'a Lwt.t
