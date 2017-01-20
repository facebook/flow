(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


exception Select_timed_out

type t = {
  stdin_fd : Unix.file_descr;
  stdout_fd : Unix.file_descr;
  stderr_fd : Unix.file_descr;
  pid : int;
}
