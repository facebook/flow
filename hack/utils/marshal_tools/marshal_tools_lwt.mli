(*
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

val expected_preamble_size : int

val to_fd_with_preamble :
  ?flags:Marshal.extern_flags list -> Lwt_unix.file_descr -> 'a -> int Lwt.t

val from_fd_with_preamble : Lwt_unix.file_descr -> 'a Lwt.t
