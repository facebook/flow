(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val to_fd : ?flags:Marshal.extern_flags list -> Lwt_unix.file_descr -> 'a -> int Lwt.t

val from_fd : Lwt_unix.file_descr -> 'a Lwt.t
