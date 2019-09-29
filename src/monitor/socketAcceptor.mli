(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val run : Lwt_unix.file_descr -> autostop:bool -> unit Lwt.t

val run_legacy : Lwt_unix.file_descr -> unit Lwt.t
