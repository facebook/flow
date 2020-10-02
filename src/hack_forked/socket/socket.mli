(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type addr

val with_addr : addr -> (Unix.sockaddr -> 'a) -> 'a

val addr_for_open : string -> addr

val init_unix_socket : string -> Unix.file_descr
