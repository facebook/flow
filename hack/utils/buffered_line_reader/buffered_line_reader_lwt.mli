(*
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include
  Buffered_line_reader_sig.S
    with type 'a result = 'a Lwt.t
     and type fd = Lwt_unix.file_descr
