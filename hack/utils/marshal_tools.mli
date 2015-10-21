(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception Invalid_Int_Size_Exception
exception Payload_Size_Too_Large_Exception
exception Malformed_Preamble_Exception
exception Writing_Preamble_Exception
exception Writing_Payload_Exception
exception Reading_Preamble_Exception
exception Reading_Payload_Exception

val to_fd_with_preamble: Unix.file_descr -> 'a -> unit
val from_fd_with_preamble: Unix.file_descr -> 'a
