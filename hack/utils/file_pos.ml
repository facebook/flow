(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include (val (
   if 1 lsl 31 <> 0
   (* TODO: Implement special compact encoding for 64 bit wordsize *)
   then (module File_pos_32 : File_pos_sig.S)
   else (module File_pos_32 : File_pos_sig.S)) : File_pos_sig.S)
