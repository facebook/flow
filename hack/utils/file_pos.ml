(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

include (val (
   (* If we're on a 64-bit architecture then use the small (integer) rep
    * representation for file positions *)
   if 1 lsl 31 <> 0
   then (module File_pos_small : File_pos_sig.S)
   (* Otherwise use the full record representation *)
   else (module File_pos_large : File_pos_sig.S)) : File_pos_sig.S)
