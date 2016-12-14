(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * Main return type of the parser - parsed message, or an error
 *)
type result_t = Ide_message.request Ide_rpc_protocol_parser_types.return_t
