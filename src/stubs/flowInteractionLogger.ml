(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let init () = ()

let set_server_config ~flowconfig_name:_ ~root:_ ~root_name:_ = ()

let interaction
    ~source:_
    ~trigger:_
    ~ux:_
    ~start_time_ms:_
    ~end_time_ms:_
    ~start_server_status:_
    ~end_server_status:_
    ~start_buffer_status:_
    ~end_buffer_status:_ =
  ()

let flush () = Lwt.return_unit

let disable_logging () = ()
