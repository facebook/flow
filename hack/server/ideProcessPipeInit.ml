(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open IdeProcessPipe

let monitor_make_and_send typechecker_out ide_out =
  let in1, out1 = Unix.pipe () in
  let in2, out2 = Unix.pipe () in

  let send x y =
    let status = Libancillary.ancil_send_fd x y in
    if (status <> 0) then begin
      Hh_logger.log "Failed to handoff IDE pipe";
      raise (Exit_status.(Exit_with IDE_init_failure))
    end;
    Unix.close y in

  send typechecker_out in1;
  send typechecker_out out2;
  send ide_out in2;
  send ide_out out1

let process_recv monitor_in =
  let in_fd = Libancillary.ancil_recv_fd monitor_in in
  let out_fd = Libancillary.ancil_recv_fd monitor_in in
  { in_fd; out_fd; }

(* Typed (in .mli file) wrappers *)
let typechecker_recv = process_recv
let ide_recv = process_recv
