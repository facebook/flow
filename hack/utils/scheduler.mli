(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Make (EnvType : sig
  type t
end) : sig
  type t = EnvType.t

  (* Remove all scheduled jobs *)
  val reset : unit -> unit

  val wait_for_fun :
    ?once:bool ->
    priority:(* Should the job be removed after it's executed *)
             int ->
    (t -> bool) ->
    ((* The job can run when this function return true *)
     t -> t) ->
    (* The job to run *)
    unit

  val wait_for_channel :
    priority:int ->
    Unix.file_descr ->
    ((* The job can run when this fd is readable *)
     t -> t) ->
    (* The job to run *)
    unit

  val stop_waiting_for_channel :
    Unix.file_descr -> (* fd that was passed to wait_for_channel before *)
                       unit

  val wait_and_run_ready : t -> t
end
