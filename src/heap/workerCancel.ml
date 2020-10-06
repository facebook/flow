(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Worker_should_exit

let () = Callback.register_exception "worker_should_exit" Worker_should_exit

external stop_workers : unit -> unit = "hh_stop_workers"

external resume_workers : unit -> unit = "hh_resume_workers"

external check_should_exit : unit -> unit = "hh_check_should_exit"

external set_can_worker_stop : bool -> unit = "hh_set_can_worker_stop"

external get_can_worker_stop : unit -> bool = "hh_get_can_worker_stop"

let on_worker_cancelled = ref (fun () -> ())

let set_on_worker_cancelled f = on_worker_cancelled := f

let with_worker_exit f =
  try f ()
  with Worker_should_exit ->
    !on_worker_cancelled ();
    exit 0

(* Check if the workers are stopped and exit if they are *)
let check_should_exit () = with_worker_exit check_should_exit

let with_no_cancellations f =
  if get_can_worker_stop () then
    Utils.try_finally
      ~f:
        begin
          fun () ->
          set_can_worker_stop false;
          f ()
        end
      ~finally:(fun () ->
        set_can_worker_stop true;
        check_should_exit ())
  else
    (* If can_worker_stop is set to false, then that means we are already within an enclosing
     * call to with_no_cancellations. So, we can just call `f` directly. Note that a more
     * sophisticated approach will be necessary if we involve Lwt here, since then calls to
     * with_no_cancellations could be interleaved without being nested. *)
    f ()
