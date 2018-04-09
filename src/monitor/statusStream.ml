(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is a singleton that is responsible for keeping track of the Flow server's status.
 * The basic idea is that we have a stream of status updates coming from the Flow server. When
 * The Flow server dies and is replaced with a new server, we create a new stream.
 *
 * The main goal of this module is to answer the question "What is the server's current status" and
 * to invoke callbacks when the server becomes free
 *)

module Logger = FlowServerMonitorLogger

(* This is the status info for a single Flow server *)
type t = {
  mutable status: ServerStatus.status;
  mutable ever_been_free: bool;
  stream: ServerStatus.status Lwt_stream.t;
  push_to_stream: ServerStatus.status option -> unit
}

(* Multiple threads might call StreamStatus functions. *)
let mutex = Lwt_mutex.create ()

(* A list of callbacks which will be invoked once the next time the server is free *)
let to_call_on_free = ref []

let significant_transition = Lwt_condition.create ()

module UpdateLoop = LwtLoop.Make (struct
  type acc = t

  let invoke_all_call_on_free () =
    let%lwt to_call = Lwt_mutex.with_lock mutex (fun () ->
      let to_call = !to_call_on_free in
      to_call_on_free := [];
      Lwt.return to_call
    ) in
    Lwt_list.iter_p (fun f -> f ()) to_call

  let process_update t new_status =
    Logger.debug "Server status: %s" (ServerStatus.string_of_status new_status);

    let old_status = t.status in
    (* We don't need a lock here, since we're the only thread processing statuses *)
    t.status <- new_status;

    if ServerStatus.is_free new_status then begin
      t.ever_been_free <- true;
      Lwt.async invoke_all_call_on_free
    end;

    if ServerStatus.is_significant_transition old_status new_status then begin
      Lwt_condition.broadcast significant_transition new_status
    end;

    Lwt.return t


  let main t =
    let%lwt new_status = Lwt_stream.next t.stream in
    process_update t new_status

  let catch _ exn =
    match exn with
    | Lwt_stream.Empty ->
      Lwt.return_unit (* This is the signal to stop *)
    | exn ->
      Logger.error ~exn "ServerStatus update loop hit an unexpected exception";
      Lwt.return_unit
end)

let empty () =
  let stream, push_to_stream = Lwt_stream.create () in
  let ret = {
    status = ServerStatus.initial_status;
    ever_been_free = false;
    stream;
    push_to_stream;
  } in
  Lwt.async (fun () -> UpdateLoop.run ret);
  ret

(* This is the status info for the current Flow server *)
let current_status = ref (empty ())

(* Call f the next time the server is free. If the server is currently free, then call now *)
let call_on_free ~f =
  if ServerStatus.is_free !current_status.status
  then f ()
  else Lwt_mutex.with_lock mutex (fun () ->
    to_call_on_free := f::!to_call_on_free;
    Lwt.return_unit
  )

(* When a new server starts up, we close the old server's status stream and start over *)
let reset () = Lwt_mutex.with_lock mutex (fun () ->
  !current_status.push_to_stream None;
  current_status := empty ();
  Lwt.return_unit
)

let get_status () = !current_status.status

let ever_been_free () = !current_status.ever_been_free

let wait_for_signficant_status ~timeout =
  (* If there is a significant transition before the timeout, the cancel the sleep and return the
   * new status. Otherwise, stop waiting on the condition variable and return the current status *)
  Lwt.pick [
    (let%lwt () = Lwt_unix.sleep timeout in Lwt.return (get_status ()));
    Lwt_condition.wait significant_transition;
  ]

(* Updates will show up on the connection in order. Let's push them immediately to a stream to
 * preserve that order *)
let update ~status = !current_status.push_to_stream (Some status)
