(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


(*****************************************************************************)
(* Periodically called by the daemon *)
(*****************************************************************************)

module Periodical: sig
  val always : float
  val one_day: float
  val one_week: float

  (* Called just before going to sleep *)
  val check: unit -> unit

  (* register_callback X Y
   * Registers a new callback Y called every X seconds.
   * The time is an approximation, don't expect it to be supper accurate.
   * More or less 1 sec is a more or less what you can expect.
   * More or less 30 secs if the server is busy.
   *)
  val register_callback: seconds:float -> job:(unit -> unit) -> unit

end = struct
  let always = 0.0
  let one_day = 86400.0
  let one_week = 604800.0

  let callback_list = ref []
  let last_call = ref (Unix.gettimeofday())

  let check () =
    let current = Unix.gettimeofday() in
    let delta = current -. !last_call in
    last_call := current;
    List.iter begin fun (seconds_left, period, job) ->
      seconds_left := !seconds_left -. delta;
      if !seconds_left < 0.0
      then begin
        seconds_left := period;
        job()
      end
    end !callback_list

  let register_callback ~seconds ~job =
    callback_list :=
      (ref seconds, seconds, job) :: !callback_list
end

let call_before_sleeping = Periodical.check

(*****************************************************************************)
(*
 * kill the server every 24h. We do this to save resources and
 * make sure everyone is +/- running the same version.
 *
 * TODO: improve this check so the server only restarts
 *       if there hasn't been any activity for x hours/days.
 *)
(*****************************************************************************)

(* We want to keep track of when the server was last used. Every few hours, we'll
 * check this variable. If the server hasn't been used for a few days, we exit.
 *)
let last_client_connect: float ref = ref (Unix.gettimeofday())

let stamp_connection() =
  last_client_connect := Unix.gettimeofday();
  ()

let exit_if_unused() =
  let delta: float = Unix.gettimeofday() -. !last_client_connect in
  if delta > Periodical.one_week
  then begin
    Printf.fprintf stderr "Exiting server. Last used >7 days ago\n";
    flush stderr;
    exit(5)
  end

(*****************************************************************************)
(* The registered jobs *)
(*****************************************************************************)
let init () =
  let jobs = [
    Periodical.always   , (fun () -> SharedMem_js.collect `aggressive);
    Periodical.one_day  , exit_if_unused;
  ] in
  List.iter (fun (period, cb) -> Periodical.register_callback period cb) jobs
