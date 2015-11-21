(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Utils

(*****************************************************************************)
(* Periodically called by the daemon *)
(*****************************************************************************)

type callback =
  | Periodic of (float ref * float * (unit -> unit))
  | Once of (float ref * (unit -> unit))

module Periodical: sig
  val always : float
  val one_hour: float
  val one_day: float
  val one_week: float

  val check: unit -> unit

  (* register_callback X Y
   * Registers a new callback Y called every X seconds.
   * The time is an approximation, don't expect it to be supper accurate.
   * More or less 1 sec is a more or less what you can expect.
   * More or less 30 secs if the server is busy.
   *)
  val register_callback: callback -> unit

end = struct
  let always = 0.0
  let one_hour = 3600.0
  let one_day = 86400.0
  let one_week = 604800.0

  let callback_list = ref []
  let last_call = ref (Unix.time())

  let check () =
    let current = Unix.time() in
    let delta = current -. !last_call in
    last_call := current;
    callback_list := List.filter !callback_list begin fun callback ->
      (match callback with
      | Periodic (seconds_left, _, job)
      | Once (seconds_left, job) ->
          seconds_left := !seconds_left -. delta;
          if !seconds_left < 0.0 then job ());
      (match callback with
      | Periodic (seconds_left, period, _) ->
          if !seconds_left < 0.0 then seconds_left := period;
          true
      | Once _ -> false);
    end

  let register_callback cb =
    callback_list := cb :: !callback_list
end

let go = Periodical.check

let async f = Periodical.register_callback (Once (ref 0.0, f))

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
let last_client_connect: float ref = ref (Unix.time())

let stamp_connection() =
  last_client_connect := Unix.time();
  ()

let exit_if_unused() =
  let delta: float = Unix.time() -. !last_client_connect in
  if delta > Periodical.one_week
  then begin
    Printf.eprintf "Exiting server. Last used >7 days ago\n";
    Exit_status.(exit Unused_server)
  end

(*****************************************************************************)
(* The registered jobs *)
(*****************************************************************************)
let init (root : Path.t) =
  let jobs = [
    (* I'm not sure explicitly invoking the Gc here is necessary, but
     * major_slice takes something like ~0.0001s to run, so why not *)
    Periodical.always   , (fun () -> ignore @@ Gc.major_slice 0);
    Periodical.one_hour *. 3., EventLogger.log_gc_stats;
    Periodical.always   , (fun () -> SharedMem.collect `aggressive);
    Periodical.always   , EventLogger.flush;
    Periodical.one_day  , exit_if_unused;
    Periodical.one_day  , Hhi.touch;
    (* try_touch wraps Unix.lutimes, which doesn't open/close any fds, so we
     * won't lose our lock by doing this. We are only touching the top level
     * of files, however -- we don't want to do it recursively so that old
     * files under e.g. /tmp/hh_server/logs still get cleaned up. *)
    Periodical.one_day  , (fun () ->
      Array.iter begin fun fn ->
        let fn = Filename.concat GlobalConfig.tmp_dir fn in
        if (try Sys.is_directory fn with _ -> false)
          (* We don't want to touch things like .watchman_failed *)
          || str_starts_with fn "."
          || not (ServerFiles.is_of_root root fn) then ()
        else Sys_utils.try_touch ~follow_symlinks:false fn
      end (Sys.readdir GlobalConfig.tmp_dir);
    );
  ] in
  List.iter jobs begin fun (period, cb) ->
    Periodical.register_callback (Periodic (ref period, period, cb))
  end
