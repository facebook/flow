(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type subscribe_mode =
  | All_changes
  | Defer_changes
  | Drop_changes
      (** See also Watchman docs on drop. This means the subscriber will not
          get a list of files changed during a repo update. Practically, this
          is not useful for the typechecker process which needs to actually
          know which files were changed. This is useful for the monitor to
          aggressively kill the server. *)

type timeout = float option

type init_settings = {
  debug_logging: bool;
  defer_states: string list;  (** defer notifications while these states are asserted *)
  expression_terms: Hh_json.json list;  (** See watchman expression terms. *)
  mergebase_with: string;  (** symbolic commit to find changes against *)
  roots: Path.t list;
  should_track_mergebase: bool;
  subscribe_mode: subscribe_mode;
  subscription_prefix: string;
  sync_timeout: int option;
}

type pushed_changes =
  (*
   * State name and metadata.
   *
   * For example:
   *   State name: "hg.update"
   * Metadata:
   *   {
   *    "partial":false,
   *    "rev":"780dab9ff0a01691c9b18a5ee1194810e555c78b",
   *    "distance":2,
   *    "status":"ok"
   *   }
   *
   * Note: The distance is HG Revision distance, not SVN revision distance.
   *)
  | State_enter of string * Hh_json.json option
  | State_leave of string * Hh_json.json option
  | Files_changed of {
      changes: SSet.t;
      changed_mergebase: bool option;
    }
  | Missed_changes of {
      prev_mergebase: string;
      mergebase: string;
      changes_since_mergebase: SSet.t;
    }

type failure =
  | Dead
  | Restarted

type env

val init : init_settings -> (env * SSet.t, string) Result.t Lwt.t

val recover_from_restart : env -> (env * pushed_changes, failure) Result.t Lwt.t

val get_changes : env -> (env * pushed_changes, failure) Result.t Lwt.t

val close : env -> unit Lwt.t

(* Expose some things for testing. *)
module Testing : sig
  type error_kind

  val get_test_env : unit -> env Lwt.t

  val test_settings : init_settings

  val transform_asynchronous_get_changes_response :
    env -> Hh_json.json -> (env * pushed_changes, error_kind) Result.t Lwt.t
end
