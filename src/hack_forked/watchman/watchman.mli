(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Watchman_restarted

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
  subscribe_mode: subscribe_mode;
  subscription_prefix: string;
  sync_timeout: int option;
}

(** The message's clock. *)
type clock = string

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
  | Changed_merge_base of string * SSet.t * clock
  | Files_changed of SSet.t

type changes =
  | Watchman_unavailable
  | Watchman_pushed of pushed_changes

type watchman_instance

val init : init_settings -> watchman_instance option Lwt.t

val get_mergebase_and_changes :
  watchman_instance -> (watchman_instance * (string * SSet.t, string) Stdlib.result) Lwt.t

val get_changes : watchman_instance -> (watchman_instance * changes) Lwt.t

val close : watchman_instance -> unit Lwt.t

(* Expose some things for testing. *)
module Testing : sig
  type env

  type error_severity

  val get_test_env : unit -> env Lwt.t

  val test_settings : init_settings

  val transform_asynchronous_get_changes_response :
    env -> Hh_json.json -> (env * pushed_changes, error_severity) Result.t
end
