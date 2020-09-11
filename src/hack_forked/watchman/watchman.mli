(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Timeout

exception Watchman_error of string

exception Subscription_canceled_by_watchman

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
  subscribe_mode: subscribe_mode;
  expression_terms: Hh_json.json list;  (** See watchman expression terms. *)
  debug_logging: bool;
  defer_states: string list;  (** defer notifications while these states are asserted *)
  roots: Path.t list;
  subscription_prefix: string;
  sync_timeout: int option;
}

type clock = string
(** The message's clock. *)

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

type env

type dead_env

(* This has to be repeated because they depend on the abstract types. *)
type watchman_instance =
  | Watchman_dead of dead_env
  | Watchman_alive of env

type conn

val init : init_settings -> env option Lwt.t

val get_changes_since_mergebase : timeout:timeout -> env -> string list Lwt.t

val get_mergebase :
  timeout:timeout ->
  watchman_instance ->
  (watchman_instance * (string, string) Pervasives.result) Lwt.t

val get_mergebase_and_changes :
  timeout:timeout ->
  watchman_instance ->
  (watchman_instance * (string * SSet.t, string) Pervasives.result) Lwt.t

val get_changes : ?deadline:float -> watchman_instance -> (watchman_instance * changes) Lwt.t

val conn_of_instance : watchman_instance -> conn option

val close : env -> unit Lwt.t

val with_instance :
  watchman_instance ->
  try_to_restart:bool ->
  on_alive:(env -> 'a Lwt.t) ->
  on_dead:(dead_env -> 'a Lwt.t) ->
  'a Lwt.t

(* Expose some things for testing. *)
module Testing : sig
  val get_test_env : unit -> env Lwt.t

  val test_settings : init_settings

  val transform_asynchronous_get_changes_response :
    env -> Hh_json.json option -> env * pushed_changes
end
