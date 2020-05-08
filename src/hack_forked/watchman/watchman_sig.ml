(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Types = struct
  exception Timeout

  exception Watchman_error of string

  exception Subscription_canceled_by_watchman

  exception Watchman_restarted

  type subscribe_mode =
    | All_changes
    | Defer_changes
    (* See also Watchman docs on drop. This means the subscriber will not
     * get a list of files changed during a repo update. Practically, this
     * is not useful for the typechecker process which needs to actually
     * know which files were changed. This is useful for the monitor to
     * aggressively kill the server. *)
    | Drop_changes
    | Scm_aware

  type timeout = float option

  type init_settings = {
    (* None for query mode, otherwise specify subscriptions mode. *)
    subscribe_mode: subscribe_mode option;
    (* See watchman expression terms. *)
    expression_terms: Hh_json.json list;
    debug_logging: bool;
    roots: Path.t list;
    subscription_prefix: string;
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
    | Watchman_synchronous of pushed_changes list
end

(** The abstract types, and the types that are defined in terms of
 * abstract types must be split out. The reason is left as an exercise
 * to the reader (i.e. try it yourself out a few ways and you'll discover the
 * limitations - whether your strategy is splitting up the definitions
 * into 3 modules "base types, abstract types, dependent types", or
 * if you change this to a functor). *)
module Abstract_types = struct
  type env

  type dead_env

  (* This has to be repeated because they depend on the abstract types. *)
  type watchman_instance =
    | Watchman_dead of dead_env
    | Watchman_alive of env
end

module type S = sig
  include module type of Types

  include module type of Abstract_types

  type conn

  val init : ?since_clockspec:string -> init_settings -> unit -> env option Lwt.t

  val get_changes_since_mergebase : timeout:timeout -> env -> string list Lwt.t

  val get_mergebase :
    timeout:timeout ->
    watchman_instance ->
    (watchman_instance * (string, string) Pervasives.result) Lwt.t

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
end
