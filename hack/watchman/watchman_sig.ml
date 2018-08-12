(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Types = struct

  exception Timeout
  exception Watchman_error of string
  exception Subscription_canceled_by_watchman


  type subscribe_mode =
    | All_changes
    | Defer_changes
    (** See also Watchman docs on drop. This means the subscriber will not
     * get a list of files changed during a repo update. Practically, this
     * is not useful for the typechecker process which needs to actually
     * know which files were changed. This is useful for the monitor to
     * aggressively kill the server. *)
    | Drop_changes
    | Scm_aware

  type init_settings = {
    (** None for query mode, otherwise specify subscriptions mode. *)
    subscribe_mode: subscribe_mode option;
    (** Seconds used for init timeout - will be reused for reinitialization. *)
    init_timeout: int;
    (** See watchman expression terms. *)
    expression_terms: Hh_json.json list;
    debug_logging: bool;
    roots: Path.t list;
    subscription_prefix: string;
  }

  (** The message's clock. *)
  type clock = string

  type pushed_changes =
    (**
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
    | Watchman_synchronous of SSet.t
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
  (** This has to be repeated because they depend on the abstract types. *)
  type watchman_instance =
    | Watchman_dead of dead_env
    | Watchman_alive of env
end


module type WATCHMAN_PROCESS = sig
  type 'a result
  type conn

  exception Read_payload_too_long

  val (>>=): 'a result -> ('a -> 'b result) -> 'b result
  val (>|=): 'a result -> ('a -> 'b) -> 'b result
  val return: 'a -> 'a result
  val catch: f:(unit -> 'b result) -> catch:(exn -> 'b result) -> 'b result

  val map_fold_values: 'a SMap.t -> init:'b -> f:('a -> 'b -> 'b result) -> 'b result
  val map_iter_values_s: 'a SMap.t -> f:('a -> unit result) -> unit result

  val open_connection: timeout:float -> conn result
  val request:
    debug_logging:bool -> ?conn:conn -> ?timeout:float -> Hh_json.json -> Hh_json.json result
  val send_request_and_do_not_wait_for_response:
    debug_logging:bool -> conn:conn -> Hh_json.json -> unit result
  val blocking_read: debug_logging:bool -> ?timeout:float -> conn:conn -> Hh_json.json option result
  val close_connection: conn -> unit result

  module Testing: sig
    val get_test_conn: unit -> conn result
  end
end

module type S = sig

  include module type of Types
  include module type of Abstract_types

  type 'a result
  type conn

  val init: ?since_clockspec:string -> init_settings -> unit -> env option result

  val get_all_files: env -> string list result

  val get_changes: ?deadline:float ->
    watchman_instance -> (watchman_instance * changes) result
  val get_changes_synchronously: timeout:int ->
    watchman_instance -> (watchman_instance * SSet.t) result

  val conn_of_instance: watchman_instance -> conn option

  (** Expose some things for testing. *)
  module Testing : sig
    val get_test_env : unit -> env result
    val test_settings : init_settings

    val transform_asynchronous_get_changes_response :
      env -> Hh_json.json option -> env * pushed_changes
  end

  module Mocking : sig
    val print_env : env -> string
    val init_returns : string option -> unit
    val get_changes_returns : changes -> unit
  end

end;;
