(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Types = struct

  exception Timeout

  type subscribe_mode =
    | Defer_changes
    (** See also Watchman docs on drop. This means the subscriber will not
     * get a list of files changed during a repo update. Practically, this
     * is not useful for the typechecker process which needs to actually
     * know which files were changed. This is useful for the monitor to
     * aggressively kill the server. *)
    | Drop_changes

  type init_settings = {
    (** None for query mode, otherwise specify subscriptions mode. *)
    subscribe_mode: subscribe_mode option;
    (** Seconds used for init timeout - will be reused for reinitialization. *)
    init_timeout: int;
    sync_directory: string;
    root: Path.t;
  }

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

module type S = sig

  include module type of Types
  include module type of Abstract_types

  val crash_marker_path: Path.t -> string

  val init: init_settings -> env option

  val get_all_files: env -> string list

  val get_changes: ?deadline:float ->
    watchman_instance -> watchman_instance * changes
  val get_changes_synchronously: timeout:int ->
    watchman_instance -> watchman_instance * SSet.t

  (** Exposing things for unit tests.
   *
   * We have to double-declare the module signature in this .mli and in the .ml
   * which is unfortunate, but because we use the abstract type "env", the
   * alternative would involve verbose mutuaully-recursive modules.*)
  module type Testing_sig = sig
    val test_env : env
    val transform_asynchronous_get_changes_response :
      env -> Hh_json.json -> env * pushed_changes
  end

  module Testing : Testing_sig

end
