(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Watchman_actual = struct
  include Watchman.Functor
end

module Watchman_mock = struct
  exception Not_available_in_mocking

  type conn

  include Watchman_sig.Types

  type env = string

  type dead_env = unit

  type watchman_instance =
    | Watchman_dead of dead_env
    | Watchman_alive of env

  module Mocking = struct
    let print_env env = env

    let init = ref None

    let init_returns v = init := v

    let changes = ref Watchman_unavailable

    let get_changes_returns v = changes := v
  end

  module Testing = struct
    open Watchman_sig.Types

    let test_settings =
      {
        subscribe_mode = Some Defer_changes;
        expression_terms = [];
        debug_logging = false;
        roots = [Path.dummy_path];
        subscription_prefix = "dummy_prefix";
      }

    let get_test_env () = Lwt.return "test_env"

    let transform_asynchronous_get_changes_response _ _ = raise Not_available_in_mocking
  end

  let init ?since_clockspec:_ _ () = Lwt.return !Mocking.init

  let get_changes ?deadline instance =
    let _ = deadline in
    let result = !Mocking.changes in
    Mocking.changes := Watchman_unavailable;
    Lwt.return (instance, result)

  let conn_of_instance _ = None

  let get_changes_since_mergebase ~timeout:_ _ = Lwt.return []

  let get_mergebase ~timeout:_ instance = Lwt.return (instance, Ok "mergebase")

  let close _ = Lwt.return ()

  let with_instance instance ~try_to_restart:_ ~on_alive ~on_dead =
    match instance with
    | Watchman_dead dead_env -> on_dead dead_env
    | Watchman_alive env -> on_alive env
end

module type S = sig
  include Watchman_sig.S
end

include ( val if Injector_config.use_test_stubbing then
                (module Watchman_mock : S)
              else
                (module Watchman_actual : S) )
