(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This saved state fetcher is used by servers which don't intend to use saved state *)
let fetch ~options:_ =
  Profiling_js.with_profiling_lwt ~label:"FetchSavedState" ~should_print_summary:false (fun _ ->
      Lwt.return Saved_state_fetcher.No_saved_state)
