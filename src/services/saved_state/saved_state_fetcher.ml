(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type fetch_result =
  (* We successfully found saved state. Yay! *)
  | Saved_state of {
      saved_state_filename: Path.t;
      changed_files: SSet.t;
    }
  (* We did not attempt to find saved state. *)
  | No_saved_state
  (* We should have been able to find saved state , but for some reason we could not. *)
  | Saved_state_error

module type FETCHER = sig
  val fetch : options:Options.t -> (Profiling_js.finished * fetch_result) Lwt.t
end
