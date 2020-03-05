(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type fetch_result =
  | Saved_state of {
      saved_state_filename: Path.t;
      changed_files: SSet.t;
    }
  | No_saved_state

module type FETCHER = sig
  val fetch : options:Options.t -> (Profiling_js.finished * fetch_result) Lwt.t
end
