(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  slow_files_logging_internal: float option;
  slow_components_logging_threshold: float option;
  slow_expressions_logging_threshold: float option;
}

let default =
  {
    slow_files_logging_internal = None;
    slow_components_logging_threshold = None;
    slow_expressions_logging_threshold = None;
  }
