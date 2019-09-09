(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error = {
  msg: string;
  exit_status: FlowExitStatus.t;
}

val process_updates :
  ?skip_incompatible:bool ->
  options:Options.t ->
  libs:SSet.t ->
  SSet.t ->
  (Utils_js.FilenameSet.t, error) Core_result.t
