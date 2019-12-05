(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type msg =
  | Ready
  | Updates of SSet.t

val entry_point : (string * Path.t list, unit, msg) Daemon.entry
