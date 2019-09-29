(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type msg =
  | Ready
  | Updates of SSet.t

val entry_point : (string * Path.t list, unit, msg) Daemon.entry
