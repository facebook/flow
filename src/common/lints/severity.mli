(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type severity =
  | Off
  | Warn
  | Err

val string_of_severity : severity -> string

val output_string_of_severity : severity -> string

val severity_of_string : string -> severity option

val severity_cmp : severity -> severity -> int

val severity_min : severity -> severity -> severity

val severity_max : severity -> severity -> severity
