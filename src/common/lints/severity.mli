(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type severity =
 | Off
 | Warn
 | Err

val string_of_severity: severity -> string
val output_string_of_severity: severity -> string

val severity_of_string: string -> severity option

val severity_cmp: severity -> severity -> int
val severity_min: severity -> severity -> severity
val severity_max: severity -> severity -> severity
