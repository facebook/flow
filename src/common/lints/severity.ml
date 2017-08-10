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

let string_of_severity = function
  | Off -> "off"
  | Warn -> "warn"
  | Err -> "error"

let output_string_of_severity = function
  | Off -> "off"
  | Warn -> "warning"
  | Err -> "error"

let severity_of_string = function
  | "off" -> Some Off
  | "warn" -> Some Warn
  | "error" -> Some Err
  | _ -> None

let severity_cmp =
  let int_of_severity = function
    | Off -> 0
    | Warn -> 1
    | Err -> 2
  in fun a b -> compare (int_of_severity a) (int_of_severity b)

let severity_min a b = if severity_cmp a b < 0 then a else b

let severity_max a b = if severity_cmp a b > 0 then a else b
