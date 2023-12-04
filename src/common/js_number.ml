(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* In JavaScript, `Number.MAX_SAFE_INTEGER` (2^53-1)
   represents the maximum safe integer value. *)
let max_safe_integer = 9007199254740991.0

(* In JavaScript, `Number.MIN_SAFE_INTEGER` (-2^53-1)
   represents the minimum safe integer value. *)
let min_safe_integer = -.max_safe_integer

let in_safe_integer_range value = value >= min_safe_integer && value <= max_safe_integer

let is_float_safe_integer value = Float.is_integer value && in_safe_integer_range value
