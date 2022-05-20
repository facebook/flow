(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

external ( .!() ) : (int * int) array -> int -> int * int = "%array_unsafe_get"

let rec search (arr : _ array) (start : int) (finish : int) target =
  if start > finish then
    false
  else
    let mid = start + ((finish - start) / 2) in
    let (a, b) = arr.!(mid) in
    if target < a then
      search arr start (mid - 1) target
    else if target >= b then
      search arr (mid + 1) finish target
    else
      true

let is_valid_unicode_id (i : int) =
  search Js_id_unicode.id_continue 0 (Array.length Js_id_unicode.id_continue - 1) i
