(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let breaks = "\r\n"

let rec eol s x i =
  if i >= x then
    x
  else if String.contains breaks s.[i] then
    i
  else
    eol s x (i + 1)

let rec line s x n i =
  if n <= 0 then
    (i, eol s x (i + 1))
  else
    let i = eol s x i in
    if i >= x then
      (x, x)
    else
      line s x (n - 1) (i + 1)

let split_nth s n =
  let x = String.length s in
  let (i, j) = line s x n 0 in
  if i = x then
    None
  else
    Some String.(sub s 0 i, sub s i (j - i), sub s j (x - j))

let transform_nth s n f =
  match split_nth s n with
  | Some (pre, s, post) -> pre ^ f s ^ post
  | None -> s
