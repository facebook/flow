(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  sha1: string;
  bytelen: int;
}

(* In ASCII, 0-9 are codes 48-57, A-F are codes 65-70, a-f are codes 97-102.
   Any other ASCII code is not a valid hex digit. *)
let int_of_hex_digit chr =
  let invalid () = Printf.ksprintf invalid_arg "invalid hex digit `%c`" chr in
  let code = Char.code chr in
  if code < 48 then
    invalid ()
  else if code < 58 then
    code - 48
  else if code < 65 then
    invalid ()
  else if code < 71 then
    code - 55
  else if code < 97 then
    invalid ()
  else if code < 103 then
    code - 87
  else
    invalid ()

let hex_digit_of_int i =
  let invalid () = Printf.ksprintf invalid_arg "invalid value `%d`" i in
  if i < 0 then
    invalid ()
  else if i < 10 then
    Char.chr (i + 48)
  else if i < 16 then
    Char.chr (i + 87)
  else
    invalid ()

let sha1_of_sha1hex sha1hex =
  assert (String.length sha1hex = 40);
  let buf = Bytes.create 20 in
  for i = 0 to 19 do
    let hi = int_of_hex_digit sha1hex.[2 * i] in
    let lo = int_of_hex_digit sha1hex.[(2 * i) + 1] in
    Bytes.set buf i (Char.chr ((16 * hi) + lo))
  done;
  Bytes.unsafe_to_string buf

let sha1hex_of_sha1 sha1 =
  assert (String.length sha1 = 20);
  let buf = Bytes.create 40 in
  for i = 0 to 19 do
    let code = Char.code sha1.[i] in
    let hi = hex_digit_of_int (code / 16) in
    let lo = hex_digit_of_int (code mod 16) in
    Bytes.set buf (2 * i) hi;
    Bytes.set buf ((2 * i) + 1) lo
  done;
  Bytes.unsafe_to_string buf
