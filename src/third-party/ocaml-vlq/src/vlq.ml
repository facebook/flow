(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* VLQ (variable-length quantity) encoder
   https://en.wikipedia.org/wiki/Variable-length_quantity *)

module type Config = sig
  val shift: int
  val char_of_digit: int -> char
  val digit_of_char: char -> int
end

module type S = sig
  val encode: Buffer.t -> int -> unit
  val decode: char Stream.t -> int
end

exception Unexpected_eof
exception Invalid_base64 of char

module Make (C: Config) = struct
  let vlq_base = 1 lsl C.shift
  let vlq_base_mask = vlq_base - 1
  let vlq_continuation_bit = vlq_base (* MSB *)

  (**
   * Converts from a two-complement value to a value where the sign bit is
   * placed in the least significant bit.  For example, as decimals:
   *   1 becomes 2 (10 binary), -1 becomes 3 (11 binary)
   *   2 becomes 4 (100 binary), -2 becomes 5 (101 binary)
   *)
  let vlq_signed_of_int value =
    if value < 0 then ((-value) lsl 1) + 1 else (value lsl 1) + 0

  (* Write the value to the buffer, as multiple characters as necessary *)
  let rec encode_vlq buf vlq =
    let digit = vlq land vlq_base_mask in
    let vlq = vlq lsr C.shift in
    if vlq = 0 then Buffer.add_char buf (C.char_of_digit digit)
    else begin
      (* set the continuation bit *)
      Buffer.add_char buf (C.char_of_digit (digit lor vlq_continuation_bit));
      encode_vlq buf vlq
    end

  (* Encodes `value` as a VLQ and writes it to `buf` *)
  let encode buf value =
    let vlq = vlq_signed_of_int value in
    encode_vlq buf vlq

  let decode =
    let rec helper (acc, shift) stream =
      let chr =
        try Stream.next stream
        with Stream.Failure -> raise Unexpected_eof
      in
      let digit = C.digit_of_char chr in
      let continued = (digit land vlq_continuation_bit) != 0 in
      let acc = acc + (digit land vlq_base_mask) lsl shift in
      if continued then helper (acc, shift + C.shift) stream else acc
    in
    fun stream ->
      let acc = helper (0, 0) stream in
      let abs = acc / 2 in
      if acc land 1 = 0 then abs else -(abs)
end

module Base64 = Make (struct
  let shift = 5
  let base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  (* Convert a number between 0 and 63 to a base64 char *)
  let char_of_digit digit =
    if 0 <= digit && digit < String.length base64
    then base64.[digit]
    else failwith (Printf.sprintf "Must be between 0 and 63: %d" digit)

  let digit_of_char chr =
    try String.index base64 chr
    with Not_found -> raise (Invalid_base64 chr)
end)
