(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* VLQ (variable-length quantity) encoder
   https://en.wikipedia.org/wiki/Variable-length_quantity *)

module type Config = sig
  val shift: int
  val char_of_digit: int -> char
end

module type S = sig
  val encode: Buffer.t -> int -> unit
end

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
end

module Base64 = Make (struct
  let shift = 5
  let base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  (* Convert a number between 0 and 63 to a base64 char *)
  let char_of_digit digit =
    if 0 <= digit && digit < String.length base64
    then base64.[digit]
    else failwith (Printf.sprintf "Must be between 0 and 63: %d" digit)
end)
