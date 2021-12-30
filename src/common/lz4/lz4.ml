(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let max_input_size = 0x7E000000

let compress_bound input_size =
  if input_size > max_input_size then
    0
  else
    input_size + (input_size / 255) + 16

external compress_default : string -> bytes -> int = "caml_lz4_compress_default" [@@noalloc]

external decompress_safe : buf -> bytes -> int = "caml_lz4_decompress_safe" [@@noalloc]
