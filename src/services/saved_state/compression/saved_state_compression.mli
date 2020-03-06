(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type compressed

(* Pass in any OCaml value. We'll marshal it to a string & compress that *)
val marshal_and_compress : 'a -> compressed

(* Given the output of `marshal_and_compress`, decompress it and unmarshal it back to the original
 * OCaml value *)
val decompress_and_unmarshal : compressed -> 'a

(* How many bytes is the compressed data *)
val compressed_size : compressed -> int

(* How many bytes was the uncompressed (but marshaled) data *)
val uncompressed_size : compressed -> int
