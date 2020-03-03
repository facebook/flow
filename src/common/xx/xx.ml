(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type state

type hash = int64

external init : (int64[@unboxed]) -> state = "caml_xx_init" "caml_xx_init_unboxed"

external update : state -> string -> unit = "caml_xx_update" [@@noalloc]

external update_int : state -> 'a (* int *) -> unit = "caml_xx_update_int" [@@noalloc]

external update_int64 : state -> (int64[@unboxed]) -> unit
  = "caml_xx_update_int64" "caml_xx_update_int64_unboxed"
  [@@noalloc]

external digest : state -> (hash[@unboxed]) = "caml_xx_digest" "caml_xx_digest_unboxed" [@@noalloc]

external hash : string -> (int64[@unboxed]) -> (hash[@unboxed])
  = "caml_xx_hash" "caml_xx_hash_unboxed"
  [@@noalloc]

(* Unlike Int64.to_string, which returns a decimal string, this returns a hex
 * string which is padded out to the full 16 bytes. *)
let to_string (hash : hash) : string = Printf.sprintf "%016Lx" hash

(* 0 <= result < modulus *)
let modulo hash modulus =
  assert (modulus > 0);
  (Int64.to_int hash |> abs) mod modulus
