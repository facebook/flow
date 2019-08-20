(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
type state
type hash = int64

external init: unit -> state = "caml_xx_init"
external update: state -> string -> unit = "caml_xx_update" [@@noalloc]
external update_int: state -> 'a (* int *) -> unit = "caml_xx_update_int" [@@noalloc]
external update_int64: state -> (int64 [@unboxed]) -> unit = "caml_xx_update_int64" "caml_xx_update_int64_unboxed" [@@noalloc]
external digest: state -> (hash [@unboxed]) = "caml_xx_digest" "caml_xx_digest_unboxed" [@@noalloc]

(* Unlike Int64.to_string, which returns a decimal string, this returns a hex
 * string which is padded out to the full 16 bytes. *)
external to_string: (hash [@unboxed]) -> string = "caml_xx_to_string" "caml_xx_to_string_unboxed"

(* 0 <= result < modulus *)
let modulo hash modulus =
  assert (modulus > 0);
  (Int64.to_int hash |> abs) mod modulus
