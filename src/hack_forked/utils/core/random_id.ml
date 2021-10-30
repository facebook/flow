(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Do not use / in random ids as they appear in filenames. *)
let alphanumeric_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

(** Generates a random string containing characters from [alphabet].

    Be sure to initialize [Random] (e.g. [Random.self_init ()]). *)
let short_string_with_alphabet alphabet =
  let r = ref ((Random.bits () lsl 30) lor Random.bits ()) in
  let cs = ref [] in
  while !r > 0 do
    let c = alphabet.[!r mod String.length alphabet] in
    cs := String.make 1 c :: !cs;
    r := !r lsr 6
  done;
  String.concat "" !cs

(** Generates a random alphanumeric string.

    Be sure to initialize [Random] (e.g. [Random.self_init ()]). *)
let short_string () = short_string_with_alphabet alphanumeric_alphabet
