(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

let initialized = ref false

(* Do not use / in random ids as they appear in filenames. *)
let alphanumeric_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

let short_string_with_alphabet alphabet =
  (* If we haven't seeded random then do it now *)
  if not !initialized then begin
    initialized := true;
    Random.self_init ()
  end;
  let r = ref ((Random.bits () lsl 30) lor Random.bits ()) in
  let cs = ref [] in
  while !r > 0 do
    let c = alphabet.[!r mod (String.length alphabet)] in
    cs := String.make 1 c :: !cs;
    r := !r lsr 6
  done;
  String.concat "" !cs

let short_string () =
  short_string_with_alphabet alphanumeric_alphabet
