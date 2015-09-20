(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let () = Random.self_init ()

let base64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let () =
  assert (String.length base64_alphabet = 64)

let short_string () =
  let r = ref ((Random.bits () lsl 30) lor Random.bits ()) in
  let cs = ref [] in
  while !r > 0 do
    let c = base64_alphabet.[!r mod 64] in
    cs := String.make 1 c :: !cs;
    r := !r lsr 6
  done;
  String.concat "" !cs
