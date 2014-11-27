(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** This is probably not fully safe. It'll do a reasonable job of escaping
    silly things like spaces, quotes and so on but there are probably ways
    to break it. *)
let escape_string_for_shell str =
  let escaped = Str.global_replace (Str.regexp "'") "'\"'\"'" str in
  "'" ^ escaped ^ "'"

let escape_spaces str = Str.global_replace (Str.regexp " ") "\\ " str
