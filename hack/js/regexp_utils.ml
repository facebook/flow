(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Utility functions implemented with regular expressions.
 * Needs to be duplicated between regular ocaml and JS since the
 * JS regexp library differs. *)

let nonempty_ws_regexp = Regexp.regexp "[ \n\t\r\012]+"
let squash_whitespace s = Regexp.global_replace nonempty_ws_regexp " " s
