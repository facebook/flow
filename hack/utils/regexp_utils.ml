(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* Utility functions implemented with regular expressions.
 * Needs to be duplicated between regular ocaml and JS since the
 * JS regexp library differs. *)

let nonempty_ws_regexp = Str.regexp "[ \n\t\r\012]+"

(* Squash the whitespace in a string down the way that xhp expects it.
 * In particular, replace all whitespace with spaces and replace all
 * strings of multiple spaces with a single space. *)
let squash_whitespace s = Str.global_replace nonempty_ws_regexp " " s
