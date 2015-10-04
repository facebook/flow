(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type raw_color =
  | Default
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type style =
  | Normal of raw_color
  | Bold of raw_color

(*
 * Print a sequence of colorized strings to stdout, using ANSI color escapes
 * codes.
 *)
val print : (style * string) list -> unit

(* These two functions provide a four-state TTY-friendly spinner that
 * a client can output between sleeps if it happens to be waiting on
 * a busy server (e.g. one that's initializing) *)
val spinner : unit -> string
val spinner_used : unit -> bool

(* Output a "clear current line" escape sequence to out_channel if it's
 * a TTY and a newline otherwise *)
val print_clear_line : out_channel -> unit
