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
  | NormalWithBG of raw_color * raw_color
  | BoldWithBG of raw_color * raw_color

type color_mode =
  | Color_Always
  | Color_Never
  | Color_Auto

(*
 * Print a sequence of colorized strings to stdout, using ANSI color escapes
 * codes.
 *)
val print : ?color_mode:color_mode -> (style * string) list -> unit
val printf : ?color_mode:color_mode -> style -> ('a, unit, string, unit) format4 -> 'a

(* These two functions provide a four-state TTY-friendly spinner that
 * a client can output between sleeps if it happens to be waiting on
 * a busy server (e.g. one that's initializing) *)
val spinner : unit -> string
val spinner_used : unit -> bool

(* Output a "clear current line" escape sequence to out_channel if it's
 * a TTY and a newline otherwise *)
val print_clear_line : out_channel -> unit

(* Read a single char and return immediately, without waiting for a newline. *)
val read_char : unit -> char

(* Prompt the user to pick one character out of a given list. If other
 * characters are entered, the prompt repeats indefinitely. *)
val read_choice : string -> char list -> char
