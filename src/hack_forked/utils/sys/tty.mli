(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  | Dim of raw_color
  | Underline of raw_color
  | BoldUnderline of raw_color
  | DimUnderline of raw_color
  | NormalWithBG of raw_color * raw_color
  | BoldWithBG of raw_color * raw_color

type color_mode =
  | Color_Always
  | Color_Never
  | Color_Auto

val apply_color : ?color_mode:color_mode -> style -> string -> string

(*
 * Print a sequence of colorized strings to stdout/stderr, using ANSI color
 * escapes codes.
 *)
val cprint : ?color_mode:color_mode -> ?out_channel:out_channel -> (style * string) list -> unit

val cprintf :
  ?color_mode:color_mode ->
  ?out_channel:out_channel ->
  style ->
  ('a, unit, string, unit) format4 ->
  'a

(* These two functions provide a four-state TTY-friendly spinner that
 * a client can output between sleeps if it happens to be waiting on
 * a busy server (e.g. one that's initializing) *)
val spinner : ?angery_reaccs_only:bool -> unit -> string

val spinner_used : unit -> bool

(* Output a "clear current line" escape sequence to out_channel if it's
 * a TTY and a newline otherwise *)
val print_clear_line : out_channel -> unit

(* Read a single char and return immediately, without waiting for a newline. *)
val read_char : unit -> char

(* Prompt the user to pick one character out of a given list. If other
 * characters are entered, the prompt repeats indefinitely. *)
val read_choice : string -> char list -> char

(* Only print if we are attached to a tty *)
val eprintf : ('a, out_channel, unit) format -> 'a

(* Whether the terminal supports color *)
val supports_color : unit -> bool

val should_color : color_mode -> bool

(* Whether the terminal supports emoji *)
val supports_emoji : unit -> bool

(* Gets the column width of the current terminal. *)
val get_term_cols : unit -> int option
