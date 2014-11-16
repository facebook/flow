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

let color_num = function
  | Default -> "0"
  | Black   -> "30"
  | Red     -> "31"
  | Green   -> "32"
  | Yellow  -> "33"
  | Blue    -> "34"
  | Magenta -> "35"
  | Cyan    -> "36"
  | White   -> "37"

let style_num = function
  | Normal c -> color_num c
  | Bold c   -> color_num c ^ ";1"

let print_one (c,s) =
  Printf.printf "\x1b[%sm%s\x1b[0m" (style_num c) (s)

let print strs = List.iter print_one strs

let (spinner, spinner_used) =
  let state = ref 0 in
  (fun () ->
    begin
      let str = List.nth ["-"; "\\"; "|"; "/"] (!state mod 4) in
      state := !state + 1;
      str
    end),
  (fun () -> !state <> 0)

(* ANSI escape sequence to clear whole line *)
let clear_line_seq = "\r\x1b[0K"

let print_clear_line chan =
  if Unix.isatty (Unix.descr_of_out_channel chan)
  then Printf.fprintf chan "%s%!" clear_line_seq
  else Printf.fprintf chan "\n%!"
