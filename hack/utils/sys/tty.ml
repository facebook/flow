(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_core

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

let text_num = function
  | Default -> "39"
  | Black   -> "30"
  | Red     -> "31"
  | Green   -> "32"
  | Yellow  -> "33"
  | Blue    -> "34"
  | Magenta -> "35"
  | Cyan    -> "36"
  | White   -> "37"

let background_num = function
  | Default -> "49"
  | Black   -> "40"
  | Red     -> "41"
  | Green   -> "42"
  | Yellow  -> "43"
  | Blue    -> "44"
  | Magenta -> "45"
  | Cyan    -> "46"
  | White   -> "47"

let color_num = function
  | Default -> "0"
  | x -> text_num x

let style_num = function
  | Normal c -> color_num c
  | Bold c   -> color_num c ^ ";1"
  | Dim c    -> color_num c ^ ";2"
  | Underline c -> color_num c ^ ";4"
  | BoldUnderline c -> color_num c ^ ";1;4"
  | DimUnderline c -> color_num c ^ ";2;4"
  | NormalWithBG (text, bg) -> (text_num text) ^ ";" ^ (background_num bg)
  | BoldWithBG (text, bg) -> (text_num text) ^ ";" ^ (background_num bg) ^ ";1"

let supports_color =
  let memo = ref None in
  fun () ->
    match !memo with Some x -> x | None -> begin
      let value = match Sys_utils.getenv_term () with
      | None -> false
      | Some term -> Unix.isatty Unix.stdout && term <> "dumb"
      in
      memo := Some value;
      value
    end

let should_color color_mode =
  match color_mode with
  | Color_Always -> true
  | Color_Never -> false
  | Color_Auto -> supports_color ()

(* See https://github.com/yarnpkg/yarn/issues/405. *)
let supports_emoji () = Sys.os_type <> "Win32" && supports_color ()

let print_one ?(color_mode=Color_Auto) ?(out_channel=stdout) c s =
  if should_color color_mode
  then Printf.fprintf out_channel "\x1b[%sm%s\x1b[0m" (style_num c) (s)
  else Printf.fprintf out_channel "%s" s

let cprint ?(color_mode=Color_Auto) ?(out_channel=stdout) strs =
  List.iter strs (fun (c, s) -> print_one ~color_mode ~out_channel c s)

let cprintf ?(color_mode=Color_Auto) ?(out_channel=stdout) c =
  Printf.ksprintf (print_one ~color_mode ~out_channel c)

let (spinner, spinner_used) =
  let state = ref 0 in
  (fun () ->
    begin
      let str = List.nth_exn ["-"; "\\"; "|"; "/"] (!state mod 4) in
      state := !state + 1;
      str
    end),
  (fun () -> !state <> 0)

(* ANSI escape sequence to clear whole line *)
let clear_line_seq = "\r\x1b[0K"

let print_clear_line chan =
  if Unix.isatty (Unix.descr_of_out_channel chan)
  then Printf.fprintf chan "%s%!" clear_line_seq
  else ()

(* Read a single char and return immediately, without waiting for a newline.
 * `man termios` to see how termio works. *)
let read_char () =
  let tty = Unix.(openfile "/dev/tty" [O_RDWR] 0o777) in
  let termio = Unix.tcgetattr tty in
  let new_termio = {termio with Unix.
    c_icanon = false;
    c_vmin = 1;
    c_vtime = 0;
  } in
  Unix.tcsetattr tty Unix.TCSANOW new_termio;
  let buf = Bytes.create 1 in
  let bytes_read = UnixLabels.read tty ~buf ~pos:0 ~len:1 in
  Unix.tcsetattr tty Unix.TCSANOW termio;
  assert (bytes_read = 1);
  Bytes.get buf 0

(* Prompt the user to pick one character out of a given list. If other
 * characters are entered, the prompt repeats indefinitely. *)
let read_choice message choices =
  let rec loop () =
    Printf.printf "%s (%s)%!" message
      (String.concat "|" (List.map choices String_utils.string_of_char));
    let choice = read_char () in
    print_newline ();
    if List.mem choices choice then choice else loop ()
  in loop ()

let eprintf fmt =
  if Unix.(isatty stderr)
  then Printf.eprintf fmt
  else Printf.ifprintf stderr fmt

(* Gets the number of columns in the current terminal window through
 * [`tput cols`][1]. If the command fails in any way then `None` will
 * be returned.
 *
 * This value may change over the course of program execution if a user resizes
 * their terminal.
 *
 * [1]: http://invisible-island.net/ncurses/man/tput.1.html
 *)
let get_term_cols () =
  if not Sys.unix then
    None
  else
    try
      Some (int_of_string (Sys_utils.exec_read "tput cols"))
    with
      _ -> None
