(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Helpers for handling timeout, in particular input timeout. *)

type t

(* The function `with_timeout` executes 'do_' for at most 'timeout'
   seconds. If the `timeout` is reached, the `on_timeout` is executed
   if available, otherwise the `Timeout` exception is raised.

   On Unix platform, this function is based on `SIGALRM`. On Windows
   platform, this is based on the equivalent of `select`. Hence, this
   module exports variant of basic input functions, adding them a
   `timeout` parameter. It should correspond to the parameter of the
   `do_` function.

   For `do_` function based only on computation (and not I/O), you
   should call the `check_timeout` function on a regular
   basis. Otherwise, on Windows, the timeout will never be detected.
   On Unix, the function `check_timeout` is no-op.

   On Unix, the type `in_channel` is in fact an alias for
   `Pervasives/Stdlib.in_channel`.

*)
val with_timeout : timeout:int -> on_timeout:(unit -> 'a) -> do_:(t -> 'a) -> 'a

val check_timeout : t -> unit

type in_channel

val open_in : string -> in_channel

val close_in : in_channel -> unit

val close_in_noerr : in_channel -> unit

val in_channel_of_descr : Unix.file_descr -> in_channel

val descr_of_in_channel : in_channel -> Unix.file_descr

val select :
  ?timeout:t ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

val input : ?timeout:t -> in_channel -> bytes -> int -> int -> int

val really_input : ?timeout:t -> in_channel -> bytes -> int -> int -> unit

val input_char : ?timeout:t -> in_channel -> char

val input_line : ?timeout:t -> in_channel -> string

val input_value : ?timeout:t -> in_channel -> 'a

val open_process : string -> string array -> in_channel * out_channel

val open_process_in : string -> string array -> in_channel

val close_process_in : in_channel -> Unix.process_status

val read_process :
  timeout:int ->
  on_timeout:(unit -> 'a) ->
  reader:(t -> in_channel -> out_channel -> 'a) ->
  string ->
  string array ->
  'a

val open_connection : ?timeout:t -> Unix.sockaddr -> in_channel * out_channel

val read_connection :
  timeout:int ->
  on_timeout:(unit -> 'a) ->
  reader:(t -> in_channel -> out_channel -> 'a) ->
  Unix.sockaddr ->
  'a

val shutdown_connection : in_channel -> unit

(* Some silly people like to catch all exceptions. This means they need to explicitly detect and
 * reraise the timeout exn. *)
val is_timeout_exn : t -> exn -> bool
