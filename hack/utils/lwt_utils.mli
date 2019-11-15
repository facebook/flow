(** Drop-in replacement for [Unix.select] that works even when the Lwt main loop
is running (i.e. your function has [Lwt_main.run] somewhere higher up in the
call stack).

The Lwt main loop is an event loop pumped by [Unix.select], and so regular
[Unix.select] calls are prone to raising `EINTR`. The implementation of this
function does not use [Unix.select] at all, but Lwt primitives that accomplish
the same thing.
*)
val select :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list) Lwt.t

module Process_success : sig
  type t = {
    command_line: string;
    stdout: string;
    stderr: string;
  }
end

module Process_failure : sig
  type t = {
    command_line: string;
    process_status: Unix.process_status;
    stdout: string;
    stderr: string;
    exn: exn option;
  }

  val to_string : t -> string
end

(** Run a command with a given input and return the output. If the command exits
with an exit status other than zero, raises [Process_failure] instead.

NOTE: on cancellation, this function will not kill the underlying process. (I
tried to implement it, but after killing the process, both [Lwt_io.close] and
[Lwt_io.abort] would hang when trying to close the process's
stdin/stdout/stderr.)
*)
val exec_checked :
  ?input:string ->
  ?env:string array ->
  string ->
  string array ->
  (Process_success.t, Process_failure.t) Lwt_result.t

(** Asynchronous version of [Utils.try_finally]. Run and wait for [f] to
complete, and be sure to invoke [finally] asynchronously afterward, even if [f]
raises an exception. *)
val try_finally :
  f:(unit -> 'a Lwt.t) -> finally:(unit -> unit Lwt.t) -> 'a Lwt.t

(** Reads all the contents from the given file on disk, or returns an error
message if unable to do so. *)
val read_all : string -> (string, string) Lwt_result.t

module Promise : Promise.S with type 'a t = 'a Lwt.t
