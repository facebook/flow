val select :
    Unix.file_descr list ->
    Unix.file_descr list ->
    Unix.file_descr list ->
    float ->
    (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list) Lwt.t
(** Drop-in replacement for [Unix.select] that works even when the Lwt main loop
is running (i.e. your function has [Lwt_main.run] somewhere higher up in the
call stack).

The Lwt main loop is an event loop pumped by [Unix.select], and so regular
[Unix.select] calls are prone to raising `EINTR`. The implementation of this
function does not use [Unix.select] at all, but Lwt primitives that accomplish
the same thing.
*)
