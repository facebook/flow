type out_port
type in_port

(** Optionally write the event to the optional port (does nothing if None). On
 * failure (e.g. pipe broken) returns None.
 *
 * Note: It is safe to ignore the returned `out_port option` if you are not
 * in a context where the debug_port can be conveniently modified. The returned
 * value flipping into a None is only a cached error condition. *)
val write_opt : Debug_event.event -> out_port option -> out_port option
(** Uses Marshal.from_channel to read and transpoarently throws any
 * exceptions that it may throw. *)
val read : in_port -> Debug_event.event

val create : unit -> in_port * out_port

(** Get the handle of the out port. Use this to get the handle to pass as
 * commandline arg when forking the server process. *)
val handle_of_out : out_port -> Handle.handle
val out_port_of_handle : Handle.handle -> out_port
