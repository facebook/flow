type out_port
type in_port

(** Can't read from this port anymore. *)
exception Port_closed

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

val out_port_of_handle : Handle.handle -> out_port
val in_port_of_fd : Unix.file_descr -> in_port
val in_port_of_in_channel : Debug_event.event Daemon.in_channel -> in_port
val out_port_of_out_channel : Debug_event.event Daemon.out_channel -> out_port
