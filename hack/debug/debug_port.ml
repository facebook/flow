type out_port = Pervasives.out_channel
type in_port = Timeout.in_channel

exception Port_closed

let write event o =
  Marshal.to_channel o event []

let write_opt event o_opt =
  try begin
    let () = Option.value_map o_opt ~default:() ~f:(write event) in
    o_opt
  end with
    | Sys_error(msg) when msg = "Broken pipe" -> None

let read i = try Timeout.input_value i with
  | End_of_file -> raise Port_closed
  | Failure s when s = "input_value: truncated object" -> raise Port_closed
  | _ -> raise Port_closed

(** Type ceremony. This is a little messy because of a few things: we have
 * custom Debug_port.in_port and out_port types to force reads and writes
 * to go through this module; the underlying channels themselves may be
 * created by a Daemon.spawn, or by a Unix.pipe call which produce different
 * types. *)
let out_port_of_handle h = Unix.out_channel_of_descr @@ Handle.wrap_handle h
let in_port_of_fd fd = Timeout.in_channel_of_descr fd
let in_port_of_in_channel ic = Daemon.cast_in ic
let out_port_of_out_channel oc = Daemon.cast_out oc
