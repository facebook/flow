type out_port = out_channel
type in_port = in_channel

exception Port_closed

let write event o =
  Marshal.to_channel o event [];
  flush o

let write_opt event o_opt =
  try begin
    let () = Option.value_map o_opt ~default:() ~f:(write event) in
    o_opt
  end with
    | Sys_error(msg) when msg = "Broken pipe" -> None

let read i = try Marshal.from_channel i with
  | End_of_file -> raise Port_closed
  | Failure s when s = "input_value: truncated object" -> raise Port_closed

let handle_of_out o = Handle.get_handle @@ Unix.descr_of_out_channel o
let out_port_of_handle h = Unix.out_channel_of_descr @@ Handle.wrap_handle h

let in_port_of_in_channel ic =
  let fd = Daemon.descr_of_in_channel ic in
  Unix.in_channel_of_descr fd

let out_port_of_out_channel oc =
  let fd = Daemon.descr_of_out_channel oc in
  Unix.out_channel_of_descr fd

(** Creates a channel by making a pipe.
 * Must call this before forking the server process so the child server process
 * can write to the out port.
 *
 * NB: THe out and in ports are buffered, so it is not safe to write/read from
 * them before forking. *)
let create () =
   let parent_in, child_out = Unix.pipe () in
   (* Close descriptors on exec so they are not leaked. *)
   Unix.set_close_on_exec parent_in;
   Unix.in_channel_of_descr parent_in, Unix.out_channel_of_descr child_out
