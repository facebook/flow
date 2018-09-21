type state
type hash

external init: unit -> state = "caml_xx_init"
external update: state -> string -> unit = "caml_xx_update" [@@noalloc]
external update_int: state -> 'a (* int *) -> unit = "caml_xx_update_int" [@@noalloc]
external digest: state -> hash = "caml_xx_digest"

(* Returns a 16 character hex string representation of the hash *)
external to_string: hash -> string = "caml_xx_to_string"

let foo x = x
