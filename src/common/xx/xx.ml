type state
type hash (* = string *)

external init: unit -> state = "caml_xx_init"
external update: state -> string -> unit = "caml_xx_update" [@@noalloc]
external update_int: state -> 'a (* int *) -> unit = "caml_xx_update_int" [@@noalloc]
external digest: state -> hash = "caml_xx_digest"
