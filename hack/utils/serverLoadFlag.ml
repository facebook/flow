(* TODO: turn this into a bool option with None indicating
 * an uninitialized state for greater strictness *)
let no_load_ref : bool ref = ref false

let set_no_load b = no_load_ref := b

let get_no_load () = !no_load_ref
