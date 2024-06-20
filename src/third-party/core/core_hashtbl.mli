(** @inline *)
include Core_hashtbl_intf.Hashtbl with type ('a, 'b) t = ('a, 'b) Base.Hashtbl.t
