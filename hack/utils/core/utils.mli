(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

type callstack = Callstack of string

val debug : bool ref

val profile : bool ref

val log : (string -> unit) ref

val d : string -> unit

val dn : string -> unit

module Map : sig end

val spf : ('a, unit, string) format -> 'a

val print_endlinef : ('a, unit, string, unit) format4 -> 'a

val prerr_endlinef : ('a, unit, string, unit) format4 -> 'a

val opt : ('a -> 'b -> 'a * 'c) -> 'a -> 'b option -> 'a * 'c option

val opt_fold : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a

val singleton_if : bool -> 'a -> 'a list

val smap_inter : 'a SMap.t -> 'b SMap.t -> 'a SMap.t

val imap_inter : 'a IMap.t -> 'b IMap.t -> 'a IMap.t

val smap_inter_list : 'a SMap.t list -> 'a SMap.t

val imap_inter_list : 'a IMap.t list -> 'a IMap.t

val wfold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a

val sl : string list -> string

val maybe : ('a -> 'b -> unit) -> 'a -> 'b option -> unit

val unsafe_opt_note : string -> 'a option -> 'a

val unsafe_opt : 'a option -> 'a

val inter_list : SSet.t list -> SSet.t

val list_last : ('a -> 'b) -> ('a -> unit) -> 'a list -> unit

val is_prefix_dir : string -> string -> bool

val try_with_channel : out_channel -> (out_channel -> 'a) -> (exn -> 'a) -> 'a

val try_with_stack : (unit -> 'a) -> ('a, exn * callstack) result

val iter_n_acc : int -> ('a -> 'a * 'b) -> 'a -> 'a * 'b

val map_of_list : (SMap.key * 'a) list -> 'a SMap.t

val set_of_list : SSet.elt list -> SSet.t

(* Strip NS removes only the leading backslash *)
val strip_ns : string -> string

(* Strip XHP removes only the leading colon *)
val strip_xhp_ns : string -> string

(* Strip Both removes either leading backslash and colon, or both *)
val strip_both_ns : string -> string

(* Strip All removes all backslash-based namespaces, but does nothing to XHP *)
val strip_all_ns : string -> string

val add_ns : string -> string

val split_ns_from_name : string -> string * string

val expand_namespace : (string * string) list -> string -> string

val split_class_from_method : string -> (string * string) option

val iter2_shortest : ('a -> 'b -> 'c) -> 'a list -> 'b list -> unit

val fold_fun_list : 'a -> ('a -> 'a) list -> 'a

val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

module With_complete_flag : sig
  type 'a t = {
    is_complete: bool;
    value: 'a;
  }
end

val try_finally : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

val with_context :
  enter:(unit -> 'a) -> exit:(unit -> 'b) -> do_:(unit -> 'c) -> 'c

val assert_false_log_backtrace : string option -> 'a

val infimum : 'a array -> 'b -> ('a -> 'b -> int) -> int option

val unwrap_snd : 'a * 'b option -> ('a * 'b) option
