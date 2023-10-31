(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

(* For every read of a variable x, we are interested in tracking writes to x
   that can reach that read. Ultimately the writes are going to be represented
   as a list of locations, where each location corresponds to a "single static
   assignment" of the variable in the code. But for the purposes of analysis, it
   is useful to represent these writes with a data type that contains either a
   single write, or a "join" of writes (in compiler terminology, a PHI node), or
   a reference to something that is unknown at a particular point in the AST
   during traversal, but will be known by the time traversal is complete. *)
type t = {
  id: int;
  write_state: write_state;
}

and write_state

val empty : unit -> t

val uninitialized : ALoc.t -> t

val undefined : reason -> t

val number : reason -> t

val merge : t -> t -> t

val empty_array : reason -> Loc_collections.ALocSet.t -> t

val function_this : reason -> t

val global_this : reason -> t

val illegal_this : reason -> t

val class_instance_this : reason -> t

val class_static_this : reason -> t

val class_instance_super : reason -> t

val class_static_super : reason -> t

val module_scoped : string -> t

val global : string -> t

val one : ALoc.t virtual_reason -> t

val providers : Provider_api.provider list -> t

val illegal_write : ALoc.t virtual_reason -> t

val of_write : write_state -> t

val simplify : ALoc.t option -> Bindings.kind option -> string option -> t -> Env_api.read

val id_of_val : t -> int

val base_id_of_val : t -> int

val refinement : int -> t -> t

val projection : ALoc.t -> t

val undeclared : string -> ALoc.t -> t

val declared_but_skipped : string -> ALoc.t -> t

val declared_function : ALoc.t -> t

(* unwraps a Refinement into just the underlying write *)
val unrefine : int -> t -> t

val unrefine_deeply : int -> t -> t

(* Replace the base write of the refinement with the new base.
   If the write is not a refinement, replace the entire write with the base.

   This is useful for attaching a refinement that is known to be associated with a write, but
   is not attached due to syntactic difference.
   e.g. refinements on obj.x should be attached to x in const {x} = obj *)
val replace_refinement_base_write : base:t -> t -> t

val writes_of_uninitialized : (int -> bool) -> t -> write_state list

val is_global_undefined : t -> bool

val is_global : t -> bool

val is_undeclared : t -> bool

val is_undeclared_or_skipped : t -> bool

val is_declared_function : t -> bool

val is_projection : t -> bool

val debug_to_string : (int -> 'a * Env_api.refinement_kind) -> t -> string

val clear : unit -> unit
