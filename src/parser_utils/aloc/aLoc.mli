(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type table

val make_table : File_key.t -> table

val shrink_table : table -> unit

type reverse_table

type key

type t [@@deriving show]

(* Creates an ALoc.t with a concrete underlying representation *)
val of_loc : Loc.t -> t

(* Takes an ALoc.t with a concrete underlying representation and finds
 * the existing keyed representation for it from a reverse table
 *
 * Preconditions:
 * - The file key with which the table was created must match the `source` of the given location.
 * *)
type id = private t [@@deriving show]

val id_none : id

val id_of_aloc : reverse_table Lazy.t -> t -> id

val equal_id : id -> id -> bool

(* Converts an ALoc.t back to a Loc.t, looking up the underlying location in the given table if
 * necessary. We will have to look up tables in the shared heap at some point, so making it lazy
 * allows us to avoid fetching the table if the underlying location is actually concrete. *)
val to_loc : table Lazy.t -> t -> Loc.t

(* Like to_loc, but conveniently picks the correct table for the conversion out of the map *)
val to_loc_with_tables : table Lazy.t Utils_js.FilenameMap.t -> t -> Loc.t

(* TODO move to ALocRepresentationDoNotUse *)
(* Unsafe: fails if the location has an keyed underlying representation. *)
val to_loc_exn : t -> Loc.t

(* The specific contents of this string should not be used to influence typechecking, but it can be
 * used as a unique identifier within a given source file. *)
val to_string_no_source : t -> string

val none : t

val source : t -> File_key.t option

val update_source : (File_key.t option -> File_key.t option) -> t -> t

val compare : t -> t -> int

(* Only does the expensive source compare if positional comparisons tie.
 * This is useful for data structures that do not need equal files to be
 * sorted closely to each other.
 *
 * This comparison also does not throw an error when concrete and keyed
 * locations are compared.
 *)
val quick_compare : t -> t -> int

val equal : t -> t -> bool

(* If one of the provided locations has an keyed underlying representation, and the other is
 * concrete, attempt to concretize the keyed one using the given table, before comparing *)
val concretize_compare : table Lazy.t Utils_js.FilenameMap.t -> t -> t -> int

val concretize_equal : table Lazy.t Utils_js.FilenameMap.t -> t -> t -> bool

(* Stringifies the underlying representation of the ALoc.t, without concretizing it, for debugging
 * purposes. If you make any typechecking behavior depend on the result of this function you are a
 * bad person. *)
val debug_to_string : ?include_source:bool -> t -> string

val reverse_table : table -> reverse_table

val make_empty_reverse_table : unit -> reverse_table

(* Exposes the internal representation of an ALoc.t. Typechecking behavior should not be
 * made to depend on this module. If you find yourself tempted to use anything here, really think
 * through your options. *)
module ALocRepresentationDoNotUse : sig
  val is_keyed : t -> bool

  (* Should only be called if `is_keyed` returns `true`. Otherwise it will raise *)
  val get_key_exn : t -> key

  val string_of_key : key -> string

  val make_table : File_key.t -> Loc.t array -> table

  val init_table : File_key.t -> int -> (unit -> Loc.t) -> table

  val make_keyed : File_key.t option -> int -> t

  val make_id : File_key.t option -> int -> id
end
