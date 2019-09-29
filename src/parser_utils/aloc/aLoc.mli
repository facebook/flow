(**
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

type t

(* Creates an ALoc.t with a concrete underlying representation *)
val of_loc : Loc.t -> t

(* Takes an ALoc.t with a concrete underlying representation and makes it abstract.
 *
 * Preconditions:
 * - The given location cannot have already been abstractified.
 * - The file key with which the table was created must match the `source` of the given location.
 *   - This also implies that locations with `None` as the source cannot be abstractified. This
 *     could be relaxed in the future if necessary.
 * *)
val abstractify : table -> t -> t

(* Takes an ALoc.t with a concrete underlying representation and finds
 * the existing abstract representation for it from a reverse table
 *
 * Preconditions:
 * - The file key with which the table was created must match the `source` of the given location.
 * *)
val lookup_key_if_possible : reverse_table Lazy.t -> t -> t

(* Converts an ALoc.t back to a Loc.t, looking up the underlying location in the given table if
 * necessary. We will have to look up tables in the shared heap at some point, so making it lazy
 * allows us to avoid fetching the table if the underlying location is actually concrete. *)
val to_loc : table Lazy.t -> t -> Loc.t

(* Like to_loc, but conveniently picks the correct table for the conversion out of the map *)
val to_loc_with_tables : table Lazy.t Utils_js.FilenameMap.t -> t -> Loc.t

(* TODO move to ALocRepresentationDoNotUse *)
(* Unsafe: fails if the location has an abstract underlying representation. *)
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
 * This comparison also does not throw an error when concrete and abstract
 * locations are compared.
 *)
val quick_compare : t -> t -> int

val equal : t -> t -> bool

(* If one of the provided locations has an abstract underlying representation, and the other is
 * concrete, attempt to concretize the abstract one using the given table, before comparing *)
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
  val is_abstract : t -> bool

  (* Should only be called if `is_abstract` returns `true`. Otherwise it will raise *)
  val get_key_exn : t -> key

  val string_of_key : key -> string
end
