(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | Default
  | Named
  | NamedType
  | Namespace

and source =
  | Global
  | Builtin of string  (** [Builtin "foo"] refers to a `declare module "foo"` lib *)
  | File_key of File_key.t

and export = source * kind [@@deriving show, ord]

module ExportSet : sig
  include Set.S with type elt = export

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end

type t [@@deriving show]

val empty : t

val add : string -> source -> kind -> t -> t

val merge : t -> t -> t

(** [subtract to_remove t] removes all of the exports in [to_remove] from [t], and
    also returns a list of keys that no longer are exported by any file. *)
val subtract : t -> t -> t * string list

val find : string -> t -> ExportSet.t

val find_seq : string -> t -> export Seq.t

val fold_names : f:('acc -> string -> ExportSet.t -> 'acc) -> init:'acc -> t -> 'acc

val fold : f:('acc -> string -> export -> 'acc) -> init:'acc -> t -> 'acc

val map : f:(export -> export) -> t -> t

val keys : t -> string list

val kind_is_type : kind -> bool

val kind_is_value : kind -> bool
