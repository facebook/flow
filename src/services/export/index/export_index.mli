(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module ExportMap : sig
  include WrappedMap.S with type key = export

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

type t [@@deriving show]

val empty : t

val add : string -> source -> kind -> t -> t

val merge : t -> t -> t

val merge_export_import : t -> t -> t

(** [subtract to_remove t] removes all of the exports in [to_remove] from [t], and
    also returns a list of keys that no longer are exported by any file. *)
val subtract : t -> t -> t * string list

val subtract_count : t -> t -> t

val find : string -> t -> int ExportMap.t

val find_seq : string -> t -> (export * int) Seq.t

val fold_names : f:('acc -> string -> int ExportMap.t -> 'acc) -> init:'acc -> t -> 'acc

val fold : f:('acc -> string -> export -> 'acc) -> init:'acc -> t -> 'acc

val map : f:('a -> 'b) -> 'a ExportMap.t SMap.t -> 'b ExportMap.t SMap.t

val keys : t -> string list

val kind_is_type : kind -> bool

val kind_is_value : kind -> bool
