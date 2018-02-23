(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
type line_col = {
  line: int;
  col: int;
}

val create: ?file:string -> ?source_root:string -> unit -> t

val add_mapping:
  ?name:string ->
  source:string ->
  original:line_col ->
  generated:line_col ->
  t -> t

val add_source_content: source:string -> content:string -> t -> t

val version: t -> string
val string_of_mappings: t -> string
val names: t -> string list
val source_root: t -> string option
val sources: t -> string list
val sources_contents: t -> string option list

module type Json_serializer_intf = sig
  type t
  val string: string -> t
  val obj: (string * t) list -> t
  val array: t list -> t
  val number: string -> t
  val null: t
end

module type Json = sig
  type json
  val json_of_sourcemap: t -> json
end

module Make_json (Serializer : Json_serializer_intf) : (Json with type json = Serializer.t)
