(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t
and original = {
  source: string;
  original_loc: line_col;
  name: string option;
}
and line_col = {
  line: int;
  col: int;
}

val create: ?file:string -> ?source_root:string -> unit -> t

val freeze_for_lookup: t -> t
val find_original: t -> line_col -> original option

val compose: t -> t -> t

val add_mapping: original:original -> generated:line_col -> t -> t

val add_source_content: source:string -> content:string -> t -> t

val version: t -> string
val file: t -> string option
val string_of_mappings: t -> string
val names: t -> string list
val source_root: t -> string option
val sources: t -> string list
val sources_contents: t -> string option list

module type Json_writer_intf = sig
  type t
  val of_string: string -> t
  val of_obj: (string * t) list -> t
  val of_array: t list -> t
  val of_number: string -> t
  val null: t
end

module type Json_reader_intf = sig
  type t
  val to_string: t -> string
  val to_obj: t -> (string * t) list
  val to_array: t -> t list
  val to_number: t -> string
  val is_null: t -> bool
end

module type Json_writer = sig
  type json
  val json_of_sourcemap: t -> json
end

module type Json_reader = sig
  type json
  val sourcemap_of_json: json -> t
end

module Make_json_writer (Writer : Json_writer_intf) : (Json_writer with type json = Writer.t)
module Make_json_reader (Reader : Json_reader_intf) : (Json_reader with type json = Reader.t)
