(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  buffer: Buffer.t;
  sourcemap: Sourcemap.t option;
  pos: Sourcemap.line_col;
  loc_stack: Loc.t list;
  names: Source_map_config.names option
}

val create: source_maps:Source_map_config.t option -> unit -> t

val push_loc: Loc.t -> t -> t
val pop_loc: t -> t

val add_string: ?name:string -> string -> t -> t
val add_identifier: Loc.t -> string -> t -> t
val add_newline: t -> t
val add_space: int -> t -> t
val add_source: t -> t -> t

val contents: t -> string
val json_of_source : t -> Hh_json.json
