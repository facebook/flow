(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

type search_options = Fuzzy_path.options

type search_result = {
  name: string;
  source: Export_index.source;
  kind: Export_index.kind;
}
[@@deriving show]

type search_results = {
  results: search_result list;
  is_incomplete: bool;
}
[@@deriving show]

val init : Export_index.t -> t

val merge : Export_index.t -> t -> t

val subtract : Export_index.t -> t -> t

val default_options : search_options

val search_values : ?options:search_options -> string -> t -> search_results

val search_types : ?options:search_options -> string -> t -> search_results

val get : string -> t -> Export_index.ExportSet.t

val get_values : string -> t -> Export_index.ExportSet.t

val get_types : string -> t -> Export_index.ExportSet.t
