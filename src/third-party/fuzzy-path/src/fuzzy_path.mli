(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  first_match_can_be_weak: bool;

  num_threads: int;

  max_results: int;

  weighted: bool;
}

type match_result = {
  value: string;
  score: int;
}

type t

val default_options : options

val init : (string * int) list -> t

val search : ?options:options -> string -> t -> match_result list

val add_candidate : t -> string -> int -> t

val add_candidates : t -> (string * int) list -> t

val remove_candidates : t -> string list -> t

val fuzzy_score :
  ?boost_full_match:bool ->
  ?first_match_can_be_weak:bool ->
  pattern:string ->
  string ->
  int option
