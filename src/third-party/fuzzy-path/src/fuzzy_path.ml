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

  (** Whether to use the weights of each item *)
  weighted: bool;
}

type match_result = {
  value: string;
  score: int;
}

type t

external ext_create : unit -> t = "fuzzy_create"
external ext_add_candidate : t -> string -> int -> unit = "fuzzy_add_candidate"
external ext_add_candidates : t -> (string * int) list -> unit = "fuzzy_add_candidates"
external ext_remove_candidate : t -> string -> unit = "fuzzy_remove_candidate"
external ext_match : t -> string -> options -> match_result list = "fuzzy_match"

let default_options = {
  first_match_can_be_weak = true;
  num_threads = 1;
  max_results = max_int;
  weighted = false;
}

let search ?(options = default_options) query t : match_result list =
  ext_match t query options

let add_candidate t name weight =
  ext_add_candidate t name weight;
  t

let add_candidates t to_add =
  ext_add_candidates t to_add;
  t

let remove_candidate t to_rem =
  ext_remove_candidate t to_rem;
  t

let remove_candidates t to_rem = List.fold_left remove_candidate t to_rem

let init candidates =
  let t = ext_create () in
  let t = add_candidates t candidates in
  t

external ext_fuzzy_score : string -> string -> bool -> bool -> int option = "fuzzy_score"

let fuzzy_score
    ?(boost_full_match = true)
    ?(first_match_can_be_weak = false)
    ~pattern
    word =
  ext_fuzzy_score
    (Base.String.Search_pattern.(replace_all (create "\\") ~in_:word ~with_:"/"))
    pattern
    boost_full_match
    first_match_can_be_weak
