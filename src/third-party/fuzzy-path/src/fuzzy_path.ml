(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  case_sensitive: bool;

  (** Prefers case-sensitive matches when the query contains an uppercase letter. *)
  smart_case: bool;

  num_threads: int;

  max_results: int;

  (** Maximum gap to allow between consecutive letters in a match.
      Provide a smaller maxGap to speed up query results. *)
  max_gap: int;

  root_path: string;
}

type match_result = {
  value: string;

  (** A number in the range (0-1]. Higher scores are more relevant.
      0 denotes "no match" and will never be returned. *)
  score: int;
}

type t

external ext_create : unit -> t = "fuzzy_create"
external ext_add_candidate : t -> string -> unit = "fuzzy_add_candidate"
external ext_add_candidates : t -> string list -> unit = "fuzzy_add_candidates"
external ext_remove_candidate : t -> string -> unit = "fuzzy_remove_candidate"
external ext_match : t -> string -> options -> match_result list = "fuzzy_match"

let default_options = {
  case_sensitive = false;
  smart_case = false;
  num_threads = 1;
  max_results = max_int;
  max_gap = 0;
  root_path = "";
}

let search ?(options = default_options) query t : match_result list =
  ext_match t query options

let add_candidate t to_add =
  ext_add_candidate t to_add;
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
