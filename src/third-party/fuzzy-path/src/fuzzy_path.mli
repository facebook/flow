(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type options = {
  case_sensitive: bool;
  smart_case: bool;
      (** Prefers case-sensitive matches when the query contains an uppercase letter. *)
  num_threads: int;
  max_results: int;
  max_gap: int;
      (** Maximum gap to allow between consecutive letters in a match.
          Provide a smaller maxGap to speed up query results. *)
  root_path: string;
}

type match_result = {
  value: string;
  score: float;
      (** A number in the range [(0-1]]. Higher scores are more relevant.
          0 denotes "no match" and will never be returned. *)
}

type t

val default_options : options
val init : string list -> t
val search : ?options:options -> string -> t -> match_result list
val add_candidate : t -> string -> t
val add_candidates : t -> string list -> t
val remove_candidates : t -> string list -> t
