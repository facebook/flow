(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  index: Export_index.t;
  value_matcher: Fuzzy_path.t;
  type_matcher: Fuzzy_path.t;
}

type search_result = {
  name: string;
  source: Export_index.source;
  kind: Export_index.kind;
}
[@@deriving show]

type search_result_scored = {
  search_result: search_result;
  score: int;
  weight: int;
}
[@@deriving show]

type search_results = {
  results: search_result_scored list;
  is_incomplete: bool;
}
[@@deriving show]

type search_options = Fuzzy_path.options = {
  first_match_can_be_weak: bool;
  num_threads: int;
  max_results: int;
  weighted: bool;
}

let default_options : search_options =
  Fuzzy_path.{ default_options with first_match_can_be_weak = false }

type candidates = {
  values: (string * int) list;
  types: (string * int) list;
}

let summarize_exports exports =
  Export_index.ExportMap.fold
    (fun (_file, kind) num (has_value, has_type, max_count) ->
      let max_count = max num max_count in
      match kind with
      | Export_index.Default -> (true, has_type, max_count)
      | Export_index.Named -> (true, has_type, max_count)
      | Export_index.NamedType -> (has_value, true, max_count)
      | Export_index.Namespace -> (true, has_type, max_count))
    exports
    (false, false, 0)

let partition_candidates index =
  Export_index.fold_names
    ~f:(fun { values; types } name exports ->
      let (has_value, has_type, max_count) = summarize_exports exports in
      let values =
        if has_value then
          (name, max_count) :: values
        else
          values
      in
      let types =
        if has_type then
          (name, max_count) :: types
        else
          types
      in
      { values; types })
    ~init:{ values = []; types = [] }
    index

let init index =
  let { values; types } = partition_candidates index in
  let value_matcher = Fuzzy_path.init values in
  let type_matcher = Fuzzy_path.init types in
  { index; value_matcher; type_matcher }

let subtract old_index { index; value_matcher; type_matcher } =
  let (index, dead_candidates) = Export_index.subtract old_index index in
  let value_matcher = Fuzzy_path.remove_candidates value_matcher dead_candidates in
  let type_matcher = Fuzzy_path.remove_candidates type_matcher dead_candidates in
  { index; value_matcher; type_matcher }

let subtract_count removed_imports { index; value_matcher; type_matcher } =
  let index = Export_index.subtract_count removed_imports index in
  { index; value_matcher; type_matcher }

let merge new_index { index; value_matcher; type_matcher } =
  let index = Export_index.merge new_index index in
  let { values; types } = partition_candidates new_index in
  let value_matcher = Fuzzy_path.add_candidates value_matcher values in
  let type_matcher = Fuzzy_path.add_candidates type_matcher types in
  { index; value_matcher; type_matcher }

(*Merge_import *)
let merge_export_import new_index { index; value_matcher; type_matcher } =
  let index = Export_index.merge_export_import new_index index in
  { index; value_matcher; type_matcher }

type query =
  | Value of string
  | Type of string

let string_of_query = function
  | Value str
  | Type str ->
    str

let search_result_of_export ~query name source kind =
  let open Export_index in
  match (query, kind) with
  | (Value _, (Default | Named | Namespace))
  | (Type _, NamedType) ->
    Some { name; source; kind }
  | (Value _, NamedType)
  | (Type _, (Default | Named | Namespace)) ->
    None

let compare_search_result ~compare_score a b =
  match compare_score a b with
  | 0 ->
    (match Int.compare b.weight a.weight with
    | 0 ->
      (match String.compare a.search_result.name b.search_result.name with
      | 0 ->
        Export_index.compare_export
          (a.search_result.source, a.search_result.kind)
          (b.search_result.source, b.search_result.kind)
      | k -> k)
    | k -> k)
  | k -> k

(** [take ~n:20 ~index matches] will return up to 20 search results,
    where each match in [matches] might contribute multiple results.
    sets [is_incomplete] if [n] is exceeded. *)
let take ~weighted ~n ~index ~query fuzzy_matches =
  let rev_all =
    Base.List.fold fuzzy_matches ~init:[] ~f:(fun acc { Fuzzy_path.value; score } ->
        Export_index.ExportMap.fold
          (fun (source, kind) count acc ->
            match search_result_of_export ~query value source kind with
            | Some search_result ->
              let weight =
                if weighted then
                  count
                else
                  0
              in
              { search_result; score; weight } :: acc
            | None -> acc)
          (Export_index.find value index)
          acc
    )
  in
  let sorted =
    Base.List.stable_sort
      rev_all
      ~compare:
        ((* sorts the highest scores to the front *)
         let compare_score a b = Int.compare b.score a.score in
         fun a b ->
           match compare_search_result ~compare_score a b with
           | 0 ->
             (* since rev_all is reversed, [a; b] where a and b have equal
                scores should return [b; a]. stable_sort wants to leave them
                in the original order. *)
             1
           | k -> k
        )
  in
  let top_n = Base.List.take sorted n in
  let results =
    if weighted then
      (* Exact matches are always sorted first by Fuzzy_path. This results in
         very rare exact matches beating out more common prefix matches.

         For the query `foo`, results `foo` and `foobar` are both contiguous
         prefix matches with the same score, except the exact match gets a +2
         boost.

         We could ignore the boost and order these by weight, but that means
         that exact matches can get buried deep in the list where the user
         will be unlikely to find them. Since they can't keep typing to filter
         (since it's already exact), they're kind of out of luck. So instead,
         we potentially boost the first 3 inexact results by their score
         ignoring the exactness boost, and then by weight.

         For example, suppose the query is `foo` and an autoimport `foo` is
         very rare, `foobar` is very common, and `barfoo` is somewhere in the
         middle. Purely score-wise, it's sorted foo, foobar, barfoo. But we
         remove the boost on foo, so it has the same score as foobar, and then
         re-sort: foobar, foo, barfoo. *)
      let is_exact result =
        String.length result.search_result.name = String.length (string_of_query query)
      in
      let (exacts, rest) = Base.List.split_while top_n ~f:is_exact in
      let (next_3, rest) = Base.List.split_n rest 3 in
      let adjust_score result =
        if is_exact result then
          result.score - 2
        else
          result.score
      in
      let compare_score a b = Int.compare (adjust_score b) (adjust_score a) in
      let top =
        Base.List.stable_sort (exacts @ next_3) ~compare:(compare_search_result ~compare_score)
      in
      top @ rest
    else
      top_n
  in
  let is_incomplete = Base.List.length sorted > n in
  { results; is_incomplete }

let search ?(options = default_options) query { index; value_matcher; type_matcher } =
  let (matcher, query_txt) =
    match query with
    | Value txt -> (value_matcher, txt)
    | Type txt -> (type_matcher, txt)
  in

  let max_results = options.Fuzzy_path.max_results in
  let options =
    (* if max_results is set, then increase it by 1 so that we can tell the difference
       between getting exactly max_results results (is_incomplete = false) and getting
       too many (is_incomplete = true). *)
    if max_results < max_int then
      Fuzzy_path.{ options with max_results = max_results + 1 }
    else
      options
  in

  let weighted = options.Fuzzy_path.weighted in

  Fuzzy_path.search ~options query_txt matcher |> take ~weighted ~n:max_results ~index ~query

let search_values ?options query t = search ?options (Value query) t

let search_types ?options query t = search ?options (Type query) t

let get name { index; value_matcher = _; type_matcher = _ } = Export_index.find name index

let get_values name t =
  get name t
  |> Export_index.ExportMap.filter (fun (_file_key, kind) _num -> Export_index.kind_is_value kind)

let get_types name t =
  get name t
  |> Export_index.ExportMap.filter (fun (_file_key, kind) _num -> Export_index.kind_is_type kind)

let pp fmt { index; _ } = Export_index.pp fmt index

let show { index; _ } = Export_index.show index
