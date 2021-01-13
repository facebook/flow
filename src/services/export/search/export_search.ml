(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  file_key: File_key.t;
  kind: Export_index.kind;
}
[@@deriving show]

type search_results = {
  results: search_result list;
  is_incomplete: bool;
}
[@@deriving show]

type search_options = Fuzzy_path.options

let default_options : search_options = Fuzzy_path.default_options

type candidates = {
  values: string list;
  types: string list;
}

let kinds_of_exports (has_value, has_type) exports =
  Export_index.ExportSet.fold
    (fun (_file, kind) (has_value, has_type) ->
      match kind with
      | Export_index.Default -> (true, has_type)
      | Export_index.Named -> (true, has_type)
      | Export_index.NamedType -> (has_value, true)
      | Export_index.Namespace -> (true, has_type))
    exports
    (has_value, has_type)

let partition_candidates index =
  Export_index.fold_names
    ~f:(fun { values; types } name exports ->
      let (has_value, has_type) = kinds_of_exports (false, false) exports in
      let values =
        if has_value then
          name :: values
        else
          values
      in
      let types =
        if has_type then
          name :: types
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

let merge new_index { index; value_matcher; type_matcher } =
  let index = Export_index.merge new_index index in
  let { values; types } = partition_candidates new_index in
  let value_matcher = Fuzzy_path.add_candidates value_matcher values in
  let type_matcher = Fuzzy_path.add_candidates type_matcher types in
  { index; value_matcher; type_matcher }

type query =
  | Value of string
  | Type of string

let fold_search_result_of_export ~query name file_key kind acc =
  let open Export_index in
  match (query, kind) with
  | (Value _, (Default | Named | Namespace))
  | (Type _, NamedType) ->
    { name; file_key; kind } :: acc
  | (Value _, NamedType)
  | (Type _, (Default | Named | Namespace)) ->
    acc

(** [flatten_results ~n:20 ~index [] matches] will accumulate up to 20
    search results, where each match in [matches] might contribute multiple results.
    sets [is_incomplete] if [n] is exceeded. *)
let rec flatten_results ~n ~index ~query acc fuzzy_matches =
  if n = 0 then
    let is_incomplete = fuzzy_matches <> [] in
    { results = Base.List.rev acc; is_incomplete }
  else
    match fuzzy_matches with
    | [] -> { results = Base.List.rev acc; is_incomplete = false }
    | { Fuzzy_path.value; _ } :: fuzzy_matches ->
      let (n, acc) =
        match Export_index.find_opt value index with
        | Some files ->
          Export_index.ExportSet.fold
            (fun (file_key, kind) (n, acc) ->
              let acc =
                if n > 0 then
                  fold_search_result_of_export ~query value file_key kind acc
                else
                  acc
              in
              (n - 1, acc))
            files
            (n, acc)
        | None -> (n, acc)
      in
      flatten_results ~n ~index ~query acc fuzzy_matches

let search ?(options = Fuzzy_path.default_options) query { index; value_matcher; type_matcher } =
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

  Fuzzy_path.search ~options query_txt matcher |> flatten_results ~n:max_results ~index ~query []

let search_values ?options query t = search ?options (Value query) t

let search_types ?options query t = search ?options (Type query) t

let find_opt name { index; value_matcher = _; type_matcher = _ } = Export_index.find_opt name index

let pp fmt { index; _ } = Export_index.pp fmt index

let show { index; _ } = Export_index.show index
