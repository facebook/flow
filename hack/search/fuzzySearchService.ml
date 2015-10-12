(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Utils

module Make(S : SearchUtils.Searchable) = struct

open SearchUtils

type search_result_type = S.t

let all_types = S.fuzzy_types

module TMap = MyMap(struct
  type t = search_result_type
  let compare = S.compare_result_type
end)

type type_to_key_to_term_list = (Pos.t, S.t) term list SMap.t TMap.t
type type_to_keyset = SSet.t TMap.t

(* Note only shared memory is modified within workers - and the keys are files
 * so that no two workers will modify the same key at the same time. *)

(* Used for communicating between the workers and master thread, it's the map
 * from type -> keys that is used to build our index for searching.  It's not
 * cached since each key is only accessed once from the master process and then
 * discarded after it's been read
 * Here's a JSONish version of what it might look like:
 * {
 *   '/home/user/www-hg/banana.php': {
 *     Class Ast.Cnormal: ['Banana', ...]
 *       ...
 *     }
 *     ...
 *   }
 * }
 *)
module SearchKeys = SharedMem.NoCache (Relative_path.S) (struct
  type t = type_to_keyset
  let prefix = Prefix.make()
end)

(* The workers are in charge of keeping this up to date per file, we use it
 * to look up terms once we know which files they exist in.
 * It's a map from filename -> type -> key -> term list.
 * Here's a JSONish version of what it might look like:
 * {
 *   '/home/user/www-hg/banana.php': {
 *     Class Ast.Cnormal: {
 *       'Banana': [{
 *         'name': '\Banana',
 *         'pos': Pos.t object,
 *         'result_type': Class Ast.Cnormal,
 *       }, ...]
 *       ...
 *     }
 *     ...
 *   }
 * }
 *)
module SearchKeyToTermMap = SharedMem.WithCache (Relative_path.S) (struct
  type t = type_to_key_to_term_list
  let prefix = Prefix.make()
end)

(* This is the table which we can find which terms are relevant to the query.
 * It's a mapping from a letter to a HashSet of the defs which might be related
 * to the query.  None of these are modified in a worker, they're all built from
 * the results of worker processes, in the main process
 * Here's a JSONish version of what it might look like:
 * {
 *   'a': ['Banana', 'ClassWithUppercaseA', 'abstract', ...],
 *   'b': ['Banana', 'abstract', 'underscore_b', ...],
 *   ...
 * }
 *)

let term_indexes = ref TMap.empty


(* Keeps a list of defs defined in files in the previous iteration.  It's a
 * direct copy of whatever was in SearchKeyToTermMap, so the format is the same
 * This is never modified in a worker, it's built in the main process using the
 * results from the worker processes.
 *)
let old_search_terms = ref (Hashtbl.create 160000)

(* Allows us to lookup by term name to find the files it is in - it's a reverse
 * map of SearchKeys.  It's not worth adding a type layer, since we only use
 * this table after we've already got our 50 results, so it's not much of a win
 * for performance on search time, and it would hurt our startup time a bit.
 * This is never modified in a worker, it's built in the main process using the
 * results from the worker processes.
 * Here's a JSONish version of what it might look like:
 * {
 *   'Banana': [
 *     '/home/user/www-hg/banana.php',
 *     '/home/user/www-hg/fruits.php',
 *     ...
 *   ]
 *   ...
 * }
 *)
let term_lookup = ref (Hashtbl.create 250000)

let marshal chan =
  Marshal.to_channel chan !term_indexes [];
  Marshal.to_channel chan !old_search_terms [];
  Marshal.to_channel chan !term_lookup []

let unmarshal chan =
  term_indexes := Marshal.from_channel chan;
  old_search_terms := Marshal.from_channel chan;
  term_lookup := Marshal.from_channel chan

(* We take out special characters from the string for the query - they aren't
 * useful during searches *)
let key_regex = Str.regexp "[^a-zA-Z_:]"
let strip_special_characters key =
  Str.global_replace key_regex "" key

let get_index_for_type type_ =
  TMap.find_unsafe type_ !term_indexes

(* Checks if `needle` matches the uppercase letters of `haystack` so we can
 * allow for something like 'bftc' to match 'BaseFacebookTestCase' *)
let check_if_matches_uppercase_chars needle haystack =
  let uppercase = ref "" in
  (* We want all uppercase letters, and also the first letter *)
  let first_letter = ref true in
  String.iter begin fun c ->
    if !first_letter || (Char.uppercase c) = c
    then uppercase := !uppercase^Char.escaped (Char.lowercase c);
    first_letter := false;
  end haystack;

  if String.compare (String.lowercase needle) !uppercase = 0
  then true
  else false

(* Checks if `needle` is a common subsequence of `haystack`
 * for example, 'heo' is a common subsequence of 'hello'
 *               ^^^                              ^^  ^
 *)
let rec is_cs ?ni:(ni=0) ?hi:(hi=0) ?score:(score=0) needle haystack =
  if String.length needle > String.length haystack then
    false, 0
  else if String.length needle = String.length haystack then
    let score =
      if String.compare needle haystack = 0 then 0
      else String.length haystack
    in
    String.compare (String.lowercase needle) (String.lowercase haystack) = 0,
    score
  else if ni >= String.length needle then
    true, score + hi + String.length haystack
  else if hi >= String.length haystack then
    false, 0
  else begin
    let cmp = Char.compare
      (Char.lowercase (String.get needle ni))
      (Char.lowercase (String.get haystack hi)) in

    (* Increment score if characters are not equal case *)
    let score =
      if Char.compare (String.get needle ni) (String.get haystack hi) = 0
      then score
      else score + 1
    in

    if cmp = 0
      then is_cs ~ni:(ni+1) ~hi:(hi+1) ~score:(score) needle haystack
    else if ni = 0
      then is_cs ~ni:(ni) ~hi:(hi+1) ~score:(score) needle haystack
      else is_cs ~ni:(ni) ~hi:(hi+1) ~score:(score+2) needle haystack
  end

(* Checks if `needle` is a case-insensitive substring of `haystack` and returns
 * the location where it occurs, or -1 if not found. *)
let is_substring needle haystack =
  let needle = String.lowercase needle in
  let haystack = String.lowercase haystack in
  let re = Str.regexp_string needle in
  try Str.search_forward re haystack 0
  with Not_found -> -1

(* Indexes the given word at the given letter with a check that it's not already
 * indexed there. *)
let add_letter_to_index letter word used type_ =
  if not (CSet.mem letter used) then
    let letter_val = get_index_for_type type_ in
    if Hashtbl.mem letter_val letter
    then
      let old_val = Hashtbl.find letter_val letter in
      HashSet.add old_val word;
    else begin
      let old_val = HashSet.create 20000 in
      HashSet.add old_val word;
      Hashtbl.add letter_val letter old_val;
    end;
    CSet.add letter used
  else
    used

(* We'll count a letter as important if it's near the start, an uppercase letter
 * or if it's directly after an underscore. *)
let is_letter_important word idx =
  let c = String.get word idx in
  if idx <= 1 then true (* First 2 letters *)
  else if Char.uppercase c = c then true (* Uppercase letter *)
  else if String.get word (idx - 1) = '_' then true (* Preceding underscore *)
  else false (* Not important *)

(* Indexes up to `max_num` important letters of a word *)
let rec add_letters_to_index ?x:(x=0) ?used:(used=CSet.empty) word max_num
                                                                      type_ =
  if x >= String.length word || CSet.cardinal used >= max_num then
    ()
  else
    if (is_letter_important word x) then
      let letter = Char.lowercase (String.get word x) in
      let used = add_letter_to_index letter word used type_ in
      add_letters_to_index ~x:(x+1) ~used:(used) word max_num type_
    else
      add_letters_to_index ~x:(x+1) ~used:(used) word max_num type_

(* Adds a set of terms to the `idx_table` index *)
let add_terms_to_index type_ terms =
  SSet.iter begin fun term ->
    add_letters_to_index term 10 type_;
  end terms

(* Removes a set of terms from the `idx_table` index *)
let remove_terms_from_index type_ terms =
  let defmap = get_index_for_type type_ in
  SSet.iter begin fun term ->
    Hashtbl.iter begin fun _ v ->
      HashSet.remove v term;
    end defmap;
  end terms

(* This updates the term_lookup table which we can use to get the term object
 * from a key. *)
let update_term_lookup file add_terms remove_terms =
  SSet.iter begin fun term ->
    let old_val =
      try Hashtbl.find !term_lookup term
      with Not_found -> Relative_path.Set.empty
    in
    Hashtbl.replace !term_lookup term (Relative_path.Set.remove file old_val);
  end remove_terms;
  SSet.iter begin fun term ->
    let old_val =
      try Hashtbl.find !term_lookup term
      with Not_found -> Relative_path.Set.empty
    in
    Hashtbl.replace !term_lookup term (Relative_path.Set.add file old_val);
  end add_terms

(* Updates the keylist and defmap for a file (will be used to populate
 * SearchKeys and SearchKeyToTermMap respectively) *)
let process_term key name pos type_ defs_acc =
  let existing_defmap =
    try TMap.find_unsafe type_ defs_acc
    with Not_found -> SMap.empty
  in

  let existing_def_list =
    try SMap.find_unsafe key existing_defmap
    with Not_found -> []
  in

  let updated_defmap = SMap.add key ({
    name = name;
    pos = pos;
    result_type = type_;
  } :: existing_def_list) existing_defmap in
  TMap.add type_ updated_defmap defs_acc

let update fn fuzzy_defs =
  SearchKeyToTermMap.add fn fuzzy_defs;
  let fuzzy_keys = TMap.fold begin fun key value acc ->
    let terms_for_key = SMap.keys value in
    let terms_for_key = List.fold_left terms_for_key ~f:begin fun acc x ->
      SSet.add x acc
    end ~init:SSet.empty in
    TMap.add key terms_for_key acc
  end fuzzy_defs TMap.empty in
  SearchKeys.add fn fuzzy_keys

(* Called from the main process, processes the results of the workers *)
let index_files files =
  if TMap.is_empty !term_indexes
  then begin
    term_indexes := List.fold_left all_types ~f:begin fun acc x ->
      TMap.add x (Hashtbl.create 30) acc
    end ~init:TMap.empty
  end;
  Relative_path.Set.iter begin fun file ->
    let new_terms = try SearchKeys.find_unsafe file
    with Not_found -> TMap.empty in
    let old_terms = try Hashtbl.find !old_search_terms file
    with Not_found -> TMap.empty in
    Hashtbl.replace !old_search_terms file new_terms;

    TMap.iter begin fun type_ new_terms ->
      let old_terms =
        try TMap.find_unsafe type_ old_terms
        with Not_found -> SSet.empty
      in

      let added_terms = SSet.diff new_terms old_terms in
      let removed_terms = SSet.diff old_terms new_terms in

      update_term_lookup file added_terms removed_terms;

      remove_terms_from_index type_ removed_terms;
      add_terms_to_index type_ added_terms;
    end new_terms;
  end files

(* Looks up relevant terms in the index given a search query *)
let get_terms needle type_ =
  try
    let key = String.get needle 0 in
    let key = Char.lowercase key in
    match type_ with
    | Some type_ -> begin
      let letter_val = get_index_for_type type_ in
      try
        let hset = Hashtbl.find letter_val key in
        HashSet.fold begin fun term acc ->
          (term, type_) :: acc
        end hset []
      with Not_found -> []
    end
    | None -> begin
      List.fold_left all_types ~f:begin fun acc type_ ->
        let letter_val = get_index_for_type type_ in
        try
          let hset = Hashtbl.find letter_val key in
          HashSet.fold begin fun term acc ->
            (term, type_) :: acc
          end hset acc
        with Not_found -> acc
      end ~init:[]
    end
  with Invalid_argument _ -> [] (* Catches if the query is an empty string *)

(* Looks up the actual `term` objects based on the given strings
 * i.e. use the previously built `term_lookup` table so we can return
 * `term` objects instead of just relevant strings *)
let get_terms_from_string_and_type strings =
  List.fold_left strings ~f:begin fun acc ((str, type_), score) ->
    let files =
      try Hashtbl.find !term_lookup str
      with Not_found -> Relative_path.Set.empty
    in
    Relative_path.Set.fold begin fun file acc ->
      let defmap =
        try SearchKeyToTermMap.find_unsafe file
        with Not_found -> TMap.empty
      in
      try
        let term_map =
          try TMap.find_unsafe type_ defmap
          with Not_found -> SMap.empty
        in

        let term_list = SMap.find_unsafe str term_map in
        List.fold_left term_list ~f:begin fun acc term ->
          (term, score) :: acc
        end ~init:acc
      with Not_found -> acc
    end files acc
  end ~init:[]

let query needle type_ =
  let needle = strip_special_characters needle in
  let terms = get_terms needle type_ in
  let terms = List.fold_left terms ~f:begin fun acc (term, type_) ->
    if check_if_matches_uppercase_chars needle term then
      ((term, type_), 0) :: acc
    else
      let sub = is_substring needle term in
      if sub <> -1 && (String.length needle) < (String.length term) then
        (* We add the length of the term so we can be ranked alongside the
         * scores generated by `is_cs` which also factors in the length.
         * This way when you search for `EdisonController`,
         * EdisonController scores 0
         * EdixxsonController scores 42 (from `is_cs` scoring)
         * SomethingBlahBlahEdisonController scores 50 from substring scoring
         * EdiASDFsonController scores 53 (from `is_cs` scoring)
         * WebDecisionController scores 57 (from `is_cs` scoring)
         *)
        ((term, type_), sub + String.length term) :: acc
      else
        let cs = is_cs needle term in
        if fst cs then
          ((term, type_), (snd cs)) :: acc
        else
          acc
  end ~init:[] in
  let terms = List.sort begin fun a b ->
    (snd a) - (snd b)
  end terms in
  let res_terms = List.take terms 50 in
  let res = get_terms_from_string_and_type res_terms in
  List.rev res

end
