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
open String_utils

module Make(S : SearchUtils.Searchable) = struct

  open SearchUtils

  (* Shared memory for workers to put lists of pairs of keys and results
    * of our index. Indexed on file name. Cached because we read from here
    * every time the user searches *)
  module SearchUpdates = SharedMem.WithCache (Relative_path.S) (struct
    type t = (string * (Pos.t, S.t) term) list
    let prefix = Prefix.make()
    let description = "SearchUpdates"
  end)
  (* Maps file name to a list of keys that the file has results for *)
  (* This is only read once per update, so cache gives us no advantage *)
  module SearchKeys = SharedMem.NoCache (Relative_path.S) (struct
    type t = string list
    let prefix = Prefix.make()
    let description = "SearchKeys"
  end)

  let cut_str_after cut_char str =
    try
      let i = String.index str cut_char in
      String.sub str 0 i
    with Not_found ->
      str

  let double_colon = Str.regexp_string "::"

  let cut_after_double_colon str =
    try
      let i = Str.search_forward double_colon str 0 in
      String.sub str 0 i
    with Not_found ->
      str

  (* function that shortens the keys stored in the trie to make things faster *)
  let simplify_key key =
    let key =
      try
        (* Testing showed this max length gave us some indexing time back
         * without making search feel any slower *)
        String.sub key 0 6
      with Invalid_argument _ ->
        key
    in
    (* stuff after the dot is class members and they're all probably
     * in the same file anyways (JS) *)
    let key = cut_str_after '.' key in
    (* stuff after the double colon is class members and they're all probably
     * in the same file anyways (PHP) *)
    cut_after_double_colon key

  module WorkerApi = struct
    let process_term key name pos type_ trie_acc =
       let res =  {
                    pos         = pos;
                    name        = name;
                    result_type = type_
                  } in
       (key, res) :: trie_acc

    let process_term_for_search key =
      (* When performing user searches, we want them to be case-insensitive *)
      process_term (String.lowercase key)

    let update fn trie_defs =
      SearchUpdates.add fn trie_defs;
      let trie_keys = List.fold_left trie_defs ~f:begin fun acc def ->
        let key = simplify_key (fst def) in
        SSet.add key acc
      end ~init:SSet.empty in
      let trie_keys = SSet.elements trie_keys in
      SearchKeys.add fn trie_keys
  end


  module MasterApi = struct

    exception Search_limit
    (* what keys a specific file currently has results for *)
    (* 10000 is a number that seemed reasonable for the amount of files
     * in a codebase *)
    let removal_index = Hashtbl.create 1000

    (* Hashtable the names of files with results for a string key *)
    let main_index = Hashtbl.create 1000

    (* trie used to store ONLY KEYS to give a typeahead feeling for searching *)
    let trie = Trie.create ()

    let lookup str =
     try
       Hashtbl.find main_index str
     with Not_found ->
       []

    let replace key value =
      if value = [] then Hashtbl.remove main_index key
      else Hashtbl.replace main_index key value

    (* Insert the selected filename into the hashtable, and the trie
     * Takes in a set of keys that are currently empty that we plan to delete
     * from the trie. Removes those keys from the set if we inserted on them *)
    let insert_fn fn key empty_keys =
      let current_val = lookup key in
      replace key (fn :: current_val);
      if current_val == [] && not (SSet.mem key empty_keys)
      then begin
        (* We don't actually store useful stuff in the trie. Just
         * whether a key exists or not *)
        Trie.add trie key ()
          ~if_exist: (fun _ _ -> ()) (* this can probably throw an exception *)
          ~transform: (fun _ -> ())
      end;
      SSet.remove key empty_keys

    (* Remove this filename from the hashtable at this key, if it
     * exists. Accumulates keys have no values anymore
     *  (so we can remove them from the trie) *)
    let remove_fn fn key empty_keys =
      let current_val = lookup key in
      let new_val = List.fold_left current_val ~f:begin fun acc file ->
        if file= fn
        then acc
        else (file :: acc)
      end ~init:[] in
      replace key new_val;
      if new_val == [] then SSet.add key empty_keys else empty_keys

    let process_file fn =
      let old_keys =
        try Hashtbl.find removal_index fn
        with Not_found ->
          [] (* This will happen when we haven't seen this file before *)
      in
      let new_keys =
        try SearchKeys.find_unsafe fn
        with Not_found -> [] (* This shouldn't actually happen *)
      in
      (* Compute diff between old and new keys for this file*)
      let old_keys_set = List.fold_left old_keys ~f:begin fun acc file ->
        SSet.add file acc
      end ~init:SSet.empty in
      let new_keys_set = List.fold_left new_keys ~f:begin fun acc file ->
        SSet.add file acc
      end ~init:SSet.empty in
      let to_add = SSet.diff new_keys_set old_keys_set in
      let to_remove = SSet.diff old_keys_set new_keys_set in

      let removed_keys = SSet.fold (remove_fn fn) to_remove SSet.empty in
      Hashtbl.replace removal_index fn new_keys;
      let removed_keys = SSet.fold (insert_fn fn) to_add removed_keys in
      (* removed keys now contains any keys that we removed and didn't
       * add again for this file change. So we remove from the trie *)
      SSet.iter (Trie.remove trie) removed_keys

    let index_files fns =
      List.iter fns process_file

    (* Note: the score should be able to compare to the scoring in
     * Fuzzy so that the results can be merged and the ordering still
     * makes sense. *)
    let rec get_score ?qi:(qi=0) ?ti:(ti=0) ?score:(score=0) term query =
      if (String.length query = String.length term.name) then
        if String.compare query term.name = 0 then 0
        else String.length term.name
      else if (qi >= String.length query || ti >= String.length term.name) then
        score + ti + String.length term.name
      else
        let qc = String.get query qi in
        let tc = String.get term.name ti in
        if Char.compare qc tc = 0 then
          get_score ~qi:(qi+1) ~ti:(ti+1) ~score:(score) term query
        else
          get_score ~qi:(qi+1) ~ti:(ti+1) ~score:(score+1) term query

    let results_for_file results results_count fn str ~filter_map ~limit =
      (* for a file, look in shared memory for all the results the file
       * contains. anything where the key starts with the full search
       * term is a match *)
      let defs =
        try SearchUpdates.find_unsafe fn
        with Not_found -> []
      in
      List.iter defs begin fun (key, res) ->
        (* Now we're comparing results in shared memory. These keys
         * have not been simplified, so we check if they start with
         * the full search term *)
        if string_starts_with key str then begin
          match filter_map str key res with
          | Some res ->
            results := res :: !results;
            incr results_count;
            begin match limit with
              | Some limit ->
                if !results_count >= limit then raise Search_limit;
              | None -> ()
            end
          | None -> ()
        end
      end

    let query input ~filter_map ~limit =
      let str = Utils.strip_ns input in
      let short_key = simplify_key str in
      (* get all the keys beneath short_key in the trie *)
      let keys =
        try Trie.find_prefix_limit 25 trie short_key (fun k _ -> k)
        with Not_found -> []
      in

      let results = ref [] in
      let results_count = ref 0 in
      let seen = Hashtbl.create 1000 in
      (* Go through set of filenames that contain results for those keys. We
       * accumulate results by reference to be able to early return from
       * computation by throwing an exception. *)
      (try
        List.iter keys ~f:begin fun key ->
          let filenames = Hashtbl.find main_index key in
          List.iter filenames ~f:begin fun fn ->
            if not (Hashtbl.mem seen fn) then begin
              results_for_file results results_count fn str ~filter_map ~limit;
              Hashtbl.add seen fn true
            end
          end
        end
      with Search_limit -> ());
      !results

    let search_query input =
      let input = String.lowercase input in
      let compute_score str key res =
        let score =
          if string_starts_with (String.lowercase res.name) str
          then get_score res str
          else (String.length key) * 2
        in
        Some (res, score)
      in
      let results = query input ~filter_map:compute_score ~limit:None in
      let res = List.sort begin fun a b ->
        (snd a) - (snd b)
      end results in
      List.take res 50

  end
end
