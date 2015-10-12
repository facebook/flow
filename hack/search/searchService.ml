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

module Make(S : SearchUtils.Searchable) = struct
  module Fuzzy = FuzzySearchService.Make(S)
  module Trie = TrieSearchService.Make(S)

  module WorkerApi = struct

    let update fn trie_defs fuzzy_defs =
      Trie.WorkerApi.update fn trie_defs;
      Fuzzy.update fn fuzzy_defs

    let process_trie_term = Trie.WorkerApi.process_term

    let process_fuzzy_term = Fuzzy.process_term

  end

  module MasterApi = struct

    let marshal chan =
      Fuzzy.marshal chan;
      Trie.MasterApi.marshal chan

    let unmarshal chan =
      Fuzzy.unmarshal chan;
      Trie.MasterApi.unmarshal chan

    (* Called by the master process when there is new information in
     * shared memory for us to index *)
    let update_search_index files =
      Trie.MasterApi.index_files files;
      Fuzzy.index_files files;
      (* At this point, users can start searching again so we should clear the
       * cache that contains the actual results. We don't have to worry
       * about the string->keys list shared memory because it's uncached *)
      SharedMem.invalidate_caches()

    let clear_shared_memory fns =
      Trie.SearchUpdates.remove_batch fns;
      Trie.SearchKeys.remove_batch fns;
      Fuzzy.SearchKeyToTermMap.remove_batch fns;
      Fuzzy.SearchKeys.remove_batch fns

    let query input type_ =
      let is_fuzzy_indexed = match type_ with
        | Some ty -> List.mem S.fuzzy_types ty
        | None -> true
      in
      let trie_results = match type_, is_fuzzy_indexed with
        | Some _, false
        | None, _ -> Trie.MasterApi.search_query input
        | _ -> []
      in
      let fuzzy_results = match type_, is_fuzzy_indexed with
        | Some _, true
        | None, _ -> Fuzzy.query input type_
        | _ -> []
      in
      let res = List.merge fuzzy_results trie_results ~cmp:begin fun a b ->
        (snd a) - (snd b)
      end in
      let res = List.take res 50 in
      List.map res fst
  end
end
