(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Eviction is O(n) -- see comment in cache_sig.ml *)
module Make (Map : WrappedMap.S) = struct
  type key = Map.key

  type 'a entry = {
    cached_value: 'a;
    mutable last_hit: float;
  }

  type 'a t = {
    mutable entries: 'a entry Map.t;
    mutable size: int;
    max_size: int;
  }

  let make ~max_size = { entries = Map.empty; size = 0; max_size }

  let clear cache =
    cache.entries <- Map.empty;
    cache.size <- 0

  let remove_entry key cache =
    cache.entries <-
      Map.update
        key
        (function
          | None -> None
          | Some _ ->
            cache.size <- cache.size - 1;
            None)
        cache.entries

  (* Eviction is O(n) -- see comment in cache_sig.ml *)
  let remove_oldest cache =
    let oldest =
      Map.fold
        (fun key { last_hit; _ } acc ->
          match acc with
          | Some (_, oldest_hit) when oldest_hit <= last_hit -> acc
          | _ -> Some (key, last_hit))
        cache.entries
        None
    in
    match oldest with
    | Some (oldest_key, _) -> remove_entry oldest_key cache
    | None -> ()

  let add_after_miss key value cache =
    let entry = { cached_value = value; last_hit = Unix.gettimeofday () } in
    cache.entries <- Map.add key entry cache.entries;
    cache.size <- cache.size + 1;
    if cache.size > cache.max_size then
      remove_oldest cache
    else
      ()

  let get_from_cache key cache =
    match Map.find_opt key cache.entries with
    | None -> None
    | Some entry ->
      entry.last_hit <- Unix.gettimeofday ();
      cache.entries <- Map.add key entry cache.entries;
      Some entry.cached_value

  let with_cache key value cache =
    let cached_result = get_from_cache key cache in
    match cached_result with
    | None ->
      let%lwt value = Lazy.force value in
      add_after_miss key value cache;
      Lwt.return (value, false)
    | Some result -> Lwt.return (result, true)
end
