(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Not all stats are worth logging for every user. Things like the initial heap
 * size are pretty deterministic if you know the input (i.e. the files being
 * checked). In fact, it's *only* useful information if you know the input.
 * This file is for storing these types of stats: Things that would be useful
 * for a benchmark script to know, so it can say "for these inputs, under these
 * conditions, here's how hh_server behaves".
 *)
type t = {
  mutable init_parsing_heap_size: int;
  mutable init_heap_size: int;
  mutable max_heap_size: int;
  gc_stat: Gc.stat;
}

let stats : t =
  { init_parsing_heap_size = 0; init_heap_size = 0; max_heap_size = 0; gc_stat = Gc.quick_stat () }

let get_stats () = { stats with gc_stat = Gc.quick_stat () }

let update_max_heap_size x = stats.max_heap_size <- max stats.max_heap_size x

let to_json stats =
  Hh_json.JSON_Object
    [
      ("init_parsing_heap_size", Hh_json.int_ stats.init_parsing_heap_size);
      ("init_shared_heap_size", Hh_json.int_ stats.init_heap_size);
      ("max_shared_heap_size", Hh_json.int_ stats.max_heap_size);
      ("master_heap_words", Hh_json.int_ stats.gc_stat.Gc.heap_words);
      ("master_top_heap_words", Hh_json.int_ stats.gc_stat.Gc.top_heap_words);
    ]
