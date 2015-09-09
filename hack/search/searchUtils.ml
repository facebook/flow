(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Shared Search code between Fuzzy and Trie based searches *)

module type Searchable = sig
  type t
  val fuzzy_types : t list
  val compare_result_type : t -> t -> int
end

(* The results we'll return to the client *)
type ('a, 'b) term = {
  name: string;
  pos: 'a;
  result_type: 'b;
}

let to_absolute t = { t with pos = Pos.to_absolute t.pos }
