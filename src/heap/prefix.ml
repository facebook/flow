(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*****************************************************************************)
(* The prefix is used to guarantee that we are not mixing different kind of
 * keys in the heap.
 * It just creates a new prefix every time its called.
 * The $ at the end of the prefix ensures that we don't have ambiguities if a key
 * happens to start with a digit.
 *)
(*****************************************************************************)

type t = string

let make =
  let prefix_count = ref 0 in
  fun () ->
    incr prefix_count;
    string_of_int !prefix_count ^ "$"

let make_key prefix k = prefix ^ k

let remove prefix k =
  let prefix_size = String.length prefix in
  assert (String.sub k 0 prefix_size = prefix);
  String.sub k prefix_size (String.length k - prefix_size)
