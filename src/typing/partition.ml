(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Type

(** Partitions on Type.t
    Given a function f: 'a -> Type.t, we can partition a
    collection of 'a along f: each cell in the partition
    contains values x of type 'a which share a common f x.
    This module provides a basic data structure and API
    for creating, updating and querying these.
  *)

type 'a t =
  ('a -> Type.t) *        (* partition function *)
  ('a list) TypeMap.t *   (* map from Type.t to cells *)
  bool                    (* true if partition is discrete *)

(** empty partition along f *)
let empty f = f, TypeMap.empty, true

(** return a partition's cell for a given x *)
let cell x (f, m, _) =
  let k = f x in
  match TypeMap.get k m with
  | None -> []
  | Some ts -> ts

(** true if x is a member of p *)
let mem x p =
  List.find ((=) x) (cell x p)

(** add x to the given partition.
    Note: no duplicate checking is done.
  *)
let add x (f, m, d) =
  let k = f x in
  let cell = match TypeMap.get k m with
    | None -> []
    | Some ts -> ts in
  f,
  TypeMap.add k (x :: cell) m,  (* add x to cell *)
  d && cell = []                (* track discreteness *)

(** true if every cell contains a single item *)
let is_discrete (_, _, d) = d

(** convenience: build a partition from a list
    Note: cell lists preserve original order.
  *)
let from f lst =
  List.fold_left (fun p x -> add x p) (empty f) (List.rev lst)
