(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
*)

open Utils_js

(* Find the root of a type variable, potentially traversing a chain of type
   variables, while short-circuiting all the type variables in the chain to the
   root during traversal to speed up future traversals. *)
let rec find_root cx id =
  let open Constraint in
  match IMap.get id (Context.graph cx) with
  | Some (Goto next_id) ->
      let root_id, root = find_root cx next_id in
      if root_id != next_id then Context.set_tvar cx id (Goto root_id) else ();
      root_id, root

  | Some (Root root) ->
      id, root

  | None ->
      let msg = spf "find_root: tvar %d not found in file %s" id
        (Debug_js.string_of_file cx)
      in
      assert_false msg
