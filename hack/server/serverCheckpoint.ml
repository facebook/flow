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
open Reordered_argument_collections

let checkpoints = ref SMap.empty

let process_updates updates =
  (* Appending changed files to each checkpoint in the map *)
  checkpoints := SMap.map !checkpoints begin fun cur_set ->
    Relative_path.Map.fold updates ~f:begin fun path _ acc ->
      Relative_path.Set.add acc path
    end ~init:cur_set
  end

let create_checkpoint x =
  checkpoints := SMap.add !checkpoints ~key:x ~data:Relative_path.Set.empty

let retrieve_checkpoint x =
  match SMap.get !checkpoints x with
  | Some files ->
      Some(
        List.map
          (Relative_path.Set.elements files)
          Relative_path.to_absolute
      )
  | None -> None

let delete_checkpoint x =
  match SMap.get !checkpoints x with
  | Some _ ->
      checkpoints := SMap.remove x !checkpoints;
      true
  | None -> false
