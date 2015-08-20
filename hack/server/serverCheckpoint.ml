(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let checkpoints = ref SMap.empty

let process_updates updates =
  (* Appending changed files to each checkpoint in the map *)
  checkpoints := SMap.map begin fun cur_set ->
    Relative_path.Map.fold begin fun path _ acc ->
      Relative_path.Set.add path acc
    end updates cur_set
  end !checkpoints

let create_checkpoint x =
  checkpoints := SMap.add x Relative_path.Set.empty !checkpoints

let retrieve_checkpoint x =
  match SMap.get x !checkpoints with
  | Some files ->
      Some(
        List.map
          Relative_path.to_absolute
          (Relative_path.Set.elements files)
      )
  | None -> None

let delete_checkpoint x =
  match SMap.get x !checkpoints with
  | Some _ ->
      checkpoints := SMap.remove x !checkpoints;
      true
  | None -> false
