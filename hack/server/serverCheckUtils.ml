(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let extend_fast fast files_info additional_files =
  Relative_path.Set.fold additional_files ~init:fast ~f:begin fun x acc ->
    match Relative_path.Map.get fast x with
    | None ->
        (try
           let info = Relative_path.Map.find_unsafe x files_info in
           if info.FileInfo.consider_names_just_for_autoload then acc else
           let info_names = FileInfo.simplify info in
           Relative_path.Map.add acc ~key:x ~data:info_names
         with Not_found ->
           acc)
    | Some _ -> acc
  end
