(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let upload_blob _ = None

let distributed_check_job _ _ _ files =
  let results = List.fold_left (fun acc file -> (file, Ok None) :: acc) [] files in
  let unfinished_files = [] in
  (results, unfinished_files)
