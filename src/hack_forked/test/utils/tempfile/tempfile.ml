(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Out_of_retries

let rec mkdtemp ~retries =
  if retries < 0 then
    raise Out_of_retries
  else
    let tmp_dir = Sys_utils.temp_dir_name in
    let tmp_dir = Path.make tmp_dir in
    let name = Random_id.(short_string_with_alphabet alphanumeric_alphabet) in
    let tmp_dir = Path.concat tmp_dir name in
    try
      let () = Disk.mkdir_p (Path.to_string tmp_dir) in
      tmp_dir
    with
    | Unix.Unix_error _ -> mkdtemp ~retries:(retries - 1)

let with_tempdir g =
  Random.self_init ();
  let dir = mkdtemp ~retries:30 in
  let f () = g dir in
  Exception.protect ~f ~finally:(fun () -> Disk.rm_dir_tree (Path.to_string dir))
