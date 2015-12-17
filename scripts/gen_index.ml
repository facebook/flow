(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

#use "scripts/utils.ml"

let get_idx =
  let idx = ref 100 in
  fun () -> incr idx; !idx

let rec iter rc dir =
  let files = Sys.readdir dir in
  let index_file = Filename.concat dir "INDEX" in
  let index = open_out index_file in
  Array.iter
    (fun name ->
        let idx = get_idx () in
        let file = Filename.concat dir name in
        if Sys.is_directory file then begin
          Printf.fprintf index "%d dir %s\n" idx name;
          let file, digest = iter rc file in
          Printf.fprintf rc "%d 256 %s // %s\n" idx
            file (Digest.to_hex digest)
       end else if name <> "INDEX" then begin
         let digest = Digest.file file in
         Printf.fprintf index "%d file %s\n" idx name;
         Printf.fprintf rc "%d 257 %s // %s\n"
           idx file (Digest.to_hex digest);
        end)
       files;
  close_out index;
  index_file, Digest.file index_file

let () =
  let out_file = Sys.argv.(1) in
  let dir = Sys.argv.(2) in
  let temp_file = Filename.temp_file "flowlib" ".rc" in
  let rc = open_out temp_file in
  let file, digest = iter rc dir in
  Printf.fprintf rc "100 256 %s // %s\n" file (Digest.to_hex digest);
  close_out rc;
  let do_copy =
    not (Sys.file_exists out_file) ||
    string_of_file out_file <> string_of_file temp_file in
  if do_copy then begin
        if (Sys.file_exists out_file) then Sys.remove out_file;
        Sys.rename temp_file out_file
  end
