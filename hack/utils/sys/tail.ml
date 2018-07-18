(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type env =
  { filename : string;
    mutable ic_opt : in_channel option;
    mutable lines : string list;
    mutable last_line : string option;
  }

let open_in_opt filename =
  let open Unix in
  try
    Sys_utils.mkdir_no_fail (Filename.dirname filename);
    let fd = openfile filename [O_RDONLY;O_NONBLOCK;O_CREAT] 0o777 in
    let ic = in_channel_of_descr fd in
    begin set_binary_mode_in ic false; Some(ic) end
  with _ ->
    (Printf.eprintf "Tail.open_in_opt: Couldn't open file %s\n%!" filename;
     None)

let create_env filename =
  { filename; ic_opt=None; lines=[]; last_line=None }

let open_env env =
  match env.ic_opt with
  | None ->
      begin
        let target_file = Sys_utils.readlink_no_fail env.filename in
        env.lines <- [];
        env.last_line <- None;
        env.ic_opt <- open_in_opt target_file;
      end
  | Some _ -> ()

let close_env env =
  env.lines <- [];
  match env.ic_opt with
  | None -> ()
  | Some ic -> (close_in ic; env.ic_opt <- None)

let update_env filter_fn env =
  match env.ic_opt with
  | None -> ()
  | Some ic ->
      let continue = ref true in
      let line = ref "" in
      while !continue do
        try
          line := input_line ic;
          if filter_fn !line then
            begin
              env.lines <- !line :: env.lines;
              env.last_line <- Some(!line)
            end
        with End_of_file ->
          continue := false;
      done

let is_open_env env =
  match env.ic_opt with
  | None -> false
  | Some _ -> true

let last_line env =
  match env.ic_opt, env.last_line with
  | None, _ -> ""
  | Some _, None -> ""
  | Some _, Some l -> l

let get_lines env = env.lines

let set_lines env l = env.lines <- l
