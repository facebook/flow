(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env =
  { filename : string;
    mutable ic_opt : in_channel option;
    mutable lines : string list;
    mutable last_line : string option;
  }

let open_in_opt filename =
  try
    Some(open_in filename)
  with _ ->
    (Printf.eprintf "Tail.open_in_opt: Couldn't open file %s\n%!" filename;
     None)

let create_env filename =
 { filename; ic_opt=None; lines=[]; last_line=None }

let open_env env =
  match env.ic_opt with
  | None ->
      begin
        env.lines <- [];
        env.last_line <- None;
        env.ic_opt <- open_in_opt env.filename;
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
          else
            env.last_line <- None
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
