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
    ic_opt : in_channel option;
    filter_fn : string -> bool;
    lines : string Queue.t;
  }

let open_env filename filter_fn =
  let ic_opt =
    try
      Some(open_in filename)
    with _ -> begin
      Printf.eprintf "Couldn't open file %s\n%!" filename;
      None
      end
  in
  { filename; ic_opt; filter_fn; lines=Queue.create (); }

let close_env env =
  match env.ic_opt with
  | None -> ()
  | Some ic -> (close_in ic; Queue.clear env.lines)

let update env =
  match env.ic_opt with
  | None -> ()
  | Some ic ->
      let continue = ref true in
      let line = ref "" in
      while !continue do
        try
          line := input_line ic;
          if env.filter_fn !line then
            Queue.add !line env.lines;
        with End_of_file ->
          continue := false;
      done

let last_line env =
  match env.ic_opt with
  | None -> ""
  | Some _ ->
      if Queue.length env.lines = 1 then
        Queue.peek env.lines
      else if Queue.length env.lines > 1 then
        Queue.take env.lines
      else
        ""
