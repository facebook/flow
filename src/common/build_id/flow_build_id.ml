(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let build_id = ref None

let get_build_id () =
  match !build_id with
  | None ->
    let state = Xx.init 0L in
    Xx.update state (Sys_utils.cat Sys.executable_name);
    let hash = Xx.digest state |> Xx.to_string in
    build_id := Some hash;
    hash
  | Some hash -> hash
