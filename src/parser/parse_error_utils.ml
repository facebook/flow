(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let camelize str =
  match String.split_on_char '-' str with
  | [] -> str
  | [str] -> str
  | hd :: rest ->
    let parts = hd :: List.map String.capitalize_ascii rest in
    String.concat "" parts
