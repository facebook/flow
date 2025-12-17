(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { buffer: Buffer.t }

let create () = { buffer = Buffer.create 127 }

let add_string str src =
  Buffer.add_string src.buffer str;
  src

let add_identifier str src = add_string str src

(* TODO: Remove any right trailing whitespace *)
let add_newline source =
  Buffer.add_string source.buffer "\n";
  source

let add_space num b = add_string (String.make num ' ') b

let contents b = Buffer.contents b.buffer
