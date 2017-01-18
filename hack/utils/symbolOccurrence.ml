(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type kind =
  | Class
  | Function
  | Method of string * string
  | LocalVar
  | Property of string * string
  | ClassConst of string * string
  | Typeconst of string * string
  | GConst

type 'a t = {
  name:  string;
  type_: kind;
  (* Span of the symbol itself *)
  pos: 'a Pos.pos;
}

let to_absolute x = { x with
  pos = Pos.to_absolute x.pos;
}
