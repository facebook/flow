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
  is_declaration: bool;
  (* Span of the symbol itself *)
  pos: 'a Pos.pos;
}

let to_absolute x = { x with
  pos = Pos.to_absolute x.pos;
}

let kind_to_string = function
  | Class -> "type_id"
  | Method _ -> "method"
  | Function -> "function"
  | LocalVar -> "local"
  | Property _ -> "property"
  | ClassConst _ -> "member_const"
  | Typeconst _ -> "typeconst"
  | GConst -> "global_const"
