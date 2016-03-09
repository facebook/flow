(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

module N = Nast

let unwrap_class_hint = function
  | (_, N.Happly ((pos, class_name), type_parameters)) ->
      pos, class_name, type_parameters
  | p, N.Habstr(_, _) ->
      Errors.expected_class ~suffix:" or interface but got a generic" p;
      Pos.none, "", []
  | p, _ ->
      Errors.expected_class ~suffix:" or interface" p;
      Pos.none, "", []

let unwrap_class_type = function
  | r, Tapply (name, tparaml) -> r, name, tparaml
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
  | Tfun _ | Ttuple _ | Tshape _ | Taccess (_, _) | Tthis) ->
    raise @@ Invalid_argument "unwrap_class_type got non-class"

let try_unwrap_class_type x = Option.try_with (fun () -> unwrap_class_type x)
