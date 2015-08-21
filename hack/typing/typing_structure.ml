(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module implements the typing for type_structure. *)
open Core
open Nast
open Typing_defs

module Env = Typing_env
module Reason = Typing_reason
module SN = Naming_special_names
module TUtils = Typing_utils

(* For TypeStructure<T> (which is an alias for a shape) where the type
parameter T is a class/enum/interface or an alias that resolves to a
class, we can guarantee the existence of the 'classname' field, hence
this function transforms the 'classname' field of the TypeStructure<T>
from ?classname<T> to classname<T>. *)
let transform_classname_ty ty =
  match ty with
  | r, Tshape (fk, fdm) ->
    let p = Reason.to_pos r in
    let kClassname = Nast.SFlit (p, "classname") in
    let f_clsname = ShapeMap.get kClassname fdm in
    (match f_clsname with
     | Some (_, Toption (_, Tabstract (AKnewtype (c, tyl), _) as fty))
         when c = SN.Classes.cClassname ->
       (match List.hd tyl with
        | Some tty ->
          (match TUtils.get_base_type tty with
           | (_, Tclass _)
           | (_, Tabstract (AKenum _, _)) ->
             let fdm = ShapeMap.add kClassname fty fdm in
             (r, Tshape (fk, fdm))
           | _ -> ty)
        | _ -> ty)
     | _ -> ty (* should not reach *))
  | _ -> ty (* should not reach *)
