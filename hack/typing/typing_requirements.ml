(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Typing_defs

module Reason = Typing_reason
module TUtils = Typing_utils

(* Only applied to classes. Checks that all the requirements of the traits
 * and interfaces it uses are satisfied. *)
let check_fulfillment env impls (parent_pos, req_ty) =
  match TUtils.try_unwrap_class_type req_ty with
  | None -> ()
  | Some (_r, (_p, req_name), _paraml) ->
    match SMap.get req_name impls with
    | None ->
      let req_pos = Reason.to_pos (fst req_ty) in
      Errors.unsatisfied_req parent_pos req_name req_pos;
      ()
    | Some impl_ty ->
      ignore @@ Typing_ops.sub_type_decl parent_pos Reason.URclass_req env
        req_ty impl_ty

let check_class env tc =
  match tc.tc_kind with
  | Ast.Cnormal | Ast.Cabstract ->
    List.iter tc.tc_req_ancestors (check_fulfillment env tc.tc_ancestors)
  | Ast.Ctrait | Ast.Cinterface | Ast.Cenum -> ()
