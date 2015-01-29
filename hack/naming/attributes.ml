(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Ast

module SN = Naming_special_names

let mem x xs =
  List.exists (fun { ua_name; _ } -> x = snd ua_name) xs

let find x xs =
  try Some (List.find (fun { ua_name; _ } -> x = snd ua_name) xs)
  with Not_found -> None

let deprecated ~kind (_, name) attrs =
  let attr = find SN.UserAttributes.uaDeprecated attrs in
  match attr with
  | Some { ua_name; ua_params = [msg] } -> begin
      (* we have no reason to support string interpolation, so only allow
       * single-quoted strings *)
      match msg with
      | _, Ast.String (_p, s) ->
          let name = Utils.strip_ns name in
          let deprecated_msg =
            Printf.sprintf "The %s %s is deprecated: " kind name in
          Some (deprecated_msg ^ s)
      | p, _ ->
          Errors.attribute_param_type p "single-quoted string"; None
    end
  | Some { ua_name = (pos, _); _ }  ->
      Errors.attribute_arity pos SN.UserAttributes.uaDeprecated 1;
      None
  | None -> None
