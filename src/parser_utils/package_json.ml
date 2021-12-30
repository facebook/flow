(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type t = {
  name: string option;
  main: string option;
}

let empty = { name = None; main = None }

let create ~name ~main = { name; main }

let name package = package.name

let main package = package.main

(* Given a list of JSON properties, extract the string properties and turn it into a string SMap.t
 *)
let extract_property map property =
  let open Ast in
  let open Expression.Object in
  match property with
  | Property
      ( _,
        Property.Init
          {
            key = Property.Literal (_, { Literal.value = Literal.String key; _ });
            value = (_, Expression.Literal { Literal.value = Literal.String value; _ });
            _;
          }
      ) ->
    SMap.add key value map
  | _ -> map

(* prop_map is [ "main" ] by default but could be something like [ "foo", "bar" ]. In that case
 * we treat the "foo" property like the main property if it exists. If not, we fall back to the
 * "bar" property
 *
 * Spec'd on https://github.com/facebook/flow/issues/5725 *)
let rec find_main_property prop_map = function
  | prop :: rest ->
    let ret = SMap.find_opt prop prop_map in
    if ret = None then
      find_main_property prop_map rest
    else
      ret
  | [] -> None

let parse ~node_main_fields { Ast.Expression.Object.properties; comments = _ } =
  let prop_map = List.fold_left extract_property SMap.empty properties in
  let name = SMap.find_opt "name" prop_map in
  let main = find_main_property prop_map node_main_fields in
  { name; main }
