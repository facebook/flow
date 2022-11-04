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
  haste_commonjs: bool;
}

let empty = { name = None; main = None; haste_commonjs = false }

let create ~name ~main ~haste_commonjs = { name; main; haste_commonjs }

let name package = package.name

let main package = package.main

let haste_commonjs package = package.haste_commonjs

let string_opt = function
  | Some (Ast.Literal.String value) -> Some value
  | Some _
  | None ->
    None

let bool_opt = function
  | Some (Ast.Literal.Boolean value) -> Some value
  | Some _
  | None ->
    None

(** Given a list of JSON properties, loosely extract the properties and turn it into a
    [Literal.value SMap.t]. We aren't looking to validate the file, and don't currently
    care about any non-literal properties, so we skip over everything else. *)
let extract_property map property =
  let open Ast in
  let open Expression.Object in
  match property with
  | Property
      ( _,
        Property.Init
          {
            key = Property.Literal (_, { Literal.value = Literal.String key; _ });
            value = (_, Expression.Literal { Literal.value; _ });
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
      string_opt ret
  | [] -> None

let parse ~node_main_fields { Ast.Expression.Object.properties; comments = _ } =
  let prop_map = List.fold_left extract_property SMap.empty properties in
  let name = SMap.find_opt "name" prop_map |> string_opt in
  let main = find_main_property prop_map node_main_fields in
  let haste_commonjs =
    SMap.find_opt "haste_commonjs" prop_map |> bool_opt |> Base.Option.value ~default:false
  in
  { name; main; haste_commonjs }
