(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let loc_and_string_of_property_key key =
  let open Flow_ast in
  let open Expression.Object in
  match key with
  | Property.Computed _
  | Property.PrivateName _ ->
    failwith "records do not parse computed properties or private elements"
  | Property.Identifier (loc, { Identifier.name; _ }) -> (loc, name)
  | Property.StringLiteral (loc, { StringLiteral.value; _ }) -> (loc, value)
  | Property.NumberLiteral (loc, { NumberLiteral.value; _ }) ->
    (loc, Dtoa.ecma_string_of_float value)
  | Property.BigIntLiteral (loc, bigint) -> (loc, Flow_ast_utils.string_of_bigint bigint)

(* The set of record properties that have a default value supplied, e.g. `foo: number = 0,` *)
let defaulted_props_of_record record =
  let open Flow_ast.Statement.RecordDeclaration in
  let { body = (_, { Body.body; _ }); _ } = record in
  List.fold_left
    (fun acc -> function
      | Body.Property (_, { Property.key; default_value = Some _; _ }) ->
        let (_, name) = loc_and_string_of_property_key key in
        SSet.add name acc
      | _ -> acc)
    SSet.empty
    body
