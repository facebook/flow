(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val loc_and_string_of_property_key :
  'loc. ('loc, 'loc) Flow_ast.Expression.Object.Property.key -> 'loc * string

val defaulted_props_of_record : 'loc. ('loc, 'loc) Flow_ast.Statement.RecordDeclaration.t -> SSet.t
