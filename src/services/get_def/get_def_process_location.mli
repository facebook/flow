(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ('M, 'T) result =
  | OwnDef of 'M
  | Request of ('M, 'T) Get_def_request.t
  | LocNotFound

val process_location :
  loc_of_annot:('T -> 'M) ->
  ast:('M, 'T) Flow_ast.Program.t ->
  is_legit_require:('T -> bool) ->
  module_ref_prefix:string option ->
  covers_target:('M -> bool) ->
  ('M, 'T) result

val process_location_in_ast :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  is_legit_require:(Loc.t -> bool) ->
  module_ref_prefix:string option ->
  Loc.t ->
  (Loc.t, Loc.t) result

val process_location_in_typed_ast :
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  is_legit_require:(ALoc.t * Type.t -> bool) ->
  module_ref_prefix:string option ->
  Loc.t ->
  (ALoc.t, ALoc.t * Type.t) result
