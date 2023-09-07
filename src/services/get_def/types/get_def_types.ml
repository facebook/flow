(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type single_property_def_info =
  | ClassProperty of Loc.t
  | ObjectProperty of Loc.t

(* If there are multiple relevant definition locations (e.g. the request was issued on an object
 * literal which is associated with multiple types) then there will be multiple locations in no
 * particular order. *)
type property_def_info = single_property_def_info Nel.t * string

type def_info =
  | VariableDefinition of Loc.t list * string option
  | PropertyDefinition of property_def_info
  | NoDefinition of string option (* Reason of expected no definition *)

module Purpose = struct
  type t =
    | GoToDefinition
    | FindReferences
end
