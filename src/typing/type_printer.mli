(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val string_of_t: ?size:int -> Context.t -> Type.t -> string

val rest_parameter_name: Context.t -> string option -> Type.t -> string
val parameter_name: Context.t -> string option -> Type.t -> string
val string_of_param_t: Context.t -> Type.t -> string

val is_printed_type_parsable:
  ?weak:bool -> Context.t -> Type.t -> bool
val is_printed_param_type_parsable:
  ?weak:bool -> Context.t -> Type.t -> bool

(***
 * internal printer harness, here for Debug access
 *)

type enclosure_t =
  | EnclosureNone
  | EnclosureUnion
  | EnclosureIntersect
  | EnclosureParam
  | EnclosureMaybe
  | EnclosureAppT
  | EnclosureRet
  | EnclosureProp
  | EnclosureMethod

val type_printer:
  ?size:int ->
  (Context.t -> Type.t -> string option) ->
  enclosure_t ->
  Context.t ->
  Type.t ->
  string
