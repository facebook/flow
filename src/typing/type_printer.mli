(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val string_of_t: Context.t -> Type.t -> string

val parameter_name: Context.t -> string -> Type.t -> string
val string_of_param_t: Context.t -> Type.t -> string

val is_printed_type_parsable:
  ?weak:bool -> Context.t -> Type.t -> bool
val is_printed_param_type_parsable:
  ?weak:bool -> Context.t -> Type.t -> bool

(***
 * internal printer harness, here for Debug access
 *)

type enclosure_t =
    EnclosureNone
  | EnclosureUnion
  | EnclosureIntersect
  | EnclosureParam
  | EnclosureMaybe
  | EnclosureAppT
  | EnclosureRet

val type_printer:
  (Context.t -> Type.t -> string option) ->
  (Type.t -> string) ->
  enclosure_t ->
  Context.t ->
  Type.t ->
  string
