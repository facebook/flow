(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type named_binding = {
  remote_name: string;
  local_name: string option;
}

type bindings =
  | Default of string
  | Named of named_binding list
  | NamedType of named_binding list
  | Namespace of string

val add_import :
  options:Js_layout_generator.opts ->
  bindings:bindings ->
  from:string ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t * string) list

val loc_is_type : ast:(Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> bool
