(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type binding = Export_index.kind * string

val add_import :
  options:Js_layout_generator.opts ->
  binding:binding ->
  from:string ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  (Loc.t * string) list
