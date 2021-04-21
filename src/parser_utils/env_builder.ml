(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type refinement =
  | And of refinement * refinement
  | Or of refinement * refinement
  | Not of refinement
  | Truthy

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L) =
struct
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)

  class env_builder =
    object (_this)
      inherit Ssa_builder.ssa_builder as _super

      val mutable expression_refinements = IMap.empty
    end

  let program_with_scope ?(ignore_toplevel = false) program =
    let open Hoister in
    let (loc, _) = program in
    let ssa_walk = new env_builder in
    let bindings =
      if ignore_toplevel then
        Bindings.empty
      else
        let hoist = new hoister ~with_types:true in
        hoist#eval hoist#program program
    in
    ignore @@ ssa_walk#with_bindings loc bindings ssa_walk#program program;
    (ssa_walk#acc, ssa_walk#values)

  let program program =
    let (_, values) = program_with_scope ~ignore_toplevel:true program in
    values
end
