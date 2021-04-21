(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L) =
struct
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)

  class env_builder =
    object (_this)
      inherit Ssa_builder.ssa_builder as _super
    end
end
