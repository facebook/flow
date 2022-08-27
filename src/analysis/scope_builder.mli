(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (L : Loc_sig.S) (Api : Scope_api_sig.S with module L = L) :
  Scope_builder_sig.S with module L = L and module Api = Api

module With_Loc :
  Scope_builder_sig.S with module L = Loc_sig.LocS and module Api = Scope_api.With_Loc

module With_ALoc :
  Scope_builder_sig.S with module L = Loc_sig.ALocS and module Api = Scope_api.With_ALoc

module With_ILoc :
  Scope_builder_sig.S with module L = Loc_sig.ILocS and module Api = Scope_api.With_ILoc

include module type of With_Loc
