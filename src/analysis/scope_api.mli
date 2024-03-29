(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module With_Loc : Scope_api_sig.S with module L = Loc_sig.LocS

module With_ALoc : Scope_api_sig.S with module L = Loc_sig.ALocS

module With_ILoc : Scope_api_sig.S with module L = Loc_sig.ILocS
