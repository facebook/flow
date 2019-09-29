(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module With_Loc :
  File_sig_sig.S
    with module L = Loc_sig.LocS
     and module Signature_builder_deps = Signature_builder_deps.With_Loc

module With_ALoc :
  File_sig_sig.S
    with module L = Loc_sig.ALocS
     and module Signature_builder_deps = Signature_builder_deps.With_ALoc

val abstractify_locs : With_Loc.t -> With_ALoc.t

val abstractify_tolerable_errors : With_Loc.tolerable_error list -> With_ALoc.tolerable_error list
