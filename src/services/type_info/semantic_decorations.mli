(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val compute_from_context :
  Context.t -> loc_of_aloc:(ALoc.t -> Loc.t) -> Lsp.SemanticDecorations.result
