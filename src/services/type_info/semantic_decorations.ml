(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp
open Loc_collections

let compute_from_context cx ~loc_of_aloc =
  let decorations =
    Base.List.map
      (Context.refined_locations cx |> ALocMap.keys)
      ~f:(fun loc ->
        {
          SemanticDecorations.range = Lsp.loc_to_lsp_range (loc_of_aloc loc);
          kind = SemanticDecorations.RefinedValue;
        })
  in
  { SemanticDecorations.decorations }
