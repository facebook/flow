(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap
type t = (Loc.t * Signature_builder_kind.t) LocMap.t SMap.t

let empty = SMap.empty

let singleton ((loc, x), kind) =
  SMap.singleton x (LocMap.singleton loc kind)

let add ((loc, x), kind) t =
  SMap.add x (match SMap.get x t with
    | Some u -> LocMap.add loc kind u
    | None -> LocMap.singleton loc kind
  ) t

let push entries t =
  List.fold_left (fun t entry -> add entry t) t entries
