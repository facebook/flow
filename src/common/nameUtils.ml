(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module Map = WrappedMap.Make (struct
  type t = name

  let compare = compare_name
end)

module Set = Flow_set.Make (struct
  type t = name

  let compare = compare_name
end)

(** Converts all the names to display string. Not to be used to determine typechecker behavior. *)
let display_smap_of_namemap map =
  Map.fold (fun name v smap -> SMap.add (display_string_of_name name) v smap) map SMap.empty

let namemap_of_smap map =
  SMap.fold (fun name v namemap -> Map.add (OrdinaryName name) v namemap) map Map.empty
