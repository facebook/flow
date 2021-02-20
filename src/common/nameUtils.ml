(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

module Map = WrappedMap.Make (struct
  type t = name

  let compare = compare_name
end)

module Set = Set.Make (struct
  type t = name

  let compare = compare_name
end)

(* We might have an SMap that consists only of ordinary names. We only need to use a NameUtils.Map if
  * the map might contain internal names. If we can prove by construction that a map only contains
  * ordinary names, we should use an SMap to avoid losing that information. *)
let smap_mem name map =
  match name with
  | OrdinaryName str -> SMap.mem str map
  | InternalName _
  | InternalModuleName _ ->
    false

let smap_find_opt name map =
  match name with
  | OrdinaryName str -> SMap.find_opt str map
  | InternalName _
  | InternalModuleName _ ->
    None

(* Converts all the names to display string. Not to be used to determine typechecker behavior. *)
let display_smap_of_namemap map =
  Map.fold (fun name v smap -> SMap.add (display_string_of_name name) v smap) map SMap.empty

let namemap_of_smap map =
  SMap.fold (fun name v namemap -> Map.add (OrdinaryName name) v namemap) map Map.empty
