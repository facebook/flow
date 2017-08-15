(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Disjoint sets of checked files (files which we not only parse but also merge)
 *
 * Lazy mode:
 *   Focused files - files which the user cares about.
 *   Dependent files - files which directly or transitively depend on focused files
 *   Dependency files - files on which Focused or Dependent files  directly or transitively depend
 *
 * Non-lazy mode:
 *   Focused files - Every checked file
 *   Dependent files - Empty
 *   Dependency files - Empty
 *)

open Utils_js

(* A file foo.js can be focused, a dependent, and a dependency all at the same time. However, in
 * CheckedSet.t, we just keep track of its most important role. A focused file is more important
 * than a dependent file, which is more important than a dependency file *)
type kind =
  | Focused
  | Dependent
  | Dependency
type t = kind FilenameMap.t

let combine a b = match (a, b) with
| (Focused, _) | (_, Focused) -> Focused
| (Dependent, _) | (_, Dependent) -> Dependent
| _ -> Dependency

let empty = FilenameMap.empty
let of_focused_list = List.fold_left (fun acc f -> FilenameMap.add f Focused acc) empty

let mem = FilenameMap.mem

let add =
  let add_all files kind checked =
    Option.value_map files
      ~f:(fun files ->
        FilenameSet.fold (fun f checked -> FilenameMap.add ~combine f kind checked) files checked
      )
      ~default:checked
  in
  fun ?focused ?dependents ?dependencies checked ->
    checked
    |> add_all focused Focused
    |> add_all dependents Dependent
    |> add_all dependencies Dependency

let remove set_to_remove =
  FilenameMap.filter (fun k _ -> not (FilenameSet.mem k set_to_remove))

let fold f acc checked =
  FilenameMap.fold (fun k _ acc -> f acc k) checked acc

let union = FilenameMap.union ~combine:(fun _ a b -> Some (combine a b))

let diff a b = FilenameMap.filter (fun k _ -> not (FilenameMap.mem k b)) a

let filter_into_set ~f checked = FilenameMap.fold
  (fun key kind acc -> if f kind then FilenameSet.add key acc else acc)
  checked
  FilenameSet.empty

(* Gives you a FilenameSet of all the checked files *)
let all = filter_into_set ~f:(fun _ -> true)
(* Gives you a FilenameSet of all the focused files *)
let focused = filter_into_set ~f:(fun kind -> kind = Focused)
(* Gives you a FilenameSet of all the dependent files *)
let dependents = filter_into_set ~f:(fun kind -> kind = Dependent)
(* Gives you a FilenameSet of all the dependency files *)
let dependencies = filter_into_set ~f:(fun kind -> kind = Dependency)

(* Helper function for debugging *)
let debug_to_string =
  let string_of_set set =
    Utils_js.FilenameSet.elements set
    |> List.map (fun f -> spf "\"%s\"" (Loc.string_of_filename f))
    |> String.concat "\n"
  in
  fun checked ->
    Printf.sprintf "Focused:\n%s\nDependents:\n%s\nDependencies:\n%s"
      (checked |> focused |> string_of_set)
      (checked |> dependents |> string_of_set)
      (checked |> dependencies |> string_of_set)
