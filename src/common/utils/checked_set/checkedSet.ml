(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

(* This uses polymorphic compare. Use caution if `kind` becomes a more complex type. *)
let debug_equal = FilenameMap.equal ( = )

let combine a b =
  match (a, b) with
  | (Focused, _)
  | (_, Focused) ->
    Focused
  | (Dependent, _)
  | (_, Dependent) ->
    Dependent
  | _ -> Dependency

let empty = FilenameMap.empty

let is_empty = FilenameMap.is_empty

let of_focused_list = List.fold_left (fun acc f -> FilenameMap.add f Focused acc) empty

let cardinal = FilenameMap.cardinal

let mem = FilenameMap.mem

let add =
  let add_all files kind checked =
    Base.Option.value_map
      files
      ~f:(fun files ->
        FilenameSet.fold (fun f checked -> FilenameMap.add ~combine f kind checked) files checked)
      ~default:checked
  in
  fun ?focused ?dependents ?dependencies checked ->
    checked
    |> add_all focused Focused
    |> add_all dependents Dependent
    |> add_all dependencies Dependency

let remove set_to_remove = FilenameMap.filter (fun k _ -> not (FilenameSet.mem k set_to_remove))

let fold f acc checked = FilenameMap.fold (fun k _ acc -> f acc k) checked acc

let union = FilenameMap.union ~combine:(fun _ a b -> Some (combine a b))

(** [diff a b] removes from [a] every key which exists in [b] and which has an equal or higher
  kind in [b] than it does in [a], where Focused > Dependent > Dependency. So

  {[
    diff
      { A: Focused, B: Focused,   C: Dependency, D: Dependent }
      { A: Focused, B: Dependent, C: Dependent}
    = { B: Focused, D: Dependent }
  ]}
 *)
let diff a b =
  FilenameMap.filter
    (fun k kind1 ->
      let kind2 = FilenameMap.find_opt k b in
      match (kind1, kind2) with
      | (_, None) -> true (* Key doesn't exist in b, so keep k around *)
      | (_, Some Focused) -> false (* Focused removes anything *)
      | (Focused, _) -> true (* Focused survives anything except Focused *)
      | (_, Some Dependent) -> false (* Dependent removes anything except Focused *)
      | (Dependent, Some Dependency) -> true (* Dependent survives Dependency *)
      | (Dependency, Some Dependency) -> false)
    (* Dependency removes Dependency *)
    a

let filter ~f checked = FilenameMap.filter (fun k _ -> f k) checked

let filter_into_set ~f checked =
  FilenameMap.fold
    (fun key kind acc ->
      if f kind then
        FilenameSet.add key acc
      else
        acc)
    checked
    FilenameSet.empty

let count ~f checked =
  FilenameMap.fold
    (fun _key kind acc ->
      if f kind then
        acc + 1
      else
        acc)
    checked
    0

(* Gives you a FilenameSet of all the checked files *)
let all = filter_into_set ~f:(fun _ -> true)

let is_focused kind = kind = Focused

let is_dependent kind = kind = Dependent

let is_dependency kind = kind = Dependency

(* Gives you a FilenameSet of all the focused files *)
let focused = filter_into_set ~f:is_focused

let focused_cardinal = count ~f:is_focused

(* Gives you a FilenameSet of all the dependent files *)
let dependents = filter_into_set ~f:is_dependent

let dependents_cardinal = count ~f:is_dependent

(* Gives you a FilenameSet of all the dependency files *)
let dependencies = filter_into_set ~f:is_dependency

let dependencies_cardinal = count ~f:is_dependency

(* Helper function for debugging *)
let debug_to_string ?limit =
  let string_of_set set =
    let files =
      Utils_js.FilenameSet.elements set
      |> Base.List.map ~f:(fun f -> spf "\"%s\"" (File_key.to_string f))
    in
    let files =
      match limit with
      | None -> files
      | Some n -> ListUtils.first_upto_n n (fun t -> Some (spf "[shown %d/%d]" n t)) files
    in
    String.concat "\n" files
  in
  fun checked ->
    Printf.sprintf
      "Focused:\n%s\nDependents:\n%s\nDependencies:\n%s"
      (checked |> focused |> string_of_set)
      (checked |> dependents |> string_of_set)
      (checked |> dependencies |> string_of_set)

let debug_counts_to_string checked =
  let (focused, dependents, dependencies) =
    FilenameMap.fold
      (fun _ kind (focused, dependents, dependencies) ->
        match kind with
        | Focused -> (focused + 1, dependents, dependencies)
        | Dependent -> (focused, dependents + 1, dependencies)
        | Dependency -> (focused, dependents, dependencies + 1))
      checked
      (0, 0, 0)
  in
  Printf.sprintf "Focused: %d, Dependents: %d, Dependencies: %d" focused dependents dependencies
